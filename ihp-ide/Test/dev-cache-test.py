"""Real-GHC cache regressions and a reproducible synthetic worktree benchmark.

Run with python3 ihp-ide/Test/dev-cache-test.py (ghc and ghc-pkg on PATH).
No external application, database, Python package or private fixture required.
"""
from __future__ import annotations

import importlib.util
import concurrent.futures
import json
import os
from pathlib import Path
import queue
import shutil
import shlex
import socket
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("dev_cache", Path(__file__).parents[1] / "data/dev-cache.py")
sys.dont_write_bytecode = True
assert spec and spec.loader
cache_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cache_module)


def ghci_arguments(script: str) -> list[str]:
    # Keep the production argv from IHP.IDE.GhciSupport, including RTS settings.
    return ["-threaded", "-fomit-interface-pragmas", "-j", "-O0", "-package-env -",
            "-ignore-dot-ghci", "-ghci-script", script,
            "+RTS", "-A32m", "-n4m", "-H64m", "-Iw60", "-N4", "-Fd1"]


class WorktreeCacheTest(unittest.TestCase):
    def setUp(self) -> None:
        self.original = Path.cwd()
        self.temp = tempfile.TemporaryDirectory(prefix="ihp-dev-cache-test-")
        self.base = Path(self.temp.name)
        self.root = self.base / "source"
        self.root.mkdir()
        os.chdir(self.root)
        self.old_xdg = os.environ.get("XDG_CACHE_HOME")
        os.environ["XDG_CACHE_HOME"] = str(self.base / "host-cache")
        self.git("init", "-q")
        self.git("config", "user.name", "Cache Test")
        self.git("config", "user.email", "cache@example.invalid")
        (self.root / ".gitignore").write_text("build/\n")
        for index in range(24):
            lines = ["{-# LANGUAGE DeriveGeneric #-}", f"module Part{index} where", "import Prelude", "value :: Int", f"value = {index}"]
            records = int(os.environ.get("IHP_CACHE_BENCH_RECORDS", "0"))
            if records:
                lines.insert(3, "import GHC.Generics (Generic)")
                for record in range(records):
                    fields = ", ".join(f"field{record}_{field} :: Maybe Int" for field in range(8))
                    lines.append(f"data Record{record} = Record{record} {{ {fields} }} deriving (Eq, Show, Generic)")
            lines += [f"function{n} :: Int -> Int\nfunction{n} x = x * {n + 1} + value" for n in range(60)]
            (self.root / f"Part{index}.hs").write_text("\n".join(lines) + "\n")
        (self.root / "Main.hs").write_text("module Main where\nimport Prelude\n" +
            "\n".join(f"import qualified Part{i}" for i in range(24)) +
            "\nmain :: IO ()\nmain = print (" + " + ".join(f"Part{i}.value" for i in range(24)) + ")\n")
        self.git("add", ".")
        self.git("commit", "-qm", "Synthetic compiler fixture")
        self.samples: list[dict[str, object]] = []

    def tearDown(self) -> None:
        os.chdir(self.original)
        if self.old_xdg is None:
            os.environ.pop("XDG_CACHE_HOME", None)
        else:
            os.environ["XDG_CACHE_HOME"] = self.old_xdg
        self.temp.cleanup()

    def git(self, *args: str) -> str:
        return subprocess.check_output(["git", *args], text=True, stderr=subprocess.DEVNULL).strip()

    def worktree(self, name: str) -> Path:
        os.chdir(self.root)
        target = self.base / name
        self.git("worktree", "add", "--detach", str(target), "HEAD")
        os.chdir(target)
        return target

    def build(self, label: str, expect_success: bool = True) -> tuple[int, str]:
        arguments = ["--make", "-O0", "Main.hs", "-outputdir", "build/ihp-dev-cache/command/objects", "-o", "build/probe"]
        start = time.monotonic()
        cache = cache_module.Cache("command", arguments)
        try:
            cache.begin()
            result = subprocess.run(["ghc", *arguments], text=True, capture_output=True)
            cache.finish(result.returncode == 0)
        finally:
            cache.close()
        if expect_success:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            value = subprocess.check_output(["build/probe"], text=True).strip()
        else:
            self.assertNotEqual(result.returncode, 0)
            value = "failed"
        count = result.stdout.count("Compiling ")
        self.samples.append({"scenario": label, "seconds": time.monotonic() - start, "compiled_modules": count, "value": value})
        return count, value

    def test_real_worktree_reuse_and_invalidation(self) -> None:
        cold, value = self.build("cold")
        self.assertEqual(cold, 25)
        self.assertEqual(value, "276")
        self.worktree("identical")
        restored, value = self.build("identical-worktree")
        self.assertEqual(value, "276")
        self.assertEqual(restored, 0, "identical worktree must reuse all objects")
        changed = self.worktree("changed")
        source = changed / "Part0.hs"
        source.write_text(source.read_text().replace("value = 0", "value = 1000"))
        changed_count, value = self.build("dirty-leaf-change")
        self.assertEqual(value, "1276", "never execute the previous cached result")
        self.assertLessEqual(changed_count, 2)
        source.write_text(source.read_text().replace("value = 1000", 'value = "bad type"'))
        self.build("failed-compilation", expect_success=False)
        self.worktree("after-failure")
        count, value = self.build("reuse-after-failure")
        self.assertEqual((count, value), (0, "276"))
        print("\nCACHE_BENCHMARK=" + json.dumps(self.samples), flush=True)

    def test_source_race_does_not_publish(self) -> None:
        cache = cache_module.Cache("command", [])
        try:
            cache.begin()
            (cache.objects / "Fake.o").write_bytes(b"object")
            path = self.root / "Part0.hs"
            path.write_text(path.read_text() + "\n")
            cache.finish(True)
            self.assertEqual(list(cache.shared.glob("*/manifest.json")), [])
        finally:
            cache.close()

    def test_executables_are_never_published(self) -> None:
        cache = cache_module.Cache("command", [])
        try:
            cache.begin()
            (cache.objects / "stale-web-binary").write_bytes(b"executable")
            cache.finish(True)
            self.assertEqual(list(cache.shared.glob("*/manifest.json")), [])
        finally:
            cache.close()

    def test_real_ghci_config_reuses_objects(self) -> None:
        ihp_lib = Path(__file__).resolve().parents[1] / "data/lib/IHP"
        (self.root / ".ghci").write_text(":loadFromIHP applicationGhciConfig\n")
        self.git("add", ".ghci")
        self.git("commit", "-qm", "GHCi startup fixture")
        with patch.dict(os.environ, {"IHP_LIB": str(ihp_lib)}):
            baseline_script = self.base / "baseline.ghci"
            baseline_script.write_text((ihp_lib / "sharedApplicationGhciConfig").read_text() + "\n:l Main.hs\n")
            start = time.monotonic()
            baseline = subprocess.run(["ghci", *ghci_arguments(str(baseline_script))],
                                      input="main\n:quit\n", text=True, capture_output=True, timeout=120)
            self.assertIn("276", baseline.stdout, baseline.stdout + baseline.stderr)
            self.assertNotIn("Failed,", baseline.stdout)
            self.samples.append({"scenario": "uncached-bytecode", "seconds": time.monotonic() - start,
                                 "compiled_modules": baseline.stdout.count("Compiling "), "value": "276"})
            for label in ("ghci-cold", "ghci-other-worktree"):
                if label.endswith("worktree"):
                    self.worktree("ghci-peer")
                arguments = ghci_arguments(".ghci")
                start = time.monotonic()
                cache = cache_module.Cache("web", arguments)
                try:
                    arguments = cache.ghci_arguments()
                    cache.begin()
                    result = subprocess.run(["ghci", *arguments],
                                            input="main\n:quit\n", text=True, capture_output=True, timeout=120)
                    success = "276" in result.stdout and "Failed," not in result.stdout
                    cache.finish(success)
                    self.assertTrue(success, result.stdout + result.stderr)
                    count = result.stdout.count("Compiling ")
                    self.samples.append({"scenario": label, "seconds": time.monotonic() - start,
                                         "compiled_modules": count, "value": "276"})
                    if label.endswith("worktree"):
                        self.assertEqual(count, 0, result.stdout + result.stderr)
                    else:
                        self.assertGreaterEqual(count, 25)
                finally:
                    cache.close()
        print("\nCACHE_BENCHMARK=" + json.dumps(self.samples), flush=True)

    def test_seven_concurrent_identical_worktrees_build_once(self) -> None:
        paths = [self.worktree(f"parallel-{i}") for i in range(7)]
        # Each subprocess owns its cwd; threads never change the parent's cwd.
        runner = '''import importlib.util, pathlib, subprocess, sys, json, time
spec = importlib.util.spec_from_file_location("cache", sys.argv[1])
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
args = ["--make", "-O0", "Main.hs", "-outputdir", "build/ihp-dev-cache/command/objects", "-o", "build/probe"]
start = time.monotonic()
c = module.Cache("command", args)
try:
    c.begin()
    result = subprocess.run(["ghc", *args], capture_output=True, text=True)
    c.finish(result.returncode == 0)
    assert result.returncode == 0, result.stderr
    assert subprocess.check_output(["build/probe"], text=True).strip() == "276"
    print(json.dumps({"seconds":time.monotonic()-start,"compiled_modules":result.stdout.count("Compiling ")}))
finally:
    c.close()
'''
        backend = str(Path(__file__).resolve().parents[1] / "data/dev-cache.py")
        def run(path: Path) -> dict[str, object]:
            output = subprocess.check_output(["python3", "-c", runner, backend], cwd=path, text=True)
            return json.loads(output)
        with concurrent.futures.ThreadPoolExecutor(max_workers=7) as pool:
            results = list(pool.map(run, paths))
        self.assertEqual(sum(int(r["compiled_modules"]) for r in results), 25)
        events = [json.loads(line) for line in (self.root / ".git/ihp-dev-cache-v1/events.jsonl").read_text().splitlines()]
        active = peak = 0
        for event in sorted(events, key=lambda event: event["time"]):
            active += {"acquired": 1, "finished": -1}.get(event["event"], 0)
            peak = max(peak, active)
        self.assertLessEqual(peak, 2)
        self.assertEqual(active, 0)
        print("\nCACHE_PARALLEL=" + json.dumps(results), flush=True)

    def test_worker_config_reuses_objects(self) -> None:
        ihp_lib = Path(__file__).resolve().parents[1] / "data/lib/IHP"
        with patch.dict(os.environ, {"IHP_LIB": str(ihp_lib)}):
            for peer in (False, True):
                if peer:
                    self.worktree("worker-peer")
                Path("build").mkdir(exist_ok=True)
                Path("build/RunJobs.hs").write_text(Path("Main.hs").read_text().replace("module Main where", "module RunJobs where"))
                Path("build/.ghci-worker").write_text(":loadFromIHP workerApplicationGhciConfig\n")
                cache = cache_module.Cache("worker", ghci_arguments("build/.ghci-worker"))
                try:
                    arguments = cache.ghci_arguments()
                    cache.begin()
                    result = subprocess.run(["ghci", *arguments], input="main\n:quit\n",
                                            text=True, capture_output=True, timeout=120)
                    success = "276" in result.stdout and "Failed," not in result.stdout
                    cache.finish(success)
                    self.assertTrue(success, result.stdout + result.stderr)
                    self.assertEqual(result.stdout.count("Compiling "), 0 if peer else 25)
                    if not peer:
                        self.assertIn("build/RunJobs.hs", result.stdout)
                finally:
                    cache.close()

    def test_helper_death_releases_locks(self) -> None:
        backend = str(Path(__file__).resolve().parents[1] / "data/dev-cache.py")
        environment = {**os.environ, "IHP_LIB": str(Path(__file__).resolve().parents[1] / "data/lib/IHP")}
        Path(".ghci").write_text(":loadFromIHP applicationGhciConfig\n")
        with subprocess.Popen(["python3", backend, "web", json.dumps(ghci_arguments(".ghci"))],
                              stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, env=environment) as helper:
            assert helper.stdin and helper.stdout
            json.loads(helper.stdout.readline())
            helper.stdin.write("begin\n")
            helper.stdin.flush()
            self.assertEqual(helper.stdout.readline().strip(), "ok")
            helper.kill()
            helper.wait(timeout=15)
        # Nonblocking probes prove both lifetime and transaction locks are gone.
        lock_paths = (list(Path("build/ihp-dev-cache/web").glob("*.lock"))
                      + list((self.base / "host-cache/ihp/build-slots").iterdir())
                      + list(Path(".git/ihp-dev-cache-v1").glob("building-*")))
        for path in lock_paths:
            with path.open("r+") as handle:
                cache_module.fcntl.flock(handle, cache_module.fcntl.LOCK_EX | cache_module.fcntl.LOCK_NB)

    @unittest.skipUnless(os.environ.get("IHP_TEST_DEV_WORKER"), "set IHP_TEST_DEV_WORKER to the built RunDevWorker executable")
    def test_native_worker_start_reload_and_release(self) -> None:
        backend = Path(__file__).resolve().parents[1] / "data/dev-cache.py"
        helper = self.base / "cache-helper"
        helper.write_text("#!/bin/sh\nexec " + shlex.quote(sys.executable) + " " + shlex.quote(str(backend)) + ' "$@"\n')
        helper.chmod(0o700)
        Path(".ghci").write_text(":loadFromIHP applicationGhciConfig\n")
        Path("build/Generated").mkdir(parents=True)
        Path("build/Generated/Types.hs").write_text("module Generated.Types where\n")
        worker_source = "module RunJobs where\nimport Prelude\nimport Control.Concurrent\nimport System.IO\nmain :: IO ()\nmain = hSetBuffering stdout LineBuffering >> putStrLn \"Starting worker v1\" >> threadDelay 600000000\n"
        Path("build/RunJobs.hs").write_text(worker_source)
        environment = {**os.environ, "IHP_DEV_CACHE_HELPER": str(helper), "IHP_DEV_WRAP_DIRENV": "0",
                       "IHP_LIB": str(backend.parent / "lib/IHP")}
        with subprocess.Popen([os.environ["IHP_TEST_DEV_WORKER"]], stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT, text=True, env=environment) as worker:
            assert worker.stdout
            output: queue.Queue[str] = queue.Queue()
            def read() -> None:
                assert worker.stdout
                for line in worker.stdout:
                    output.put(line)
                output.put("")
            threading.Thread(target=read, daemon=True).start()
            def until(marker: str) -> None:
                lines = []
                deadline = time.monotonic() + 120
                while True:
                    try:
                        line = output.get(timeout=max(0.01, deadline - time.monotonic()))
                    except queue.Empty:
                        self.fail("Worker timeout: " + "".join(lines))
                    self.assertTrue(line, "Worker exited: " + "".join(lines))
                    lines.append(line)
                    if marker in line:
                        break
            try:
                until("[worker] worker started.")
                events_path = self.root / ".git/ihp-dev-cache-v1/events.jsonl"
                self.assertIn('"event": "finished"', events_path.read_text())
                Path("build/RunJobs.hs").write_text(worker_source.replace("v1", "v2"))
                with socket.socket(socket.AF_UNIX) as signal:
                    signal.connect("build/.dev-worker.sock")
                    signal.sendall(b"reload\n")
                until("Starting worker v2")
                until("[worker] worker started.")
                self.assertEqual(events_path.read_text().count('"event": "finished"'), 2)
            finally:
                worker.terminate()
                worker.wait(timeout=20)

    def test_corrupt_snapshot_is_rejected(self) -> None:
        cache = cache_module.Cache("command", [])
        try:
            cache.begin()
            (cache.objects / "Fixture.o").write_bytes(b"valid-object")
            cache.finish(True)
            snapshot = next(cache.shared.glob("*/manifest.json")).parent
            (snapshot / "objects/Fixture.o").write_bytes(b"corrupt")
        finally:
            cache.close()
        self.worktree("corruption-peer")
        peer = cache_module.Cache("command", [])
        try:
            peer.begin()
            self.assertEqual(list(peer.objects.iterdir()), [])
            peer.finish(False)
        finally:
            peer.close()

    def test_absolute_application_dependency_is_rejected(self) -> None:
        cache = cache_module.Cache("command", [])
        try:
            cache.begin()
            (cache.objects / "Fixture.hi").write_bytes(str(cache.root).encode() + b"/schema.sql")
            cache.finish(True)
            self.assertEqual(list(cache.shared.glob("*/manifest.json")), [])
        finally:
            cache.close()

    def test_changed_compile_arguments_miss(self) -> None:
        cache = cache_module.Cache("command", ["-O0"])
        try:
            cache.begin()
            (cache.objects / "Fixture.o").write_bytes(b"object")
            cache.finish(True)
        finally:
            cache.close()
        peer = cache_module.Cache("command", ["-O1"])
        try:
            peer.begin()
            self.assertEqual(list(peer.objects.iterdir()), [])
            peer.finish(False)
        finally:
            peer.close()

    def test_live_helper_protocol_and_ghci_reload(self) -> None:
        ihp_lib = Path(__file__).resolve().parents[1] / "data/lib/IHP"
        backend = str(Path(__file__).resolve().parents[1] / "data/dev-cache.py")
        (self.root / ".ghci").write_text(":loadFromIHP applicationGhciConfig\n:set prompt \"\"\n")
        self.git("add", ".ghci")
        self.git("commit", "-qm", "Live reload fixture")
        environment = {**os.environ, "IHP_LIB": str(ihp_lib)}
        helper = subprocess.Popen(["python3", backend, "web", json.dumps(ghci_arguments(".ghci"))],
                                  stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, env=environment)
        assert helper.stdin and helper.stdout
        compiler: subprocess.Popen[str] | None = None
        def send(event: str) -> None:
            assert helper.stdin and helper.stdout
            helper.stdin.write(event + "\n")
            helper.stdin.flush()
            self.assertEqual(helper.stdout.readline().strip(), "ok")
        try:
            arguments = json.loads(helper.stdout.readline())
            send("begin")
            compiler = subprocess.Popen(["ghci", *arguments], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT, text=True)
            assert compiler.stdin and compiler.stdout
            output: queue.Queue[str] = queue.Queue()
            def read_output() -> None:
                assert compiler and compiler.stdout
                for line in compiler.stdout:
                    output.put(line)
                output.put("")
            threading.Thread(target=read_output, daemon=True).start()
            def until(marker: str) -> str:
                lines = []
                deadline = time.monotonic() + 120
                while True:
                    try:
                        line = output.get(timeout=max(0.01, deadline - time.monotonic()))
                    except queue.Empty:
                        self.fail("Timed out waiting for " + marker + ":\n" + "".join(lines))
                    self.assertTrue(line, "GHCi ended before " + marker)
                    lines.append(line)
                    if marker in line or (marker == "modules loaded." and "modules reloaded." in line):
                        return "".join(lines)
            until("modules loaded.")
            send("success")
            # The IDE serves requests here without holding a build slot.
            for replacement, expected in [("value = 1000", "1276"), ("value = 2000", "2276")]:
                source = self.root / "Part0.hs"
                lines = source.read_text().splitlines()
                source.write_text("\n".join(replacement if line.startswith("value = ") else line for line in lines) + "\n")
                send("begin")
                compiler.stdin.write(":r\n")
                compiler.stdin.flush()
                until("modules loaded.")
                send("success")
                compiler.stdin.write('import qualified System.IO\nmain\nSystem.IO.putStrLn "IHP-TEST-END"\nSystem.IO.hFlush System.IO.stdout\n')
                compiler.stdin.flush()
                self.assertIn(expected, until("IHP-TEST-END"))
            compiler.stdin.write(":quit\n")
            compiler.stdin.flush()
            compiler.wait(timeout=15)
        finally:
            if compiler and compiler.poll() is None:
                compiler.kill()
                compiler.wait(timeout=15)
            if compiler:
                if compiler.stdin:
                    compiler.stdin.close()
                if compiler.stdout:
                    compiler.stdout.close()
            if helper.stdin:
                helper.stdin.close()
            helper.wait(timeout=15)
            if helper.stdout:
                helper.stdout.close()


if __name__ == "__main__":
    if not shutil.which("ghc"):
        raise SystemExit("ghc and ghc-pkg must be available on PATH")
    unittest.main()
