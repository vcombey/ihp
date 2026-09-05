#!/usr/bin/env python3
"""Host-local, trusted-source development cache. Python standard library only.

The IDE supplies explicit begin/success/failure events, not parsed compiler logs.
Objects are private to a worktree/lane; only completed snapshots are shared.
"""
from __future__ import annotations

import contextlib
import fcntl
import hashlib
import json
import os
from pathlib import Path
import shutil
import shlex
import subprocess
import sys
import tempfile
import time
from typing import Iterator
import uuid

OBJECT_SUFFIXES = {".o", ".hi", ".dyn_o", ".dyn_hi"}


def command(*args: str) -> str:
    return subprocess.check_output(args, text=True).strip()


def digest(value: object) -> str:
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def private_directory(path: Path) -> Path:
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    if path.is_symlink() or path.stat().st_uid != os.getuid() or path.stat().st_mode & 0o022:
        raise RuntimeError(f"cache directory must be owned by this user and not writable by others: {path}")
    return path


@contextlib.contextmanager
def lock(path: Path) -> Iterator[None]:
    fd = os.open(path, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        os.close(fd)


class Cache:
    def __init__(self, role: str, arguments: list[str]) -> None:
        if role not in {"web", "worker", "command"}:
            raise ValueError("unknown cache role")
        self.role = role
        self.root = Path(command("git", "rev-parse", "--show-toplevel")).resolve()
        if Path.cwd().resolve() != self.root:
            raise RuntimeError("run the cache from the project root")
        common = Path(command("git", "rev-parse", "--git-common-dir")).resolve()
        self.shared = private_directory(common / "ihp-dev-cache-v1")
        self.local = private_directory(self.root / "build" / "ihp-dev-cache" / role)
        self.objects = self.local / "objects"
        self.session = uuid.uuid4().hex
        self.arguments = arguments
        self.slots = int(os.environ.get("IHP_DEV_BUILD_SLOTS", "2"))
        if not 1 <= self.slots <= 64:
            raise ValueError("IHP_DEV_BUILD_SLOTS must be between 1 and 64")
        self.jobs = max(1, (os.cpu_count() or 1) // self.slots)
        cache_home = Path(os.environ.get("XDG_CACHE_HOME", str(Path.home() / ".cache")))
        self.budget = private_directory(cache_home / "ihp" / "build-slots")
        self.slot_fd: int | None = None
        self.content_fd: int | None = None
        self.first = True
        self.before: dict[str, tuple[str, int, int]] = {}
        self.compatibility = ""
        self.initial_compatibility: str | None = None
        if os.environ.get("IHP_DEV_GHCI", "ghci") != "ghci":
            raise RuntimeError("disable IHP_DEV_GHCI overrides before enabling the integrated cache")
        self.lane_fd = os.open(self.local / "session.lock", os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
        # A second IDE for the same worktree/lane must not mutate a live GHCi's objects.
        fcntl.flock(self.lane_fd, fcntl.LOCK_EX)
        self.runtime = [command("ghc", "--print-libdir"), command("ghc", "--info"),
                        command("ghc-pkg", "dump"), os.environ.get("IHP_LIB", ""),
                        os.environ.get("IHP_RELATION_SUPPORT", ""), arguments, self.jobs,
                        hashlib.sha256(Path(__file__).read_bytes()).hexdigest()]
        self.ghc_version = tuple(int(p) for p in command("ghc", "--numeric-version").split(".")[:2])
        for name in ("sharedApplicationGhciConfig", "applicationGhciConfig", "workerApplicationGhciConfig"):
            config = Path(os.environ.get("IHP_LIB", "")) / name
            self.runtime.append(hashlib.sha256(config.read_bytes()).hexdigest() if config.is_file() else None)
        self.runtime.append([os.environ.get(name) for name in ("GHC_PACKAGE_PATH", "GHC_ENVIRONMENT")])

    def event(self, event: str, **fields: object) -> None:
        record = {"session": self.session, "role": self.role, "event": event,
                  "time": time.time(), **fields}
        fd = os.open(self.shared / "events.jsonl", os.O_WRONLY | os.O_CREAT | os.O_APPEND | os.O_NOFOLLOW, 0o600)
        try:
            os.write(fd, (json.dumps(record) + "\n").encode())
        finally:
            os.close(fd)

    def inputs(self) -> dict[str, tuple[str, int, int]]:
        names = set(subprocess.check_output(
            ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"]
        ).decode().split("\0")) - {""}
        generated = self.root / "build" / "Generated"
        if generated.exists():
            names.update(str(p.relative_to(self.root)) for p in generated.rglob("*") if p.is_file())
        if (self.root / "build/RunJobs.hs").exists():
            names.add("build/RunJobs.hs")
        extra = json.loads(os.environ.get("IHP_DEV_CACHE_INPUTS", "[]"))
        if not isinstance(extra, list) or not all(isinstance(p, str) for p in extra):
            raise ValueError("IHP_DEV_CACHE_INPUTS must be a JSON array of relative file paths")
        names.update(extra)
        result: dict[str, tuple[str, int, int]] = {}
        for name in sorted(names):
            if name.startswith("build/ihp-dev-cache/") or Path(name).suffix in OBJECT_SUFFIXES:
                continue
            path = self.root / name
            if not path.exists():
                continue
            if not path.is_file() or path.is_symlink() or not path.resolve().is_relative_to(self.root):
                raise RuntimeError(f"cache input must be a regular project file: {name}")
            stat = path.stat()
            result[name] = (hashlib.sha256(path.read_bytes()).hexdigest(), stat.st_mtime_ns, stat.st_ctime_ns)
        return result

    def acquire(self) -> None:
        while self.slot_fd is None:
            if time.monotonic() - self.queued > 1800:
                raise TimeoutError("timed out waiting for a development build slot")
            for slot in range(self.slots):
                fd = os.open(self.budget / str(slot), os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
                try:
                    fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                except BlockingIOError:
                    os.close(fd)
                    continue
                self.slot_fd = fd
                self.acquired = time.monotonic()
                self.event("acquired", slot=slot, wait_seconds=self.acquired - self.queued)
                break
            if self.slot_fd is None:
                time.sleep(0.05)

    def begin(self) -> None:
        if self.slot_fd is not None:
            raise RuntimeError("compilation already active")
        self.queued = time.monotonic()
        self.event("queued")
        self.before = self.inputs()
        structural = {p: v[0] for p, v in self.before.items() if not p.endswith((".hs", ".lhs", ".hs-boot"))}
        self.compatibility = digest([self.runtime, structural, self.role])
        if self.initial_compatibility is None:
            self.initial_compatibility = self.compatibility
        # Identical cold checkouts should build once, not occupy every host slot.
        # Acquire this before the host slot so a duplicate waiter consumes no CPU budget.
        content = digest([self.compatibility, {p: v[0] for p, v in self.before.items()}])
        self.content_fd = os.open(self.shared / ("building-" + content), os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
        while True:
            try:
                fcntl.flock(self.content_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() - self.queued > 1800:
                    raise TimeoutError("timed out waiting for an identical development build")
                time.sleep(0.05)
        exact_snapshot = self.shared / (self.compatibility + "-" + digest({p: v[0] for p, v in self.before.items()}))
        if (exact_snapshot / "manifest.json").is_file():
            os.close(self.content_fd)
            self.content_fd = None
        self.acquire()
        if self.first:
            self.restore()
            self.first = False

    def restore(self) -> None:
        # Do not use the previous mutable lane contents as a completed snapshot.
        if self.objects.exists():
            if self.objects.is_symlink():
                raise RuntimeError("object directory must not be a symlink")
            shutil.rmtree(self.objects)
        self.objects.mkdir()
        with lock(self.shared / "snapshots.lock"):
            candidates = sorted(self.shared.glob(self.compatibility + "-*"),
                                key=lambda p: p.stat().st_mtime_ns, reverse=True)
            exact = digest({p: v[0] for p, v in self.before.items()})
            candidates.sort(key=lambda p: not p.name.endswith(exact))
            for snapshot in candidates:
                if snapshot.is_symlink() or not (snapshot / "manifest.json").is_file():
                    continue
                try:
                    manifest = json.loads((snapshot / "manifest.json").read_text())
                    objects = manifest["objects"]
                    if not isinstance(objects, dict):
                        continue
                    for name, expected in objects.items():
                        path = snapshot / "objects" / name
                        if (Path(name).is_absolute() or ".." in Path(name).parts or path.is_symlink()
                                or not path.resolve().is_relative_to(snapshot.resolve())
                                or path.suffix not in OBJECT_SUFFIXES):
                            raise ValueError("invalid snapshot path")
                        if hashlib.sha256(path.read_bytes()).hexdigest() != expected:
                            raise ValueError("damaged snapshot")
                    for name in objects:
                        target = self.objects / name
                        target.parent.mkdir(parents=True, exist_ok=True)
                        shutil.copy2(snapshot / "objects" / name, target)
                    self.event("restored", objects=len(objects), exact=snapshot.name.endswith(exact))
                    return
                except (OSError, ValueError, KeyError, TypeError):
                    continue
        self.event("miss")

    def finish(self, success: bool) -> None:
        if self.slot_fd is None:
            raise RuntimeError("no active compilation")
        try:
            if success and self.inputs() == self.before and self.compatibility == self.initial_compatibility:
                self.publish()
            else:
                self.event("not-published", reason="inputs-or-configuration-changed" if success else "compile-failed")
        finally:
            self.event("finished", success=success, run_seconds=time.monotonic() - self.acquired)
            print(f"[IHP cache] wait {self.acquired - self.queued:.2f}s; compile transaction {time.monotonic() - self.acquired:.2f}s; success={success}", file=sys.stderr)
            os.close(self.slot_fd)
            self.slot_fd = None
            if self.content_fd is not None:
                os.close(self.content_fd)
                self.content_fd = None

    def publish(self) -> None:
        sources = {p: v[0] for p, v in self.before.items()}
        target = self.shared / (self.compatibility + "-" + digest(sources))
        with lock(self.shared / "snapshots.lock"):
            if target.exists():
                return
            with tempfile.TemporaryDirectory(prefix=".publishing-", dir=self.shared) as temporary:
                staging = Path(temporary) / "snapshot"
                (staging / "objects").mkdir(parents=True)
                objects: dict[str, str] = {}
                # Never cache a linked executable. GHCi links current objects itself.
                for source in self.objects.rglob("*"):
                    if not source.is_file() or source.is_symlink() or source.suffix not in OBJECT_SUFFIXES:
                        continue
                    data = source.read_bytes()
                    # Absolute application paths in TH dependencies cannot be relocated safely.
                    if source.suffix in {".hi", ".dyn_hi"} and str(self.root).encode() in data:
                        self.event("not-published", reason="absolute-project-dependency")
                        return
                    name = str(source.relative_to(self.objects))
                    dest = staging / "objects" / name
                    dest.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(source, dest)
                    objects[name] = hashlib.sha256(data).hexdigest()
                if not objects:
                    self.event("not-published", reason="no-objects")
                    return
                (staging / "manifest.json").write_text(json.dumps({"sources": sources, "objects": objects}))
                staging.rename(target)
                self.event("published", objects=len(objects))
            snapshots = sorted((p for p in self.shared.iterdir()
                                if len(p.name) == 129 and p.name[64] == "-"
                                and all(c in "0123456789abcdef" for c in p.name.replace("-", ""))
                                and p.is_dir() and not p.is_symlink()),
                               key=lambda p: p.stat().st_mtime_ns, reverse=True)
            # Bounded, disposable cache; never traverse anything outside this namespace.
            used = 0
            for index, old in enumerate(snapshots):
                used += sum(p.stat().st_size for p in old.rglob("*") if p.is_file() and not p.is_symlink())
                if old != target and (index >= 16 or used > 8 * 1024**3):
                    shutil.rmtree(old)

    def ghci_arguments(self) -> list[str]:
        script = self.root / (".ghci" if self.role == "web" else "build/.ghci-worker")
        contents = script.read_text()
        self.runtime.append(contents)
        marker = ":loadFromIHP " + ("applicationGhciConfig" if self.role == "web" else "workerApplicationGhciConfig")
        if contents.count(marker) != 1:
            raise RuntimeError("development cache requires one standard IHP configuration marker")
        shared = (Path(os.environ["IHP_LIB"]) / "sharedApplicationGhciConfig").read_text()
        includes: list[str] = []
        def filter_settings(text: str) -> str:
            lines = []
            for line in text.splitlines():
                if line.strip() in {":set -fbyte-code", ":set -j"}:
                    continue
                if self.ghc_version >= (9, 14) and line.startswith(":set -i"):
                    includes.extend(shlex.split(line[len(":set "):]))
                else:
                    lines.append(line)
            return "\n".join(lines) + "\n"
        settings = filter_settings(shared)
        contents = filter_settings(contents)
        relative = str(self.local.relative_to(self.root))
        settings += f"\n:set -fobject-code -outputdir {relative}/objects -j{self.jobs}\n"
        if self.ghc_version < (9, 14):
            settings += ":l " + ("Main.hs" if self.role == "web" else "build/RunJobs.hs") + "\n"
        (self.local / "settings.ghci").write_text(settings)
        (self.local / "startup.ghci").write_text(contents.replace(marker, f":script {relative}/settings.ghci"))
        arguments = list(self.arguments)
        index = arguments.index("-ghci-script") + 1
        arguments[index] = relative + "/startup.ghci"
        if self.ghc_version >= (9, 14):
            # GHC 9.14 distinguishes the interactive unit from the main home unit.
            # Put source paths and objects on main, not interactive-ghci.
            target = "Main.hs" if self.role == "web" else "build/RunJobs.hs"
            unit = ["-this-unit-id=main", *includes, f"-outputdir={relative}/objects", target]
            (self.local / "main.unit").write_text("\n".join(json.dumps(a) for a in unit) + "\n")
            arguments = ["-unit", "@" + relative + "/main.unit", *arguments]
        return arguments

    def close(self) -> None:
        if self.slot_fd is not None:
            os.close(self.slot_fd)
            self.slot_fd = None
        if self.content_fd is not None:
            os.close(self.content_fd)
            self.content_fd = None
        os.close(self.lane_fd)


def main() -> None:
    role, raw_arguments = sys.argv[1:]
    arguments = json.loads(raw_arguments)
    if not isinstance(arguments, list) or not all(isinstance(a, str) for a in arguments):
        raise ValueError("expected GHCi argument array")
    cache = Cache(role, arguments)
    try:
        print(json.dumps(cache.ghci_arguments()), flush=True)
        for line in sys.stdin:
            event = line.strip()
            if event == "begin":
                cache.begin()
            elif event in {"success", "failure"}:
                cache.finish(event == "success")
            else:
                raise ValueError("unknown lifecycle event")
            print("ok", flush=True)
    finally:
        cache.close()


if __name__ == "__main__":
    main()
