# Development cache benchmark notes

Measured on 2026-09-05, macOS 27.0 arm64 (Mac16,13, 10 CPUs, 24 GiB RAM),
GHC 9.14.1. The installed package-database dump's SHA-256 was
`6233e100f0b3ae98470ae0cc10a13decbb0f64178dfbc7e4609804fc6ea298a9`.
These are synthetic,
reproducible compiler workloads, not a production application or HTTP-readiness
benchmark. Other compiler processes, including an IHP package build, were active.
Treat the ranges as contended observations, not an isolated hardware benchmark.

## Record-heavy workload: three trials

The fixture has 25 modules, 960 eight-field record types deriving `Eq`, `Show`
and `Generic`, plus 1,440 small integer functions. Each scenario executes the
program and verifies its result. Every new worktree is created by Git, rather
than copying a build directory over the original sources.

The baseline uses IHP's shared GHCi configuration in its default bytecode mode.
The cache scenarios use the same production GHCi arguments, including RTS flags,
with the opt-in object-mode configuration. An empty cache is measured before a
new worktree restores its completed snapshot. Durations include helper startup,
restoration, GHCi compilation/loading, execution and publication, through process
exit. They exclude fixture and Git-worktree creation.

| Scenario | Trial 1 (s) | Trial 2 (s) | Trial 3 (s) | Median (s) | Modules compiled |
| --- | ---: | ---: | ---: | ---: | ---: |
| Default uncached bytecode | 37.797750 | 36.844963 | 23.999898 | 36.844963 | 25 |
| Object cache, first fill | 72.833027 | 77.543384 | 50.118148 | 72.833027 | 25 |
| Object cache, new worktree hit | 22.143540 | 20.023205 | 18.127055 | 20.023205 | 0 |

The warm-cache median is 45.7% lower than default bytecode in this workload.
The individual reductions are 41.4%, 45.7% and 24.5%. The first object build is
roughly twice as expensive as bytecode. This is a reuse tradeoff, not a free
cold-build optimization. A cache hit still has substantial loading/linking cost.

Reproduce each trial from the IHP repository with its compiler environment:

```sh
IHP_CACHE_BENCH_RECORDS=40 python3 ihp-ide/Test/dev-cache-test.py \
    WorktreeCacheTest.test_real_ghci_config_reuses_objects
```

Each invocation creates an isolated cache and prints `CACHE_BENCHMARK` JSON with
full-precision samples. The default fixture has no additional record types.

## Small workload: the cache loses against bytecode

Do not extrapolate the record-heavy result to small applications. Three trials
with the default 25-module fixture and production GHCi arguments measured:

| Scenario | Trial 1 (s) | Trial 2 (s) | Trial 3 (s) |
| --- | ---: | ---: | ---: |
| Default uncached bytecode | 6.060114 | 4.323774 | 23.949832 |
| Object cache, first fill | 20.848168 | 17.920959 | 67.053107 |
| Object cache, new worktree hit | 16.020111 | 15.821523 | 68.567823 |

All cache hits compiled zero modules, but remained slower than recompiling this
small workload to bytecode. A 120-record variant also lost: 7.091287 s bytecode,
18.116398 s cold objects and 12.848072 s warm objects. This is why the feature is
opt-in and why module-count reductions alone must not be presented as time gains.
The third run occurred later during the full regression suite with much higher
machine contention; no timing samples were excluded from this comparison.

## Dirty edits and failed compilations

The direct `ghc --make -O0` regression measures object-cache mechanics separately
from GHCi package loading. One complete passing run produced:

| Scenario | Total (s) | Modules compiled | Executed result |
| --- | ---: | ---: | --- |
| Cold | 14.317000 | 25 | 276 |
| Identical new worktree | 4.495484 | 0 | 276 |
| Dirty leaf edit | 4.946919 | 1 | 1276 |
| Type error in that leaf | 2.260401 | 1 | Compilation fails |
| Original source in another worktree after failure | 4.621862 | 0 | 276 |

The changed value is checked by executing the freshly linked program; an old
cached executable cannot satisfy the test. Failed compilation does not poison
the completed snapshot. Two successive live GHCi reloads similarly verify new
results (1276 and 2276) through the actual helper protocol.

A later highly contended full-suite repeat measured 79.588436, 32.678405,
17.113989, 16.505625 and 26.897343 s in the same scenario order, with unchanged
module counts and executed results. These timings include startup and I/O;
their substantial variation is not an isolated compiler-speed measurement.

## Seven concurrent worktrees

Three real-GHC trials started seven identical cold worktrees against an empty
shared cache. In each trial exactly one process compiled 25 modules and the
other six compiled zero: 25 module compilations instead of the 175 required by
seven independent cold object builds. The journal verified at most two acquired
build slots simultaneously and no remaining active transaction after completion.

Total durations in submission order, including cache initialization, queue wait
and linking:

- Trial 1: 16.665764, 18.707463, 18.850773, 20.731654, 16.574022, 14.340633, 20.501048 s.
- Trial 2: 28.134706, 18.395796, 25.271422, 22.231299, 27.862253, 21.843975, 25.528208 s.
- Trial 3 (later full suite under heavier contention): 95.235129, 86.148495, 71.746888, 72.315574, 57.720129, 95.943267, 86.381437 s.

This proves build deduplication and the slot cap. It is not a measured seven-way
wall-time comparison against uncached bytecode. JSONL acquisition records expose
lock waits separately; the first trial's transaction waits ranged from 0.06 s
to 14.13 s, rather than being counted as compiler execution.

## Validation boundaries

GHCi restoration and live reloads have been exercised on GHC 9.12 and 9.14.
Correctness checks cover source races, changed flags, corrupted snapshots,
absolute application dependency rejection, exclusion of linked executables,
worker-specific startup and helper-death lock release. A compiled `RunDevWorker`
also starts a synthetic one-module worker, reloads it through its Unix socket,
executes the changed worker and releases each build slot before running jobs.
This caught and fixed the singular `one module loaded` completion-message case.
Nix checks run the
synthetic suite with both compiler sets when available.

These measurements do not establish a twelve-second schema reload, production
Nix cache reuse, arbitrary `runghc` caching or an end-to-end web/worker readiness
improvement in an existing application. Table-scoped TypedSQL invalidation and
generated relation decoupling require their own correctness and timing evidence.
