# Sharing Development Builds Between Git Worktrees

## Enable the cache

IHP can share completed development compiler objects between trusted worktrees
of the same Git repository. This is opt-in; the default bytecode development
mode and production Nix builds are unchanged.

In your application's existing `perSystem.ihp` configuration:

```nix
devBuildCache = {
    enable = true;
    slots = 2;
    # Include ignored files read by Template Haskell or preprocessors:
    extraInputs = [];
};
```

Reload the development environment and restart `devenv up`. Both the web process
and the separate job worker participate automatically. No project-specific GHCi
wrapper is needed. Use the same `slots` value in every participating project on
the host. This feature currently requires a POSIX host (Linux or macOS), Git and
the standard IHP GHCi configuration marker. Nix supplies Python; no Python
packages need to be installed.

## What is shared

Each worktree and process role compiles into its own
`build/ihp-dev-cache/<role>/objects` directory. A successful compilation publishes
an immutable snapshot under the Git common directory's `ihp-dev-cache-v1`.
Another worktree copies a compatible snapshot into its own private directory.
Concurrent compilers never write into a shared object directory.

Only GHC interfaces and objects are cached. Linked application executables are
never cached: GHCi links the current object graph. Files keep their original
timestamps; the cache does not touch source files or manufacture freshness.
GHC's native source and interface fingerprints decide which Haskell modules
need recompilation after restoration.

Web and worker lanes are separate. Objects are shared across worktrees within
each lane, not between the two lanes. Seven identical cold web worktrees have
one initial compiler transaction; the others wait for its snapshot and then
reuse it. Different source trees can compile concurrently within the host limit.

## Compatibility and invalidation

The compatibility key includes the compiler, installed package database, IHP
GHCi configuration, compiler arguments, process role, helper implementation and
non-Haskell project inputs. Tracked and non-ignored untracked files are hashed,
along with generated model sources and the worker entry point. Ignored custom
Template Haskell inputs must be declared in `extraInputs`.

The snapshot is published only if the inputs still have the same content and
file metadata after compilation. Failures and source edits during compilation
do not publish. Snapshots have checksummed manifests and are restored only after
validation. Retention keeps at most 16 snapshots within an 8 GiB target budget;
the just-published snapshot is retained even if it alone exceeds that budget.

Absolute application paths embedded in interfaces (for example by
`addDependentFile`) prevent safe relocation. The cache refuses to publish such
a snapshot and reports `absolute-project-dependency`. Template Haskell should
register application dependencies relative to the project root; immutable
framework dependencies in the Nix store can remain absolute. In particular,
TypedSQL needs relocatable schema dependency paths to benefit from this cache.

This is a trusted-source, same-user local cache, not a remote or adversarial
build cache. Symlink and external project inputs are unsupported. Template
Haskell depending on undeclared external state, network responses, environment
variables or database contents cannot be made hermetic by copying GHC objects.
Disable the cache for such builds. Restart the IDE after changing compilation
configuration; a running compiler is not reconfigured by a file watcher.

## Scheduling and measurements

The IDE explicitly starts and finishes cache transactions around initial
compilation and reloads. A host build slot is released before the application
serves requests; an idle development server does not reserve a compiler slot.
Duplicate cold builds wait on a content lock before acquiring a host slot.
Kernel locks are released when the helper exits. Each compiler receives a
bounded job count derived from the host CPU count and configured slot count.

The helper prints queue wait and compilation-transaction duration separately.
The JSON-lines event journal at `ihp-dev-cache-v1/events.jsonl` records a session
identifier, role, queue/acquisition/completion times, snapshot hits and failures.
Transaction duration includes restore and publication I/O; it is not a pure CPU
compilation measurement. The journal is not a Codex-specific session metric and
is not automatically rotated in this initial implementation.

## Limits and verification

Cache reuse removes redundant compilation, not package loading, dynamic linking,
database startup or application initialization. Object mode can be slower than
bytecode on a cold build. A schema change still invalidates genuinely dependent
code; this feature does not promise a fixed schema-edit latency. Table-scoped SQL
dependencies and smaller generated-model dependency graphs are complementary
optimizations, not substitutes for the cache.

Custom GHCi wrappers (`IHP_DEV_GHCI`) are rejected when the cache is enabled.
On GHC 9.14, the application belongs to the main home unit and loads after startup
scripts; custom script commands must not require `Main` to be loaded already.
Arbitrary `runghc` commands and application-specific OpenAPI generators are not
automatically cached or scheduled by this integration.

From the IHP repository with its compiler environment loaded:

```sh
python3 ihp-ide/Test/dev-cache-test.py
nix build .#checks.aarch64-darwin.dev-build-cache
nix build .#checks.aarch64-darwin.ghc914-dev-build-cache
```

Use your host's system name instead of `aarch64-darwin`. The tests generate a
synthetic 25-module repository, execute real GHC/GHCi builds in actual Git
worktrees and verify outputs, failed builds, dirty edits, live reloads,
corruption rejection and seven concurrent worktrees. No application database or
private fixture is required. See [the benchmark notes](development-build-cache-benchmarks.markdown)
for measured results and their limitations.
