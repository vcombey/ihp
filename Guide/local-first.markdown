# Local-First Actions (Experimental)

```toc

```

## Overview

IHP can mark controller actions as local-first with `local do`.

When a route is marked as local-first and the browser is offline, the frontend can:

1. Run supported DataSync queries/mutations against a browser database runtime
2. Queue mutations for replay
3. Replay queued mutations after reconnect (server-wins conflict strategy)
4. Trigger local auto-refresh events for the current route

This feature is currently **experimental** and should be rolled out incrementally.

## Basic Usage

### 1. Initialize in `initContext`

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initLocalFirst
```

### 2. Mark action as local

```haskell
action TodosAction = local do
    todos <- query @Todo |> fetch
    render TodosView { .. }
```

Or with explicit options:

```haskell
action TodosAction = localWith defaultLocalOptions do
    todos <- query @Todo |> fetch
    render TodosView { .. }
```

## Offline Todo Update Example

The following example shows an update action that can run offline, update the local browser DB, and trigger local AutoRefresh rerendering.

### 1. Mark both routes as local

```haskell
instance Controller TodosController where
    action TodosAction = autoRefresh $ local do
        todos <- query @Todo |> orderBy #createdAt |> fetch
        render TodosView { .. }

    action UpdateTodoAction { todoId } = local do
        todo <- fetch todoId
        let title = param @Text "title"
        let isCompleted = paramOrDefault False "isCompleted"
        todo
            |> set #title title
            |> set #isCompleted isCompleted
            |> updateRecord
        redirectTo TodosAction
```

### 2. Use a form that targets the local update route

```haskell
renderTodoRow :: Todo -> Html
renderTodoRow todo = [hsx|
    <form method="POST" action={pathTo UpdateTodoAction { todoId = todo.id }}>
        <input type="hidden" name="todoId" value={tshow todo.id}/>
        <input type="text" name="title" value={todo.title}/>
        <input type="checkbox" name="isCompleted" checked={todo.isCompleted}/>
        <button>Save</button>
    </form>
|]
```

### 3. Automatic transpilation output (generated)

When `initLocalFirst` runs, IHP generates `static/ihp-local-routes.js` from `local do` actions and loads it in the page.

For an action like `UpdateTodoAction`, the generated script registers a local browser handler automatically (no custom JS needed in your app code) and maps form fields to the local DB mutation.
Form submissions are intercepted automatically when the route is local and the browser is offline.

### What happens offline

1. The submit is intercepted by `IHPLocalRuntime` when offline and on a local route.
2. The transpiled local action from `ihp-local-routes.js` runs instead of issuing a network request.
3. `updateRecord` writes to PGlite (local Postgres in the browser).
4. The mutation is queued for replay on reconnect.
5. The local mutation emits `ihp:local-refresh`, and `ihp-auto-refresh.js` rerenders the local AutoRefresh route.

## Runtime Script

Load the local runtime before `ihp-auto-refresh.js`:

```haskell
<script src={assetPath "/ihp-local-runtime.js"}></script>
<script src={assetPath "/ihp-local-routes.js"}></script>
<script src={assetPath "/ihp-auto-refresh.js"}></script>
```

The runtime exposes:

```js
window.IHPLocalRuntime
```

`IHPLocalRuntime` expects a browser Postgres runtime exposed as `window.PGlite`.
If your app does not bundle PGlite yet, local DB operations will fail with an explicit runtime error.

Key capabilities:

1. Local query/mutation dispatch for supported DataSync payloads
2. Mutation queue + replay hook
3. User-scoped local storage namespace
4. Automatic local action registration from generated `ihp-local-routes.js`
5. Local auto-refresh event dispatch via `ihp:local-refresh`

## Safety Checks

Use `IHP.LocalFirst.Safety.scanLocalSafetySource` in tooling/CI to detect unsupported APIs in `local` blocks (for example, external HTTP, process spawning, raw SQL helpers).

## Discovery and Manifest Generation

`IHP.LocalFirst.CodeGen.writeLocalRouteArtifacts` can generate:

1. `Generated/LocalRoutes.hs`
2. `local-routes.manifest.json`

from source scanning of actions containing `local do` / `localWith`.

## Current Limitations

1. Local transpilation currently supports a restricted subset (for example, simple `fetch` + `set ... |> updateRecord` style actions).
2. The browser DB runtime requires a PGlite-compatible runtime in the page.
3. `CreateDataSubscription` websocket subscriptions remain server-side.
4. Safety checks are heuristic and should be complemented by app-level tests.
