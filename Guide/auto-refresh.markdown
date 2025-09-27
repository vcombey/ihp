# Auto Refresh

```toc

```

## Introduction

Auto Refresh offers a way to re-render views of your application when the underlying data changes. This is useful when you want your views to always reflect the live database state. Auto Refresh can be an easy replacement for manually polling for changes using AJAX.

**Use Cases:**

1. This is used in Shipnix to display the current deployment status. Whenever e.g. the deployment progress or status changes, the view gets updated automatically.
2. When building a monitoring tool for background job workers. Using auto refresh the view can always represent the current state of all the job queues.
3. When building a small social media site: Automatically display new posts in the feed when they become available.

## How It Works

It's good to have a general understanding of how IHP Auto Refresh works.

Auto Refresh first has to be activated for an action by calling [`autoRefresh`](https://ihp.digitallyinduced.com/api-docs/IHP-AutoRefresh.html#v:autoRefresh). Once activated the framework will automatically track all tables your action is using e.g. in `SELECT * FROM ...` queries. Once the action sends a response IHP will start watching for any kind of `INSERT`, `UPDATE` or `DELETE` statement to all the tables used by your action.

When the page is rendered a small JavaScript function will connect back to the IHP server using a WebSocket connection.

Whenever an `INSERT`, `UPDATE` or `DELETE` happens to the tables used by your action, IHP checks whether the concrete row change could affect the output of your page before rerendering. If a change is relevant, the action is rerun on the server-side. When the generated HTML looks different than the HTML generated on the initial page load it will send the new HTML to the browser using the WebSocket connection. The JavaScript listening on the WebSocket will use the new HTML to update the current page. It uses morphdom to only touch the parts of your current DOM that have changed.

### Targeted Re-Rendering (Row-Aware Auto Refresh)

Starting with this version, Auto Refresh is “row-aware” and avoids unnecessary rerenders on hot tables:

- Row-level notifications: Database triggers now fire per row and include the changed row id as the notification payload.
- Query tracking: During the initial render inside `autoRefresh do`, IHP records the SELECT statements (SQL + parameters) that produced the page (this is automatic for QueryBuilder `fetch` calls; see below for custom SQL).
- Membership checks on change: On each notification, Auto Refresh tests whether the changed id would be part of any recorded SELECT for that table by running a tiny `SELECT EXISTS(SELECT 1 FROM (<recorded-select>) WHERE (id)::text = ?)`.
  - If true (row is or became part of the page), the server rerenders the action and pushes the HTML diff to the client.
  - If false (e.g. row belongs to another user, or doesn’t match the WHERE), the page is left untouched.

This keeps the classic developer experience (SSR views, automatic DOM patching) while scaling better with many users and frequent writes.


### Using Auto Refresh

Let's say we have a `ShowProjectAction` like this:

```haskell
action ShowProjectAction { projectId } = do
    project <- fetch projectId
    render ShowView { .. }
```

To enable auto refresh we have to add [`autoRefresh`](https://ihp.digitallyinduced.com/api-docs/IHP-AutoRefresh.html#v:autoRefresh) in front of the `do`:

```haskell
action ShowProjectAction { projectId } = autoRefresh do
    project <- fetch projectId
    render ShowView { .. }
```

That's it. When you open your browser dev tools, you will see that a WebSocket connection has been started when opening the page. When we update the project from a different browser tab, we will see that the page instantly updates to reflect our changes.

## Advanced Auto Refresh

### Auto Refresh Only for Specific Tables

By default IHP tracks all the tables in an action with Auto Refresh enabled.

In scenarios where you're processing a lot of data for a view, but only a small portion needs Auto Refresh, you can enable Auto Refresh only for the specific tables:

```haskell
action MyAction = do -- <-- We don't enable auto refresh at the action start in this case

    -- This part is not tracked by auto refresh, as `autoRefresh` wasn't called yet
    -- Therefore we can do our "expensive" operations here
    expensiveModels <- query @Expensive |> fetch

    autoRefresh do
        -- Inside this block auto refresh is active and all queries here are tracked
        cheap <- query @Cheap |> fetch
        render MyView { expensiveModels, cheap }
```

### Custom SQL Queries with Auto Refresh

Auto Refresh automatically tracks all tables your action is using by hooking itself into the Query Builder and `fetch` functions.

Let's say we're using custom sql query like this:

```haskell
action StatsAction = autoRefresh do
    dailyNewCompanies <- sqlQuery "SELECT date, COUNT(distinct id) AS count FROM (SELECT date_trunc('day', companies.created_at) AS date, id FROM companies) AS companies_with_date GROUP BY date" ()

    pure StatsView { ..}
```

When using this custom query with [`sqlQuery`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:sqlQuery), Auto Refresh is not aware that we're reading from the `companies` table. In this case we need to help out Auto Refresh by calling [`trackTableRead`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:trackTableRead):


```haskell
action StatsAction = autoRefresh do
    dailyNewCompanies <- sqlQuery "SELECT date, COUNT(distinct id) AS count FROM (SELECT date_trunc('day', companies.created_at) AS date, id FROM companies) AS companies_with_date GROUP BY date" ()

    trackTableRead "companies"

    pure StatsView { ..}
```

The [`trackTableRead`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:trackTableRead) marks the table as accessed for Auto Refresh and leads to the table being watched.

#### Making Targeted Re-Rendering Work With Custom SQL

QueryBuilder-based `fetch` calls automatically register their SELECT SQL and parameters so Auto Refresh can run precise membership checks. For custom SQL, you can optionally register the SELECT you used so Auto Refresh can determine whether a specific row change should rerender your page:

```haskell
action StatsAction = autoRefresh do
    let sql = "SELECT id, name FROM companies WHERE plan = ? ORDER BY created_at DESC LIMIT 50"
    let params = (Only ("pro" :: Text))
    companies <- sqlQuery sql params

    -- 1) Mark table as accessed so it will be watched
    trackTableRead "companies"

    -- 2) Register the SELECT used to produce the page (SQL + params)
    -- This lets Auto Refresh run `SELECT EXISTS` checks for inserts/updates
    trackSelectQuery "companies" sql (PG.toRow params)

    render StatsView { .. }
```

Notes:

- You don’t need to register every SELECT; register the ones that influence the output you want to gate rerenders on (e.g., the main list).
- For most pages, QueryBuilder-based code needs no changes — registration happens automatically.

#### (Optional) Recording Returned Ids

If you have the ids handy after a custom query, you can further reduce work by registering the ids that were actually rendered. Then updates/deletes to those ids can rerender without a membership roundtrip:

```haskell
-- After loading companies
let idsAsText = map (tshow . unpackId . (.id)) companies
trackRecordIdsRead "companies" idsAsText
```

This is optional; the membership checks alone are sufficient and work well when the WHERE clause is selective.

### Differences to DataSync

- DataSync pushes JSON patches (per-record change sets) to clients of SPA-like UIs; Auto Refresh stays fully SSR and pushes HTML snapshots.
- Both now filter updates by id: DataSync at the row level for subscriptions, Auto Refresh before re-rendering a page.
- Use DataSync for high-frequency, fine-grained interactive UIs; use Auto Refresh for SSR pages that benefit from targeted rerendering.

