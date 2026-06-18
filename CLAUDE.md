# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Shiny (R) web app for running pétanque tournaments in **any** of six formats — round-robin,
single/double elimination, Swiss, two-leg knockout, and group→knockout — powered by the
[`bracketeer`](https://github.com/bbtheo/bracketeer) package. Recurring tournaments (e.g. a weekly
club night) are grouped into a **series** with a long-running cross-tournament stats view. The UI is
in **Finnish** (e.g. "Sarja" = series, "Aloita peli!" = start, "Sarjatilastot" = series stats); keep
user-facing strings Finnish.

## Running, dependencies & verification

Dependencies are managed with **renv** (`renv.lock`). `bracketeer` is on CRAN.

```r
renv::restore()        # install pinned deps
shiny::runApp()        # run the app from the repo root
```

There is no unit-test framework, but `dev/` holds three **runnable check scripts** that are the de
facto test suite (no Shiny needed; the first two need no credentials):

```sh
Rscript dev/determinism_check.R   # spec+seed+replay reconstructs identically, all 6 formats (the GATE)
Rscript dev/engine_check.R        # tournament_engine.R round-trip + last-write-wins + series stats
Rscript dev/live_check.R          # full round-trip against the real Google Sheet (additive, self-cleaning)
```

## Authentication & secrets

The app talks to Google Sheets as a **single service account** (the app is public; the sheet is
private and shared with that account as Editor; the account is the only writer for all users —
anonymous writes are impossible via the Sheets API). `sheets_auth()` resolves credentials in order:

1. **`PETANQUE_GS_KEY_JSON`** env var holding the *contents* of the service-account JSON — used for
   deploys (e.g. **Posit Connect Cloud**, which builds from Git so the key file isn't present). Set it
   as a secret in the deploy's Variables.
2. **`secrets/service-account.json`** (or `PETANQUE_GS_KEY` path) for local dev. `secrets/` is git-ignored.

Google setup: create a service account, enable the **Sheets API + Drive API**, download a JSON key,
and share the spreadsheet with the account's email as Editor.

## Architecture

The app is three sourced R files plus the bracketeer package. The hard parts (engine determinism,
persistence round-trip) are proven by the `dev/` scripts.

- **`tournament_engine.R`** — thin wrapper over `bracketeer`. Builds a format's stage pipeline,
  reconstructs/queries a live tournament, and aggregates series stats. bracketeer calls are
  fully qualified `bracketeer::…` because the app's other packages mask `matches()` and `top_n()`.
- **`sheets_io.R`** — event-sourced Google Sheets persistence + `ensure_sheets()` bootstrap + auth.
- **`app.R`** — `bslib` UI + server. Sidebar = series picker + instance picker + format selector +
  per-format option inputs + participants. Main = "Peli" tab (current match, score slider,
  save/skip/refresh, schedule, leaderboard) and "Sarjatilastot" tab.

### Persistence: event sourcing on Google Sheets (the key idea)

State is **never** the live tournament object — it's a write-once spec plus an append-only results
log, from which the live `trn` is rebuilt deterministically. This is what makes concurrent
connections safe (no full-overwrite clobber) and tournaments resumable across days/devices.

Two tabs (legacy `games`/`attendance` tabs are left untouched; this is greenfield):

- **`tournaments`** (`col_types = "ccTccci"`): `series`, `label` (unique instance id), `created_dttm`,
  `format`, `participants` (JSON array), `options` (JSON object), `rng_seed`. One **write-once** row
  per tournament instance.
- **`results`** (`col_types = "cccicciiT"`): `label`, `stage`, `match`, `leg`, `player_1`, `player_2`,
  `score_1`, `score_2`, `recorded_dttm`. **Append-only** game-log.

`series` groups recurring instances. `label` is the partition key for results. `status`/`winner` are
**derived** on load (never stored). Long-running stats (`series_standings`) aggregate the results
game-log across a series with **no tournament reconstruction** — which is why `player_1`/`player_2` are
denormalized onto each results row.

### Reconstruction & determinism (read before changing the engine)

`tournament_reconstruct()` = `set.seed(rng_seed)` → build the pipeline → replay the results in the
bracket's own pending order. Invariants that make this work (all verified in `dev/`):

- **Seed pinning is mandatory.** Round-robin player1/player2 orientation draws from the RNG even with
  deterministic seeding, so the stored `rng_seed` must wrap build+replay. Avoid the `"random"` seeding
  method.
- **No `advance()` calls.** `tournament(auto_advance = TRUE)` materializes downstream stages as results
  land. `get_ready_stages()` is **not exported** — don't use it.
- **group→knockout is composed**, not the monolithic verb:
  `round_robin("groups", groups=N) |> single_elim("ko", take = bracketeer::top_n(N*advance_per_group))`.
  The monolithic `group_stage_knockout()` has an internal phase the runtime won't auto-advance.
- **Last-write-wins**: duplicate `(label, stage, match, leg)` rows resolve to the latest `recorded_dttm`.
- `match_id` is **character**; `matches()` exposes `compound_match_id` ("stage::match") as the dedup key.
- **Swiss has no `winner()`** (it's `NA`); completion = all stages complete (`tournament_complete()`).
- **Multi-leg / best-of** record one results row per `leg`; the engine interleaves them into the
  `score` vector `bracketeer::result()` expects.

## Gotchas

- **Schema sync:** the `col_types` strings in `sheets_io.R` are positional — keep them in lockstep with
  the column order of the tibble templates and the sheet writes.
- **Package masking:** always call bracketeer's API as `bracketeer::…` in code that also loads dplyr/shiny.
- `SHEET_URL` is hard-coded in `app.R`.
- **Attendance tracking was dropped** in the bracketeer rewrite (the old `attendance` tab/feature). The
  `attendance` tab still exists untouched; series stats cover "appearances". Re-add if needed.
