# Petankkiliiga 🎯

A mobile-friendly **[Shiny](https://shiny.posit.co/) (R) web app for running pétanque tournaments**,
powered by the [`bracketeer`](https://github.com/bbtheo/bracketeer) engine and backed by Google Sheets.
Built for club nights at the court: open it on a phone, type your league name, and start scoring.

The interface is **Finnish-first with a live English toggle** (🇫🇮 / 🇬🇧).

---

## Features

- **Five tournament formats** out of the box (a sixth, two-leg knockout, is supported by the engine but not yet exposed in the UI).
- **Series** — group recurring tournaments (e.g. a weekly club night) and track a long-running, cross-tournament **leaderboard** ("Sarjatilastot").
- **Built for concurrent use.** Multiple people can open the same tournament and score matches from their own phones at the same time — persistence is append-only, so connections never clobber each other.
- **Resumable.** A tournament can be paused and continued across days and devices; state is rebuilt deterministically from the log.
- **Player management.** Edit the roster before the first result, or withdraw a player mid-tournament (their remaining matches are forfeited).
- **Confirm-on-save** dialog that echoes the exact scoreline, plus **round prompts** (final / bronze / semi-final / quarter-final) for elimination brackets.
- **Bilingual, live** — switch FI ⇄ EN without reloading.
- **Mobile layout** — `bslib` UI with the controls inline above the content on phones, a March-Madness-style bracket view, and big touch targets.
- **Non-blocking I/O** — slow Google Sheets calls run on background [`mirai`](https://mirai.r-lib.org) daemons (optional; falls back to synchronous execution if mirai isn't installed), so one person's save doesn't freeze everyone else.

## Tournament formats

| UI label (Finnish) | Format | Options |
|---|---|---|
| Sarjataulukko | Round robin | home & away, number of rounds |
| Cup | Single elimination | third-place (bronze) match |
| Tuplacup | Double elimination | reseed each round |
| Sveitsiläinen | Swiss | number of rounds |
| Lohkot + pudotuspelit | Group → knockout | number of groups, advancers per group |

Scores are recorded **one mène (end) at a time** with a ±3 margin slider.

---

## Getting started

### Prerequisites

- **R** (4.x) and the [`renv`](https://rstudio.github.io/renv/) package.
- A **Google Cloud service account** with access to a Google Sheet (see below).

### 1. Install dependencies

Dependencies are pinned with renv:

```r
renv::restore()
```

### 2. Set up Google Sheets

The app talks to one Google Sheet as a **single service account** — the app itself is public, the
sheet is private, and the account is the only writer (so anonymous writes are impossible via the API).

1. Create a Google Cloud **service account** and download a **JSON key**.
2. Enable the **Google Sheets API** and **Google Drive API** for the project.
3. Create a spreadsheet and **share it with the service account's email as an Editor**.
4. Point `SHEET_URL` in `app.R` at your spreadsheet.

### 3. Provide credentials

`sheets_auth()` resolves credentials in this order:

1. **`PETANQUE_GS_KEY_JSON`** — an environment variable holding the *contents* of the service-account
   JSON. Use this for deploys (the key file isn't committed).
2. **`secrets/service-account.json`** — a local file (the `secrets/` directory is git-ignored). You can
   point `PETANQUE_GS_KEY` at a different path.

### 4. Run

```r
shiny::runApp()
```

The first run bootstraps the required sheet tabs automatically.

### (Optional) Enable async I/O

```r
renv::install("mirai")
renv::snapshot()
```

Without mirai the app still works — it just runs Sheets I/O synchronously.

---

## How it works

State is **event-sourced on Google Sheets**: a tournament is a write-once spec row (format,
participants, options, RNG seed) plus an **append-only results game-log**. The live bracket is never
stored — it is rebuilt deterministically on load via `set.seed(seed)` → build the format's pipeline →
replay the logged results. This is what makes concurrent connections safe and tournaments resumable.

Two tabs back it:

- **`tournaments`** — one spec row per tournament instance, grouped by `series`.
- **`results`** — append-only, one row per recorded match leg.

Long-running series stats aggregate the results log directly, with no tournament reconstruction.

> For the full architecture, determinism invariants, and gotchas, see **[CLAUDE.md](CLAUDE.md)**.

### Project structure

```
app.R                 bslib UI + Shiny server
tournament_engine.R   thin wrapper over the bracketeer package (build / reconstruct / query)
sheets_io.R           event-sourced Google Sheets persistence + auth
translations/         shiny.i18n FI/EN dictionary
dev/                  runnable check scripts (the de-facto test suite)
renv.lock             pinned dependencies
manifest.json         Posit Connect Cloud deploy manifest
```

---

## Development

There is no unit-test framework; `dev/` holds three runnable check scripts (the first two need no
credentials):

```sh
Rscript dev/determinism_check.R   # spec+seed+replay reconstructs identically, all formats (the GATE)
Rscript dev/engine_check.R        # engine round-trip + last-write-wins + series stats
Rscript dev/live_check.R          # full round-trip against the real Google Sheet (needs credentials)
```

If you run these outside an renv-activated session, prefix them with the project library, e.g.
`Rscript -e 'source("renv/activate.R"); source("dev/determinism_check.R")'`.

## Deployment

Designed for **[Posit Connect Cloud](https://connect.posit.cloud/)**, which builds from Git. Set
`PETANQUE_GS_KEY_JSON` as a secret in the deploy's Variables (the contents of the service-account JSON).
`manifest.json` pins the dependency set, including the optional `mirai` async backend.

## Tech stack

R · Shiny · bslib · googlesheets4 · DT · shinyWidgets · shiny.i18n · mirai · and the
[bracketeer](https://github.com/bbtheo/bracketeer) tournament engine.
