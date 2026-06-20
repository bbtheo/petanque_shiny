# sheets_io.R
#
# Event-sourced persistence on Google Sheets for the bracketeer-backed app.
# A tournament is stored as a write-once spec row (`tournaments` tab) plus an
# append-only results game-log (`results` tab). The live `trn` is rebuilt by
# tournament_engine.R from these. Nothing here is ever fully overwritten, which
# is what makes concurrent connections (one shared service account, public app)
# safe. See CLAUDE.md / the plan for the schema.

library(googlesheets4)

# --- Schema (column order is positional; keep in sync with the templates) ----

TOURNAMENTS_SHEET <- "tournaments"
RESULTS_SHEET     <- "results"

TOURNAMENTS_COLTYPES <- "ccTccci"     # series,label,created_dttm,format,participants,options,rng_seed
RESULTS_COLTYPES     <- "cccicciiT"   # label,stage,match,leg,player_1,player_2,score_1,score_2,recorded_dttm

tournaments_template <- function() {
  tibble::tibble(
    series = character(), label = character(),
    created_dttm = as.POSIXct(character()),
    format = character(), participants = character(),
    options = character(), rng_seed = integer()
  )
}

results_template <- function() {
  tibble::tibble(
    label = character(), stage = character(), match = character(),
    leg = integer(), player_1 = character(), player_2 = character(),
    score_1 = integer(), score_2 = integer(),
    recorded_dttm = as.POSIXct(character())
  )
}

# --- Auth & bootstrap --------------------------------------------------------

# Authenticate the single shared service account (the app's only writer).
#
# Works in both environments without committing the key:
#   * Deployed (e.g. Posit Connect Cloud): set the secret env var
#     PETANQUE_GS_KEY_JSON to the *contents* of the service-account JSON.
#     gs4_auth(path=) accepts a JSON string directly.
#   * Local dev: drop the JSON file at secrets/service-account.json
#     (git-ignored), or point PETANQUE_GS_KEY at another path.
sheets_auth <- function() {
  json <- Sys.getenv("PETANQUE_GS_KEY_JSON", "")
  if (nzchar(json)) {
    return(googlesheets4::gs4_auth(path = json))   # JSON string from a deploy secret
  }
  key_path <- Sys.getenv("PETANQUE_GS_KEY", "secrets/service-account.json")
  if (!file.exists(key_path)) {
    stop("No Google credentials: set PETANQUE_GS_KEY_JSON (deploy) ",
         "or place a service-account JSON at '", key_path, "' (local).")
  }
  googlesheets4::gs4_auth(path = key_path)
}

# Idempotently ensure the new tabs exist with correct headers. Leaves any
# legacy tabs (e.g. `games`) untouched. Writing a 0-row typed tibble lays the
# header row that sheet_append() requires.
ensure_sheets <- function(ss) {
  nm <- googlesheets4::sheet_names(ss)
  if (!TOURNAMENTS_SHEET %in% nm)
    googlesheets4::sheet_write(tournaments_template(), ss, sheet = TOURNAMENTS_SHEET)
  if (!RESULTS_SHEET %in% nm)
    googlesheets4::sheet_write(results_template(), ss, sheet = RESULTS_SHEET)
  invisible(ss)
}

# --- JSON (de)serialization for spec columns ---------------------------------

to_json_array   <- function(x) as.character(jsonlite::toJSON(as.character(x)))
from_json_array <- function(s) if (is.na(s) || s == "") character(0) else as.character(jsonlite::fromJSON(s))
to_json_object  <- function(x) as.character(jsonlite::toJSON(as.list(x), auto_unbox = TRUE))
from_json_object <- function(s) if (is.na(s) || s == "") list() else as.list(jsonlite::fromJSON(s))

# --- Reads -------------------------------------------------------------------

read_tournaments <- function(ss) {
  googlesheets4::read_sheet(ss, sheet = TOURNAMENTS_SHEET, col_types = TOURNAMENTS_COLTYPES)
}

read_results <- function(ss) {
  googlesheets4::read_sheet(ss, sheet = RESULTS_SHEET, col_types = RESULTS_COLTYPES)
}

# Distinct series names (for the series picker).
series_names <- function(tournaments_df) {
  if (is.null(tournaments_df) || !nrow(tournaments_df)) return(character(0))
  sort(unique(tournaments_df$series))
}

# Instances belonging to a series, newest first. A label may legitimately repeat
# when a pre-start roster edit appends a *superseding* spec row (same label, new
# seed); collapse to the newest row per label so the instance list shows it once
# and reconstruction uses the latest spec.
instances_for_series <- function(tournaments_df, series) {
  if (is.null(tournaments_df) || !nrow(tournaments_df)) return(tournaments_df)
  df <- tournaments_df[tournaments_df$series == series, , drop = FALSE]
  df <- df[order(df$created_dttm, decreasing = TRUE), , drop = FALSE]
  df[!duplicated(df$label), , drop = FALSE]
}

# Results rows for one instance.
results_for_label <- function(results_df, label) {
  if (is.null(results_df) || !nrow(results_df)) return(results_df)
  results_df[results_df$label == label, , drop = FALSE]
}

# Results rows across every instance of a series (for series_standings()).
results_for_series <- function(tournaments_df, results_df, series) {
  labels <- instances_for_series(tournaments_df, series)$label
  if (is.null(results_df) || !nrow(results_df)) return(results_df)
  results_df[results_df$label %in% labels, , drop = FALSE]
}

# Distinct participant names ever seen in a series (autocomplete choices).
# Scoped to the exact series, so it never reveals other series' players.
series_participants <- function(tournaments_df, series) {
  inst <- instances_for_series(tournaments_df, series)
  if (is.null(inst) || !nrow(inst)) return(character(0))
  sort(unique(unlist(lapply(inst$participants, from_json_array), use.names = FALSE)))
}

# Roster of the most recent instance — a sensible default selection for a
# recurring group whose line-up rarely changes week to week.
latest_roster <- function(tournaments_df, series) {
  inst <- instances_for_series(tournaments_df, series)  # newest first
  if (is.null(inst) || !nrow(inst)) return(character(0))
  from_json_array(inst$participants[[1]])
}

# Decode a tournaments row into the spec list tournament_engine.R expects.
spec_from_row <- function(tournaments_df, label) {
  if (is.null(tournaments_df) || !nrow(tournaments_df)) return(NULL)
  row <- tournaments_df[tournaments_df$label == label, , drop = FALSE]
  if (!nrow(row)) return(NULL)
  row <- row[order(row$created_dttm, decreasing = TRUE), , drop = FALSE][1, ]  # newest spec wins (pre-start supersede)
  list(
    series       = row$series,
    label        = row$label,
    created_dttm = row$created_dttm,
    format       = row$format,
    participants = from_json_array(row$participants),
    options      = from_json_object(row$options),
    seed         = as.integer(row$rng_seed)
  )
}

# --- Writes (append-only) ----------------------------------------------------

# A unique, human-readable instance id under a series. The short suffix keeps two
# organizers who start the same series in the same second from colliding on the
# label (the results partition key); it is not shown in the UI, which lists
# instances by created_dttm + format. tempfile() is used for the token so we
# never touch the global RNG (which would interfere with seed determinism).
make_label <- function(series) {
  suffix <- substr(basename(tempfile()), 5L, 10L)
  paste0(series, " · ", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Europe/Helsinki"), " · ", suffix)
}

# A reproducible RNG seed to pin and store with the spec.
make_seed <- function() as.integer(Sys.time()) %% .Machine$integer.max

# Append one write-once spec row. Returns the generated label + seed.
append_tournament <- function(ss, series, format, participants, options = list(),
                              label = make_label(series), seed = make_seed()) {
  row <- tibble::tibble(
    series = series, label = label, created_dttm = Sys.time(),
    format = format, participants = to_json_array(participants),
    options = to_json_object(options), rng_seed = as.integer(seed)
  )
  googlesheets4::sheet_append(ss, row, sheet = TOURNAMENTS_SHEET)
  list(label = label, seed = seed)
}

# Append the per-leg result rows produced by make_result_rows() (engine).
append_results <- function(ss, rows) {
  googlesheets4::sheet_append(ss, rows, sheet = RESULTS_SHEET)
  invisible(rows)
}
