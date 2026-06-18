# tournament_engine.R
#
# Thin wrapper around the `bracketeer` package that powers all tournament
# formats and the event-sourced persistence model. Replaces the bespoke
# round-robin scheduler that used to live in get_games.R.
#
# Persistence contract (see CLAUDE.md / the plan): a tournament is stored as a
# write-once spec (format + participants + options + rng_seed) plus an
# append-only results game-log. The live `trn` is never serialized; it is
# rebuilt deterministically via `set.seed(rng_seed)` -> build spec -> replay
# results. Determinism for all six formats is proven by dev/determinism_check.R.
#
# bracketeer calls are fully qualified with `bracketeer::` because the app
# attaches the tidyverse, which masks `matches()` (tidyselect) and `top_n()` (dplyr).

library(bracketeer)

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Format catalog (drives the UI) -----------------------------------------

# Finnish display label -> internal format id.
# two_leg is intentionally NOT offered: it needs a per-leg (home/away) score,
# which the single-end score slider in the app cannot express. The engine still
# supports it (tournament_build below) for future use with a dedicated UI.
FORMAT_CHOICES <- c(
  "Sarjataulukko (round robin)"               = "round_robin",
  "Cup (single elimination)"                  = "single_elim",
  "Tuplacup (double elimination)"             = "double_elim",
  "Sveitsiläinen (swiss)"                     = "swiss",
  "Lohkot + pudotuspelit (group + knockout)"  = "group_knockout"
)

# Which option inputs each format exposes (for conditionalPanels in the UI).
FORMAT_OPTION_FIELDS <- list(
  round_robin    = c("home_away", "n_rounds"),
  single_elim    = c("third_place"),
  double_elim    = c("reseed"),
  swiss          = c("rounds"),
  two_leg        = character(0),
  group_knockout = c("groups", "advance_per_group")
)

# --- Building the live tournament -------------------------------------------

# Construct the bracketeer pipeline for a format. Does NOT set the RNG seed;
# callers wrap this with set.seed() so build + replay share one seed stream.
tournament_build <- function(format, participants, options = list()) {
  participants <- as.character(participants)
  trn <- bracketeer::tournament(participants, auto_advance = TRUE)
  sm  <- options$seed %||% "standard"   # seeding method; avoid "random" for reproducibility

  switch(format,
    round_robin = bracketeer::round_robin(
      trn, "main",
      home_away = isTRUE(options$home_away),
      n_rounds  = options$n_rounds %||% NULL
    ),
    single_elim = bracketeer::single_elim(
      trn, "main", seed = sm, third_place = isTRUE(options$third_place)
    ),
    double_elim = bracketeer::double_elim(
      trn, "main", seed = sm, reseed = isTRUE(options$reseed)
    ),
    swiss = bracketeer::swiss(
      trn, "main", rounds = options$rounds %||% NULL, seed = sm
    ),
    two_leg = bracketeer::two_leg(trn, "main", seed = sm),
    group_knockout = {
      n_grp <- as.integer(options$groups %||% 2L)
      n_adv <- as.integer(options$advance_per_group %||% 2L)
      trn <- bracketeer::round_robin(trn, "groups", groups = n_grp)
      bracketeer::single_elim(
        trn, "ko", seed = sm, take = bracketeer::top_n(n_grp * n_adv)
      )
    },
    stop("Unknown tournament format: ", format)
  )
}

# Create a fresh tournament at a pinned seed (used when a new instance starts).
tournament_new <- function(format, participants, options = list(), seed) {
  set.seed(as.integer(seed))
  tournament_build(format, participants, options)
}

# Rebuild the live tournament from its stored spec + results game-log.
tournament_reconstruct <- function(format, participants, options = list(), seed, results_df) {
  set.seed(as.integer(seed))
  trn <- tournament_build(format, participants, options)
  replay_results(trn, results_df)
}

# --- Replay (event sourcing) -------------------------------------------------

# Collapse the results log into a score vector per match, applying
# last-write-wins per (stage, match, leg) and interleaving legs the way
# bracketeer::result() expects: c(p1_leg1, p2_leg1, p1_leg2, p2_leg2, ...).
assemble_scores <- function(results_df) {
  df <- results_df
  df$.cid <- paste0(df$stage, "::", df$match)
  df <- df[order(df$.cid, df$leg, df$recorded_dttm), , drop = FALSE]
  keep <- !duplicated(paste0(df$.cid, "#", df$leg), fromLast = TRUE)  # last write wins
  df <- df[keep, , drop = FALSE]
  lapply(split(df, df$.cid), function(g) {
    g <- g[order(g$leg), , drop = FALSE]
    as.numeric(rbind(g$score_1, g$score_2))  # column-major -> interleaved by leg
  })
}

# Drive the tournament by its own pending order, applying logged scores until
# nothing more can be applied. Mirrors dev/determinism_check.R's reconstruct().
replay_results <- function(trn, results_df, cap = 5000L) {
  if (is.null(results_df) || nrow(results_df) == 0) return(trn)
  scores <- assemble_scores(results_df)
  for (i in seq_len(cap)) {
    pl <- pending_matches(trn)
    if (is.null(pl) || nrow(pl) == 0) break
    hit <- which(pl$compound_match_id %in% names(scores))
    if (length(hit) == 0) break
    j   <- hit[[1]]
    trn <- bracketeer::result(
      trn, pl$stage_id[[j]], match = pl$match_id[[j]],
      score = scores[[pl$compound_match_id[[j]]]], overwrite = FALSE
    )
  }
  trn
}

# --- Queries the app renders -------------------------------------------------

# Pending matches that are actually playable (both participants resolved).
pending_matches <- function(trn) {
  p <- bracketeer::matches(trn, status = "pending")
  if (is.null(p) || nrow(p) == 0) return(p)
  p[!is.na(p$participant1) & !is.na(p$participant2), , drop = FALSE]
}

# The single next match to play, or NULL when the tournament is waiting/finished.
next_match <- function(trn) {
  p <- pending_matches(trn)
  if (is.null(p) || nrow(p) == 0) return(NULL)
  p[1, , drop = FALSE]
}

# Standings table for display (whole tournament, or one stage).
standings_table <- function(trn, stage = NULL) {
  tryCatch(bracketeer::standings(trn, stage = stage), error = function(e) NULL)
}

tournament_winner <- function(trn) {
  tryCatch(bracketeer::winner(trn), error = function(e) NA_character_)
}

# Complete when every materialized stage is complete (swiss has no winner()).
tournament_complete <- function(trn) {
  st <- tryCatch(bracketeer::stage_status(trn), error = function(e) NULL)
  !is.null(st) && nrow(st) > 0 && all(st$status == "complete")
}

# --- Persistence helpers -----------------------------------------------------

# Build the results-sheet rows for one recorded match (one row per leg).
# `score` is the interleaved vector passed to bracketeer::result().
make_result_rows <- function(label, stage, match, player_1, player_2, score,
                             recorded_dttm = Sys.time()) {
  n  <- length(score)
  s1 <- score[seq.int(1L, n, by = 2L)]
  s2 <- score[seq.int(2L, n, by = 2L)]
  data.frame(
    label = label, stage = as.character(stage), match = as.character(match),
    leg = seq_along(s1),
    player_1 = player_1, player_2 = player_2,
    score_1 = as.integer(s1), score_2 = as.integer(s2),
    recorded_dttm = recorded_dttm,
    stringsAsFactors = FALSE
  )
}

# --- Long-running (cross-instance) series stats ------------------------------

# Aggregate the results game-log across all instances of a series into a
# per-player leaderboard. Pure over the log (no tournament reconstruction):
# collapses legs to one game per (label, stage, match), then tallies per player.
series_standings <- function(results_df) {
  empty <- data.frame(
    player = character(), tournaments = integer(), games = integer(),
    wins = integer(), draws = integer(), losses = integer(),
    points_for = integer(), points_against = integer(),
    diff = integer(), win_pct = numeric(), stringsAsFactors = FALSE
  )
  if (is.null(results_df) || nrow(results_df) == 0) return(empty)

  games <- results_df |>
    dplyr::group_by(label, stage, match, player_1, player_2) |>
    dplyr::summarise(s1 = sum(score_1), s2 = sum(score_2), .groups = "drop")

  long <- dplyr::bind_rows(
    dplyr::transmute(games, label, player = player_1, gf = s1, ga = s2),
    dplyr::transmute(games, label, player = player_2, gf = s2, ga = s1)
  )

  long |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      tournaments    = dplyr::n_distinct(label),
      games          = dplyr::n(),
      wins           = sum(gf > ga),
      draws          = sum(gf == ga),
      losses         = sum(gf < ga),
      points_for     = sum(gf),
      points_against = sum(ga),
      diff           = sum(gf - ga),
      win_pct        = round(100 * sum(gf > ga) / dplyr::n(), 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(wins), dplyr::desc(diff))
}
