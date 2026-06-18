#!/usr/bin/env Rscript
# Live end-to-end check against the real Google Sheet. Additive only: creates the
# tournaments/results tabs, writes a clearly-labeled ZZ_TEST tournament, verifies
# the full persistence round-trip, then resets the new tabs to empty headers.
# Never touches the legacy `games`/`attendance` tabs.
suppressMessages({ library(googlesheets4); library(dplyr) })
source("sheets_io.R")
source("tournament_engine.R")

SS <- "https://docs.google.com/spreadsheets/d/18NNCnQNt7DCJxT5NzNNg2QQCVRbrj9kWaIZWIVkPdPQ/edit"
sheets_auth()

ok <- TRUE
chk <- function(n, c) { cat(sprintf("  %-46s %s\n", n, if (isTRUE(c)) "PASS" else "FAIL")); ok <<- ok && isTRUE(c) }

cleanup <- function() {
  try(googlesheets4::sheet_write(tournaments_template(), SS, sheet = TOURNAMENTS_SHEET), silent = TRUE)
  try(googlesheets4::sheet_write(results_template(),     SS, sheet = RESULTS_SHEET),     silent = TRUE)
  cat("  (cleaned up: new tabs reset to empty headers)\n")
}

tryCatch({
  ensure_sheets(SS)
  chk("tournaments + results tabs exist", all(c("tournaments", "results") %in% googlesheets4::sheet_names(SS)))

  series  <- "ZZ_TEST"
  players <- c("Ada", "Bo", "Cy", "Di")
  info <- append_tournament(SS, series = series, format = "round_robin", participants = players, options = list())
  cat(sprintf("  created instance: %s (seed %d)\n", info$label, info$seed))

  # Build + play locally, appending each result to the sheet (mirrors the app).
  set.seed(info$seed)
  trn <- tournament_build("round_robin", players, list())
  k <- 0L
  repeat {
    nm <- next_match(trn); if (is.null(nm)) break
    sc <- c(2L + (k %% 2L), 1L)
    append_results(SS, make_result_rows(info$label, nm$stage_id, nm$match_id,
                                        nm$participant1, nm$participant2, sc))
    trn <- bracketeer::result(trn, nm$stage_id, match = nm$match_id, score = sc, overwrite = FALSE)
    k <- k + 1L
  }
  cat(sprintf("  appended %d results\n", k))

  # Reload from the sheet and reconstruct.
  tdf  <- read_tournaments(SS)
  rdf  <- read_results(SS)
  spec <- spec_from_row(tdf, info$label)
  chk("spec round-trips (format)",       identical(spec$format, "round_robin"))
  chk("spec round-trips (participants)", identical(spec$participants, players))
  chk("spec round-trips (seed)",         identical(spec$seed, info$seed))

  rec <- tournament_reconstruct(spec$format, spec$participants, spec$options, spec$seed,
                                results_for_label(rdf, info$label))
  canon <- function(t) list(s = standings_table(t), w = tournament_winner(t))
  chk("reconstructed == live (standings + winner)", isTRUE(all.equal(canon(trn), canon(rec))))
  chk("winner resolved",                            !is.na(tournament_winner(rec)))

  ss_tab <- series_standings(results_for_series(tdf, rdf, series))
  chk("series_standings rows == #players", nrow(ss_tab) == length(players))
  cat("  series leaderboard:\n"); print(ss_tab)
}, error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); ok <<- FALSE })

cleanup()
cat(if (ok) "\nLIVE OK\n" else "\nLIVE FAILED\n")
quit(status = if (ok) 0L else 1L)
