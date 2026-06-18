#!/usr/bin/env Rscript
# Validates tournament_engine.R end to end through its real API:
#   tournament_new() -> play, logging rows via make_result_rows()
#   -> tournament_reconstruct() from those rows -> identical state.
# Also checks last-write-wins replay and series_standings().
suppressMessages({ library(bracketeer); library(dplyr) })
source("tournament_engine.R")

SEED <- 7L
specs <- list(
  round_robin    = list(p = paste0("P", 1:6), o = list()),
  single_elim    = list(p = paste0("P", 1:8), o = list(third_place = TRUE)),
  double_elim    = list(p = paste0("P", 1:8), o = list()),
  swiss          = list(p = paste0("P", 1:8), o = list(rounds = 3)),
  two_leg        = list(p = paste0("P", 1:4), o = list()),
  group_knockout = list(p = paste0("P", 1:8), o = list(groups = 2, advance_per_group = 2))
)
legs_for  <- function(fmt) if (fmt == "two_leg") 2L else 1L
score_vec <- function(k, legs) rep(c(2L + (k %% 2L), 1L), legs)

play_to_log <- function(fmt, p, o, seed) {
  trn  <- tournament_new(fmt, p, o, seed)
  legs <- legs_for(fmt); rows <- list(); k <- 0L
  repeat {
    nm <- next_match(trn); if (is.null(nm)) break
    sc <- score_vec(k, legs)
    rows[[length(rows) + 1L]] <- make_result_rows(
      "L1", nm$stage_id, nm$match_id, nm$participant1, nm$participant2, sc,
      recorded_dttm = as.POSIXct(1e9 + k, origin = "1970-01-01")
    )
    trn <- bracketeer::result(trn, nm$stage_id, match = nm$match_id, score = sc, overwrite = FALSE)
    k <- k + 1L
  }
  list(trn = trn, log = do.call(rbind, rows))
}

norm_df <- function(df, key) {
  if (is.null(df) || !nrow(df)) return(df)
  if (key %in% names(df)) df <- df[order(df[[key]]), , drop = FALSE]
  rownames(df) <- NULL; df
}
canon <- function(trn) list(
  matches   = norm_df(tryCatch(export_matches(trn),   error = function(e) NULL), "compound_match_id"),
  standings = norm_df(tryCatch(export_standings(trn), error = function(e) NULL), "stage_id"),
  winner    = tournament_winner(trn)
)

cat(sprintf("Engine round-trip (seed = %d)\n", SEED)); ok <- TRUE
logs <- list()
for (fmt in names(specs)) {
  s <- specs[[fmt]]
  out <- tryCatch({
    fwd <- play_to_log(fmt, s$p, s$o, SEED)
    rec <- tournament_reconstruct(fmt, s$p, s$o, SEED, fwd$log)
    list(same = isTRUE(all.equal(canon(fwd$trn), canon(rec))),
         rows = nrow(fwd$log), winner = canon(fwd$trn)$winner, log = fwd$log)
  }, error = function(e) list(err = conditionMessage(e)))
  if (!is.null(out$err)) { cat(sprintf("  %-15s ERROR  %s\n", fmt, out$err)); ok <- FALSE }
  else { logs[[fmt]] <- out$log
         cat(sprintf("  %-15s %s  (%d log rows, winner = %s)\n", fmt,
              if (out$same) "PASS" else "FAIL", out$rows, paste(out$winner, collapse = "/")))
         ok <- ok && out$same }
}

# Last-write-wins: a stale wrong score recorded earlier must be overridden.
cat("\nLast-write-wins replay\n")
lww <- tryCatch({
  base_log <- logs[["round_robin"]]
  good <- tournament_reconstruct("round_robin", specs$round_robin$p, list(), SEED, base_log)
  first <- base_log[base_log$stage == base_log$stage[1] & base_log$match == base_log$match[1], ][1, ]
  stale <- transform(first, score_1 = 0L, score_2 = 3L,
                      recorded_dttm = first$recorded_dttm - 100000)  # earlier => must lose
  with_stale <- rbind(stale, base_log)
  patched <- tournament_reconstruct("round_robin", specs$round_robin$p, list(), SEED, with_stale)
  isTRUE(all.equal(canon(good), canon(patched)))
}, error = function(e) { cat("  ERROR", conditionMessage(e), "\n"); FALSE })
cat(sprintf("  %s  (stale earlier score ignored)\n", if (lww) "PASS" else "FAIL")); ok <- ok && lww

# Series stats across two instances of the same series.
cat("\nseries_standings across instances\n")
ss <- tryCatch({
  combined <- dplyr::bind_rows(
    transform(logs[["round_robin"]], label = "wk1"),
    transform(logs[["round_robin"]], label = "wk2")
  )
  tab <- series_standings(combined)
  cat("  players:", nrow(tab), " | sample:\n"); print(utils::head(tab, 3))
  nrow(tab) == length(specs$round_robin$p) && all(tab$tournaments == 2)
}, error = function(e) { cat("  ERROR", conditionMessage(e), "\n"); FALSE })
cat(sprintf("  %s  (every player has tournaments = 2)\n", if (ss) "PASS" else "FAIL")); ok <- ok && ss

cat(if (ok) "\nENGINE OK\n" else "\nENGINE FAILED\n")
quit(status = if (ok) 0L else 1L)
