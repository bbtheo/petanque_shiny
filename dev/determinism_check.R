#!/usr/bin/env Rscript
# Determinism gate for the bracketeer-backed persistence model.
#
# Proves the core assumption behind storing only (spec + results log) in Google
# Sheets: with a pinned RNG seed, building the spec and replaying the recorded
# results reconstructs a byte-identical tournament (matches + standings + winner)
# for every supported format. If this fails, the event-sourced persistence model
# is not viable as designed.
suppressMessages(library(bracketeer))

SEED <- 20260616L

# A fresh live tournament per format (one builder = one `spec`).
builders <- list(
  round_robin    = function() tournament(paste0("P", 1:6)) |> round_robin("main"),
  single_elim    = function() tournament(paste0("P", 1:8)) |> single_elim("main"),
  double_elim    = function() tournament(paste0("P", 1:8)) |> double_elim("main"),
  swiss          = function() tournament(paste0("P", 1:8)) |> swiss("main", rounds = 3),
  two_leg        = function() tournament(paste0("P", 1:4)) |> two_leg("main"),
  # group -> knockout is composed as two stages (auto-advances; no manual advance()),
  # rather than the monolithic group_stage_knockout() verb whose internal phase needs one.
  group_knockout = function() tournament(paste0("P", 1:8)) |>
                     round_robin("groups", groups = 2) |>
                     single_elim("ko", take = top_n(4))
)

# Per-format leg count (two-leg ties need a score per leg); default 1.
LEGS <- list(two_leg = 2L)

# Decisive (never a tie) so elimination always advances; varied for realism.
# For multi-leg formats `score` is interleaved c(p1_l1, p2_l1, p1_l2, p2_l2, ...).
decisive_score <- function(k, legs = 1L) rep(c(2L + (k %% 2L), 1L), legs)

# Pending matches that are actually playable (both participants known).
playable <- function(trn) {
  p <- matches(trn, status = "pending")
  if (is.null(p) || nrow(p) == 0) return(p)
  p[!is.na(p$participant1) & !is.na(p$participant2), , drop = FALSE]
}

# Forward play: repeatedly record the first playable match; rely on auto_advance.
play_and_log <- function(build_fn, seed, legs = 1L, cap = 2000L) {
  set.seed(seed)
  trn <- build_fn()
  log <- list(); k <- 0L
  for (i in seq_len(cap)) {
    pl <- playable(trn)
    if (is.null(pl) || nrow(pl) == 0) break
    row <- pl[1, ]
    sc  <- decisive_score(k, legs)
    trn <- result(trn, row$stage_id, match = row$match_id, score = sc, overwrite = FALSE)
    k <- k + 1L
    log[[k]] <- data.frame(stage = row$stage_id, match = row$match_id,
                           cmid = row$compound_match_id,
                           score = paste(sc, collapse = ","),
                           stringsAsFactors = FALSE)
  }
  list(trn = trn, log = if (k) do.call(rbind, log) else NULL)
}

# Reconstruct: same seed + build, then drive by the bracket's own pending order,
# applying the logged score for each match (mirrors play_and_log exactly).
# last-write-wins: keep the final logged score per compound match id.
reconstruct <- function(build_fn, log, seed, cap = 2000L) {
  set.seed(seed)
  trn <- build_fn()
  if (is.null(log)) return(trn)
  log <- log[!duplicated(log$cmid, fromLast = TRUE), , drop = FALSE]
  lut <- setNames(seq_len(nrow(log)), log$cmid)
  for (i in seq_len(cap)) {
    pl <- playable(trn)
    if (is.null(pl) || nrow(pl) == 0) break
    hit <- which(pl$compound_match_id %in% log$cmid)
    if (length(hit) == 0) break
    j <- hit[1]; r <- log[lut[[pl$compound_match_id[j]]], ]
    sc <- as.numeric(strsplit(r$score, ",", fixed = TRUE)[[1]])
    trn <- result(trn, pl$stage_id[j], match = pl$match_id[j],
                  score = sc, overwrite = FALSE)
  }
  trn
}

norm_df <- function(df, key) {
  if (is.null(df) || !nrow(df)) return(df)
  if (key %in% names(df)) df <- df[order(df[[key]]), , drop = FALSE]
  rownames(df) <- NULL
  df
}

canon <- function(trn) list(
  matches   = norm_df(tryCatch(export_matches(trn),   error = function(e) NULL), "compound_match_id"),
  standings = norm_df(tryCatch(export_standings(trn), error = function(e) NULL), "stage_id"),
  winner    = tryCatch(winner(trn), error = function(e) NA_character_)
)

cat(sprintf("Determinism gate (seed = %d)\n", SEED))
ok_all <- TRUE
for (nm in names(builders)) {
  out <- tryCatch({
    legs <- if (is.null(LEGS[[nm]])) 1L else LEGS[[nm]]
    fwd <- play_and_log(builders[[nm]], SEED, legs)
    rec <- reconstruct(builders[[nm]], fwd$log, SEED)
    same <- isTRUE(all.equal(canon(fwd$trn), canon(rec)))
    list(same = same, n = if (is.null(fwd$log)) 0L else nrow(fwd$log),
         winner = canon(fwd$trn)$winner)
  }, error = function(e) list(err = conditionMessage(e)))
  if (!is.null(out$err)) {
    cat(sprintf("  %-15s ERROR  %s\n", nm, out$err)); ok_all <- FALSE
  } else {
    cat(sprintf("  %-15s %s  (%d results, winner = %s)\n",
                nm, if (out$same) "PASS" else "FAIL", out$n,
                paste(out$winner, collapse = "/")))
    ok_all <- ok_all && out$same
  }
}
cat(if (ok_all) "\nGATE PASSED\n" else "\nGATE FAILED\n")
quit(status = if (ok_all) 0L else 1L)
