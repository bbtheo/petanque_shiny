# Petankkiliiga — tournament app backed by the bracketeer engine.
#
# State is event-sourced in Google Sheets (see sheets_io.R): a write-once spec
# row per tournament instance + an append-only results game-log. The live
# bracketeer tournament is rebuilt deterministically on load. Recurring weekly
# tournaments are grouped into a `series`.
#
# The `series` name is a soft password: it is NEVER listed in the UI, so a user
# can only reach a series (its tournaments + stats) by typing its exact name.
#
# UI is bilingual (Finnish default / English) via shiny.i18n: static text is
# tagged with i18n$t() and swapped client-side by update_lang(); server-rendered
# text uses a per-session translator (tr) so concurrent users can differ.
library(shiny)
library(bslib)
library(googlesheets4)
library(dplyr)
library(shinyWidgets)
library(DT)
library(shiny.i18n)

# Show all timestamps in Helsinki time regardless of the server timezone
# (deploys like Posit Connect Cloud typically run in UTC).
Sys.setenv(TZ = "Europe/Helsinki")

source("sheets_io.R")
source("tournament_engine.R")

SHEET_URL     <- "https://docs.google.com/spreadsheets/d/18NNCnQNt7DCJxT5NzNNg2QQCVRbrj9kWaIZWIVkPdPQ/edit"
NEW_INSTANCE  <- "<NEW>"
SCORE_CHOICES <- c("3-0", "2-0", "1-0", "0-0", "0-1", "0-2", "0-3")
FORMAT_IDS    <- c("round_robin", "single_elim", "double_elim", "swiss", "group_knockout")

i18n <- Translator$new(translation_json_path = "translations/translation.json")
i18n$set_translation_language("fi")
# Turn on browser-side translation BEFORE any UI is built. With use_js() on, each
# content-position i18n$t() renders as <span class="i18n" data-key=...> that
# update_lang() swaps live on language change. This must run before app_sidebar
# and ui are constructed — otherwise those strings (the navbar/sidebar labels and
# the .sidebar-help info texts) render as plain, untranslatable text. ATTRIBUTE /
# <select>-choice positions can't hold a <span>, so use ui_txt() (a plain-string
# translator) there; the lang observer re-sets those on switch.
i18n$use_js()
i18n_txt <- Translator$new(translation_json_path = "translations/translation.json")
i18n_txt$set_translation_language("fi")
ui_txt <- function(key) i18n_txt$t(key)

sheets_auth()
ensure_sheets(SHEET_URL)

# --- Async backend (ExtendedTask) -------------------------------------------
# Shiny runs one R thread, so a synchronous Google Sheets call blocks *every*
# connected player until it returns. We push the slow Sheets I/O to `mirai`
# daemons (each loads the helpers + its own Google auth) so the session stays
# responsive and the task buttons grey out while a write/read is in flight.
#
# mirai is OPTIONAL: if it is not installed or daemons can't start, run_async()
# falls back to running the job synchronously (today's behaviour), so the app
# always works. Enable async with: renv::install("mirai"); renv::snapshot().
APP_WD   <- normalizePath(getwd())
ASYNC_OK <- requireNamespace("mirai", quietly = TRUE) && tryCatch({
  mirai::daemons(2L)
  mirai::everywhere({
    setwd(wd)
    suppressMessages({
      library(googlesheets4); library(dplyr); library(bracketeer)
      source("sheets_io.R"); source("tournament_engine.R")
      sheets_auth()
    })
    SHEET_URL <- url
  }, wd = APP_WD, url = SHEET_URL)
  TRUE
}, error = function(e) {
  message("Async backend unavailable, running Sheets I/O synchronously: ", conditionMessage(e)); FALSE
})

# Run a pre-quoted job on a daemon (returns a mirai/promise) or, with no backend,
# synchronously in this process as a resolved promise. Named `...` are passed
# into the job; globals (SHEET_URL, helpers) resolve in the daemon or here.
run_async <- function(job, ...) {
  if (ASYNC_OK) return(mirai::mirai(.expr = job, ...))
  promises::promise_resolve(eval(job, envir = list(...), enclos = globalenv()))
}

# Append-only write of result rows (single score, withdrawal forfeits, ...).
APPEND_JOB  <- quote({ append_results(SHEET_URL, rows); TRUE })
# Re-read both tabs for a manual refresh.
REFRESH_JOB <- quote({ list(t = read_tournaments(SHEET_URL), r = read_results(SHEET_URL)) })
# Write a (write-once or superseding) spec row, then return the fresh tournaments tab.
CREATE_JOB  <- quote({
  append_tournament(SHEET_URL, series = series, format = format, participants = participants,
                    options = options, label = label, seed = seed)
  read_tournaments(SHEET_URL)
})

parse_score <- function(s) as.integer(strsplit(s, "-", fixed = TRUE)[[1]])

gather_options <- function(input) {
  opts <- list(seed = "standard")
  switch(input$format,
    round_robin = {
      opts$home_away <- isTRUE(input$opt_home_away)
      if (!is.null(input$opt_n_rounds) && !is.na(input$opt_n_rounds)) opts$n_rounds <- as.integer(input$opt_n_rounds)
    },
    single_elim    = { opts$third_place <- isTRUE(input$opt_third_place) },
    double_elim    = { opts$reseed <- isTRUE(input$opt_reseed) },
    swiss          = { if (!is.null(input$opt_swiss_rounds)) opts$rounds <- as.integer(input$opt_swiss_rounds) },
    group_knockout = { opts$groups <- as.integer(input$opt_groups); opts$advance_per_group <- as.integer(input$opt_advance) }
  )
  opts
}

# Translate a bracketeer build error to a friendly message (tt = translator).
friendly_error <- function(msg, tt) {
  if (grepl("n_participants|integer >= 2", msg)) return(tt("Tarvitaan vähintään 2 osallistujaa."))
  if (grepl("Dry-run failed|Group `", msg))
    return(tt("Pelaajia on liian vähän lohkoihin nähden – lisää pelaajia tai vähennä lohkoja/jatkoonpääsijöitä."))
  paste0(tt("Asetukset eivät kelpaa: "), msg)
}

pick_match <- function(trn, deferred) {
  pl <- pending_matches(trn)
  if (is.null(pl) || nrow(pl) == 0) return(NULL)
  cand <- pl[!pl$compound_match_id %in% deferred, , drop = FALSE]
  if (nrow(cand) > 0) cand[1, , drop = FALSE] else pl[1, , drop = FALSE]
}

# Format dropdown choices (translated label -> internal id).
format_choices <- function(tt) setNames(FORMAT_IDS, c(
  tt("Sarjataulukko (round robin)"), tt("Cup (single elimination)"),
  tt("Tuplacup (double elimination)"), tt("Sveitsiläinen (swiss)"),
  tt("Lohkot + pudotuspelit (group + knockout)")))

# Instances of a series, newest first (plus the translated "new" option). Only
# ever called with an exact, user-typed series name — never the full list.
instance_choices <- function(tournaments_df, series, tt = identity) {
  ch <- setNames(NEW_INSTANCE, tt("➕ Uusi turnaus"))
  if (is.null(series) || !nzchar(series)) return(ch)
  inst <- instances_for_series(tournaments_df, series)
  if (!is.null(inst) && nrow(inst)) {
    labels <- inst$label
    names(labels) <- paste0(format(inst$created_dttm, "%d.%m. %H:%M", tz = "Europe/Helsinki"),
                            " · ", inst$format)
    ch <- c(ch, labels)
  }
  ch
}

mobile_dt <- function(df, tt = identity, paging = FALSE, page = 8) {
  datatable(df, rownames = FALSE, class = "compact stripe nowrap",
            options = list(dom = if (paging) "tp" else "t", scrollX = TRUE,
                           paging = paging, pageLength = page, ordering = FALSE,
                           language = list(emptyTable = tt("Ei vielä dataa"),
                                           zeroRecords = tt("Ei vielä dataa"))))
}

# --- Match view: a bracket for elimination stages, a table otherwise ---------

ELIM_TYPES <- c("main", "winners", "losers", "grand_final")

stage_title   <- function(st, tr) switch(st, groups = tr("Lohkot"), ko = tr("Pudotuspelit"), st)
bracket_label <- function(bt, tr) switch(bt, winners = tr("Voittajien kaavio"),
                                         losers = tr("Häviäjien kaavio"),
                                         grand_final = tr("Loppuottelu"), "")

# Name the phase of an elimination match (final / bronze / semi / quarter) for
# the "current match" prompt. Computed from the match's round relative to the
# last round of its single-elim bracket: bronze is the extra round-R match
# (position 2) created by third_place. Applies to single_elim and the group→
# knockout "ko" stage (both bracket_type "main") plus the double-elim grand
# final; other double-elim rounds are left unlabelled to avoid ambiguity.
# `all_m` is bracketeer::matches(trn, status = "all"). Returns NULL when N/A.
round_phase_label <- function(m, all_m, tr) {
  bt <- m$bracket_type %||% NA_character_
  if (is.na(bt)) return(NULL)
  if (identical(bt, "grand_final")) return(tr("Finaali"))
  if (!identical(bt, "main")) return(NULL)
  same <- all_m[!is.na(all_m$stage_id) & all_m$stage_id == m$stage_id &
                !is.na(all_m$bracket_type) & all_m$bracket_type == "main", , drop = FALSE]
  if (is.null(same) || !nrow(same)) return(NULL)
  R    <- max(same$round, na.rm = TRUE)
  at_R <- same[same$round == R, , drop = FALSE]
  if (m$round == R && nrow(at_R) > 1 && m$position == max(at_R$position))
    return(tr("Pronssiottelu"))
  switch(as.character(R - m$round),
         "0" = tr("Finaali"), "1" = tr("Välierä"), "2" = tr("Puolivälierä"), NULL)
}

# A column-per-round bracket from matches with `round`/`position`/`winner`.
bracket_html <- function(bm, tr) {
  cols <- lapply(sort(unique(bm$round)), function(r) {
    rm <- bm[bm$round == r, , drop = FALSE]; rm <- rm[order(rm$position), , drop = FALSE]
    boxes <- lapply(seq_len(nrow(rm)), function(i) {
      mt <- rm[i, ]; w <- mt$winner
      prow <- function(p, sc) div(
        class = paste("bracket-row", if (!is.na(w) && identical(w, p)) "win" else ""),
        span(class = "nm", if (is.na(p)) "—" else p),
        span(if (is.na(sc)) "" else as.character(sc)))
      div(class = "bracket-match", prow(mt$participant1, mt$score1), prow(mt$participant2, mt$score2))
    })
    div(class = "bracket-round", div(class = "bracket-round-title", paste0(tr("Kierros"), " ", r)), boxes)
  })
  div(class = "bracket", cols)
}

stage_table <- function(sm, tr) {
  sm <- sm[order(sm$round, sm$position), , drop = FALSE]
  rows <- lapply(seq_len(nrow(sm)), function(i) {
    mt <- sm[i, ]
    tags$tr(tags$td(mt$participant1), tags$td(mt$participant2),
            tags$td(if (is.na(mt$score1)) "" else paste0(mt$score1, "–", mt$score2)),
            tags$td(if (identical(mt$status, "complete")) "✓" else "·"))
  })
  tags$table(class = "table table-sm table-striped mb-0",
    tags$thead(tags$tr(tags$th(tr("Pelaaja 1")), tags$th(tr("Pelaaja 2")),
                       tags$th(tr("Tulos")), tags$th(tr("Tila")))),
    tags$tbody(rows))
}

# Per stage: bracket for elimination bracket_types, table for round-robin/Swiss.
matches_view <- function(trn, tr) {
  m <- bracketeer::matches(trn, status = "all")
  if (is.null(m) || nrow(m) == 0) return(div(class = "text-muted p-2", tr("Ei vielä dataa")))
  stages <- unique(m$stage_id); multi <- length(stages) > 1
  sections <- list()
  add <- function(x) sections[[length(sections) + 1]] <<- x
  for (st in stages) {
    sm <- m[m$stage_id == st, , drop = FALSE]
    if (multi) add(tags$div(class = "bracket-section-title", stage_title(st, tr)))
    if (any(sm$bracket_type %in% ELIM_TYPES)) {
      for (bt in unique(sm$bracket_type)) {
        lbl <- bracket_label(bt, tr)
        if (nzchar(lbl)) add(tags$div(class = "bracket-round-title text-start", lbl))
        add(bracket_html(sm[sm$bracket_type == bt, , drop = FALSE], tr))
      }
    } else {
      add(stage_table(sm, tr))
    }
  }
  div(sections)
}

# --- Theme & polish ----------------------------------------------------------

app_theme <- bs_theme(version = 5, bootswatch = "minty",
                      primary = "#1f7a6d", "border-radius" = "0.8rem")

app_css <- HTML("
  .btn { border-radius: .7rem; }
  .card { box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 1rem; }
  .form-control, .form-select, .selectize-input, .selectize-dropdown { font-size: 16px; }
  .match-name { font-size: clamp(1.1rem, 5vw, 1.6rem); font-weight: 700; line-height: 1.2; }
  .match-vs { font-size: .9rem; letter-spacing: .08em; text-transform: uppercase; opacity: .55; }
  .irs--shiny .irs-bar, .irs--shiny .irs-handle>i:first-child { background: var(--bs-primary); }
  table.dataTable td, table.dataTable th { white-space: nowrap; }
  .sidebar-help { font-size: .8rem; opacity: .7; }
  .navbar .radio-group-buttons { margin-bottom: 0; }
  /* March-Madness style bracket */
  .bracket { display: flex; align-items: stretch; overflow-x: auto; padding: .5rem .25rem; }
  .bracket-round { display: flex; flex-direction: column; justify-content: space-around; min-width: 165px; }
  .bracket-round-title { font-size: .68rem; letter-spacing: .05em; text-transform: uppercase; opacity: .55; text-align: center; margin: 0 .6rem .25rem; }
  .bracket-match { background: #fff; border: 1px solid rgba(0,0,0,.1); border-radius: .5rem; margin: .4rem .6rem; box-shadow: 0 1px 2px rgba(0,0,0,.06); overflow: hidden; }
  .bracket-row { display: flex; justify-content: space-between; gap: .5rem; padding: .3rem .55rem; font-size: .85rem; }
  .bracket-row + .bracket-row { border-top: 1px solid rgba(0,0,0,.08); }
  .bracket-row.win { font-weight: 700; background: rgba(31,122,109,.10); }
  .bracket-row .nm { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .bracket-section-title { font-weight: 600; font-size: .85rem; opacity: .8; margin: .5rem .25rem .1rem; }
")

# --- UI ----------------------------------------------------------------------

opt_panel <- function(fmt, ...) conditionalPanel(sprintf("input.format == '%s'", fmt), ...)

app_sidebar <- sidebar(
  id = "sidebar", title = i18n$t("Petankki"), width = 330, bg = "white",
  # On phones the collapsed toggle was easy to miss, so show the settings
  # inline above the content; on desktop keep the normal left panel.
  open = list(desktop = "open", mobile = "always-above"),
  textInput("series", i18n$t("Sarja"), placeholder = ui_txt("Kirjoita sarjan nimi")),
  div(class = "sidebar-help mb-3", icon("lock"), " ",
      i18n$t("Toistuva liiga tai ryhmä (esim. \"Tiistailiiga\"). Nimi toimii salasanana – jaa se vain pelaajillesi.")),
  selectInput("instance", i18n$t("Turnaus"), choices = setNames(NEW_INSTANCE, ui_txt("➕ Uusi turnaus"))),
  div(class = "sidebar-help mb-3",
      i18n$t("Yksittäinen turnaus tässä sarjassa – esim. tämän illan kisa.")),

  conditionalPanel(
    sprintf("input.instance == '%s'", NEW_INSTANCE),
    hr(),
    selectInput("format", i18n$t("Pelimuoto"), choices = format_choices(ui_txt)),
    opt_panel("round_robin",
      materialSwitch("opt_home_away", i18n$t("Koti- ja vierasottelut"), status = "primary"),
      numericInput("opt_n_rounds", i18n$t("Kierroksia"), value = 1, min = 1, step = 1)),
    opt_panel("single_elim",
      materialSwitch("opt_third_place", i18n$t("Pronssiottelu"), status = "primary")),
    opt_panel("double_elim",
      materialSwitch("opt_reseed", i18n$t("Uudelleenseedaus kierroksittain"), status = "primary")),
    opt_panel("swiss",
      numericInput("opt_swiss_rounds", i18n$t("Kierroksia"), value = 5, min = 1, step = 1)),
    opt_panel("group_knockout",
      numericInput("opt_groups", i18n$t("Lohkojen määrä"), value = 2, min = 1, step = 1),
      numericInput("opt_advance", i18n$t("Jatkoon per lohko"), value = 2, min = 1, step = 1)),
    selectizeInput("participants", i18n$t("Osallistujat"), choices = NULL, multiple = TRUE,
                   options = list(create = TRUE, placeholder = ui_txt("Lisää pelaajat"))),
    input_task_button("action", i18n$t("Aloita peli!"), label_busy = i18n$t("Luodaan otteluohjelmaa..."),
                      type = "primary", class = "btn-lg w-100")
  )
)

ui <- page_navbar(
  title = i18n$t("Petankkiliiga"),
  window_title = "Petankkiliiga",   # plain string: title is now a <span>, don't infer the tab title from it
  theme = app_theme,
  fillable = FALSE,
  sidebar = app_sidebar,
  header = tagList(
    usei18n(i18n),
    useBusyIndicators(),   # page-level pulse while outputs recompute / tasks run
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$style(app_css))
  ),
  nav_panel(
    title = tagList(icon("bullseye"), " ", i18n$t("Peli")),
    uiOutput("dashboard"),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span(icon("location-crosshairs"), " ", i18n$t("Vuorossa")),
            input_task_button("refresh", icon("rotate"), label_busy = icon("rotate"),
                              type = "light", class = "btn-sm", title = ui_txt("Päivitä")))),
      uiOutput("match_area"),
      conditionalPanel(
        "output.has_match",
        sliderTextInput("game_result", NULL, choices = SCORE_CHOICES, selected = "0-0", width = "100%"),
        div(class = "d-grid gap-2",
          input_task_button("save_points", i18n$t("Tallenna pisteet"), label_busy = i18n$t("Tallennetaan..."),
                            type = "primary", class = "btn-lg"),
          actionButton("skip_button", span(icon("forward-step"), " ", i18n$t("Ohita vuoro")),
                       class = "btn-outline-secondary"))
      )
    ),
    card(card_header(span(icon("ranking-star"), " ", i18n$t("Sarjataulukko"))), DTOutput("leader_board")),
    card(card_header(span(icon("sitemap"), " ", i18n$t("Otteluohjelma"))), uiOutput("matches_view")),
    conditionalPanel("output.has_trn",
      card(card_header(span(icon("users-gear"), " ", i18n$t("Pelaajien hallinta"))),
           uiOutput("roster_panel")))
  ),
  nav_panel(
    title = tagList(icon("chart-line"), " ", i18n$t("Sarjatilastot")),
    card(DTOutput("series_stats"))
  ),
  nav_spacer(),
  nav_item(radioGroupButtons("lang", label = NULL, size = "sm",
                             choiceNames = c("🇫🇮 FI", "🇬🇧 EN"), choiceValues = c("fi", "en"),
                             selected = "fi"))
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Per-session translator so concurrent users can pick different languages
  # without clobbering each other's server-rendered text.
  i18n_s <- i18n$clone(deep = TRUE)
  i18n_r <- reactive({
    l <- input$lang %||% "fi"
    if (l %in% i18n_s$get_languages()) i18n_s$set_translation_language(l)
    i18n_s
  })
  tr <- function(key) i18n_r()$t(key)

  store <- reactiveValues(tournaments = read_tournaments(SHEET_URL),
                          results     = read_results(SHEET_URL))
  rv    <- reactiveValues(trn = NULL, label = NULL, spec = NULL, deferred = character(0))
  # Scratch state carried between a task's launch and its async completion.
  pend  <- reactiveValues(rows = NULL, new_trn = NULL, create = NULL, withdraw = NULL)

  safe_reconstruct <- function(spec, results_df) {
    tryCatch(
      tournament_reconstruct(spec$format, spec$participants, spec$options, spec$seed, results_df),
      error = function(e) {
        showNotification(paste0(tr("Turnausta ei voitu ladata: "), conditionMessage(e)),
                         type = "error", duration = 6); NULL })
  }

  # --- ExtendedTasks: slow Google Sheets I/O off Shiny's single thread --------
  # Each is bound to its trigger button so the button greys out while in flight;
  # run_async() runs the job on a mirai daemon, or synchronously if none.
  save_task <- ExtendedTask$new(function(rows) run_async(APPEND_JOB, rows = rows)) |>
    bind_task_button("confirm_save")
  withdraw_task <- ExtendedTask$new(function(rows) run_async(APPEND_JOB, rows = rows)) |>
    bind_task_button("withdraw_btn")
  refresh_task <- ExtendedTask$new(function() run_async(REFRESH_JOB)) |>
    bind_task_button("refresh")
  create_task <- ExtendedTask$new(function(series, format, participants, options, label, seed)
    run_async(CREATE_JOB, series = series, format = format, participants = participants,
              options = options, label = label, seed = seed)) |>
    bind_task_button("action")
  roster_task <- ExtendedTask$new(function(series, format, participants, options, label, seed)
    run_async(CREATE_JOB, series = series, format = format, participants = participants,
              options = options, label = label, seed = seed)) |>
    bind_task_button("apply_roster")

  # Commit a freshly created / superseded instance once its spec row is written.
  apply_create <- function(pc) {
    store$tournaments <- pc$df
    rv$label    <- pc$label
    rv$spec     <- list(series = pc$series, label = pc$label, format = pc$format,
                        participants = pc$players, options = pc$opts, seed = pc$seed)
    rv$trn      <- pc$trn
    rv$deferred <- character(0)
    updateSelectInput(session, "instance",
      choices = instance_choices(store$tournaments, pc$series, tr), selected = pc$label)
  }

  # Persistent (duration = NULL) error toast carrying a failed task's message.
  task_error <- function(task, prefix = tr("Tallennus epäonnistui: ")) {
    showNotification(paste0(prefix, tryCatch(task$result(), error = function(e) conditionMessage(e))),
                     type = "error", duration = NULL)
  }

  # Language switch: swap static UI client-side, then relabel <select> options
  # (and placeholder) which update_lang doesn't reliably reach.
  observeEvent(input$lang, {
    update_lang(input$lang)
    updateSelectInput(session, "format", choices = format_choices(tr), selected = isolate(input$format))
    updateSelectInput(session, "instance",
      choices = instance_choices(store$tournaments, isolate(input$series), tr),
      selected = isolate(input$instance))
    updateTextInput(session, "series", placeholder = tr("Kirjoita sarjan nimi"))
  }, ignoreInit = TRUE)

  # Debounced series "password" — typed, never listed. Drives the instance list
  # and suggests the series' previous participants (newest roster pre-selected).
  series_r <- debounce(reactive(input$series), 400)
  observeEvent(series_r(), {
    updateSelectInput(session, "instance", choices = instance_choices(store$tournaments, series_r(), tr))
    updateSelectizeInput(session, "participants",
      choices  = series_participants(store$tournaments, series_r()),
      selected = latest_roster(store$tournaments, series_r()))
  }, ignoreNULL = FALSE)

  observeEvent(input$instance, {
    if (identical(input$instance, NEW_INSTANCE)) {
      rv$trn <- NULL; rv$label <- NULL; rv$spec <- NULL; rv$deferred <- character(0)
      return()
    }
    spec <- spec_from_row(store$tournaments, input$instance)
    req(!is.null(spec))
    rv$spec <- spec; rv$label <- spec$label; rv$deferred <- character(0)
    rv$trn  <- safe_reconstruct(spec, results_for_label(store$results, spec$label))
  }, ignoreInit = TRUE)

  # Home/away plays every pairing twice -> toggling doubles the round-robin
  # cycles; untoggling halves them back (e.g. 1 <-> 2).
  observeEvent(input$opt_home_away, {
    cur <- input$opt_n_rounds
    if (is.null(cur) || is.na(cur)) cur <- 1L
    new <- if (isTRUE(input$opt_home_away)) as.integer(cur) * 2L else max(1L, as.integer(cur) %/% 2L)
    updateNumericInput(session, "opt_n_rounds", value = new)
  }, ignoreInit = TRUE)

  # Surface bracketeer's caveat: odd rounds with home/away may not split evenly.
  observeEvent(input$opt_n_rounds, {
    n <- input$opt_n_rounds
    if (isTRUE(input$opt_home_away) && !is.null(n) && !is.na(n) && n %% 2 == 1) {
      showNotification(
        tr("Pariton kierrosmäärä koti- ja vierasotteluissa: parit eivät välttämättä jakaudu tasan."),
        type = "warning", duration = 5)
    }
  }, ignoreInit = TRUE)

  # Start a new instance under the current series.
  observeEvent(input$action, {
    req(nzchar(input$series %||% ""))
    players <- input$participants
    if (length(players) < 2) {
      showNotification(tr("Lisää vähintään kaksi osallistujaa."), type = "error"); return()
    }
    opts <- gather_options(input)

    if (identical(input$format, "group_knockout")) {
      g <- as.integer(opts$groups %||% 2L); a <- as.integer(opts$advance_per_group %||% 2L)
      if (g < 1L || a < 1L || floor(length(players) / g) < a) {
        showNotification(sprintf(
          tr("Joka lohkoon tarvitaan vähintään %d pelaajaa (lohkoja %d, jatkoon %d/lohko). Lisää pelaajia tai vähennä lohkoja/jatkoonpääsijöitä."),
          a, g, a), type = "error", duration = 8)
        return()
      }
    }

    # Build first (also runs bracketeer's dry-run) so a broken spec is never
    # written to the sheet. The slow write + re-read run async via create_task.
    seed <- make_seed()
    trn  <- tryCatch(tournament_new(input$format, players, opts, seed),
                     error = function(e) {
                       showNotification(friendly_error(conditionMessage(e), tr), type = "error", duration = 8)
                       NULL
                     })
    if (is.null(trn)) return()
    label <- make_label(input$series)
    pend$create <- list(trn = trn, label = label, seed = seed, format = input$format,
                        players = players, opts = opts, series = input$series)
    create_task$invoke(input$series, input$format, players, opts, label, seed)
  })

  observeEvent(create_task$status(), {
    if (create_task$status() == "error") return(task_error(create_task))
    if (create_task$status() != "success") return()
    pc <- pend$create; pc$df <- create_task$result(); apply_create(pc)
  }, ignoreInit = TRUE)

  # Save flow: validate the score locally, then make the user confirm the exact
  # scoreline (defuses the slider-slip and the accidental 0-0). For elimination
  # matches the dialog names the phase (final / bronze / semi / quarter). The
  # append itself runs async; local state only advances once the write lands.
  observeEvent(input$save_points, {
    req(rv$trn, rv$label)
    m  <- pick_match(rv$trn, rv$deferred); req(!is.null(m))
    sc <- parse_score(input$game_result)
    new_trn <- tryCatch(
      bracketeer::result(rv$trn, m$stage_id, match = m$match_id, score = sc, overwrite = FALSE),
      error = function(e) {
        showNotification(paste0(tr("Tulosta ei voitu tallentaa: "), conditionMessage(e)),
                         type = "error", duration = 6); NULL })
    if (is.null(new_trn)) return()
    pend$new_trn <- new_trn
    pend$rows    <- make_result_rows(rv$label, m$stage_id, m$match_id, m$participant1, m$participant2, sc)
    phase <- round_phase_label(m, bracketeer::matches(rv$trn, status = "all"), tr)
    showModal(modalDialog(
      title = tagList(icon("location-crosshairs"), " ", tr("Vahvista tulos"),
                      if (!is.null(phase)) span(class = "badge bg-primary ms-2", phase)),
      div(class = "d-flex align-items-center justify-content-center gap-2 py-2 fs-5",
          span(class = "fw-bold text-truncate", m$participant1),
          span(class = "badge bg-secondary", paste0(sc[1], " – ", sc[2])),
          span(class = "fw-bold text-truncate", m$participant2)),
      if (sc[1] == sc[2])
        div(class = "text-center text-warning small",
            icon("triangle-exclamation"), " ", tr("Tasapeli – varmista että tulos on oikein.")),
      footer = tagList(
        modalButton(tr("Peruuta")),
        input_task_button("confirm_save", tr("Vahvista"), label_busy = tr("Tallennetaan..."), type = "primary")),
      easyClose = TRUE, size = "s"))
  })

  observeEvent(input$confirm_save, { save_task$invoke(pend$rows) })

  observeEvent(save_task$status(), {
    if (save_task$status() == "error") { removeModal(); return(task_error(save_task)) }
    if (save_task$status() != "success") return()
    save_task$result()
    store$results <- dplyr::bind_rows(store$results, pend$rows)
    rv$trn        <- pend$new_trn
    rv$deferred   <- character(0)
    updateSliderTextInput(session, "game_result", selected = "0-0")
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(input$skip_button, {
    req(rv$trn)
    m <- pick_match(rv$trn, rv$deferred); req(!is.null(m))
    rv$deferred <- union(rv$deferred, m$compound_match_id)
  })

  observeEvent(input$refresh, { refresh_task$invoke() })

  observeEvent(refresh_task$status(), {
    if (refresh_task$status() == "error") return(task_error(refresh_task, tr("Päivitys epäonnistui: ")))
    if (refresh_task$status() != "success") return()
    res <- refresh_task$result()
    store$tournaments <- res$t
    store$results     <- res$r
    if (!is.null(rv$label) && !is.null(rv$spec)) {
      rv$trn <- safe_reconstruct(rv$spec, results_for_label(store$results, rv$label))
      pend_ids <- if (is.null(rv$trn)) character(0) else {
        pm <- pending_matches(rv$trn); if (is.null(pm) || !nrow(pm)) character(0) else pm$compound_match_id }
      rv$deferred <- intersect(rv$deferred, pend_ids)   # keep this device's still-pending skips
    }
    # New instances created by other players appear only after this re-read.
    updateSelectInput(session, "instance",
      choices = instance_choices(store$tournaments, isolate(input$series), tr),
      selected = isolate(input$instance))
  }, ignoreInit = TRUE)

  # Withdraw a player: forfeit all their pending matches (append-only, so other
  # connections converge on next refresh; LWW preserved).
  observeEvent(input$withdraw_btn, {
    req(rv$trn, rv$label)
    player <- input$withdraw_player; req(nzchar(player %||% ""))
    rows <- forfeit_player_rows(rv$trn, rv$label, player)
    if (is.null(rows)) {
      showNotification(tr("Pelaajalla ei ole avoimia otteluita."), type = "warning"); return() }
    pend$withdraw <- rows
    withdraw_task$invoke(rows)
  })

  observeEvent(withdraw_task$status(), {
    if (withdraw_task$status() == "error") return(task_error(withdraw_task))
    if (withdraw_task$status() != "success") return()
    withdraw_task$result()
    store$results <- dplyr::bind_rows(store$results, pend$withdraw)
    rv$trn        <- safe_reconstruct(rv$spec, results_for_label(store$results, rv$label))
    rv$deferred   <- character(0)
    showNotification(tr("Pelaaja poistettiin ja jäljellä olevat ottelut hävittiin."), type = "message")
  }, ignoreInit = TRUE)

  # Pre-start roster edit (only while the instance has zero results): append a
  # *superseding* spec row with the same label and a fresh seed. spec_from_row
  # resolves to the newest, so reconstruction uses the new roster.
  observeEvent(input$apply_roster, {
    req(rv$spec, rv$label)
    if (nrow(results_for_label(store$results, rv$label)) > 0) {
      showNotification(tr("Turnaus on jo alkanut – osallistujia ei voi enää muokata."), type = "error"); return() }
    new_players <- input$edit_roster
    if (length(new_players) < 2) {
      showNotification(tr("Lisää vähintään kaksi osallistujaa."), type = "error"); return() }
    if (identical(rv$spec$format, "group_knockout")) {
      g <- as.integer(rv$spec$options$groups %||% 2L); a <- as.integer(rv$spec$options$advance_per_group %||% 2L)
      if (floor(length(new_players) / g) < a) {
        showNotification(tr("Pelaajia on liian vähän lohkoihin nähden."), type = "error", duration = 8); return() }
    }
    seed <- make_seed()
    trn  <- tryCatch(tournament_new(rv$spec$format, new_players, rv$spec$options, seed),
                     error = function(e) {
                       showNotification(friendly_error(conditionMessage(e), tr), type = "error", duration = 8); NULL })
    if (is.null(trn)) return()
    pend$create <- list(trn = trn, label = rv$label, seed = seed, format = rv$spec$format,
                        players = new_players, opts = rv$spec$options, series = rv$spec$series)
    roster_task$invoke(rv$spec$series, rv$spec$format, new_players, rv$spec$options, rv$label, seed)
  })

  observeEvent(roster_task$status(), {
    if (roster_task$status() == "error") return(task_error(roster_task))
    if (roster_task$status() != "success") return()
    pc <- pend$create; pc$df <- roster_task$result(); apply_create(pc)
    showNotification(tr("Osallistujat päivitettiin."), type = "message")
  }, ignoreInit = TRUE)

  output$has_match <- reactive({
    !is.null(rv$trn) && !tournament_complete(rv$trn) && !is.null(pick_match(rv$trn, rv$deferred))
  })
  outputOptions(output, "has_match", suspendWhenHidden = FALSE)

  output$has_trn <- reactive({ !is.null(rv$trn) })
  outputOptions(output, "has_trn", suspendWhenHidden = FALSE)

  # Player management: edit the roster before the first result (supersede), or
  # withdraw a player (forfeit) once the tournament is under way.
  output$roster_panel <- renderUI({
    req(rv$trn, rv$spec, rv$label)
    roster <- rv$spec$participants
    if (nrow(results_for_label(store$results, rv$label)) == 0) {
      tagList(
        div(class = "sidebar-help mb-2", icon("circle-info"), " ",
            tr("Turnaus ei ole vielä alkanut – voit lisätä tai poistaa osallistujia.")),
        selectizeInput("edit_roster", tr("Osallistujat"), choices = roster, selected = roster,
                       multiple = TRUE, options = list(create = TRUE)),
        input_task_button("apply_roster", tr("Päivitä osallistujat"),
                          label_busy = tr("Päivitetään..."), class = "w-100"))
    } else {
      tagList(
        div(class = "sidebar-help mb-2", icon("circle-info"), " ",
            tr("Pelaaja, joka poistuu, häviää kaikki jäljellä olevat ottelunsa.")),
        selectInput("withdraw_player", tr("Poistuva pelaaja"), choices = roster),
        input_task_button("withdraw_btn",
                          span(icon("user-xmark"), " ", tr("Poista pelaaja turnauksesta")),
                          label_busy = tr("Tallennetaan..."), type = "danger", class = "w-100"))
    }
  })

  output$dashboard <- renderUI({
    req(rv$trn)
    m <- bracketeer::matches(rv$trn, status = "all")
    played <- if (is.null(m)) 0L else sum(m$status == "complete")
    total  <- if (is.null(m)) 0L else nrow(m)
    st <- standings_table(rv$trn)
    leader <- if (!is.null(st) && nrow(st)) as.character(st$participant[1]) else "—"
    done <- tournament_complete(rv$trn); w <- tournament_winner(rv$trn)
    status_txt <- if (done) (if (is.na(w)) tr("Valmis") else paste0("\U0001F3C6 ", w)) else tr("Käynnissä")
    layout_columns(
      fill = FALSE, col_widths = breakpoints(xs = 6, sm = 4),
      value_box(tr("Tilanne"), status_txt, showcase = icon("flag-checkered"),
                theme = if (done) "success" else "primary"),
      value_box(tr("Otteluita"), sprintf("%d / %d", played, total),
                showcase = icon("list-check"), theme = "secondary"),
      value_box(tr("Kärjessä"), leader, showcase = icon("ranking-star"), theme = "info")
    )
  })

  output$match_area <- renderUI({
    if (is.null(rv$trn))
      return(div(class = "text-center text-muted py-4",
                 icon("arrow-left"), tags$p(class = "mt-2 mb-0",
                 tr("Avaa sarja ja aloita peli vasemmalta."))))
    if (tournament_complete(rv$trn)) {
      w <- tournament_winner(rv$trn)
      return(div(class = "text-center py-4",
                 div(class = "display-5", "\U0001F3C6"),
                 tags$h4(class = "mt-2",
                         if (is.na(w)) tr("Turnaus on valmis!") else paste0(tr("Voittaja"), ": ", w))))
    }
    m <- pick_match(rv$trn, rv$deferred)
    if (is.null(m))
      return(div(class = "text-center text-muted py-4", tr("Ei pelattavia otteluita juuri nyt – paina Päivitä.")))
    phase <- round_phase_label(m, bracketeer::matches(rv$trn, status = "all"), tr)
    tagList(
      if (!is.null(phase))
        div(class = "text-center mb-1",
            span(class = "badge rounded-pill bg-primary", icon("trophy"), " ", phase)),
      div(class = "row align-items-center text-center g-1 py-2",
          div(class = "col-5 match-name text-truncate", title = m$participant1, m$participant1),
          div(class = "col-2 match-vs", "vs"),
          div(class = "col-5 match-name text-truncate", title = m$participant2, m$participant2)))
  })

  output$leader_board <- renderDT({
    req(rv$trn)
    st <- standings_table(rv$trn); req(!is.null(st), nrow(st) > 0)
    keep <- intersect(c("rank", "participant", "wins", "draws", "losses", "points", "score_diff"), names(st))
    d <- st[, keep, drop = FALSE]
    names(d) <- c(rank = tr("Sija"), participant = tr("Pelaaja"), wins = tr("V"), draws = tr("T"),
                  losses = tr("H"), points = tr("Pist"), score_diff = "+/-")[keep]
    mobile_dt(d, tr)
  })

  output$matches_view <- renderUI({
    req(rv$trn)
    matches_view(rv$trn, tr)
  })

  output$series_stats <- renderDT({
    s <- series_r(); req(nzchar(s %||% ""))
    tab <- series_standings(results_for_series(store$tournaments, store$results, s))
    req(nrow(tab) > 0)
    names(tab) <- c(tr("Pelaaja"), tr("Turnaukset"), tr("Ottelut"), tr("V"), tr("T"), tr("H"),
                    tr("Pisteet+"), tr("Pisteet-"), "+/-", tr("Voitto-%"))
    mobile_dt(tab, tr)
  })
}

shinyApp(ui = ui, server = server)
