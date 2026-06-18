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

sheets_auth()
ensure_sheets(SHEET_URL)

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
")

# --- UI ----------------------------------------------------------------------

opt_panel <- function(fmt, ...) conditionalPanel(sprintf("input.format == '%s'", fmt), ...)

app_sidebar <- sidebar(
  id = "sidebar", title = i18n$t("Petankki"), width = 330, bg = "white",
  # On phones the collapsed toggle was easy to miss, so show the settings
  # inline above the content; on desktop keep the normal left panel.
  open = list(desktop = "open", mobile = "always-above"),
  textInput("series", i18n$t("Sarja"), placeholder = i18n$t("Kirjoita sarjan nimi")),
  div(class = "sidebar-help mb-3", icon("lock"), " ",
      i18n$t("Toistuva liiga tai ryhmä (esim. \"Tiistailiiga\"). Nimi toimii salasanana – jaa se vain pelaajillesi.")),
  selectInput("instance", i18n$t("Turnaus"), choices = setNames(NEW_INSTANCE, i18n$t("➕ Uusi turnaus"))),
  div(class = "sidebar-help mb-3",
      i18n$t("Yksittäinen turnaus tässä sarjassa – esim. tämän illan kisa.")),

  conditionalPanel(
    sprintf("input.instance == '%s'", NEW_INSTANCE),
    hr(),
    selectInput("format", i18n$t("Pelimuoto"), choices = format_choices(i18n$t)),
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
                   options = list(create = TRUE, placeholder = i18n$t("Lisää pelaajat"))),
    input_task_button("action", i18n$t("Aloita peli!"), label_busy = i18n$t("Luodaan otteluohjelmaa..."),
                      type = "primary", class = "btn-lg w-100")
  )
)

ui <- page_navbar(
  title = "Petankkiliiga",
  theme = app_theme,
  fillable = FALSE,
  sidebar = app_sidebar,
  header = tagList(
    usei18n(i18n),
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
            actionButton("refresh", icon("rotate"), class = "btn-sm btn-light",
                         title = i18n$t("Päivitä")))),
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
    card(card_header(span(icon("list-ol"), " ", i18n$t("Otteluohjelma"))), DTOutput("matches"))
  ),
  nav_panel(
    title = tagList(icon("chart-line"), " ", i18n$t("Sarjatilastot")),
    card(DTOutput("series_stats"))
  ),
  nav_spacer(),
  nav_item(radioGroupButtons("lang", label = NULL, size = "sm",
                             choiceNames = c("FI", "EN"), choiceValues = c("fi", "en"),
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

  reload_store <- function() {
    store$tournaments <- read_tournaments(SHEET_URL)
    store$results     <- read_results(SHEET_URL)
  }

  safe_reconstruct <- function(spec, results_df) {
    tryCatch(
      tournament_reconstruct(spec$format, spec$participants, spec$options, spec$seed, results_df),
      error = function(e) {
        showNotification(paste0(tr("Turnausta ei voitu ladata: "), conditionMessage(e)),
                         type = "error", duration = 6); NULL })
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
    # written to the sheet. Reuse the same seed for the stored spec.
    seed <- make_seed()
    trn  <- tryCatch(tournament_new(input$format, players, opts, seed),
                     error = function(e) {
                       showNotification(friendly_error(conditionMessage(e), tr), type = "error", duration = 8)
                       NULL
                     })
    if (is.null(trn)) return()

    info <- append_tournament(SHEET_URL, series = input$series, format = input$format,
                              participants = players, options = opts, seed = seed)
    rv$label    <- info$label
    rv$spec     <- list(format = input$format, participants = players, options = opts, seed = seed)
    rv$trn      <- trn
    rv$deferred <- character(0)
    store$tournaments <- read_tournaments(SHEET_URL)
    updateSelectInput(session, "instance",
      choices = instance_choices(store$tournaments, input$series, tr), selected = info$label)
  })

  observeEvent(input$save_points, {
    req(rv$trn, rv$label)
    m  <- pick_match(rv$trn, rv$deferred); req(!is.null(m))
    sc <- parse_score(input$game_result)
    # Apply to the engine first — validates the score and never freezes the UI.
    new_trn <- tryCatch(
      bracketeer::result(rv$trn, m$stage_id, match = m$match_id, score = sc, overwrite = FALSE),
      error = function(e) {
        showNotification(paste0(tr("Tulosta ei voitu tallentaa: "), conditionMessage(e)),
                         type = "error", duration = 6); NULL })
    if (is.null(new_trn)) return()
    # Persist; only advance local state once the sheet write succeeds.
    rows <- make_result_rows(rv$label, m$stage_id, m$match_id, m$participant1, m$participant2, sc)
    ok <- tryCatch({ append_results(SHEET_URL, rows); TRUE },
      error = function(e) {
        showNotification(paste0(tr("Tallennus epäonnistui: "), conditionMessage(e)),
                         type = "error", duration = 6); FALSE })
    if (!ok) return()
    store$results <- dplyr::bind_rows(store$results, rows)
    rv$trn        <- new_trn
    rv$deferred   <- character(0)
    updateSliderTextInput(session, "game_result", selected = "0-0")
  })

  observeEvent(input$skip_button, {
    req(rv$trn)
    m <- pick_match(rv$trn, rv$deferred); req(!is.null(m))
    rv$deferred <- union(rv$deferred, m$compound_match_id)
  })

  observeEvent(input$refresh, {
    reload_store()
    if (!is.null(rv$label) && !is.null(rv$spec)) {
      rv$trn <- safe_reconstruct(rv$spec, results_for_label(store$results, rv$label))
    }
    rv$deferred <- character(0)
  })

  output$has_match <- reactive({
    !is.null(rv$trn) && !tournament_complete(rv$trn) && !is.null(pick_match(rv$trn, rv$deferred))
  })
  outputOptions(output, "has_match", suspendWhenHidden = FALSE)

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
    div(class = "row align-items-center text-center g-1 py-2",
        div(class = "col-5 match-name text-truncate", m$participant1),
        div(class = "col-2 match-vs", "vs"),
        div(class = "col-5 match-name text-truncate", m$participant2))
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

  output$matches <- renderDT({
    req(rv$trn)
    m <- bracketeer::matches(rv$trn, status = "all"); req(!is.null(m), nrow(m) > 0)
    d <- data.frame(m$participant1, m$participant2,
      ifelse(is.na(m$score1), "", paste0(m$score1, "–", m$score2)),
      ifelse(m$status == "complete", "✓", "·"), check.names = FALSE)
    names(d) <- c(tr("Pelaaja 1"), tr("Pelaaja 2"), tr("Tulos"), tr("Tila"))
    mobile_dt(d, tr, paging = TRUE, page = 8)
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
