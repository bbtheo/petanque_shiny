# Petankkiliiga — tournament app backed by the bracketeer engine.
#
# State is event-sourced in Google Sheets (see sheets_io.R): a write-once spec
# row per tournament instance + an append-only results game-log. The live
# bracketeer tournament is rebuilt deterministically on load. Recurring weekly
# tournaments are grouped into a `series`.
#
# The `series` name is a soft password: it is NEVER listed in the UI, so a user
# can only reach a series (its tournaments + stats) by typing its exact name.
# Unknown name => they can only create a new series. This keeps groups from
# stumbling onto and wrecking each other's data.
library(shiny)
library(bslib)
library(googlesheets4)
library(dplyr)
library(shinyWidgets)
library(DT)

# Show all timestamps in Helsinki time regardless of the server timezone
# (deploys like Posit Connect Cloud typically run in UTC).
Sys.setenv(TZ = "Europe/Helsinki")

source("sheets_io.R")
source("tournament_engine.R")

SHEET_URL     <- "https://docs.google.com/spreadsheets/d/18NNCnQNt7DCJxT5NzNNg2QQCVRbrj9kWaIZWIVkPdPQ/edit"
NEW_INSTANCE  <- "<NEW>"
SCORE_CHOICES <- c("3-0", "2-0", "1-0", "0-0", "0-1", "0-2", "0-3")

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
    group_knockout = { opts$groups <- as.integer(input$opt_groups); opts$advance_per_group <- as.integer(input$opt_advance) },
    two_leg        = {}
  )
  opts
}

# Turn a bracketeer build error into a friendly Finnish message.
friendly_error <- function(msg) {
  if (grepl("n_participants|integer >= 2", msg)) return("Tarvitaan vähintään 2 osallistujaa.")
  if (grepl("Dry-run failed|Group `", msg))
    return("Pelaajia on liian vähän lohkoihin nähden – lisää pelaajia tai vähennä lohkoja/jatkoonpääsijöitä.")
  paste0("Asetukset eivät kelpaa: ", msg)
}

pick_match <- function(trn, deferred) {
  pl <- pending_matches(trn)
  if (is.null(pl) || nrow(pl) == 0) return(NULL)
  cand <- pl[!pl$compound_match_id %in% deferred, , drop = FALSE]
  if (nrow(cand) > 0) cand[1, , drop = FALSE] else pl[1, , drop = FALSE]
}

# Instances of a series, newest first (plus the "new" option). Only ever called
# with an exact, user-typed series name — never the full list.
instance_choices <- function(tournaments_df, series) {
  ch <- c("➕ Uusi turnaus" = NEW_INSTANCE)
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

mobile_dt <- function(df, paging = FALSE, page = 8) {
  datatable(df, rownames = FALSE, class = "compact stripe nowrap",
            options = list(dom = if (paging) "tp" else "t", scrollX = TRUE,
                           paging = paging, pageLength = page, ordering = FALSE,
                           language = list(emptyTable = "Ei vielä dataa", zeroRecords = "Ei tuloksia")))
}

# --- Theme & polish ----------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#1f7a6d",
  "border-radius" = "0.8rem"
)

app_css <- HTML("
  .btn { border-radius: .7rem; }
  .card { box-shadow: 0 1px 4px rgba(0,0,0,.07); margin-bottom: 1rem; }
  /* 16px inputs prevent iOS auto-zoom on focus */
  .form-control, .form-select, .selectize-input, .selectize-dropdown { font-size: 16px; }
  .match-name { font-size: clamp(1.1rem, 5vw, 1.6rem); font-weight: 700; line-height: 1.2; }
  .match-vs { font-size: .9rem; letter-spacing: .08em; text-transform: uppercase; opacity: .55; }
  .irs--shiny .irs-bar, .irs--shiny .irs-handle>i:first-child { background: var(--bs-primary); }
  table.dataTable td, table.dataTable th { white-space: nowrap; }
  .sidebar-help { font-size: .8rem; opacity: .7; }
")

# --- UI ----------------------------------------------------------------------

opt_panel <- function(fmt, ...) conditionalPanel(sprintf("input.format == '%s'", fmt), ...)

app_sidebar <- sidebar(
  id = "sidebar", title = "Petankki", width = 330, bg = "white",
  textInput("series", "Sarja", placeholder = "Kirjoita sarjan nimi"),
  div(class = "sidebar-help mb-3", icon("lock"),
      " Toistuva liiga tai ryhmä (esim. \"Tiistailiiga\"). Nimi toimii salasanana – jaa se vain pelaajillesi."),
  selectInput("instance", "Turnaus", choices = c("➕ Uusi turnaus" = NEW_INSTANCE)),
  div(class = "sidebar-help mb-3",
      "Yksittäinen turnaus tässä sarjassa – esim. tämän illan kisa."),

  conditionalPanel(
    sprintf("input.instance == '%s'", NEW_INSTANCE),
    hr(),
    selectInput("format", "Pelimuoto", choices = FORMAT_CHOICES),
    opt_panel("round_robin",
      materialSwitch("opt_home_away", "Koti- ja vierasottelut", status = "primary"),
      numericInput("opt_n_rounds", "Kierroksia", value = 1, min = 1, step = 1)),
    opt_panel("single_elim",
      materialSwitch("opt_third_place", "Pronssiottelu", status = "primary")),
    opt_panel("double_elim",
      materialSwitch("opt_reseed", "Uudelleenseedaus kierroksittain", status = "primary")),
    opt_panel("swiss",
      numericInput("opt_swiss_rounds", "Kierroksia", value = 5, min = 1, step = 1)),
    opt_panel("group_knockout",
      numericInput("opt_groups", "Lohkojen määrä", value = 2, min = 1, step = 1),
      numericInput("opt_advance", "Jatkoon per lohko", value = 2, min = 1, step = 1)),
    selectizeInput("participants", "Osallistujat", choices = NULL, multiple = TRUE,
                   options = list(create = TRUE, placeholder = "Lisää pelaajat")),
    input_task_button("action", "Aloita peli!", label_busy = "Luodaan otteluohjelmaa...",
                      type = "primary", class = "btn-lg w-100")
  )
)

ui <- page_navbar(
  title = "Petankkiliiga",
  theme = app_theme,
  fillable = FALSE,
  sidebar = app_sidebar,
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(app_css)
  ),
  nav_panel(
    title = tagList(icon("bullseye"), " Peli"),
    uiOutput("dashboard"),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span(icon("location-crosshairs"), " Vuorossa"),
            actionButton("refresh", icon("rotate"), class = "btn-sm btn-light",
                         title = "Päivitä"))),
      uiOutput("match_area"),
      conditionalPanel(
        "output.has_match",
        sliderTextInput("game_result", NULL, choices = SCORE_CHOICES, selected = "0-0", width = "100%"),
        div(class = "d-grid gap-2",
          input_task_button("save_points", "Tallenna pisteet", label_busy = "Tallennetaan...",
                            type = "primary", class = "btn-lg"),
          actionButton("skip_button", span(icon("forward-step"), " Ohita vuoro"),
                       class = "btn-outline-secondary"))
      )
    ),
    card(card_header(span(icon("ranking-star"), " Sarjataulukko")), DTOutput("leader_board")),
    card(card_header(span(icon("list-ol"), " Otteluohjelma")), DTOutput("matches"))
  ),
  nav_panel(
    title = tagList(icon("chart-line"), " Sarjatilastot"),
    card(DTOutput("series_stats"))
  )
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  store <- reactiveValues(tournaments = read_tournaments(SHEET_URL),
                          results     = read_results(SHEET_URL))
  rv    <- reactiveValues(trn = NULL, label = NULL, spec = NULL, deferred = character(0))

  reload_store <- function() {
    store$tournaments <- read_tournaments(SHEET_URL)
    store$results     <- read_results(SHEET_URL)
  }

  # Rebuild a tournament from its spec, surfacing failures as a notice (never a freeze).
  safe_reconstruct <- function(spec, results_df) {
    tryCatch(
      tournament_reconstruct(spec$format, spec$participants, spec$options, spec$seed, results_df),
      error = function(e) {
        showNotification(paste0("Turnausta ei voitu ladata: ", conditionMessage(e)),
                         type = "error", duration = 6); NULL })
  }

  # Debounced series "password" — typed, never listed. Drives the instance list
  # and suggests the series' previous participants (newest roster pre-selected).
  series_r <- debounce(reactive(input$series), 400)
  observeEvent(series_r(), {
    updateSelectInput(session, "instance", choices = instance_choices(store$tournaments, series_r()))
    updateSelectizeInput(session, "participants",
      choices  = series_participants(store$tournaments, series_r()),
      selected = latest_roster(store$tournaments, series_r()))
  }, ignoreNULL = FALSE)

  # Load an existing instance (reconstruct), or clear for a new one.
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

  # Home/away means every pairing is played twice, so toggling it doubles the
  # round-robin cycles; untoggling halves them back (e.g. 1 <-> 2).
  observeEvent(input$opt_home_away, {
    cur <- input$opt_n_rounds
    if (is.null(cur) || is.na(cur)) cur <- 1L
    new <- if (isTRUE(input$opt_home_away)) as.integer(cur) * 2L else max(1L, as.integer(cur) %/% 2L)
    updateNumericInput(session, "opt_n_rounds", value = new)
  }, ignoreInit = TRUE)

  # Start a new instance under the current series.
  observeEvent(input$action, {
    req(nzchar(input$series %||% ""))
    players <- input$participants
    if (length(players) < 2) {
      showNotification("Lisää vähintään kaksi osallistujaa.", type = "error"); return()
    }
    opts <- gather_options(input)

    # Clear upfront message for the one player-count constraint a format imposes:
    # group→knockout needs at least `advance_per_group` players in every group.
    if (identical(input$format, "group_knockout")) {
      g <- as.integer(opts$groups %||% 2L); a <- as.integer(opts$advance_per_group %||% 2L)
      if (g < 1L || a < 1L || floor(length(players) / g) < a) {
        showNotification(sprintf(
          "Joka lohkoon tarvitaan vähintään %d pelaajaa (lohkoja %d, jatkoon %d/lohko). Lisää pelaajia tai vähennä lohkoja/jatkoonpääsijöitä.",
          a, g, a), type = "error", duration = 8)
        return()
      }
    }

    # Build first — this also runs bracketeer's structural dry-run — so a broken
    # spec is never written to the sheet. Reuse the same seed for the stored spec.
    seed <- make_seed()
    trn  <- tryCatch(tournament_new(input$format, players, opts, seed),
                     error = function(e) {
                       showNotification(friendly_error(conditionMessage(e)), type = "error", duration = 8)
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
      choices = instance_choices(store$tournaments, input$series), selected = info$label)
  })

  observeEvent(input$save_points, {
    req(rv$trn, rv$label)
    m  <- pick_match(rv$trn, rv$deferred); req(!is.null(m))
    sc <- parse_score(input$game_result)
    # 1) Apply to the engine first — this validates the score (e.g. rejects a
    #    bad arity) and never freezes the UI; nothing is changed on failure.
    new_trn <- tryCatch(
      bracketeer::result(rv$trn, m$stage_id, match = m$match_id, score = sc, overwrite = FALSE),
      error = function(e) {
        showNotification(paste0("Tulosta ei voitu tallentaa: ", conditionMessage(e)),
                         type = "error", duration = 6); NULL })
    if (is.null(new_trn)) return()
    # 2) Persist; only advance local state if the sheet write succeeds, so the
    #    in-memory tournament never drifts ahead of what's stored.
    rows <- make_result_rows(rv$label, m$stage_id, m$match_id, m$participant1, m$participant2, sc)
    ok <- tryCatch({ append_results(SHEET_URL, rows); TRUE },
      error = function(e) {
        showNotification(paste0("Tallennus epäonnistui: ", conditionMessage(e)),
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

  # Whether there is a match to play right now (drives the controls' visibility).
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
    status_txt <- if (done) (if (is.na(w)) "Valmis" else paste0("\U0001F3C6 ", w)) else "Käynnissä"
    layout_columns(
      fill = FALSE, col_widths = breakpoints(xs = 6, sm = 4),
      value_box("Tilanne", status_txt, showcase = icon("flag-checkered"),
                theme = if (done) "success" else "primary"),
      value_box("Otteluita", sprintf("%d / %d", played, total),
                showcase = icon("list-check"), theme = "secondary"),
      value_box("Kärjessä", leader, showcase = icon("ranking-star"), theme = "info")
    )
  })

  output$match_area <- renderUI({
    if (is.null(rv$trn))
      return(div(class = "text-center text-muted py-4",
                 icon("arrow-left"), tags$p(class = "mt-2 mb-0",
                 "Avaa sarja ja aloita peli vasemmalta.")))
    if (tournament_complete(rv$trn)) {
      w <- tournament_winner(rv$trn)
      return(div(class = "text-center py-4",
                 div(class = "display-5", "\U0001F3C6"),
                 tags$h4(class = "mt-2", if (is.na(w)) "Turnaus on valmis!" else paste0("Voittaja: ", w))))
    }
    m <- pick_match(rv$trn, rv$deferred)
    if (is.null(m))
      return(div(class = "text-center text-muted py-4",
                 "Ei pelattavia otteluita juuri nyt – paina Päivitä."))
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
    names(d) <- c(rank = "Sija", participant = "Pelaaja", wins = "V", draws = "T",
                  losses = "H", points = "Pist", score_diff = "+/-")[keep]
    mobile_dt(d)
  })

  output$matches <- renderDT({
    req(rv$trn)
    m <- bracketeer::matches(rv$trn, status = "all"); req(!is.null(m), nrow(m) > 0)
    d <- data.frame(
      `Pelaaja 1` = m$participant1, `Pelaaja 2` = m$participant2,
      Tulos = ifelse(is.na(m$score1), "", paste0(m$score1, "–", m$score2)),
      Tila  = ifelse(m$status == "complete", "✓", "·"),
      check.names = FALSE)
    mobile_dt(d, paging = TRUE, page = 8)
  })

  output$series_stats <- renderDT({
    s <- series_r(); req(nzchar(s %||% ""))
    tab <- series_standings(results_for_series(store$tournaments, store$results, s))
    req(nrow(tab) > 0)
    names(tab) <- c("Pelaaja", "Turnaukset", "Ottelut", "V", "T", "H",
                    "Pisteet+", "Pisteet-", "+/-", "Voitto-%")
    datatable(tab, rownames = FALSE, class = "compact stripe nowrap",
              options = list(dom = "t", scrollX = TRUE, paging = FALSE, ordering = TRUE,
                             language = list(emptyTable = "Ei vielä dataa")))
  })
}

shinyApp(ui = ui, server = server)
