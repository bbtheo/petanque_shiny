library(shiny)
library(bslib)
library(here)
library(googlesheets4)
library(tidyverse)

source("get_games.R")
source("functions.R")

sheet_url <<- "https://docs.google.com/spreadsheets/d/18NNCnQNt7DCJxT5NzNNg2QQCVRbrj9kWaIZWIVkPdPQ/edit"

gs4_auth(path = readRDS("secrets/google_api"), cache = ".secrets")

attendance <<- get_attendace_sheet()
current_attendance <<- attendance %>% filter(time > Sys.time() - hours(7))


sidebar <- sidebar(
  id = "sidebar",
  open = current_attendance %>% nrow() == 0,
  selectizeInput(
    "participants",
    "Lisää osallistujat",
    choices = attendance %>% distinct(name) %>% pull(),
    selected = current_attendance %>% distinct(name) %>% pull(),
    multiple = T,
    options = list(create = TRUE)
  ),
  input_task_button(
    id = "action",
    label = "Aloita peli!",
    label_busy = "Luodaan otteluohjelmaa...",
    type = "primary"
  )
  )


point_input <- function(id, name){
  selectInput(
    id,
    label = glue::glue("{name} pisteet"),
    choices = c(0:3),
    selected = 0,
    multiple = F
    )
}


# Define UInames# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Petankkiliiga",
  theme = bs_theme(bootswatch = "minty"),
  sidebar = sidebar,
  card(

    tableOutput("selects")
  ),
  card(
    card_body(
      textOutput("players"),
      h2("Syötä pelin tulokset: "),
      fluidRow(
        point_input("player_1_points", "Pelaaja 1"),
        point_input("player_2_points", "Pelaaja 2")
      ),
      input_task_button(
        id = "save_points",
        label = "Tallenna pisteet",
        label_busy = "Tallennetaan..."
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  games <- reactiveValues(
    new = get_new_games(),
    old = get_old_games()
    )

  observe({
    games$new <-  get_games(input$participants) %>%
      select(-round) %>%
      mutate(
        set_dttm = Sys.time(),
        player_1_point = NA,
        player_2_point = NA,
        order_num = row_number(),
        play_dttm = NA,
      )
  }) %>%
  bindEvent(input$action)

  observe({
    bind_rows(
      games$old,
      games$new
    ) %>%
    write_sheet(ss = sheet_url, sheet = 'games')
  }) %>%
    bindEvent(!is.null(games$new))

  observe({
    attendace_data <- tibble(
      time = Sys.time(),
      name = input$participants,
      left = NA
    )
    sheet_append(sheet_url, data = attendace_data, sheet = 'attendance')
  }) %>%
    bindEvent(input$action)

  # after the input
  observe({
    sidebar_toggle(
      id = "sidebar"
      )
    }) %>%
    bindEvent((input$action))

  play_turn <- reactive({

    games$new %>%
      filter(
          is.na(player_1_point),
          is.na(player_2_point)
        ) %>%
      filter(
        order_num == min(order_num)
      )

  })


  observe({
    updateNumericInput(
      session = session,
      inputId = "player_1_points",
      value = 0,
      label = play_turn()$player_1)

    updateNumericInput(
      session = session,
      inputId = "player_2_points",
      value = 0,
      label = play_turn()$player_2)

    }) %>%
    bindEvent(play_turn())

  output$selects <- renderTable({
    games$new %>%
      mutate(score = glue::glue("{player_1_point} - {player_2_point}")) %>%
      mutate(score = if_else(score == "NA - NA", '', score)) %>%
      select(player_1, player_2, score)
  }) %>%
    bindEvent((input$action | !is.null(games$new)))

  # update scores ====================================

  observe({
    games$new[(
      games$new$player_2 == play_turn()$player_2 &
      games$new$player_1 == play_turn()$player_1),
      c("player_1_point","player_2_point", "play_dttm")] <- list(
        input$player_1_points %>% as.integer(),
        input$player_2_points %>% as.integer(),
        Sys.time()
        )
  }) %>%
    bindEvent(input$save_points)


}

# Run the application
shinyApp(ui = ui, server = server)

