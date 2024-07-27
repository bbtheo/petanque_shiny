library(shiny)
library(bslib)
library(here)
library(googlesheets4)
library(tidyverse)
library(shinyWidgets)
library(DT)

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
    tableOutput("selects"),
    max_height = 800,

  ),
  card(
    card_body(
      h2(textOutput("player_header")),
      sliderTextInput(
        inputId = "game_result",
        label = NULL,
        choices = c(
          "3-0",
          "2-0",
          "1-0",
          "0-0",
          "0-1",
          "0-2",
          "0-3"
        ),
        selected = "0-0",
        width = "100%"
      ),
      input_task_button(
        id = "save_points",
        label = "Tallenna pisteet",
        label_busy = "Tallennetaan..."
      ),
      input_task_button(
        id = "skip_button",
        label = "Ohita vuoro",
        label_busy = "Ohitetaan..."
      )
    ),
    min_height = 250
  ),
  card(
    DT::DTOutput("leader_board")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  games <- reactiveValues(
    new = get_new_games(),
    old = get_old_games()
    )

  points <- reactiveValues(
    player_1 = NA,
    player_2 = NA
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

  output$player_header <-
    renderText({

      glue::glue(
        "{play_turn()$player_1} - {play_turn()$player_2}"
      ) %>%
        as.character()

    })

  observe({
    updateSliderTextInput(
      session = session,
      inputId = "game_result",
      label = NULL,
      selected = "0-0"
    )
    }) %>%
    bindEvent(play_turn())





  observe({
    values <- str_split(input$game_result, '-')

    points$player_1 <- values[[1]][1]
    points$player_2 <- values[[1]][2]

  }) %>%
    bindEvent(input$game_result)


  leader_board <- reactive({

    data <- games$new %>%
      mutate(
        player_1_win = if_else(player_1_point > player_2_point, 1, 0),
        player_2_win = if_else(player_2_point > player_1_point, 1, 0)
      )

    data %>%
      select(
        player_1,
        player_1_point,
        player_2_point,
        player_1_win,
        player_2_win
      ) %>%
      rename(
        player = player_1,
        point = player_1_point,
        lost_point = player_2_point,
        win = player_1_win,
        lost = player_2_win) %>%
      bind_rows(
        data %>%
          select(
            player_2,
            player_2_point,
            player_1_point,
            player_2_win,
            player_1_win
          ) %>%
          rename(
            player = player_2,
            point = player_2_point,
            lost_point = player_1_point,
            win = player_2_win,
            lost = player_1_win
            )
        ) %>%
      group_by(player) %>%
      summarise_if(
        is.numeric,
        sum,
        na.rm = T
        ) %>%
      arrange(desc(point))



  })

  output$leader_board <- DT::renderDT({
    datatable(leader_board())

  })

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
        points$player_1 %>% as.integer(),
        points$player_2 %>% as.integer(),
        Sys.time()
        )
  }) %>%
    bindEvent(input$save_points)





  observe({
    games$new[(
      games$new$player_2 == play_turn()$player_2 &
      games$new$player_1 == play_turn()$player_1),
      c("order_num")] <- max(games$new$order_num) + 1
    }) %>%
    bindEvent(input$skip_button)

}

# Run the application
shinyApp(ui = ui, server = server)

