#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(here)
library(googlesheets4)
library(tidyverse)

source("get_games.R")

sheet_url <- "https://docs.google.com/spreadsheets/d/1yEmFOEKz64m1Ek2ZE6kI4lNQjpxplRz-_zVY_IA8dJA/edit?gid=290683855#gid=290683855"
gs4_auth(email = "helsinginpetankkiliiga@gmail.com")

get_attendace_sheet <- function() read_sheet(sheet_url, sheet = "attendace", col_types = "Tcl")
get_games_sheet <- function() read_sheet(sheet_url, sheet = "games", col_types = "cciiTiT")

get_current_attendance <- function(n_hours = 6){
  get_attendace_sheet() %>%
    filter(time > Sys.time()-hours(!!n_hours))
}


attendace <- get_attendace_sheet()

create_attendace <- function(players){

  tibble(
    time = Sys.time(),
    names = players,
    left = FALSE,
  )

}

sidebar <- sidebar(
  id = "sidebar",
  selectizeInput(
    "participants",
    "Lisää osallistujat",
    choices = attendace %>% distinct(name) %>% pull(),
    selected = attendace %>% filter(time > Sys.time() - hours(6) ) %>% distinct(name) %>% pull(),
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



# Define UInames# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Petankkiliiga",
  theme = bs_theme(bootswatch = "minty"),
  sidebar = sidebar,
  textOutput("selects")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  games <- reactive({

    new_games <-  get_games(input$participants) %>%
      select(-round) %>%
      mutate(
        set_timestamp = Sys.time(),
        order_num = row_number()
        )
    print(new_games)
    bind_rows(get_games_sheet(), new_games)

    }) %>%
    bindEvent(input$action)

  observe({
    write_sheet(games(), ss = sheet_url, sheet = 'games')
  }) %>%
  bindEvent(games())

  observe({

    attendace_data <- tibble(
      time = Sys.time(),
      name = input$participants,
      left = NA
    )

    sheet_append(sheet_url, data = attendace_data, sheet = 'attendace')

  }) %>%
    bindEvent(input$action)

  observe({
    sidebar_toggle(
      id = "sidebar",
      )
    }) %>%
    bindEvent(input$action)


  output$selects <- renderText({
    input$participants
  })



}

# Run the application
shinyApp(ui = ui, server = server)

