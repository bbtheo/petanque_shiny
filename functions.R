get_attendace_sheet <- function() read_sheet(sheet_url, sheet = "attendance", col_types = "Tcl")
get_games_sheet <- function() read_sheet(sheet_url, sheet = "games", col_types = "ccTiiiT")

get_current_attendance <- function(n_hours = 6){

  if(nrow(get_attendace_sheet()) == 0) return(NULL)

  get_attendace_sheet() %>%
    filter(time > Sys.time()-hours(!!n_hours))
}

get_new_games <- function(n_hours = 6){

  get_games_sheet() %>%
    filter(set_dttm  > Sys.time() - hours(!!n_hours))
}

get_old_games <- function(n_hours = 6){

  get_games_sheet() %>%
    filter(set_dttm  < Sys.time() - hours(!!n_hours))
}


set_scores_to_table <- function(x,y){

  result <- glue::glue("{x}-{y}")
  result[is.na(A) & is.na(B)] <- NA

  result
}

create_attendace <- function(players){

  tibble(
    time = Sys.time(),
    names = players,
    left = FALSE,
  )

}
