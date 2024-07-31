get_attendace_sheet <- function() read_sheet(sheet_url, sheet = "attendance", col_types = "Tclc")
get_games_sheet <- function() read_sheet(sheet_url, sheet = "games", col_types = "ccTiiiTci")



set_scores_to_table <- function(x,y){

  result <- glue::glue("{x}-{y}")
  result[is.na(A) & is.na(B)] <- NA

  result
}

create_attendace <- function(players, label){

  tibble(
    time = Sys.time(),
    names = players,
    left = FALSE,
    label = label
  )

}


