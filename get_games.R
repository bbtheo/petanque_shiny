library(tidyverse)


sample_one_row <- function(.data){
  if(nrow(.data) == 1) return(.data)
  else {

    row <- tryCatch(
      {.data %>% sample_n(1, replace = T)},
      error = function(e){
        .data %>% sample_n(1, replace = T)})

    return(.data %>% sample_n(1, replace = T))
  }
}

get_possible_games <- function(participants){

  expand.grid(participants, participants) %>%
    rename(
      player_1 = Var1,
      player_2 = Var2,
    ) %>%
    filter(
      player_1 != player_2
    ) %>%
    as_tibble()

}

get_games <- function(participants){



  games <- tibble(
    player_1 = NULL,
    player_2 = NULL
  )

  all_possible_games <- get_possible_games(participants)

  round <- 1

  while (nrow(all_possible_games) > 0) {

    waiting_to_play <- participants
    possible_round <- all_possible_games

    n_game <- 1

    while (length(waiting_to_play) > 0) {

      if(nrow(possible_round) == 0 ){

        going_to_play <- waiting_to_play %>% sample(1)

        possible_games <- all_possible_games %>%
          filter(
            player_1 == going_to_play | player_2 == going_to_play
          )

        if(nrow(possible_games) == 0) {
          waiting_to_play <- waiting_to_play[!waiting_to_play %in% going_to_play]
          next

        }

        game <- possible_games %>% sample_one_row()

        players <- c(game$player_1, game$player_2)

        waiting_to_play <- waiting_to_play[!waiting_to_play %in% going_to_play]

      } else {

        game <- possible_round %>% sample_one_row()

        players <- c(game$player_1, game$player_2)
        waiting_to_play <- waiting_to_play[!waiting_to_play %in% players]

        possible_round <- possible_round %>%
          filter(
            !player_1 %in% players,
            !player_2 %in% players
          )
      }

      games <- games %>%
        bind_rows(game %>% mutate(round = round))

      all_possible_games <- all_possible_games %>%
        filter(
          !(player_1 == players[1] & player_2 == players[2]),
          !(player_1 == players[2] & player_2 == players[1])
        )

      n_game <- n_game + 1

    }

    round <- round + 1

  }

  return(games)

}
