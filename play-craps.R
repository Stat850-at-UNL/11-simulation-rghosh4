library(tidyverse)

roll_dice <- function() {
  # Input: None
  # Output: an integer from 2 to 12
  # Description: Generate 2 random integers from 1 to 6 and sum them
  
  sample(1:6, 2, replace = TRUE) %>% sum()
}


simulate_craps_game <- function() {
  # Input: None
  # Output: A data frame with id, roll, and outcome for a single craps game
  # Description: Simulate a single craps game
  
  first_roll <- roll_dice()
  rolls <- data.frame(id = 1, roll = first_roll, outcome = 'continue')
  
  if (first_roll %in% c(7, 11)) {
    # Win on the first roll
    rolls$outcome <- "win"
  } else if (first_roll %in% c(2, 3, 12)) {
    # Lose on the first roll
    rolls$outcome <- "loss"
  } else {
    # Continue playing if first roll is a "point"
    point <- first_roll
    outcome <- "continue"
    while (outcome == "continue") {
      next_roll <- roll_dice()
      rolls <- rbind(rolls, data.frame(id = nrow(rolls) + 1, roll = next_roll, outcome = 'continue'))
      
      if (next_roll == point) {
        rolls$outcome[nrow(rolls)] <- "win"
        outcome <- "win"
      } else if (next_roll == 7) {
        rolls$outcome[nrow(rolls)] <- "loss"
        outcome <- "loss"
      }
    }
  }
  
  return(rolls)
}


# simulate_craps_game()


summarize_craps_game <- function(craps_data) {
  # Input: A data frame returned by `simulate_craps_game`
  # Output: A summary row with n_rolls, outcome, and point
  # Description: Summarize the result of a single craps game
  
  n_rolls <- nrow(craps_data)
  outcome <- tail(craps_data$outcome, 1)
  point <- craps_data$roll[1]
  
  return(data.frame(n_rolls = n_rolls, outcome = outcome, first_point = point))
}


# craps_data<-simulate_craps_game() 
# craps_data%>% summarize_craps_game()



run_craps_simulation <- function(N) {
  # Input: an integer N which determines the number of games to simulate
  # Output: A data frame summarizing the results of N craps games
  # Description: Run a simulation of N craps games and summarize each game
  
  results <- lapply(1:N, function(x) simulate_craps_game() %>% summarize_craps_game())
  return(do.call(rbind, results))
}

# result <- run_craps_simulation(N = 5) # demonstrate result
# result

calculate_win_probability <- function(sim_results, specified_first_point) {
  # Input: 
  #   sim_results - A data frame of simulation results
  #   first_point - The point (first roll value) to condition on
  # Output:
  #   A list containing the conditional probability of win, mean, and standard deviation
  
  # Filter for games where the first roll (point) matches the specified first_point
  games_with_point <- sim_results %>% filter(first_point ==  specified_first_point)
  
  if (nrow(games_with_point) == 0) {
    return(list(
      Error = paste("No games with the first point =",  specified_first_point)
    ))
  }
  
  # Calculate the conditional probability of winning
  p_win_given_point <- mean(games_with_point$outcome == "win")
  
  # Calculate mean and standard deviation of wins
  mean_win <- p_win_given_point
  sd_win <- sd(games_with_point$outcome == "win")
  
  # Return results as a list
  return(list(
    Probability_of_Win = p_win_given_point,
    Mean = mean_win,
    Standard_Deviation = sd_win
  ))
}

