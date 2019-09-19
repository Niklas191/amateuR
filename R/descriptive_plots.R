#' Plot the goal differences
#'
#' This function plots the goal differences for one specified team.
#' @param game_data Output of the function \code{\link[amateuR]{all_game_data}}.
#' @param team A team name as string.
#' @param cumulated If \code{TRUE}, the cummulated goal differences are plotted, if \code{FALSE} the individual differences for every game are plotted.
#' @details Description of the colours of the data points:
#' \tabular{ll}{
#' Green \tab  Matches won \cr
#' Orange \tab  Matches drawn \cr
#' Red \tab  Matches lost
#' }
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom graphics abline legend par plot plot.new points rect segments
#' @examples
#' data <- download_league_data("https://www.kicker.de/kreisliga-osterode-fb-1/spieltag/2019-20/-1")
#' data <- all_game_data(data)
#' plot_goal_difference(game_data = data, team = "RSV Göttingen 05", cumulated = TRUE)
#' plot_goal_difference(game_data = data, team = "RSV Göttingen 05", cumulated = FALSE)
#' @export

plot_goal_difference <- function(game_data, team, cumulated = TRUE) {
  # Test if team name is correct and if there are enough data points for a plot
  plot_test(game_data = game_data, team = team)

  # Filter for team name
  data <- game_data$past_games %>% dplyr::filter(Home == team | Away == team)

  # Compute goal diff
  data$diff <- 0
  data$diff[data$Home == team] <- data$Goals_Home[data$Home == team] - data$Goals_Away[data$Home == team]
  data$diff[data$Away == team] <- data$Goals_Away[data$Away == team] - data$Goals_Home[data$Away == team]

  # Cumulate goal diff
  data$cumdiff <- cumsum(data$diff)

  # Number of game day
  data$game <- seq(1, dim(data)[1], 1)

  # Create plot for cumulated == FALSE
  if (cumulated == FALSE) {
    plot(x = seq(1, max(data$game), length.out = 2), y = seq(min(data$diff), max(data$diff), length.out = 2), main = paste("Goal differences for", team), xlab = "Gameday", ylab = "Goal difference", type = "n")
    abline(h = 0, col = "grey")
    points(x = data$game, y = data$diff, col = "black", type = "l", lty = "dotted")
    points(x = data$game[data$diff > 0], y = data$diff[data$diff > 0], pch = 19, col = "green", bg = "green")
    points(x = data$game[data$diff < 0], y = data$diff[data$diff < 0], pch = 19, col = "red", bg = "red")
    points(x = data$game[data$diff == 0], y = data$diff[data$diff == 0], pch = 19, col = "orange", bg = "orange")
  }

  # Create plot for cumulated == TRUE
  if (cumulated == TRUE) {
    plot(x = seq(1, max(data$game), length.out = 2), y = seq(min(data$cumdiff), max(data$cumdiff), length.out = 2), main = paste("Cumulated goal difference for", team), xlab = "Gameday", ylab = "Cumulated goal difference", type = "n")
    abline(h = 0, col = "grey")
    points(x = data$game, y = data$cumdiff, col = "black", type = "l")
    points(x = data$game[data$diff > 0], y = data$cumdiff[data$diff > 0], pch = 19, col = "green", bg = "green")
    points(x = data$game[data$diff < 0], y = data$cumdiff[data$diff < 0], pch = 19, col = "red", bg = "red")
    points(x = data$game[data$diff == 0], y = data$cumdiff[data$diff == 0], pch = 19, col = "orange", bg = "orange")
  }
}

#' Plot the points
#'
#' This function plots the cummulated points for one specified team.
#' @param game_data Output of the function \code{\link[amateuR]{all_game_data}}.
#' @param team  A team name as string.
#' @details Description of the colours of the data points:
#' \tabular{ll}{
#' Green \tab  Matches won \cr
#' Orange \tab Matches drawn \cr
#' Red \tab  Matches lost
#' }
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom graphics abline legend par plot plot.new points rect segments
#' @examples
#' data <- download_league_data("https://www.kicker.de/kreisliga-osterode-fb-1/spieltag/2019-20/-1")
#' data <- all_game_data(data)
#' plot_points(game_data = data, team = "RSV Göttingen 05")
#' @export

plot_points <- function(game_data, team) {
  # Test if team name is correct and if there are enough data points for a plot
  plot_test(game_data = game_data, team = team)

  # Filter for team name
  data <- game_data$past_games %>% dplyr::filter(Home == team | Away == team)

  # Compute points
  data$points <- 0
  data$points[data$Home == team & (data$Goals_Home > data$Goals_Away)] <- 3
  data$points[data$Away == team & (data$Goals_Home < data$Goals_Away)] <- 3
  data$points[data$Goals_Home == data$Goals_Away] <- 1

  # Cumulate points
  data$cum_points <- cumsum(data$points)
  data$game <- seq(1, dim(data)[1], 1)

  # Create plot
  plot(x = seq(1, max(data$game), length.out = 2), y = seq(0, max(data$cum_points), length.out = 2), main = paste("Cumulated points for", team, "over time"), xlab = "Gameday", ylab = "Cumulated points", type = "n")
  points(x = data$game, y = data$cum_points, col = "black", type = "l")
  points(x = data$game[data$points == 3], y = data$cum_points[data$points == 3], pch = 19, col = "green", bg = "green")
  points(x = data$game[data$points == 1], y = data$cum_points[data$points == 1], pch = 19, col = "orange", bg = "orange")
  points(x = data$game[data$points == 0], y = data$cum_points[data$points == 0], pch = 19, col = "red", bg = "red")
}


#' plot_test
#'
#' This is an internal function for \code{\link[amateuR]{plot_goal_difference}} and \code{\link[amateuR]{plot_points}}.
#' It performs a test if the team name is correct and if there are enough data points for a plot.
#' @param game_data A league data vector, which is created with \code{\link[amateuR]{download_league_data}}.
#' @param team A team name as string.
#' @importFrom magrittr %>%
#' @keywords internal

plot_test <- function(game_data, team) {
  if (!(team %in% game_data$past_games$Home | team %in% game_data$past_games$Away)) {
    if (!(team %in% game_data$past_games$Home | team %in% game_data$past_games$Away | team %in% game_data$future$Away | team %in% game_data$future$Home | team %in% game_data$past_special_events$Away | team %in% game_data$past_special_events$Away)) {
      stop("Unknown team!")
    } else {
      stop("No past games! Plot only available after first gameday!")
    }
  }
}
