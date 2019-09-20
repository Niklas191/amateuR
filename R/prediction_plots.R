#' Plotting the Dixon Coles prediction with a confidence interval for a given match
#'
#' This function allows the user to plot the Dixon Coles estimate in a 2d diagram with the home team on the x axis and the away team on the y axis.
#' @param game_data Output of \code{\link[amateuR]{all_game_data}} with parameter \code{team = NA}
#' @param estimate Output of \code{\link[amateuR]{estimate_params}} using the same data
#' @param home Home team name as a string
#' @param away Away team name as a string
#' @details
#' \tabular{ll}{
#' Red point \tab The final full time score for a past match\cr
#' Black circle \tab The point estimate \cr
#' Black box \tab  Confidence interval
#' }
#' @importFrom dplyr filter
#' @importFrom graphics abline legend par plot plot.new points rect segments
#' @examples
#' my_games <- all_game_data(amateuR::kreisliga_goettingen)
#' estimate <- estimate_params(data = my_games, alpha = 0.9)
#'
#' plot_single_match(
#'   game_data = my_games, estimate = estimate,
#'   home = "RSV Göttingen 05", away = "TSC Dorste"
#' )
#' @export

plot_single_match <- function(game_data, estimate, home, away) {
  dat <- game_data$past_games
  CI <- estimate
  H <- home
  V <- away

  for (i in 1:nrow(dat)) {
    dat$FT[i] <- paste0(as.character(dat$Goals_Home[i]), ":", as.character(dat$Goals_Away[i]))
  }

  visitor_teams <- as.vector(sort(unique(dat$Home)))
  home_teams <- as.vector(sort(unique(dat$Away)))
  all_teams <- sort(unique(c(visitor_teams, home_teams))) # all teams who have played at least once at home or away
  n_teams <- length(all_teams)

  # errors for invalid team names
  if (!(H %in% c(all_teams, "NA"))) {
    stop("Error: Home Team name invalid")
  }

  if (!(V %in% c(all_teams, "NA"))) {
    stop("Error: Visitor Team name invalid")
  }

  teamnoH <- match(as.character(H), all_teams)
  teamnoV <- match(as.character(V), all_teams)

  # home
  lambdaL <- exp(CI["home", "L"] + CI[paste0("attack.", H), "L"] + CI[paste0("defense.", V), "L"])
  lambda <- exp(CI["home", "estimate"] + CI[paste0("attack.", H), "estimate"] + CI[paste0("defense.", V), "estimate"])
  lambdaU <- exp(CI["home", "U"] + CI[paste0("attack.", H), "U"] + CI[paste0("defense.", V), "U"])

  # away
  muL <- exp(CI[paste0("attack.", V), "L"] + CI[paste0("defense.", H), "L"])
  mu <- exp(CI[paste0("attack.", V), "estimate"] + CI[paste0("defense.", H), "estimate"])
  muU <- exp(CI[paste0("attack.", V), "U"] + CI[paste0("defense.", H), "U"])

  plot(lambda, mu,
    xlab = "Home Goals", ylab = "Away Goals", xlim = c(0, 10), ylim = c(0, 10),
    main = paste0(H, " vs ", V)
  )
  abline(0, 1, lty = 2)

  # for past matches
  if (nrow(dplyr::filter(dat, Home == H & Away == V)) == 1) {
    fth <- as.character(dplyr::filter(dat, Home == H & Away == V)$Goals_Home)
    fta <- as.character(dplyr::filter(dat, Home == H & Away == V)$Goals_Away)
    points(fth, fta, pch = 19, col = 2, lwd = 3)
    # print(fth, fta)
  }

  # drawing the confidence region
  segments(lambdaL, mu, lambdaU, mu)
  segments(lambda, muL, lambda, muU)
  rect(lambdaL, muL, lambdaU, muU)

  legend("topleft",
    inset = 0.01, legend = c("Final Score", "Prediction"),
    pch = c(19, 1), col = c(2, "black"), bty = "n", lwd = c(0, 1), cex = 0.5
  )
}

#' Plotting the Dixon Coles prediction with a confidence interval for a single team
#'
#' This function allows the user to plot the Dixon Coles estimate in a 2d diagram for a given team.
#' Use the optional argument \code{method} to return all or just future matches.
#' @param game_data Output of \code{\link[amateuR]{all_game_data}} with parameter \code{team = NA}
#' @param estimate Output of \code{\link[amateuR]{estimate_params}} using the same data
#' @param team_name Home team name as string
#' @param method \code{method = "futureH"} returns all predictions for future home matches \cr
#' \code{method = "futureHA"} returns all predictions for future home and away matchs \cr
#' \code{method = "allH"} returns all predictions for past and future home matches
#' @details
#' \tabular{ll}{
#' Red point \tab The final full time score for a past match\cr
#' Black circle \tab The point estimate \cr
#' Black box \tab  Confidence interval
#' }
#' @importFrom dplyr filter
#' @importFrom graphics abline legend par plot plot.new points rect segments
#' @examples
#' my_games <- all_game_data(amateuR::kreisliga_goettingen)
#' estimate <- estimate_params(data = my_games, alpha = 0.9)
#' plot_predictions(game_data = my_games, estimate = estimate, "RSV Göttingen 05", method = "allH")
#' @export

plot_predictions <- function(game_data, estimate, team_name, method = "future") {
  dat <- game_data$past_games
  CI <- estimate
  for (i in 1:nrow(dat)) {
    dat$FT[i] <- paste0(as.character(dat$Goals_Home[i]), ":", as.character(dat$Goals_Away[i]))
  }

  visitor_teams <- as.vector(sort(unique(dat$Home)))
  home_teams <- as.vector(sort(unique(dat$Away)))
  all_teams <- sort(unique(c(visitor_teams, home_teams))) # all teams who have played at least once at home or away
  n_teams <- length(all_teams)

  if (!(method %in% c("futureH", "allH", "futureHA"))) {
    stop("Parameter `method` must be on of: all, past or future!")
  }

  plot.new()
  par(mfrow = c(4, 4))

  # for just future home matches
  if (method == "futureH") {
    for (i in all_teams) {
      if (i == team_name) {
        next
      }
      if (nrow(dplyr::filter(dat, Home == team_name & Away == i)) == 1) {
        next
      }
      if (nrow(dplyr::filter(dat, Home == team_name & Away == all_teams[i])) != 1) {
        plot_single_match(game_data = game_data, estimate = estimate, team_name, i)
      }
    }
  }

  # for all home matches
  if (method == "allH") {
    for (i in all_teams) {
      if (i == team_name) {
        next
      }
      {
        plot_single_match(game_data = game_data, estimate = estimate, team_name, i)
      }
    }
  }

  # for all future matches
  if (method == "futureHA") {
    # home
    for (i in all_teams) {
      if (i == team_name) {
        next
      }
      if (nrow(dplyr::filter(dat, Home == team_name & Away == i)) == 1) {
        next
      }
      if (nrow(dplyr::filter(dat, Home == team_name & Away == all_teams[i])) != 1) {
        plot_single_match(game_data = game_data, estimate = estimate, team_name, i)
      }
    }
    # away
    for (j in all_teams) {
      if (j == team_name) {
        next
      }
      if (nrow(dplyr::filter(dat, Away == team_name & Home == j)) == 1) {
        next
      }
      if (nrow(dplyr::filter(dat, Away == team_name & Home == all_teams[j])) != 1) {
        plot_single_match(game_data = game_data, estimate = estimate, j, team_name)
      }
    }
  }
}
