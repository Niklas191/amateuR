#' Point estimate function for football predictions
#'
#' This function returns point estimates for the number of home and away goals for all remaining matches for one team.
#' @param data Output of \code{\link[amateuR]{all_game_data}} with parameter \code{team = NA}
#' @param estimate Output of \code{\link[amateuR]{estimate_params}} using the same data
#' @param teamname The name of the team of intrest as a string
#' @param method Optional parameter which takes the values \code{"all"}, \code{"past"} or \code{"future"} to filter predictions
#' @return A description of the columns of both prediction data frames, upcoming and future:
#' \tabular{ll}{
#'  \code{Comment} \tab A warning if prediction may not be accurate \cr
#'  \code{Home Estimate} \tab Prediction of the number of goals scored by the home team, rounded to nearest integer\cr
#'  \code{Away Estimate} \tab Prediction of the number of goals scored by the away team, rounded to nearest integer \cr
#'  \code{Final Score} \tab the full time score for past games
#' }
#' @importFrom dplyr filter
#' @examples
#' my_games <- all_game_data(download_league_data(
#'   "https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1"
#' ))
#' estimate <- estimate_params(data = my_games, alpha = 0.9)
#' prediction(data = my_games, estimate = estimate, teamname = "1. FC Schöneberg III", method = "all")
#' @export

prediction <- function(data, estimate, teamname, method = "future") {
  dat <- data$past_games
  CI <- estimate

  for (i in 1:nrow(dat)) {
    dat$FT[i] <- paste0(as.character(dat$Goals_Home[i]), ":", as.character(dat$Goals_Away[i]))
  }

  dat$Home <- as.character(dat$Home)
  dat$Away <- as.character(dat$Away)
  dat$Goals_Away <- as.numeric(as.character(dat$Goals_Away))
  dat$Goals_Home <- as.numeric(as.character(dat$Goals_Home))

  visitor_teams <- as.vector(sort(unique(dat$Home)))
  home_teams <- as.vector(sort(unique(dat$Away)))
  all_teams <- sort(unique(c(visitor_teams, home_teams))) ## all teams who have played at least once at home or away
  n_teams <- length(all_teams)

  # erros and warnings if a prediction is not possible or reliable
  if (!(teamname %in% c(all_teams, "NA"))) {
    stop("Error: Team name invalid")
  }

  if (nrow(dat) < n_teams + 2) {
    stop("Error: Too early in Season to make predictions using DC Model")
  }

  if (nrow(dat) < 2 * (n_teams + 2)) {
    warning("Warning: Too early in Season, prediction may be inaccurate")
  }

  if (!method %in% c("all", "past", "future")) {
    stop("Parameter `method` must be on of: all, past or future!")
  }

  teamno <- match(as.character(teamname), all_teams)
  attack.teamname <- paste0("attack.", teamname)
  defense.teamname <- paste0("defense.", teamname)

  ## set up vectors of length 2*n to populate with results
  lambda <- rep(0, 2 * n_teams)
  mu <- rep(0, 2 * n_teams)
  result <- rep(0, 2 * n_teams)
  Comment <- rep("", 2 * n_teams)
  completed <- rep("", 2 * n_teams)

  for (i in 1:n_teams) {
    vteamname <- all_teams[i]
    # home
    lambda[i] <- exp(CI["home", "estimate"] + CI[attack.teamname, "estimate"] +
      CI[paste0("defense.", all_teams[i]), "estimate"]) # goals scored by fixed home team against team i
    mu[i] <- exp(CI[paste0("attack.", all_teams[i]), "estimate"] + CI[defense.teamname, "estimate"])
    # away
    lambda[i + n_teams] <- exp(CI["home", "estimate"] + CI[defense.teamname, "estimate"] +
      CI[paste0("attack.", all_teams[i]), "estimate"])
    mu[i + n_teams] <- exp(CI[paste0("defense.", all_teams[i]), "estimate"] + CI[attack.teamname, "estimate"])

    # for completed matches fill in the full time score
    if (nrow(dplyr::filter(dat, Home == teamname & Away == vteamname)) == 1) {
      completed[i] <- as.character(dplyr::filter(dat, Home == teamname & Away == vteamname)$FT)
    }
    if (nrow(dplyr::filter(dat, Home == vteamname & Away == teamname)) == 1) {
      completed[n_teams + i] <- as.character(dplyr::filter(dat, Home == vteamname & Away == teamname)$FT)
    }
  }


  for (i in 1:(2 * n_teams)) {
    # final predicted result based on rounded results
    if ((round(lambda[i], 0) > round(mu[i], 0))) {
      result[i] <- "H"
    }
    else if (round(mu[i], 0) > round(lambda[i], 0)) {
      result[i] <- "A"
    }
    else if (round(mu[i], 0) == round(lambda[i], 0)) {
      result[i] <- "D"
    }

    # if the exact point estimates are close together we can not be as sure about the prediction
    if (0.25 < abs(lambda[i] - mu[i]) & abs(lambda[i] - mu[i]) < 0.5) {
      Comment[i] <- "Warning: prediction may not be accurate"
    }
  }

  # only return natural numbers for number of goals
  lambda <- round(lambda, 0)
  mu <- round(mu, 0)

  names(lambda) <- c(rep(teamname, n_teams), all_teams)
  names(mu) <- c(all_teams, rep(teamname, n_teams))
  prediction <- data.frame(
    c(rep(teamname, n_teams), all_teams),
    c(all_teams, rep(teamname, n_teams)), as.vector(lambda), ":",
    as.vector(mu), result, Comment, completed
  )
  colnames(prediction) <- c(
    "Home Team", "Away Team", "Home estimate", ":",
    "Away estimate", "Predicted Result", "Comment", "Final Score"
  )

  results <- subset(prediction, prediction[, 1] != prediction[, 2])
  results2 <- subset(results, results[, 8] != "") # completed matches
  results3 <- subset(results, results[, 8] == "") # future matches

  # exclude matches here which have already been played
  results4 <- list(results2, results3) # all matches
  names(results4) <- c("Completed", "Upcoming")

  if (method == "past") {
    return(results2)
  }
  if (method == "future") {
    return(results3)
  }
  if (method == "all") {
    return(results4)
  }
}

#' Point estimate fuction and confidence intervals for footbal predictions
#'
#' This function returns point estimates for the number of home and away goals for all remaining matches for one team.
#' @param data Output of \code{\link[amateuR]{all_game_data}} with parameter \code{team = NA}
#' @param estimate Output of \code{\link[amateuR]{estimate_params}} using the same data
#' @param teamname The name of the team of intrest as a string
#' @param method Optional parameter which takes the values \code{"all"}, \code{"past"} or \code{"future"} to filter predictions
#' @return A description of the columns of prediction data frames, \emph{upcoming} and \emph{future}:
#' \tabular{ll}{
#'  \code{Home L} \tab Lower bound of confidence interval for the predicted number of goals scored by the \emph{home team} \cr
#'  \code{Home Estimate} \tab Prediction of the number of goals scored by the \emph{home team}, rounded to one decimal point \cr
#'  \code{Home U} \tab Upper bound of confidence interval for the predicted number of goals scored by the \emph{home team} \cr
#'  \code{} \tab \cr
#'  \code{Away L} \tab Lower bound of confidence interval for the predicted number of goals scored by the \emph{away team} \cr
#'  \code{Away Estimate} \tab Prediction of the number of goals scored by the \emph{away team}, rounded to nearest integer \cr
#'  \code{Away U} \tab Upper bound of confidence interval for the predicted number of goals scored by the \emph{away team} \cr
#'  \code{} \tab \cr
#'  \code{Note H} \tab The number of stars indicate the with of the confidence interval for the \emph{home team}. The more stars the larger the interval \cr
#'  \code{Note A} \tab The number of stars indicate the with of the confidence interval for the \emph{away team}. The more stars the larger the interval \cr
#'  \code{Predicted result} \tab Home and away intervals are compared and H,D,A is returned based on the prediction \cr
#'  \code{Final Score} \tab For a completed match the final full time score is given here
#' }
#' @importFrom dplyr filter
#' @examples
#' my_games <- all_game_data(download_league_data(
#'   "https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1"
#' ))
#' estimate <- estimate_params(data = my_games, alpha = 0.9)
#'
#' prediction_interval(
#'   data = my_games, estimate = estimate,
#'   teamname = "1. FC Schöneberg III", method = "all"
#' )
#' @export

prediction_interval <- function(data, estimate, teamname, method = "future") {
  dat <- data$past_games
  CI <- estimate

  for (i in 1:nrow(dat)) {
    dat$FT[i] <- paste0(as.character(dat$Goals_Home[i]), ":", as.character(dat$Goals_Away[i]))
  }

  dat$Home <- as.character(dat$Home)
  dat$Away <- as.character(dat$Away)
  dat$Goals_Away <- as.numeric(as.character(dat$Goals_Away))
  dat$Goals_Home <- as.numeric(as.character(dat$Goals_Home))
  visitor_teams <- as.vector(sort(unique(dat$Home)))
  home_teams <- as.vector(sort(unique(dat$Away)))
  all_teams <- sort(unique(c(visitor_teams, home_teams))) # all teams who have played at least once at home or away
  n_teams <- length(all_teams)

  if (!(teamname %in% c(all_teams, "NA"))) {
    stop("Error: Team name invalid")
  }

  if (nrow(dat) < n_teams + 2) {
    stop("Error: Too early in Season to make predictions using DC Model")
  }

  if (nrow(dat) < 2 * (n_teams + 2)) {
    warning("Warning: Too early in Season, prediction may be inaccurate. Expect very large prediction intervals")
  }

  if (!method %in% c("all", "past", "future")) {
    stop('Parameter method must be on of "all", "past" or "future"!')
  }

  teamno <- match(as.character(teamname), all_teams)
  attack.teamname <- paste0("attack.", teamname)
  defense.teamname <- paste0("defense.", teamname)

  # set up vectors of length 2*n to populate with results for home and away results
  rep(0, 2 * n_teams) -> lambda
  rep(0, 2 * n_teams) -> mu
  rep(0, 2 * n_teams) -> result
  rep("", 2 * n_teams) -> Comment
  rep("", 2 * n_teams) -> completed

  # for CIs
  rep(0, 2 * n_teams) -> lambdaL
  rep(0, 2 * n_teams) -> muL
  rep(0, 2 * n_teams) -> lambdaU
  rep(0, 2 * n_teams) -> muU
  rep("", 2 * n_teams) -> realsc
  rep("", 2 * n_teams) -> Note_A
  rep("", 2 * n_teams) -> Note_H

  for (i in 1:n_teams) {
    vteamname <- all_teams[i]
    # first round
    lambdaL[i] <- exp(CI["home", "L"] + CI[attack.teamname, "L"] + CI[paste0("defense.", all_teams[i]), "L"])
    lambda[i] <- exp(CI["home", "estimate"] + CI[attack.teamname, "estimate"] +
      CI[paste0("defense.", all_teams[i]), "estimate"]) # goals scored by fixed home team against team i
    lambdaU[i] <- exp(CI["home", "U"] + CI[attack.teamname, "U"] + CI[paste0("defense.", all_teams[i]), "U"])

    muL[i] <- exp(CI[paste0("attack.", all_teams[i]), "L"] + CI[defense.teamname, "L"])
    mu[i] <- exp(CI[paste0("attack.", all_teams[i]), "estimate"] + CI[defense.teamname, "estimate"])
    muU[i] <- exp(CI[paste0("attack.", all_teams[i]), "U"] + CI[defense.teamname, "U"])

    # second round
    lambdaL[i + n_teams] <- exp(CI["home", "L"] + CI[defense.teamname, "L"] + CI[paste0("attack.", all_teams[i]), "L"])
    lambda[i + n_teams] <- exp(CI["home", "estimate"] + CI[defense.teamname, "estimate"] +
      CI[paste0("attack.", all_teams[i]), "estimate"])
    lambdaU[i + n_teams] <- exp(CI["home", "U"] - CI[defense.teamname, "U"] + CI[paste0("attack.", all_teams[i]), "U"])

    muL[i + n_teams] <- exp(CI[paste0("defense.", all_teams[i]), "L"] + CI[attack.teamname, "L"])
    mu[i + n_teams] <- exp(CI[paste0("defense.", all_teams[i]), "estimate"] + CI[attack.teamname, "estimate"])
    muU[i + n_teams] <- exp(CI[paste0("defense.", all_teams[i]), "U"] + CI[attack.teamname, "U"])

    # If match in the past, return the actual score
    if (nrow(dplyr::filter(dat, Home == teamname & Away == vteamname)) == 1) {
      realsc[i] <- as.character(dplyr::filter(dat, Home == teamname & Away == vteamname)$FT)
    }

    if (nrow(dplyr::filter(dat, Home == vteamname & Away == teamname)) == 1) {
      realsc[n_teams + i] <- as.character(dplyr::filter(dat, Home == vteamname & Away == teamname)$FT)
    }
  }

  # warning stars dependent on size of interval
  for (i in 1:(2 * n_teams)) {
    warning_A <- as.vector(round(muU, 0) - round(muL, 0))
    if (is.na(warning_A[i])) {
      Note_A[i] <- "NA"
    }
    else if (warning_A[i] < 1) {
      Note_A[i] <- ""
    }
    else if (warning_A[i] < 2) {
      Note_A[i] <- "*"
    }
    else if (warning_A[i] < 3) {
      Note_A[i] <- "**"
    }
    else if (warning_A[i] >= 3) {
      Note_A[i] <- "***"
    }

    warning_H <- as.vector(round(lambdaU, 0) - round(lambdaL, 0))
    if (is.na(warning_H[i])) {
      Note_H[i] <- "NA"
    }
    else if (warning_H[i] < 1) {
      Note_H[i] <- ""
    }
    else if (warning_H[i] < 2) {
      Note_H[i] <- "*"
    }
    else if (warning_H[i] < 3) {
      Note_H[i] <- "**"
    }
    else if (warning_H[i] >= 3) {
      Note_H[i] <- "***"
    }

    # Home win, away win or draw. Home and away intervals overlapping ==> no clear winner
    if (is.na(lambdaU[i]) | is.na(muL[i]) | is.na(lambdaL[i]) | is.na(lambdaU[i])) {
      result[i] <- NA
    }
    else if (muL[i] > lambdaU[i]) {
      result[i] <- "A"
    }
    else if (lambdaL[i] > muU[i]) {
      result[i] <- "H"
    }
    else if ((lambda[i] - mu[i]) > 0.5) {
      result[i] <- "H*"
    }
    else if ((mu[i] - lambda[i]) > 0.5) {
      result[i] <- "A*"
    }
    else if (abs(mu[i] - lambda[i]) > 0.25) {
      result[i] <- "D*"
    }
    else {
      result[i] <- "D"
    }
  }

  # rounding point estimates and CIs
  lambda <- round(lambda, 1)
  lambdaL <- round(lambdaL, 1)
  lambdaU <- round(lambdaU, 1)
  mu <- round(mu, 1)
  muL <- round(muL, 1)
  muU <- round(muU, 1)

  names(lambda) <- all_teams
  names(mu) <- all_teams
  names(lambdaL) <- c(rep(teamname, n_teams), all_teams)
  names(lambdaU) <- c(rep(teamname, n_teams), all_teams)
  names(lambda) <- c(rep(teamname, n_teams), all_teams)
  names(muL) <- c(all_teams, rep(teamname, n_teams))
  names(muU) <- c(all_teams, rep(teamname, n_teams))
  names(mu) <- c(all_teams, rep(teamname, n_teams))

  prediction <- data.frame(
    c(rep(teamname, n_teams), all_teams), c(all_teams, rep(teamname, n_teams)), as.vector(lambdaL),
    as.vector(lambda), as.vector(lambdaU), ":", as.vector(muL), as.vector(mu),
    as.vector(muU), Note_H, Note_A, result, as.character(realsc)
  )

  colnames(prediction) <- c(
    "Home Team", "Away Team", "Home L", "Home estimate", "Home U", ":",
    "Away L", "Away estimate", "Away U", "Note_H", "Note_A", "Predicted result",
    "Final Score"
  )

  results <- subset(prediction, prediction[, 1] != prediction[, 2])
  results2 <- subset(results, results[, 13] != "")
  results3 <- subset(results, results[, 13] == "")

  # exclude matches here which have already been played
  results5 <- list(results2, results3)
  names(results5) <- c("Completed", "Upcoming")

  # return correct subset of results depending on value of method parameter
  if (method == "future") {
    return(results3)
  }
  if (method == "past") {
    return(results2)
  }
  if (method == "all") {
    return(results5)
  }
}
