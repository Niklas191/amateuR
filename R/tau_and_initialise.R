#' Tau(x,y)
#'
#' This is an internal function for \code{\link[amateuR]{negloglik}}. It allows one to compensate for the lack of independence in the bivariate Poisson Model for low scoring matches.
#' @param x number of home goals
#' @param y number of away goals
#' @param lambda expected number of home goals
#' @param mu expected number of away goals
#' @param rho correlation term
#' @keywords internal

tau <- Vectorize(function(x, y, lambda, mu, rho) {
  # correction function as detailed in Dixon Coles Paper
  if (x == 0 & y == 0) {
    return(1 - (lambda * mu * rho))
  }
  else if (x == 0 & y == 1) {
    return(1 + (lambda * rho))
  }
  else if (x == 1 & y == 0) {
    return(1 + (mu * rho))
  }
  else if (x == 1 & y == 1) {
    return(1 - rho)
  }
  else {
    return(1)
  }
})

#' Initialising data
#'
#' Set up and optimise a bipoisson likelihood function with the DC correction.
#' This function returns parameter estimates with Wald Confidence intervals.
#' @param data Output of \code{\link[amateuR]{all_game_data}} without specifying a team
#' @param alpha Significance level for confidence intervals
#' @importFrom MASS ginv
#' @importFrom stats dpois optim qnorm
#' @importFrom utils relist
#' @examples
#' my_games <- all_game_data(download_league_data(
#'   "https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1"
#' ))
#'
#' estimate_params(data = my_games, alpha = 0.9)
#' @export

estimate_params <- function(data, alpha = 0.9) {
  if (alpha >= 1 | alpha <= 0) {
    stop("Alpha must be >0 and <1")
  }
  # print full time result in one column
  dat <- data$past_games
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

  # errors and warnings if predictions not possible or reliable
  if (nrow(dat) < n_teams + 2) {
    stop("Error: Too early in Season to make predictions using DC Model. A prediction with this model is only feasable from the 3rd match day onwards")
  }

  if (nrow(dat) < 2 * (n_teams + 2)) {
    warning("Note: Since not many matches have been played, predictions may be unreliable")
  }

  if (length(visitor_teams) != length(home_teams)) {
    warning("Not all teams have played matches home and away. Predictions may be unreliable")
  }

  # create list of params with initial values to use in optimisation
  init <- list(
    attack = rep(0.4, n_teams), defense = rep(-0.3, n_teams),
    home = 0.3, rho = -0.1
  )

  names(init$attack) <- all_teams
  names(init$defense) <- all_teams

  # optimisation function using negloglik() defined in internal function
  optim_res <- optim(
    par = unlist(init),
    fn = negloglik,
    goals_home = dat$Goals_Home,
    goals_visitor = dat$Goals_Away,
    team_home = dat$Home,
    team_visitor = dat$Away,
    structure = init,
    method = "BFGS", hessian = TRUE
  )

  # calculating confidence intervals for attack and defense parameters
  fisher_info <- MASS::ginv(optim_res$hessian) # in case of singular hessian, we can calculate CIs using a generalised inverse
  quant <- qnorm((1 - (1 - alpha) / 2), lower.tail = TRUE)
  prop_sigma <- as.vector(sqrt(diag(fisher_info)))
  upper <- as.vector(optim_res$par) + quant * prop_sigma
  lower <- as.vector(optim_res$par) - quant * prop_sigma
  CI <- matrix(c(lower, optim_res$par, upper), nrow = (2 * n_teams + 2)) # we have 2 estimates per team, home and rho
  rownames(CI) <- names(optim_res$par)
  colnames(CI) <- list("L", "estimate", "U")
  CI <- as.data.frame(CI)

  return(CI)
}

#' Negative loglikelihood function
#'
#' This is an internal function for \code{\link[amateuR]{estimate_params}}
#' @param params likelihood param
#' @param goals_home likelihood param
#' @param goals_visitor likelihood param
#' @param team_home likelihood param
#' @param team_visitor likelihood param
#' @param structure structure
#' @importFrom stats dpois optim qnorm
#' @importFrom utils relist
#' @keywords internal

negloglik <- function(params, goals_home, goals_visitor, team_home, team_visitor, structure) {
  coef <- relist(params, structure)

  # expected goals
  # home team
  lambda_home <- exp(coef$attack[team_home] + coef$defense[team_visitor] + coef$home)

  # Away team
  lambda_visitor <- exp(coef$attack[team_visitor] + coef$defense[team_home])

  # DC adjustment
  dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = coef$rho)

  if (any(dc_adj <= 0 | is.na(dc_adj) == TRUE)) {
    return(Inf)
  }

  # using poisson assumption
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log = TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log = TRUE)
  log_lik <- sum((log_lik_home + log_lik_visitor + log(dc_adj)))

  return(-log_lik)
}
