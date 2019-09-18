#' Extract past gameday results and date and time of future games from the league data vector
#'
#' This function extracts past gameday results and date and time of future games from the league data vector, which is created with \code{\link[amateuR]{download_league_data}}.
#' @param league_data A league data vector, which is created with \code{\link[amateuR]{download_league_data}}.
#' @param team \code{NA} or an optional parameter to filter the list for a given team name
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @examples
#' all_game_data(download_league_data(
#'   "https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1"
#' ))
#' @export

all_game_data <- function(league_data, team = NA) {
  # use shorter variable name
  data <- league_data

  # Delete unnecessary parts
  ind <- grepl("Montag", data, fixed = TRUE) |
    grepl("Dienstag", data, fixed = TRUE) |
    grepl("Mittwoch", data, fixed = TRUE) |
    grepl("Donnerstag", data, fixed = TRUE) |
    grepl("Freitag", data, fixed = TRUE) |
    grepl("Samstag", data, fixed = TRUE) |
    grepl("Sonntag", data, fixed = TRUE) |
    grepl("Schema", data, fixed = TRUE) |
    grepl("Spielinfo", data, fixed = TRUE) |
    grepl("Vorschau", data, fixed = TRUE) |
    grepl("Analyse", data, fixed = TRUE) |
    grepl("Spieltag", data, fixed = TRUE)
  data <- data[!ind]

  # Extract game day information only
  data <- data[(min(which(data == "Begegnungen")) + 1):(min(which(data == "Tabelle")) - 1)]

  # Split the data in past and future gamedays
  ind <- which(!is.na(as.Date(data, format = "%d.%m."))) %>%
    c(., which(data %in% c("Mo.", "Di.", "Mi.", "Do.", "Fr.", "Sa.", "So.", "29.02."))) %>%
    sv(vec = ., shift = seq(-2, 3)) %>%
    unique()
  ind <- which(data == "heute") %>%
    sv(vec = ., shift = seq(-3, 2)) %>%
    unique() %>%
    c(ind, .) %>%
    sort()
  future_games <- data[ind]
  if (length(ind) > 0) {
    data <- data[-ind]
  }

  # Reshape the data for future games to a two dimentional dataframe
  future_games <- matrix(future_games, ncol = 6, byrow = TRUE) %>% as.data.frame(., stringsAsFactors = FALSE)
  future_games <- future_games[, c(1, 5, 3, 4)]
  colnames(future_games) <- c("Home", "Away", "Date", "Time")
  future_games$Time[future_games$Time == "heute"] <- future_games$Date[future_games$Time == "heute"]
  future_games$Date[future_games$Time == future_games$Date] <- "heute"

  # Generate Output
  if (length(data) == 0) {
    # Output if the season has not started
    past_games <- data.frame(Home = character(), Goals_Home = numeric(), Goals_Away = numeric(), Away = character())
    past_special_events <- data.frame(Home = character(), Away = character(), Event = character())
    L <- list(past_games = past_games, past_special_events = past_special_events, future = future_games)
  } else {
    # Output if the season has already started or is finished
    L <- started_season_data_wrangling(data = data, future_games = future_games)
  }

  L <- all_game_data_filter(L = L, team = team)

  return(L)
}


#' started_season_data_wrangling
#'
#' This is an internal function for \code{\link[amateuR]{all_game_data}}, it performs the data wrangling if the season is already started.
#' @importFrom magrittr %>%
#' @keywords internal


started_season_data_wrangling <- function(data, future_games) {
  # Delete half time results
  past_data <- data[!(seq(1, length(data), 1) %in% (which(data == ":")[c(0, diff(which(data == ":"))) == 3] %>% sv(vec = ., shift = c(-1, 0, 1))))]

  # Delete game comments/notes
  ind <- which(past_data == ":")[c(0, diff(which(past_data == ":"))) == 8] %>% sv(vec = ., shift = c(-6))
  if (length(ind) > 0) {
    past_data <- past_data[-ind]
  }

  # Extract games, where special events happend (e.g. Canceled games)
  ind2 <- which(past_data == ":")[c(0, diff(which(past_data == ":"))) == 12] %>% sv(vec = ., shift = c(-8, -7, -6, -5, -4))
  past_special_events <- past_data[ind2]
  if (length(ind2) > 0) {
    past_games <- past_data[-ind2]
  } else {
    past_games <- past_data
  }

  # Reshape the data for past games to a two dimentional dataframe
  past_games <- matrix(past_games, ncol = 7, byrow = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
  past_games <- past_games[, c(1, 3, 5, 6)]
  colnames(past_games) <- c("Home", "Goals_Home", "Goals_Away", "Away")
  past_games$Goals_Home <- as.numeric(past_games$Goals_Home)
  past_games$Goals_Away <- as.numeric(past_games$Goals_Away)

  # Reshape the data for past special events to a two dimentional dataframe
  past_special_events <- matrix(past_special_events, ncol = 5, byrow = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
  past_special_events <- past_special_events[, c(1, 5, 3)]
  colnames(past_special_events) <- c("Home", "Away", "Event")

  return(list(past_games = past_games, past_special_events = past_special_events, future = future_games))
}


#' all_game_data_filter
#'
#' This is an internal function for \code{\link[amateuR]{all_game_data}}, it perfoms the optional filter for \code{\link[amateuR]{all_game_data}}.
#' @param L Unfiltered result
#' @param team \code{NA}, a team name or a vector of team names. If \code{NA} (default), all games are returned. If team is specified, only the games of this team(s) are returned. If you want to use the result for prediction, use NA.
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @keywords internal

all_game_data_filter <- function(L, team) {
  if (!is.na(team)) {
    if (!(team %in% L$past_games$Home | team %in% L$past_games$Away | team %in% L$past_special_events$Home | team %in% L$past_special_events$Away | team %in% L$future$Home | team %in% L$future)) {
      warning("Unknown team! Returning whole league!")
    } else {
      L$past_games <- L$past_games %>% dplyr::filter(Home == team | Away == team)
      L$past_special_events <- L$past_special_events %>% dplyr::filter(Home == team | Away == team)
      L$future <- L$future %>% dplyr::filter(Home == team | Away == team)
    }
  }
  return(L)
}

#' Shift a vector
#'
#' This is an internal function for \code{\link[amateuR]{all_game_data}}, it shifts the values in vec by the values of the shift vector.
#' @param v A numerical vector to shift
#' @param l A numerical vector with the shifting parameters
#' @param sorted If \code{TRUE}, the returned vector is sorted
#' @importFrom magrittr %>%
#' @keywords internal

sv <- function(vec, shift, sorted = TRUE) {
  new_vec <- c()
  for (i in 1:length(shift)) {
    new_vec <- c(new_vec, vec + shift[i]) %>% unlist(.)
  }
  if (sorted == TRUE) {
    return(sort(new_vec))
  } else {
    return(new_vec)
  }
}
