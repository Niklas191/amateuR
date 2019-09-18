#' Extract the current table from the league data vector
#'
#' This function extracts the current table from the league data vector, which is created with \code{\link[amateuR]{download_league_data}}.
#' @param league_data A league data vector, which is created with \code{\link[amateuR]{download_league_data}}.
#' @examples
#' current_table(download_league_data(
#'   "https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1"
#' ))
#' @export

current_table <- function(league_data) {
  # use shorter variable name
  data <- league_data

  # Extract table
  start <- which(data == "Tabelle")[1] + 1
  end <- which(data == "Tabelle")[2] - 1
  data <- data[start:end]

  # Delete unessesary parts
  ind <- seq(8, length(data), 6)
  data <- data[-ind]

  # Reshape the data vector to a two dimentional dataframe representing the table
  data <- matrix(data, ncol = 5, byrow = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
  data <- data[-1, ]
  colnames(data) <- c("Position", "Team", "Games", "Goal Diff.", "Points")
  rownames(data) <- as.character(seq(1,nrow(data)))

  return(data)
}
