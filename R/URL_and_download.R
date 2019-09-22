#' URL Builder
#'
#' This function allows you to build a URL for using the function \code{\link[amateuR]{download_league_data}}.
#' @param league_url_name A unique name for the league used in the URL. Copy this one from kicker.de!
#' @param season The season you are interested in. Since a season always lasts from summer to summer, these are two years. The format is \code{"YYYY-YY"} and the default value is \code{"2018-19"}. Please make sure, the season of interest is still/already online.
#' @examples
#' build_league_url("kreisliga-osterode-fb-1", "2019-20")
#' @export

build_league_url <- function(league_url_name, season = "2018-19") {
  if (any((nchar(season)) != 7 | (unlist(strsplit(season, split = ""))[c(1, 2, 5)] != c("2", "0", "-")))) {
    stop("Incorrect format for season!")
  }

  url <- paste("https://www.kicker.de/", league_url_name, "/spieltag/", season, "/-1", sep = "")
  return(url)
}

#' Data download from kicker.de
#'
#' This function downloads data from kicker.de! Please note that this function is only for the download of German amateur football leagues. It works mostly for professional and international leagues, but occasionally errors can occur here.
#' @param league_url The URL of the kicker.de webpage of the league of interest. Please create this with \code{\link[amateuR]{build_league_url}}. Alternatively you can copy & paste it manually, but make sure it ends with \code{"/-1"}.
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @examples
#' download_league_data(
#'   "https://www.kicker.de/kreisliga-osterode-fb-1/spieltag/2019-20/-1"
#' )
#'
#' download_league_data(build_league_url(
#'   "berlin-kreisklasse-a-staffel-1-6322",
#'   season = "2018-19"
#' ))
#' @export

download_league_data <- function(league_url) {
  # Download the html text
  webpage_content <- xml2::read_html(league_url)
  data_html <- rvest::html_nodes(webpage_content, "div#kick__page")
  data <- rvest::html_text(data_html)

  # Clean string
  data <- gsub("\t", "", data, fixed = TRUE)
  data <- gsub("\r", "", data, fixed = TRUE)
  data <- gsub("[ ]{2,}", " ", data, fixed = FALSE)
  data <- gsub("\n ", "\n", data, fixed = TRUE)
  data <- gsub(" \n", "\n", data, fixed = TRUE)
  data <- gsub("[\n]{2,}", "\n", data, fixed = FALSE)

  # Split string to string vector
  data <- strsplit(x = data, split = "\n", fixed = TRUE) %>% unlist()

  # Delete empty strings
  data <- data[!data %in% c("", " ") ]

  return(data)
}
