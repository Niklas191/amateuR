test_that("The league url is pasted correctly", {
  expect_equal(build_league_url(league_url_name = "kreisliga-osterode-fb-1", "2019-20"), expected = "https://www.kicker.de/kreisliga-osterode-fb-1/spieltag/2019-20/-1")
})

test_that("Warning if format of season is incorrect", {
  expect_warning(build_league_url(league_url_name = "kreisliga-osterode-fb-1", season = "2019-2020"))
})

test_that("Warning, if the team name is wrong", {
  expect_warning(all_game_data(league_data = amateuR::kreisliga_goettingen, team = "This does not make any sense"))
})

test_that("past_games has correct format", {
  expect_equal(ncol(all_game_data(league_data = amateuR::kreisliga_goettingen)$past_games), expected = 4)
})

test_that("past_special_events has correct format", {
  expect_equal(ncol(all_game_data(league_data = amateuR::kreisliga_goettingen)$past_special_events), expected = 3)
})

test_that("future has correct format", {
  expect_equal(ncol(all_game_data(league_data = amateuR::kreisliga_goettingen)$future), expected = 4)
})

test_that("past_games is filled if season is over!", {
  testthat::expect_true(nrow(all_game_data(league_data = amateuR::kreisliga_goettingen)$past_games) > 0)
})

test_that("plot function does not give any output", {
  testthat::expect_null(plot_goal_difference(all_game_data(amateuR::kreisliga_goettingen), team = "TSC Dorste"))
})

test_that("plot function does not give any output", {
  testthat::expect_null(plot_points(all_game_data(amateuR::kreisliga_goettingen), team = "TSC Dorste"))
})

test_that("Test that estimate_params returns a list", {
  testthat::expect_type(estimate_params(all_game_data(download_league_data("https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1")), alpha = 0.9), "list")
})

test_that("Test that estimate_params gives error if alpha is wrong", {
  testthat::expect_error(estimate_params(all_game_data(download_league_data("https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1")), alpha = 2))
})

test_that("Test that prediction returns an error for an invalid team", {
  testthat::expect_error(prediction(
    data = all_game_data(download_league_data(
      "https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1"
    )),
    CI = (estimate_params(all_game_data(download_league_data("https://www.kicker.de/berlin-kreisklasse-a-staffel-1-6322/spieltag/2018-19/-1")), alpha = 0.9)),
    teamname = "1. FC Schöneberg", method = "all"
  )) ## false name here
})
