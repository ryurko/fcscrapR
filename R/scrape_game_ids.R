#' Scrapes the game ids from ESPN's soccer scoreboard
#' @param scoreboard_name Name of league or tournament (must be in the
#' league_url_data table) to access the scoreboard for in order to obtain the
#' game ids (default is "show all leagues").
#' @param game_date Date to find scoreboard of game ids for, using the format
#' of "YYYY-MM-DD" (default is the current date).
#' @return Data frame of game ids and columns for each team from the given
#' league/tournament and date's soccer scoreboard.
#' @examples
#' # Scrape today's game ids for all leagues:
#' today_game_ids <- scrape_scoreboard_ids()
#' # Scrape world cup games from June 28th, 2018:
#' wc_game_ids <- scrape_scoreboard_ids("fifa world cup", "2018-06-18")
#' @export

scrape_scoreboard_ids <- function(scoreboard_name = "show all leagues",
                                  game_date = Sys.Date()) {

  # First assert that the scoreboard_name is in the league_url_data table:
  assertthat::assert_that(scoreboard_name %in% league_url_data$name,
                          msg = "Please use a league or tournament name that is in the league_url_data table!")

  # Create the scoreboard url by appending the date to the corresponding
  # scoreboard_name's url:
  scoreboard_url <- paste(league_url_data$url[which(league_url_data$name == scoreboard_name)],
                          "date", stringr::str_remove_all(game_date, "-"), sep = "/")

  # Parse this url so the soccer match urls can be grabbed:
  scoreboard_parse <- scrapeR::scrape(url = scoreboard_url, headers = TRUE,
                                    parse = FALSE)

  # First check to see if there are any soccer matches available, if there
  # are none then print a message, return NA:

  soccer_match_present <- scoreboard_parse %>%
    unlist() %>%
    stringr::str_detect("\"http://www.espn.com/soccer/match\\?gameId=[0-9]{6}\"")

  if (!soccer_match_present) {
    print(paste0("No matches available for ",
                scoreboard_name, " on ", as.character(game_date)))
    return(NA)
  } else {
    # First grab all unique game ids from soccer matches:
    game_ids <- scoreboard_parse %>%
      stringr::str_extract_all("\"http://www.espn.com/soccer/match\\?gameId=[0-9]{6}\"") %>%
      unlist() %>%
      stringr::str_extract("[0-9]{6}") %>%
      unlist() %>%
      unique()

    # Use all the games from the scoreboard and return a dataframe with the
    # game id and columns denoting the two teams:
    games_df <- data.frame(game_id = game_ids)

    teams_df <- suppressWarnings(purrr::map_dfr(game_ids,
                           function(x) {
                             teams <- paste0("http://www.espn.com/soccer/match?gameId=", x) %>%
                               xml2::read_html() %>%
                               rvest::html_nodes(".short-name") %>%
                               rvest::html_text()

                             # Return as data frame:
                             data.frame(team_one = teams[1],
                                        team_two = teams[2])
                             }))

    # Cbind together and return:
    games_df %>%
      dplyr::bind_cols(teams_df) %>%
      return

  }

}
