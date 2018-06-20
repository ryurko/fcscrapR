#' Scrapes the game ids from the current soccer scoreboard on ESPN
#' @return Vector of game ids from the current day's soccer scoreboard
#' @examples
#' # Scrape today's game ids:
#' today_game_ids <- scrape_scoreboard_ids()
#' @export

scrape_scoreboard_ids <- function() {

  # Access ESPN's soccer scoreboard and parse it so the soccer match urls
  # can be grabbed:
  scoreboard_url <- scrapeR::scrape(url = "http://www.espn.com/soccer/scoreboard",
                                headers = TRUE,
                                parse = FALSE)

  # Find all the soccer match urls then return the unique game ids:
  scoreboard_url %>%
    unlist() %>%
    stringr::str_extract_all("\"http://www.espn.com/soccer/match\\?gameId=[0-9]{6}\"") %>%
    unlist() %>%
    stringr::str_extract("[0-9]{6}") %>%
    unlist() %>%
    unique()
}
