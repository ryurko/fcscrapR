#' Scrapes the game ids from the current soccer scoreboard on ESPN
#' @return Data-Frame of game ids and competing team names from the selected league/day's soccer scoreboard
#' @param league league name from league_df
#' @param date_in date submitted in (MM/DD/YYYY) format suggested
#' @examples scrape_scoreboard_ids("Show All Leagues","02/07/2018")
#' scrape_scoreboard_ids("FIFA World Cup","02/07/2018")
#' # Scrape today's game ids:
#' today_game_ids <- scrape_scoreboard_ids()
#' @export

scrape_scoreboard_ids <- function(league,date_in) {
  require(lubridate)
  require(stringr)
  ## conver date to yyyy-mm-dd
  date_in2 <- as.Date(parse_date_time(date_in,"dmy"))
  date_f <- str_replace_all(as.character(date_in2), "[^[:alnum:]]", "")

  ## conver date to yyyy-mm-dd
  base_url <- "http://www.espn.com/soccer"
  league_index <- which(league == league_df$league_names)
  combine_url <- paste0(base_url,league_df$league_urls[league_index],"/date/",date_f)

  scoreboard_url <- scrapeR::scrape(url = combine_url,
                  headers = TRUE,
                  parse = FALSE) %>% unlist()

  game_ids <- scoreboard_url %>%
    stringr::str_extract_all("\"http://www.espn.com/soccer/match\\?gameId=[0-9]{6}\"") %>%
    unlist() %>%
    stringr::str_extract("[0-9]{6}") %>%
    unlist() %>%
    unique()

  game_df <- data.frame(game_ids)

  game_df$teams <- purrr::map(game_ids,function(x){
    paste0("http://www.espn.com/soccer/match?gameId=",x) %>%
      read_html() %>% html_nodes(".short-name") %>% html_text()
  })
  return(game_df)
}
