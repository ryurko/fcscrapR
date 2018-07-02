#' Scrapes the game ids from the current soccer scoreboard on ESPN
#' @return Vector of game ids from the current day's soccer scoreboard
#' @examples
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
  base_url <- "http://www.espn.com"
  league_index <- which(league == league_df$league_names)
  combine_url <- paste0(base_url,league_df$league_urls[league_index],"/date/",date_f)
  combine_url

  read_html(url) %>%
    html_nodes("summary") %>%
    html_text()





}
