### Extract league
library(rvest)
url <- "http://www.espn.com/soccer/scoreboard/_/league/all/date/20180620"
league_names <- read_html(url) %>% html_nodes(".drop-left a") %>% html_text()
league_urls <- read_html(url) %>% html_nodes(".drop-left a") %>% html_attr("href")

league_df <- data.frame(league_names,league_urls)
league_df$league_urls <- as.character(league_df$league_urls)
league_df$league_urls[1] <- "/scoreboard/_/league/all"

save(league_df,file="data/league_df.rda")

