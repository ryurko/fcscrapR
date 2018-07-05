#' Scrapes ESPN match commentary for a given game ID
#'
#' @param game_id ESPN game ID to scrape the match commentary for.
#' @details Through string parsing of the commentary the following columns are
#' created:
#' \itemize{
#'  \item{"game_id"} - Unique identifier for the game from ESPN
#'  \item{"commentary"} - Raw text commentary
#'  \item{"match_time"} - Match time provided by ESPN in character format
#'  \item{"team_one"} - Name of first team in game based on order on game page
#'  \item{"team_two"} - Name of second team in game based on order on game page
#'  \item{"team_one_score"} - Score for team one
#'  \item{"team_two_score"} - Score for team two
#'  \item{"half_begins"} - Indicator for when a half begins
#'  \item{"half_end"} - Indicator for when a half ends
#'  \item{"match_end"} - Indicator for when the match ends
#'  \item{"shot_attempt"} - Indicator for if a shot was attempted
#'  \item{"penalty_shot"} - Indicator for if a the shot attempt was a penalty shot
#'  which includes penalty shootout
#'  \item{"shot_result"} - Outcome for shot attempt, either: goal, blocked,
#'  missed, saved, or own_goal
#'  \item{"shot_with"} -  Which foot (left or right) or header person shot with
#'  \item{"shot_where"} - Location from field relative to box shot took place from
#'  \item{"shot_by_player"} - Player who attempted shot
#'  \item{"shot_by_team"} - Team that attempted shot
#'  \item{"assist_by_player"} - Player who assisted shot attempt
#'  \item{"assist_type"} - Denotes if assist was with a through or cross
#'  \item{"foul"} - Indicator if there was a foul
#'  \item{"foul_by_player"} - Which player committed the foul
#'  \item{"foul_by_team"} - Which team committed the foul
#'  \item{"net_location"} - Location relative to net of shot attempt
#'  \item{"follow_set_piece"} - Indicator if followed set piece situation
#'  \item{"follow_corner"} - Indicator if followed corner kick
#'  \item{"offside"} - Indicator if offside was called
#'  \item{"offside_team"} - Which team was offside
#'  \item{"offside_player"} - Which player was offside
#'  \item{"offside_pass_from"} - Which player passed the ball leading to offside
#'  \item{"shown_card"} - Indicator if a card was shown
#'  \item{"card_type"} - Type of card: red or yellow
#'  \item{"card_player"} - Player given the card
#'  \item{"card_team"} - Team given the card
#'  \item{"video_review"} - Indicator if a video review took place
#'  \item{"video_review_event"} - Which event was reviewed
#'  \item{"video_review_result"} - Result of the video review
#'  \item{"delay_in_match"} - Indicator for if a delay in match was called
#'  \item{"delay_team"} - Which team caused the delay
#'  \item{"free_kick_won"} - Indicator for if a free kick was won
#'  \item{"free_kick_player"} - Which player won the free kick
#'  \item{"free_kick_team"} - Which team won the free kick
#'  \item{"free_kick_where"} - Where was the free kick won
#'  \item{"corner"} - Indicator if a corner kick was awarded
#'  \item{"corner_team"} - Which team was awarded the corner kick
#'  \item{"corner_conceded_by"} - Which player conceded the corner kick
#'  \item{"substitution"} - Indicator for if a substitution took place
#'  \item{"sub_injury"} - Indicator for if substitution took place because of injury
#'  \item{"sub_team"} - Which team had the substitution
#'  \item{"sub_player"} - Which player came in during the substitution
#'  \item{"replaced_player"} - Which player was replaced during the substitution
#'  \item{"penalty"} - Indicator if penalty occurred
#'  \item{"team_drew_penalty"} - Which team was awarded the penalty
#'  \item{"team_conceded_penalty"} - Which team conceded the penalty
#'  \item{"player_drew_penalty"} - Which player drew the penalty
#'  \item{"player_conceded_penalty"} - Which player conceded the penalty
#'  \item{"half"} - Numeric indicator for the half: 1 = first, 2 = second,
#'  3 = first extra, 4 = second extra, 5 = penalty shootout
#'  \item{"comment_id"} - Row id for comment
#'  \item{"match_time_numeric"} - Numeric value for match time minutes
#'  \item{"stoppage_time"} - Indicator for if in stoppage time
#'  \item{"team_one_penalty_score"} - Tally of team one score in penalty shootout
#'  \item{"team_two_penalty_score"} - Tally of team two score in penalty shootout
#' }
#' @return Data frame of the match commentary with a row for each event in the
#' commentary along with the columns described above.
#' @examples
#' # Get the match commentary for Serbia vs Costa Rica in the 2018 World Cup:
#' srb_crc_commentary <- scrape_commentary(498194)
#' @export

scrape_commentary <- function(game_id) {

  # Create the game url:
  game_url <- tryCatch(xml2::read_html(paste("http://www.espn.com/soccer/commentary?gameId=",
                                     game_id, sep = "")),
                       error = function(cond) {
                         message("Invalid URL - either the commentary page does not exist yet or you entered an invalid game id.")
                         message(paste("Here is the url: ", "http://www.espn.com/soccer/commentary?gameId=",
                                       game_id, sep = ""))
                         message(cond)
                         # Just return NA
                         return(NA)
                       })

  # Read the commentary text from the url:
  commentary_text <- game_url %>%
    rvest::html_nodes("#match-commentary-1-tab-1 .game-details") %>%
    rvest::html_text()

  # If length of commentary is 0 then display message and return NA:
  if (length(commentary_text) == 0) {
    print("Commentary page is not yet available for the game id you entered.")
    print(paste("Here is the url: ", "http://www.espn.com/soccer/commentary?gameId=",
                game_id, sep = ""))
    return(NA)
  }

  # Read the commentary time stamps from the url:
  commentary_time <- game_url %>%
    rvest::html_nodes("#match-commentary-1-tab-1 .time-stamp") %>%
    rvest::html_text()

  # Create dataframe to store the commentary text and time trimming spacing,
  # along with a column for the game id:
  commentary_df <- data.frame(game_id = rep(game_id, length(commentary_text)),
                              commentary = commentary_text,
                              match_time = commentary_time) %>%
    dplyr::mutate(commentary = stringr::str_trim(commentary, side = "both"))

  # Extract team_one and team_two from the page heading:
  game_teams <- game_url %>%
    rvest::html_nodes(".short-name") %>%
    rvest::html_text()

  # Manual check for DC United since they're in commentary as D.C. United:
  game_teams <- ifelse(game_teams == "DC United",
                       "D.C. United", game_teams)

  # Next, need to do a check regarding the fact ESPN will sometimes use
  # Spanish spelling for the team name inside the commentary, will first
  # create a vector original_game_teams to store the original game_teams:
  original_game_teams <- game_teams

  # Need to find the commentary that is either half ends or match ends to then
  # get the team names used in the dataset:
  commentary_game_teams <- commentary_df %>%
    dplyr::pull(commentary) %>%
    .[stringr::str_detect(., "First Half ends")] %>%
    .[1] %>%
    stringr::str_remove("First Half ends, ") %>%
    stringr::str_split(",") %>%
    unlist() %>%
    stringr::str_extract("(([:alpha:])+(\\s){0,1})+") %>%
    stringr::str_trim()


  # Find which team names don't match:
  incorrect_team_i <- which(!(original_game_teams %in% commentary_game_teams))

  # If the length of this is 1 exactly then modify that team in game_teams:
  if (length(incorrect_team_i) == 1) {
    game_teams[incorrect_team_i] <- commentary_game_teams[incorrect_team_i]
  }

  # Both would require more sophisticated matching techniques

  # Next add columns for these two teams and then go through a series of conditional
  # parsing of the actual commentary to extract useful features for analysis:
  commentary_df <- commentary_df %>%
    dplyr::mutate(team_one = game_teams[1],
                  team_two = game_teams[2],
                  # Extract the team one score:
                  team_one_score = ifelse(stringr::str_detect(commentary,
                                                              paste(team_one,
                                                                    "(\\s){1}([:digit:]){1,2}",
                                                                    sep = "")),
                                          stringr::str_extract(commentary,
                                                               paste(team_one,
                                                                     "(\\s){1}([:digit:]){1,2}",
                                                                     sep = "")) %>%
                                            stringr::str_extract("([:digit:]){1,2}"), NA),
                  # Team two score
                  team_two_score = ifelse(stringr::str_detect(commentary,
                                                              paste(team_two,
                                                                    "(\\s){1}([:digit:]){1,2}",
                                                                    sep = "")),
                                          stringr::str_extract(commentary,
                                                               paste(team_two,
                                                                     "(\\s){1}([:digit:]){1,2}",
                                                                     sep = "")) %>%
                                            stringr::str_extract("([:digit:]){1,2}"), NA),
                  # Set the score to be 0-0 for when the First Half begins:
                  team_one_score = ifelse(stringr::str_detect(commentary,
                                                              "First Half begins"),
                                          0, team_one_score),
                  team_two_score = ifelse(stringr::str_detect(commentary,
                                                              "First Half begins"),
                                          0, team_two_score),
                  # Indicator for half ends:
                  half_end = ifelse(stringr::str_detect(commentary,
                                                        "Half ends"), 1, 0),
                  # Indicator for match ends:
                  match_end = ifelse(stringr::str_detect(commentary,
                                                         "Match ends"), 1, 0),
                  # Indicator for half begins:
                  half_begins = ifelse(stringr::str_detect(commentary,
                                                           "Half begins"), 1, 0),
                  # Indicator for shot attempt:
                  shot_attempt = ifelse(stringr::str_detect(commentary,
                                                            "^(Attempt|Goal|(Penalty (saved|missed)))"),
                                        1, 0),
                  # Penalty shot indicator:
                  penalty_shot = ifelse(stringr::str_detect(commentary,"converts penalty") |
                                          stringr::str_detect(commentary, "^Penalty (saved|missed)"),
                                        1, 0),
                  # Shot result:
                  shot_result = ifelse(shot_attempt == 1 &
                                         stringr::str_detect(commentary,
                                                             "Goal"),
                                       "goal",
                                       ifelse(shot_attempt == 1 &
                                                stringr::str_detect(commentary,
                                                                    "^(Attempt|Penalty) (blocked|saved|missed)"),
                                              stringr::str_extract(commentary,
                                                                   "^(Attempt|Penalty) (blocked|saved|missed)") %>%
                                                stringr::str_extract("(blocked|saved|missed)"),
                                              NA)),
                  # Detect own goal result:
                  shot_result = ifelse(stringr::str_detect(commentary,
                                                           "Own Goal"),
                                       "own_goal", shot_result),

                  # Which player shot - first for goals:
                  shot_by_player = ifelse(shot_result == "goal",
                                          stringr::str_extract(commentary,
                                                               paste("Goal\\!(\\s){1,2}",
                                                                     team_one, " ([:digit:]){1,2}, ",
                                                                     team_two, " ([:digit:]){1,2}\\. ",
                                                                     "([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? ",
                                                                     "\\((",team_one,"|",team_two,")\\)", sep = "")) %>%
                                            stringr::str_extract("\\.(\\s){1}([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?( (S|J)r\\.)?(\\s){1}\\(") %>%
                                            stringr::str_extract("([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?"),
                                          NA),
                  # Next for attempts that are not penalty shots:
                  shot_by_player = ifelse(shot_result %in% c("blocked", "saved", "missed") & penalty_shot == 0,
                                          stringr::str_extract(commentary,
                                                               paste("Attempt(\\s){1}", shot_result, "\\.(\\s){1}",
                                                                     "([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?( (S|J)r\\.)? ",
                                                                     "\\((",team_one,"|",team_two,")\\)", sep = "")) %>%
                                            stringr::str_extract("\\.(\\s){1}([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?(\\s){1}\\(") %>%
                                            stringr::str_extract("([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?"),
                                          shot_by_player),
                  # Now for penalty shots:
                  shot_by_player = ifelse(shot_result %in% c("saved", "missed") & penalty_shot == 1,
                                          stringr::str_extract(commentary,
                                                               paste("Penalty(\\s){1,2}", shot_result, "\\!(\\s){1,2}",
                                                                     "([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?( (S|J)r\\.)? \\(", sep = "")) %>%
                                            stringr::str_remove(paste("Penalty ", shot_result,"\\!(\\s){1,2}", sep = "")) %>%
                                            stringr::str_remove(" \\("),
                                          shot_by_player),
                  # Next for own goal:
                  shot_by_player = ifelse(shot_result == "own_goal",
                                          stringr::str_extract(commentary,
                                                               paste("Own Goal by ",
                                                                     "([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?,", sep = "")) %>%
                                            stringr::str_remove("Own Goal by ") %>%
                                            stringr::str_remove(","),
                                          shot_by_player),
                  # Which team shot:
                  shot_by_team = ifelse(!is.na(shot_result) & shot_result != "own_goal",
                                        stringr::str_extract(commentary,
                                                             paste("\\(",
                                                                   team_one,"|",
                                                                   team_two,"\\)", sep = "")) %>%
                                          stringr::str_extract(paste(team_one, "|",
                                                                     team_two, sep = "")),
                                        NA),
                  # Which team shot the own_goal:
                  shot_by_team = ifelse(!is.na(shot_result) & shot_result == "own_goal",
                                        stringr::str_extract(commentary,
                                                             paste(", ", team_one, "|",
                                                                   team_two, "\\.", sep = "")) %>%
                                          stringr::str_extract(paste(team_one, "|",
                                                                     team_two, sep = "")),
                                        shot_by_team),
                  # What did they shoot with:
                  shot_with = ifelse(shot_attempt == 1,
                                     stringr::str_extract(commentary,
                                                          "((left|right)(\\s){1}footed)|(header)") %>%
                                       stringr::str_extract("left|right|header"), NA),
                  # Where did they shoot it from:
                  shot_where = ifelse(shot_attempt == 1,
                                      stringr::str_extract(commentary,
                                                           "from ((([:alpha:])+(\\s){1})+box)|((([:alpha:])+(\\s){1})+range)") %>%
                                        stringr::str_remove("from ") %>%
                                        stringr::str_remove("header ") %>%
                                        stringr::str_remove("footed "), NA),

                  # Location relative to the net (this is done in stages based on type of shot_result).
                  # First for goals:
                  net_location = ifelse(shot_result == "goal",
                                        stringr::str_extract(commentary,
                                                             "to the (([:alpha:])+(\\s){0,1})+\\.") %>%
                                          stringr::str_remove("to the ") %>%
                                          stringr::str_remove("\\.") %>%
                                          stringr::str_remove(" following a set piece situation"), NA),
                  # For saves:
                  net_location = ifelse(shot_result == "saved",
                                        stringr::str_extract(commentary,
                                                             "saved in (([:alpha:])+(\\s){0,1})+\\.") %>%
                                          stringr::str_remove("saved in ") %>%
                                          stringr::str_remove("\\.") %>%
                                          stringr::str_remove(" following a set piece situation"), net_location),
                  # For misses:
                  net_location = ifelse(shot_result == "missed",
                                        stringr::str_extract(commentary,
                                                             "(range|box) (is|(misses to the)) (([:alpha:])+(,)?(\\s){0,1})+\\.") %>%
                                          stringr::str_remove("is ") %>%
                                          stringr::str_remove("close ") %>%
                                          stringr::str_remove("box ") %>%
                                          stringr::str_remove("misses to the ") %>%
                                          stringr::str_remove("\\.") %>%
                                          stringr::str_remove(" following a set piece situation") %>%
                                          stringr::str_remove("range ") %>%
                                          stringr::str_remove(" following a corner") %>%
                                          stringr::str_remove(" from a (direct)? free kick"), net_location),

                  # Who had the assist (even if it wasn't a goal, just to record):
                  assist_by_player = ifelse(stringr::str_detect(commentary, "Assisted by"),
                                            stringr::str_extract(commentary,
                                                                 "Assisted by ([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?((\\.)|with)?") %>%
                                              stringr::str_remove("Assisted by ") %>%
                                              stringr::str_remove("\\.") %>% stringr::str_remove(" with"), NA),
                  # Foul indicator:
                  foul = ifelse(stringr::str_detect(commentary,
                                                    "Foul by"), 1, 0),
                  # Which player fouled:
                  foul_by_player = ifelse(foul == 1,
                                          stringr::str_remove(commentary,
                                                              "Foul by ") %>%
                                            stringr::str_extract("([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? \\(") %>%
                                            stringr::str_remove(" \\("), NA),
                  # Which team fouled:
                  foul_by_team = ifelse(foul == 1,
                                        stringr::str_extract(commentary,
                                                             paste("\\(", team_one,
                                                                   "|", team_two, "\\)",
                                                                   sep = "")) %>%
                                          stringr::str_extract(paste(team_one, "|",
                                                                     team_two, sep = "")),
                                        NA),

                  # Follow set piece indicator:
                  follow_set_piece = ifelse(stringr::str_detect(commentary,
                                                                "following a set piece situation"),
                                            1, 0),
                  # Type of assist:
                  assist_type = ifelse(shot_attempt == 1 &
                                         stringr::str_detect(commentary,
                                                             "with a (through)|(cross)"),
                                       stringr::str_extract(commentary,
                                                            "with a (through)|(cross)") %>%
                                         stringr::str_remove("with a "), NA),
                  # Follows corner indicator:
                  follow_corner = ifelse(stringr::str_detect(commentary,
                                                             "following a corner"),
                                         1, 0),

                  # Offside indicator:
                  offside = ifelse(stringr::str_detect(commentary,
                                                       "Offside"),
                                   1, 0),
                  # Offside team:
                  offside_team = ifelse(offside == 1,
                                        stringr::str_extract(commentary,
                                                             paste("Offside, ",
                                                                   team_one, "|",
                                                                   team_two, sep = "")) %>%
                                          stringr::str_extract(paste(team_one, "|",
                                                                     team_two, sep = "")),
                                        NA),
                  # Offside player:
                  offside_player = ifelse(offside == 1,
                                          stringr::str_extract(commentary,
                                                               ", but ([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? is caught") %>%
                                            stringr::str_remove(", but ") %>%
                                            stringr::str_remove(" is caught"), NA),
                  # Offside pass player:
                  offside_pass_from = ifelse(offside == 1,
                                             stringr::str_extract(commentary,
                                                                  "\\. ([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? tries") %>%
                                               stringr::str_remove("\\. ") %>%
                                               stringr::str_remove(" tries"), NA),

                  # Was a player shown a card:
                  shown_card = ifelse(stringr::str_detect(commentary,
                                                          "shown the (red)|(yellow) card"),
                                      1, 0),
                  # Type of card shown:
                  card_type = ifelse(shown_card == 1,
                                     stringr::str_extract(commentary,
                                                          "shown the (red)|(yellow) card") %>%
                                       stringr::str_extract("(red)|(yellow)"), NA),
                  # Player shown card:
                  card_player = ifelse(shown_card == 1,
                                       stringr::str_extract(commentary,
                                                            "^([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? \\(") %>%
                                         stringr::str_remove(" \\("),
                                       NA),
                  # Team shown card:
                  card_team = ifelse(shown_card == 1,
                                     stringr::str_extract(commentary,
                                                          paste("([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? \\(",
                                                                team_one, "|", team_two, "\\)", sep = "")) %>%
                                       stringr::str_extract(paste(team_one, "|", team_two, sep = "")),
                                     NA),

                  # Video review indicator:
                  video_review = ifelse(stringr::str_detect(commentary,
                                                            "Video Review:"),
                                        1, 0),
                  # What is reviewed:
                  video_review_event = ifelse(video_review == 1,
                                              stringr::str_extract(commentary,
                                                                   "Video Review: (([:alpha:])+(\\s){0,1})+\\.") %>%
                                                stringr::str_remove("Video Review: ") %>%
                                                stringr::str_remove("\\."), NA),
                  # Review result:
                  video_review_result = ifelse(video_review == 1,
                                               stringr::str_extract(commentary,
                                                                    "Referee decision on field (([:alpha:])+(\\s){0,1})+\\.") %>%
                                                 stringr::str_remove("Referee decision on field ") %>%
                                                 stringr::str_remove("\\."), NA),

                  # Delay in match:
                  delay_in_match = ifelse(stringr::str_detect(commentary,
                                                              "Delay in match"),
                                          1, 0),
                  # Which team delayed:
                  delay_team = ifelse(delay_in_match == 1,
                                      stringr::str_extract(commentary,
                                                           paste(team_one, "|",
                                                                 team_two, sep = "")),
                                      NA),

                  # Free kick won indicator:
                  free_kick_won = ifelse(stringr::str_detect(commentary,
                                                             "wins a free kick"),
                                         1, 0),
                  # Which player won free kick:
                  free_kick_player = ifelse(free_kick_won == 1,
                                            stringr::str_extract(commentary,
                                                                 "^([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? \\(") %>%
                                              stringr::str_remove(" \\("),
                                            NA),
                  # Which team won free kick:
                  free_kick_team = ifelse(free_kick_won == 1,
                                          stringr::str_extract(commentary,
                                                               paste(team_one, "|",
                                                                     team_two, sep = "")),
                                          NA),
                  # Where did they win the free kick:
                  free_kick_where = ifelse(free_kick_won == 1,
                                           stringr::str_extract(commentary,
                                                                "free kick in the (([:alpha:])+(\\s){0,1})+\\.") %>%
                                             stringr::str_remove("free kick in the ") %>%
                                             stringr::str_remove("\\."), NA),

                  # Corner kick?
                  corner = ifelse(stringr::str_detect(commentary,
                                                      "^Corner"),
                                  1, 0),
                  # Corner team:
                  corner_team = ifelse(corner == 1,
                                       stringr::str_extract(commentary,
                                                            paste(team_one, "|",
                                                                  team_two, sep = "")),
                                       NA),
                  # Who conceded the corner:
                  corner_conceded_by = ifelse(corner == 1,
                                              stringr::str_extract(commentary,
                                                                   "Conceded by ([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?") %>%
                                                stringr::str_remove("Conceded by "),
                                              NA),

                  # Substitution?
                  substitution = ifelse(stringr::str_detect(commentary,
                                                            "^Substitution"),
                                        1, 0),
                  # Substitution because of injury:
                  sub_injury = ifelse(substitution == 1 & stringr::str_detect(commentary,
                                                          " because of an injury"),
                                      1, 0),
                  # Substitution team:
                  sub_team = ifelse(substitution == 1,
                                    stringr::str_extract(commentary,
                                                         paste(team_one, "|",
                                                               team_two, sep = "")),
                                    NA),
                  # Sub player:
                  sub_player = ifelse(substitution == 1,
                                      stringr::str_extract(commentary,
                                                           "([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? replaces") %>%
                                        stringr::str_remove(" replaces"), NA),
                  # Replaced player:
                  replaced_player = ifelse(substitution == 1,
                                           stringr::str_extract(commentary,
                                                                "replaces ([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?") %>%
                                             stringr::str_remove("replaces ") %>%
                                             stringr::str_remove(" because of") %>%
                                             stringr::str_remove(" because"), NA),

                  # Penalty:
                  penalty = ifelse(stringr::str_detect(commentary,
                                                       "^Penalty "),
                                   1, 0),
                  # Team that drew penalty:
                  team_drew_penalty = ifelse(penalty == 1 & stringr::str_detect(commentary,
                                                                                paste("^Penalty ",
                                                                                      team_one, "|",
                                                                                      team_two, sep = "")),
                                             stringr::str_extract(commentary,
                                                                  paste(team_one, "|",
                                                                        team_two, sep = "")),
                                             NA),
                  # Player that drew penalty:
                  player_drew_penalty = ifelse(penalty == 1 & stringr::str_detect(commentary,
                                                                                  paste("^Penalty ",
                                                                                        team_one, "|",
                                                                                        team_two, sep = "")),
                                               stringr::str_extract(commentary,
                                                                    "\\. ([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? draws a") %>%
                                                 stringr::str_remove("\\. ") %>%
                                                 stringr::str_remove(" draws a"), NA),
                  # Player conceded penalty:
                  player_conceded_penalty = ifelse(penalty == 1 & stringr::str_detect(commentary,
                                                                                    "^Penalty conceded by"),
                                                 stringr::str_extract(commentary,
                                                                      "^Penalty conceded by ([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)? \\(") %>%
                                                   stringr::str_remove("Penalty conceded by ") %>%
                                                   stringr::str_remove(" \\("), NA),
                  # Team conceded penalty:
                  team_conceded_penalty = ifelse(penalty == 1 & stringr::str_detect(commentary,
                                                                                    "^Penalty conceded by"),
                                                 stringr::str_extract(commentary,
                                                                      paste(team_one, "|",
                                                                            team_two, sep = "")),
                                                 NA),

                  # Create a numeric marker for the half:
                  half = ifelse(stringr::str_detect(commentary,
                                               "First Half (begins|ends)"), 1,
                           ifelse(stringr::str_detect(commentary,
                                                      "Second Half (begins|ends)"),
                                  2, ifelse(stringr::str_detect(commentary, "First Half Extra"),
                                            3, ifelse(stringr::str_detect(commentary, "Second Half Extra"),
                                                      4, ifelse(stringr::str_detect(commentary, "Penalty Shootout (begins|ends)"), 5, NA))))),
                  # Create a comment ID column and which will allow the data to be
                  # reordered and then manipulated more to populate the half and
                  # score variables:
                  comment_id = seq(from = n(), to = 1, by = -1),
                  # Indicator variable for stoppage time:
                  stoppage_time = ifelse(stringr::str_detect(match_time,
                                                             "\\+"), 1, 0)) %>%
    # Reorder the rows so the game starts is first:
    dplyr::arrange(comment_id) %>%
    # Fill in the missings based on the assumption all changes in half or score
    # are already captured:
    dplyr::mutate(half = zoo::na.locf(half, na.rm = FALSE),
                  team_one_score = zoo::na.locf(team_one_score, na.rm = FALSE),
                  team_two_score = zoo::na.locf(team_two_score, na.rm = FALSE),
                  # Update the penalty_shot indicator for the penalty shootout:
                  penalty_shot = ifelse(half == 5, 1, penalty_shot),
                  # Get the player who attempted the shot during penalty_shootouts:
                  shot_by_player = ifelse(penalty_shot == 1 & half == 5,
                                          stringr::str_extract(commentary,
                                                               "(\\.|\\!)(\\s){1}([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?(\\s){1}\\(") %>%
                                            stringr::str_extract("([:alpha:]){1,20}(')?(\\s)?([:alpha:]){1,20}(('|-| )?([:alpha:]){1,14})?('o)?((\\s){1}(S|J)r\\.)?"),
                                          shot_by_player),
                  # Create columns for the penalty_goal counts for each team,
                  # based on the numbers in () after the corresponding team's
                  # score - where 0 happens when the () is missing:
                  team_one_penalty_score = ifelse(penalty_shot == 1 & half == 5,
                                                  stringr::str_extract(commentary,
                                                                       paste(team_one,
                                                                             " ", team_one_score,
                                                                             "(\\(([:digit:]){1}\\))?(,|\\.)",
                                                                             sep = "")),
                                                  NA),
                  team_one_penalty_score = ifelse(!is.na(team_one_penalty_score),
                                                  ifelse(stringr::str_detect(team_one_penalty_score,
                                                                             "\\(([:digit:]){1}\\)"),
                                                         stringr::str_extract(team_one_penalty_score,
                                                                              "\\(([:digit:]){1}\\)") %>%
                                                           stringr::str_extract("([:digit:]){1}"),
                                                         0), NA),
                  team_two_penalty_score = ifelse(penalty_shot == 1 & half == 5,
                                                  stringr::str_extract(commentary,
                                                                       paste(team_two,
                                                                             " ", team_two_score,
                                                                             "(\\(([:digit:]){1}\\))?(,|\\.)",
                                                                             sep = "")),
                                                  NA),
                  team_two_penalty_score = ifelse(!is.na(team_two_penalty_score),
                                                  ifelse(stringr::str_detect(team_two_penalty_score,
                                                                             "\\(([:digit:]){1}\\)"),
                                                         stringr::str_extract(team_two_penalty_score,
                                                                              "\\(([:digit:]){1}\\)") %>%
                                                           stringr::str_extract("([:digit:]){1}"),
                                                         0), NA),
                  # Make the teams the old names again to match the top:
                  team_one = original_game_teams[1],
                  team_two = original_game_teams[2])
    # Create a new form of match_time that is numeric:
    commentary_df$match_time_numeric <- sapply(c(1:nrow(commentary_df)),
                                              function(x) {
                                                ifelse(stringr::str_detect(commentary_df$match_time[x], "-"), NA,
                                                       stringr::str_extract_all(commentary_df$match_time[x],
                                                                "([:digit:]){1,3}") %>% unlist() %>%
                                                         as.numeric() %>% sum())
                                              })

  # Return the commentary_df
  return(commentary_df)
}

