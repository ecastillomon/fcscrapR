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
  if (length(incorrect_team_i) >= 1) {
    game_teams[incorrect_team_i] <- commentary_game_teams[incorrect_team_i]
  }
  
  # Both would require more sophisticated matching techniques
  
  # Next add columns for these two teams and then go through a series of conditional
  # parsing of the actual commentary to extract useful features for analysis:
  commentary_df <- commentary_df %>%
    dplyr::mutate(team_one = game_teams[1],
                  team_two = game_teams[2],
                  # Create a comment ID column and which will allow the data to be
                  # reordered and then manipulated more to populate the half and
                  # score variables:
                  comment_id = seq(from = n(), to = 1, by = -1))
  

  # Return the commentary_df
  return(commentary_df)
}

