library(here)
library(stringr)
library(rvest)
library(dplyr)
library(logger)

dir <- here('replays', 'series13')


#' Convenience function for pulling an element from a match
#' @param i The line number to extract from
#' @param j The element to pull from that line (pipe-delimited)
#' @param data html text element from the match
extract_element <- function(i, j, data = game_string){
  str_split(data[[i]], "\\|", simplify = TRUE)[[j]]
}

#Create a subdirectory battles in the project that contains all match data in html files
files <- list.files(path=dir, pattern="*.html", full.names=TRUE, recursive=FALSE)



#convenience/debugging function for viewing a match file 
view_file <- function(file_path){
  game <- rvest::read_html(file_path)
  
  game_data <- game %>%
    html_element('script') %>%
    html_text() 
  
  game_string_view <- str_split(game_data, "\n")
  return(game_string_view)
  
}


# This will fail now if any files have matchups where a player has less than 6 pokemon.
# I show it in logs so you can delete it but will add some checks here later
for (file_path in files){
  
  logger::log_info('starting file {file_path}')
  game <- rvest::read_html(file_path)
  
  g1 <- html_element( game, 'script')
  g2 <- html_text(g1)
  logger::log_info('g2 is {g2}')
  
  game_string <-  str_split(g2, "\n", simplify = TRUE) 
  
  id_loc <- which(str_detect(game_string, "t:"))[1]
  game_id <- extract_element(id_loc, 3, data = game_string)
  log_info('game id is {game_id}')
  
  
  player_loc <-which(str_detect(game_string, "player"))
  p1 <- extract_element(player_loc[1], 4)
  p1_rating_start <- extract_element(player_loc[1], 6)
  p2 <- extract_element(player_loc[2],4)
  p2_rating_start <- extract_element(player_loc[2],6)
  
  log_info('{p1}: {p1_rating_start} vs {p2}:{p2_rating_start}')
  
  
  poke_loc <- which(str_detect(game_string, "\\|poke\\|"))[c(1:12)]
  
  if(sum(is.na(poke_loc)) > 0){
    log_error('team with 5 pokemon.  delete file {file_path}')
  }
  for (line in poke_loc){
    if(line %in% poke_loc[c(1:6)]){
      assign(paste0('p1_mon', match(line, poke_loc) ), 
             str_split(extract_element(line, 4), ",", simplify = TRUE)[[1]]   )
    }
    else if(line %in% poke_loc[c(7:12)]){
      idx <- poke_loc[c(7:12)]
      assign(paste0('p2_mon', match(line, idx) ), str_split(extract_element(line, 4), ",", simplify = TRUE)[[1]]   )
    }
  }
  
  ## Extract leads
  lead_loc <- which(str_detect(game_string, "\\|switch\\|"))[c(1:4)]
  lead_locp1 <- lead_loc[1:2]
  lead_locp2 <- lead_loc[3:4]
  
  for(line in lead_locp1){
    assign(paste0('p1_lead', match(line, lead_locp1)), str_split(extract_element(line, 4), ",", simplify = TRUE)[[1]] )
  }
  for(line in lead_locp2){
    assign(paste0('p2_lead', match(line, lead_locp2)), str_split(extract_element(line, 4), ",", simplify = TRUE)[[1]] )
  } 
  ####
  
  ## Extract results and rating updates
  new_rating_ind <- which(str_detect(game_string, "rating: "))
  p <- 1
  for( line in new_rating_ind){
    
    string <- game_string[line] 
    string_start <- str_locate(game_string[line], "<strong>")[[2]] +1
    new_string <- substr(string, string_start, string_start + 3)
    assign(paste0('p', p, '_newrating'),  gsub("[^0-9]", "", new_string))
    
    #Extract the player name
    string <- str_split(game_string[line], "\\|", simplify= TRUE)[[3]]
    string_end <- str_locate(string, "'s")[1] -1
    assign(paste0('endp', p),  substr(string, 1, string_end) )
    
    p <- p+1
  }
  
  match_start_data <-   
    data.frame(
      match_id = game_id,
      player_id = p1,
      elo_start = p1_rating_start,
      mon1 = p1_mon1,
      mon2 = p1_mon2,
      mon3 = p1_mon3,
      mon4 = p1_mon4,
      mon5 = p1_mon5,
      mon6 = p1_mon6,
      lead1 = p1_lead1,
      lead2 = p1_lead2
    ) %>%
    union_all(
      (
        data.frame(
          match_id = game_id,
          player_id = p2,
          elo_start = p2_rating_start,
          mon1 = p2_mon1,
          mon2 = p2_mon2,
          mon3 = p2_mon3,
          mon4 = p2_mon4,
          mon5 = p2_mon5,
          mon6 = p2_mon6,
          lead1 = p2_lead1,
          lead2 = p2_lead2
        )
      )
    )
  
  result_data <-
    data.frame(
      player_id = endp1,
      elo_end = p1_newrating
    ) %>%
    union_all(
      data.frame(
        player_id = endp2,
        elo_end = p2_newrating
      ) 
    )
  
  final_data <- 
    match_start_data %>%
    inner_join(result_data, by='player_id') %>%
    mutate(
      win_flag = ifelse(elo_end > elo_start, 1, 0)
    )
  
  
  log_info("Writing file")
  #readr::write_csv(final_data,   here('processed', paste0('match-', game_id, '.csv') ) )
  
  assign( paste0('match_', game_id), final_data )
  #
  
}
rm(match_start_data, result_data,  final_data)

replays_raw <- do.call(rbind, mget(ls(pattern = "match_")))


rm(list = ls(pattern = 'match_'))
