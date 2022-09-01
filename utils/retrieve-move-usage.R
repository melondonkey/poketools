#' Construct move usage dataset for a given format and tier
#' 
#' @param monyr YYYY-MM meta snapshot to capture
#' @param format The format slug used by Pokemon Showdown (eg 'gen8vgc2022')
#' @param tier The elo tier (1760)
#' @param pkmnlist Vector of valid Pokemon for the format

retrieve_move_usage <- function(monyr, format = 'gen8vgc2022', tier = '1760', pkmnlist){
  
  #construct the url
  move_meta <- paste0('https://www.smogon.com/stats/', monyr, '/', 'moveset/', format,"-", tier, '.txt')
  
  monnum  = 1
  obj <- list()
  flg_moves = 0
  
  testdata <- read_lines(move_meta)
  for (line in testdata){
    
    
    if(any(str_detect(line, pkmnlist)) & str_detect(line, '%', negate = TRUE)){ # Mon name appears with no % sign
      
      monname <-  gsub("\\|", '' , gsub(' ' ,'', line))  #extract the mon name
      print(monname)
      
      append(obj,  list('name' = monname ) )
      
      monnum = monnum + 1
    }
    
    if(flg_moves == 1){
      if(str_detect(line, '----')){
        flg_moves = 0
      }
      
      if(flg_moves == 1){
        
        #steps to get the move name
        lt2 <- substr(line, 4, nchar(line))
        lt3 <- gsub('%', '', lt2)
        lt4 <- gsub('\\|', '', lt3)
        lt5 <- qdapRegex::rm_number(lt4)
        
        movename <- lt5
        usage <- as.numeric(str_extract(line, "\\d+\\.*\\d*"))/100  #Extract the number
        
        obj <- rlist::list.append(obj, list(name = monname, move = movename, move_usage = usage))
        #     print(paste0(movename, ' - ', usage) )  ##Do the thing
      }
    }
    
    if(str_detect(line, 'Moves')){
      flg_moves = 1
    }
  }
  
  movemeta <- do.call(rbind.data.frame, obj)
  movemeta$format <- format
  movemeta$monyr <- monyr
  movemeta$tier <- tier
  
  return(movemeta)
}
