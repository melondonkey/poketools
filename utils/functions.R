#' Compute a Pokemon's stat
#' 
#' @param base Base stat
#' @param level The Pokemon's level. Default = 50
#' @param IV The number of individual values 0-31. Default = 31
#' @param EV The number of effort values 0-252. Default = 0
#' @param nature The nature modifier (.8, 1, 1.2). Default = 1
stat_actual <- function(base, level=50, IV=31, EV=0, nature=1.0){
  x <- ((( IV + 2* base + (EV/4) ) * level/100) + 5) * nature  
  floor(x)
}

#' Compute actual HP
#' 
#' @base Base HP
#' @level Level.  Default = 50
#' @IV Individual value 0-31. Default = 31
#' @EV Effort value 0-252. Default = 0
hp_actual <- function(base, level=50, IV=31, EV=0){
  
  x <- ((IV + 2*base + (EV/4) ) * level/100 )  + 10 + level 
  floor(x)
}

#' Damage calculation
#' 
#' @att Attacking stat.  Attack or Special Attack
#' @def Defending stat.  Defense or special defense
#' @power Move power. Base power of the move
#' @targets Number of targets
#' @weather Weather modifier
#' @critical Critical modifier 1/1.5

damage <- function(att, def, level=50, power = 100, targets='one', weather=1,
                   critical = 1, return_type = 'expectation', 
                   STAB = 1, type=1, burn = 1, other=1){
  targets <- ifelse(targets=='one', 1, .75)
  
  weather %in% c(.5, 1, 1.5)
  critical %in% c(1, 1.5)
  
  STAB %in% c(1, 1.5)
  type %in% c(0, .25, .5, 1, 1.5, 2, 4)
  burn %in% c(.5, 1)
  
  if(return_type== 'expectation'){
    rand <- (1-.85)/2 + .85
    x <- ((2*level/5 + 2)*power*att/def/50 + 2)*targets*weather*critical*rand*STAB*type*burn*other
    x <- floor(x)
    return(x)
  } else if (return_type == 'range'){
    rand <- c(.85, 1)
    x <- ((2*level/5 + 2)*power*att/def/50 + 2)*targets*weather*critical*rand*STAB*type*burn*other
    x <- floor(x)
    return(x)
  }
  
}


stat_stage <- function(stat, stage){
  stage <- stage + 7
  mods <- c(.25, .285, .33, .4, .5, .66, 1, 1.5, 2, 2.5, 3, 3.5, 4)
  
  floor(stat*mods[stage])
}