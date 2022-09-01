
## Data extraction script

## Pulls moves, pokedex, learnsets, and meta

library(jsonlite)
library(tidyverse)
library(here)

#Compile functions
sapply(list.files(here::here('utils'), full.names = TRUE), source)

generation <- 8

monyr <- '2022-08'
format_name <- 'gen8vgc2022'
tier <- '1760'

usage_url <- paste0('https://www.smogon.com/stats/', monyr, '/', format_name, '-', tier, '.txt' )
base_url <- 'https://play.pokemonshowdown.com/data/'


## MOVES
table <- fromJSON(paste0(base_url, 'moves.json'), simplifyVector = TRUE)
moves <- enframe(unlist(table))

moves <-
  moves %>%
  mutate(
    move_id = sub("\\..*", "", name),
    field = sub(".+?\\.", "", name),
    field2 = str_replace(field, "\\.", "_")
  ) %>%
  pivot_wider(
    id_cols = move_id, names_from = field2, values_from = value
  )


## POKEMON
table2 <- fromJSON(paste0(base_url, 'pokedex.json'), simplifyVector = TRUE)
pokedex <- enframe(unlist(table2))

pokedex <-
  pokedex %>%
  mutate(
    mon_id = sub("\\..*", "", name),
 #   field = gsub(".*\\.", "" ,name),
    field = sub(".+?\\.", "", name),
 field2 = str_replace(field, "\\.", "_")
  )

pokedex <- 
  pokedex %>%
  pivot_wider(
    id_cols = mon_id, names_from = field2, values_from = value
  ) %>%
  filter(
    num > 0 # remove hackmons
  ) %>%
  select(
    'mon_id', 'num', 'name', 'baseSpecies', 'types', 'types1', 'types2', starts_with('baseStats_'), starts_with('abilities'), 
    'heightm', 'weightkg', starts_with('gender')
  ) %>%
  mutate_at(vars(starts_with('baseStats')), as.numeric)

## Learnsets.  Which Pokemon can learn which moves.

table3 <- fromJSON(paste0(base_url, 'learnsets.json'), simplifyVector = TRUE)
learnsets <- enframe(unlist(table3))


z_extract_element <- function(x, pos = 3){
  x2 = str_split(x, "\\.")
  
  return(x2[[1]][pos])
}

extract_element <- Vectorize(z_extract_element)

# The set of moves that can be learned this generation
current_movesets <- 
  learnsets %>%
  filter(!is.na(value)) %>%
  mutate(
    gen = substr(value, 1, 1)
  ) %>%
  filter(
    gen == generation
  ) %>%
  mutate(
    mon_id = sub("\\..*", "", name),
    move_id = str_replace_all(extract_element(name) , "[:digit:]", ""),
    learn_method = substr(value, 2, nchar(value))
    ) %>%
  select(mon_id, move_id) %>%
  distinct()


# get mons from a format from smogon usage stats


meta_data <- data.table::fread(usage_url)
colnames(meta_data) <- c('nada' ,'rank', 'pokemon', 'usage', 'raw', 'pct', 'real', 'pct2', 'zilch')
meta_data <- meta_data[, -c(1,9) ]
meta_data$usage <- as.numeric(gsub("%", "",meta_data$usage))/100
meta_data$pct   <- as.numeric(gsub("%", "",meta_data$pct))/100 
meta_data$pct2   <- as.numeric(gsub("%", "",meta_data$pct2))/100

meta_data <- meta_data %>%
  select(rank, pokemon, usage, raw)

current_meta <- 
  meta_data %>%
  inner_join(pokedex, by = c('pokemon'='name'))

rm(table, table2, table3, pokedex, learnsets)


# Move meta.  Takes the longest
movemeta <- retrieve_move_usage(monyr = monyr, 
                                format = format_name,
                                tier = tier,
                                pkmnlist = unique(current_meta$pokemon) )
