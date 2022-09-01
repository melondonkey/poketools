

current_meta %>%
  filter(usage >= .004) %>%
  inner_join(current_movesets, by = c('mon_id')) %>%
  left_join(moves, by = 'move_id') %>%
  #View()
  inner_join(movemeta, by = c('pokemon'='name', 'name' = 'move')) %>%
  View()


#missing 2, 14, 26

#zaciancrowned vs zacian
#landorus
#tapu fini