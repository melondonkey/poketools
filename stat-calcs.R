
current_meta2 <- 
  current_meta %>%
 # rowwise() %>%
  mutate(
    lv50_max_hp = ifelse(pokemon=='Shedinja', 1, hp_actual(base = baseStats_hp, IV = 31, EV = 252) ),
    lv50_max_atk = stat_actual(baseStats_atk, EV = 252, nature = 1.2),
    lv50_max_def = stat_actual(baseStats_def, EV = 252, nature = 1.2)
  )