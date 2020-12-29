h %>% 
  group_by(game, hand, color) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(game, hand) %>% 
  mutate(percent = (n / sum(n))) %>%
  filter(percent == max(percent) & sum(n) > 7) %>%
  ungroup() %>%
  ggplot(aes(percent)) +
  geom_histogram()
