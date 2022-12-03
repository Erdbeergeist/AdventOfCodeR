# Advent of Code - Day 1

library(tidyverse)

input <- read_csv('1/input.txt', col_names = 'Calories', skip_empty_rows = FALSE)

input %>%  
  mutate(RI = row_number()) %>% 
  left_join(input %>% 
              mutate(RI = row_number()) %>% 
              filter(is.na(Calories)) %>% 
              mutate(t2 = row_number(),
                     n = RI - lag(RI, default = 0) ) %>% 
              select(-Calories),
            by = 'RI') %>% 
  mutate(Elf_number = rep(t2, replace_na(n, 0))) %>% 
  filter(!is.na(Calories)) %>% 
  select(Calories, Elf_number) %>% 
  group_by(Elf_number) %>% 
  summarise(Total = sum(Calories)) %T>%
  {
    slice(., which.max(Total)) %>% 
    print()
  }