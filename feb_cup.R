library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(gt)

pick_data <- read_excel("feb_cup_data.xlsx", sheet = "picks - Picks")
player_data <- read_excel("feb_cup_data.xlsx", sheet = "players - Players")
player_list <- read_excel("feb_cup_data.xlsx", sheet = "player_list - Player List")

pick_stack <- pick_data |> 
  pivot_longer(everything(), names_to = NULL, values_to = 'Pick')
player_stack <- player_data |> 
  pivot_longer(everything(), names_to = NULL, values_to = 'Player')
player_pick <- bind_cols(player_stack, pick_stack)
pp_count <- player_pick |> count(Player, Pick)

player_list_stack <- player_list |> 
  pivot_longer(cols = !Role, names_to = 'Team', values_to = 'Player')
data <- right_join(player_list_stack, pp_count)

data_sorted <- data %>% arrange(Role, Team, desc(n))

data_combined <- data_sorted %>% 
  mutate(n = as.character(n)) %>% 
  mutate(pick_n = str_c(Pick, ', ', n)) %>% 
  group_by(Player) %>% 
  summarise(all_pick_n = str_flatten(pick_n, '\n'))

df_picks <- data_combined %>% 
  left_join(player_list_stack) %>% 
  arrange(Role, Team) %>% 
  select(Role, Team, all_pick_n) %>% 
  pivot_wider(names_from = Team, values_from = all_pick_n) %>% 
  rename_with(.fn = ~ paste0(., ' Pick', recycle0 = TRUE),
              .cols = !Role)

df <- player_list %>% 
  full_join(df_picks) %>% 
  select(sort(colnames(.))) %>% 
  relocate(Role)

data_table <- df %>% 
  gt() %>% 
  cols_width(
    ends_with('Pick') ~ px(120),
    everything() ~ px(120)
  ) %>% 
  tab_header('February Cup NA, Players\' Pokemon Preferences') %>% 
  opt_stylize()

gtsave(data_table, 'February Cup.html')

data_table
