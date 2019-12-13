library(dplyr)
library(here)
library(purrr)
library(readr)
library(tidyr)
source(here("r/read_rda.r"))

# From MIT Election Data and Science Lab
# https://doi.org/10.7910/DVN/IG0UN2
results_raw <- read_rda(here("data/1976-2018-house.RData"))

# From The Cook Political Report
# https://cookpolitical.com/pvi-map-and-district-list
pvi_raw <- read_csv(here("data/data-5vPn3.csv"))

results <- results_raw %>%
  mutate_at(c("state_po", "party", "candidatevotes"), parse_guess) %>%
  mutate(
    party = if_else(party == "democratic-farmer-labor", "democrat", party)
  ) %>%
  filter(year == 2018, party %in% c("democrat", "republican")) %>%
  select(
    state = state_po, district, candidate,
    party, votes = candidatevotes, totalvotes
  ) %>%
  group_by(state, district) %>%
  mutate(votes = votes / totalvotes * 100) %>%
  group_by(state, district, party) %>%
  summarize(votes = sum(votes)) %>%
  ungroup() %>%
  pivot_wider(names_from = party, values_from = votes) %>%
  mutate_at(c("democrat", "republican"), ~ replace_na(., 0)) %>%
  mutate(
    unopposed = republican == 0 | democrat == 0,
    result    = republican - democrat
  ) %>%
  rename_at(c("democrat", "republican"), ~ paste0(substr(., 1, 3), "_votes"))

winners <- results_raw %>%
  filter(year == 2018) %>%
  select(
    state = state_po, district, candidate, party, votes = candidatevotes
  ) %>%
  mutate_at(c("state", "candidate", "party", "votes"), parse_guess) %>%
  group_by(state, district) %>%
  filter(votes == max(votes)) %>%
  ungroup() %>%
  select(state, district, winner = candidate, winning_party = party)

pvi <- pvi_raw %>%
  separate(Dist, into = c("state", "district")) %>%
  mutate(
    district = as.numeric(district) %>%
      replace_na(0)
  ) %>%
  separate(PVI, into = c("pvi_party", "pvi")) %>%
  mutate(
    pvi = as.numeric(pvi),
    pvi = case_when(
      pvi_party == "R" ~ pvi,
      pvi_party == "D" ~ -pvi,
      pvi_party == "EVEN" ~ 0
    )
  ) %>%
  select(state, district, pvi)

district_swings <- reduce(list(winners, results, pvi), full_join) %>%
  mutate(
    district = case_when(
      district == 0        ~ "AL",
      nchar(district) == 1 ~ paste0(0, district),
      nchar(district) == 2 ~ as.character(district)
    )
  ) %>%
  mutate(swing = result - pvi) %>%
  mutate(adj_swing = swing - median(.$swing)) %>%
  mutate(
    swing_r = round(swing, 0),
    swing_string = paste0(
      case_when(swing_r < 0 ~ "D", swing_r > 0 ~ "R", swing_r == 0 ~ ""),
      if_else(swing_r == 0, "", "+"),
      abs(swing_r)
    ),
    swing_r = NULL,
    a_swing_r = round(adj_swing, 0),
    adj_swing_string = paste0(
      case_when(a_swing_r < 0 ~ "D", a_swing_r > 0 ~ "R", a_swing_r == 0 ~ ""),
      if_else(a_swing_r == 0, "", "+"),
      abs(a_swing_r)
    ),
    a_swing_r = NULL,
  )

write_csv(district_swings, here("district_swings.csv"))
