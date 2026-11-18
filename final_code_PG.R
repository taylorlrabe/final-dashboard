
library(tidyverse)
library(readr)

orcas <- read_csv("data/orcas.csv")

library(tidyverse)
library(stringr)

orcas_clean <- orcas %>%
  mutate(
    duration_seconds = parse_number(duration),
    duration_hours = duration_seconds / 3600,
    vessel = str_trim(vessel)
  ) %>%
  filter(
    !is.na(vessel),
    !is.na(duration_hours),
    duration_hours > 0
  )

orcas_clean <- orcas_clean %>%
  mutate(Mike1 = as.integer(str_detect(vessel,
                                  regex("Mike\\s*1", ignore_case = TRUE))),
    Orcinus = as.integer(str_detect(vessel,
                                    regex("Orcinus", ignore_case = TRUE))),
    Chimo = as.integer(str_detect(vessel,
                                  regex("Chimo", ignore_case = TRUE))),
    Morning_Star = as.integer(str_detect(vessel,
                                         regex("Morning\\s*Star", ignore_case = TRUE))),
    Shachi = as.integer(str_detect(vessel,
                                   regex("Shachi", ignore_case = TRUE))),
    KCB_III = as.integer(str_detect(vessel,
                                    regex("KCB\\s*III", ignore_case = TRUE))),
    Orca = as.integer(str_detect(vessel,
                                 regex("\\bOrca\\b", ignore_case = TRUE))))


vessel_long <- orcas_clean %>%
  pivot_longer(cols = c(Mike1, Orcinus, Chimo, Morning_Star,
             Shachi, KCB_III, Orca),
    names_to = "vessel_name",
    values_to = "present") %>%
  filter(present == 1)

vessel_long %>%
  group_by(vessel) %>%
  summarize(encounters = n(),
    total_hours = sum(duration_hours),
    encounter_rate = encounters / total_hours,
    .groups = "drop")

vessel_rates <- vessel_long %>%
  filter(!is.na(duration_hours),
         duration_hours > 0) %>%
  group_by(vessel_name) %>%
  summarize(
    observations = n(),
    total_hours = sum(duration_hours),
    observation_rate = observations / total_hours,
    .groups = "drop")


# encounter rates by vessel

# figure
fig_1_encounter_rate<-ggplot(vessel_rates,
       aes(x = reorder(vessel_name, observation_rate),
           y = observation_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Orca Encounter Rates by Vessel",
    x = "Research Vessel",
    y = "Encounters per Recorded Hour") +
  theme_minimal()



vessel_year_rates <- orcas_clean %>%
  pivot_longer(
    cols = c(Mike1, Orcinus, Chimo, Morning_Star,
             Shachi, KCB_III, Orca),
    names_to = "vessel_name",
    values_to = "present"
  ) %>%
  filter(present == 1,
         !is.na(duration_hours),
         duration_hours > 0) %>%
  group_by(year, vessel_name) %>%
  summarize(
    observations = n(),
    total_hours = sum(duration_hours),
    observation_rate = observations / total_hours,
    .groups = "drop"
  )


fig_2_yearly_observations <- ggplot(vessel_year_rates,
       aes(x = factor(year),
           y = observation_rate,
           fill = vessel_name)) +
  geom_col(position = "dodge", color = "black") +
  labs(
    title = "Orca Observation Rates by Vessel and Year",
    x = "Year",
    y = "Observations per Recorded Encounter-Hour",
    fill = "Vessel") +
  theme_minimal()




