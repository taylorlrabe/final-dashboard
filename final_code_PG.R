
library(tidyverse)
library(readr)

orcas <- read_csv("data/orcas.csv")

#cleaning data duration

orcas_clean <- orcas %>%
  mutate(duration_seconds = parse_number(duration),
    duration_hours = duration_seconds / 3600,
    vessel = str_trim(vessel)) %>%
  filter(!is.na(vessel),
    !is.na(duration_hours),
    duration_hours > 0)

# encounter rates by vessel
 vessel_rates <- orcas_clean %>%
  group_by(vessel) %>%
  summarize(encounters = n(),
    total_hours = sum(duration_hours),
    encounter_rate = encounters / total_hours,
    .groups = "drop") %>%
   filter(encounters >= 10)%>%
  arrange(desc(encounter_rate))

# figure
ggplot(vessel_rates,
       aes(x = reorder(vessel, encounter_rate),
           y = encounter_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Orca Encounter Rates by Vessel",
    x = "Research Vessel",
    y = "Encounters per Recorded Hour") +
  theme_minimal()

