library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(flexdashboard)

orca_link <- paste0("https://raw.githubusercontent.com/jadeynryan/orcas/refs/heads/master/data-raw/cwr_tidy.csv")
orca_tidy <- read.csv(orca_link)

#removing the encounter that have NA for the end time or the beginning time
orca_tidy <- orca_tidy |>
  filter(!is.na(end_time)) |> 
  filter(!is.na(begin_time)) |> 
  filter(!is.na(date)) |> 
  mutate(end_date_time = paste(date, end_time)) |> 
  mutate(begin_date_time = paste(date, begin_time)) |> 
  mutate(end_date_time = ymd_hm(end_date_time)) |> 
  mutate(begin_date_time = ymd_hm(begin_date_time))

#map?
orca_sightings <- orca_tidy |> 
  select(date, pods_or_ecotype, location, begin_longitude, begin_latitude,
         end_longitude, end_latitude, encounter_summary) |> 
  filter(!is.na(begin_longitude)) |>
  filter(!is.na(begin_latitude))

leaflet(data = orca_sightings) |> 
  addTiles() |> 
  addCircleMarkers(~ begin_longitude, ~ begin_latitude,
                   popup = ~ paste("Date:", date, 
                                   "<br>",
                                   "Pod:", pods_or_ecotype, 
                                   "<br>",
                                   "Encounter Summary:", encounter_summary), 
                   color = ~ pods_or_ecotype,
                   clusterOptions = markerClusterOptions())
         