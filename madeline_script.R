library(readr)
library(tidyverse)
library(lubridate)

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

#figuring out the duration of the encounter through assigning AM or PMs
orca_tidy <- orca_tidy |> 
  mutate(am_pm = case_when())
