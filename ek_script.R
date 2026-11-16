library(tidyverse)
orcas <- read_csv("https://raw.githubusercontent.com/jadeynryan/orcas/refs/heads/master/data-raw/cwr_tidy.csv")

orcas |> 
  filter(duration == "-420s (~-7 minutes)") |> 
  pull(encounter_summary)
