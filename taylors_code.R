library(tidyverse)





orcas <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-10-15/orcas.csv")

# Requirement	What it means
#flexdashboard	An R Markdown file that knits to HTML laid out in boxes (“panes”) instead of a long document
#Text block	A pane explaining the data, its source, what people can explore, and how to navigate. 
#It’s usually a sidebar on the left.
#3+ output panes	At least three figures or tables, not counting the text block
#1+ interactive	At least one pane people can hover over, zoom, click or search, built with an “htmlwidget” package


#clean data for J, K, L, northern, and biggs
# lowercase everything, and drop "killer whale(s)" so the "K" in Killer doesn't count as K pod
orcas_clean <- orcas %>%
  mutate(pods_clean = str_to_lower(pods_or_ecotype) %>%
           str_remove_all("killer whales?"),
         biggs = str_detect(pods_clean, "bigg|transient|t ransient|ca/u|ca's|\\bt\\d"),
         northern = str_detect(pods_clean, "northern"),
         J = !biggs & str_detect(pods_clean, "\\bj"),
         K = !biggs & str_detect(pods_clean, "\\bk"),
         L  = !biggs & str_detect(pods_clean, "\\bl")) %>%
  mutate(`Bigg's (transients)` = biggs,
         `Northern Residents` = northern) %>%
  select(-biggs, -northern) %>%
  pivot_longer(c(J, K, L, `Bigg's (transients)`, `Northern Residents`),
               names_to = "pod", values_to = "present") %>%
  filter(present)

# plot1 with new mutated data

plot1 <- orcas_clean %>% 
  count(year, pod) %>%
  ggplot(aes(x = year, y = n, fill = pod)) +
  geom_col() +
  labs(x = "Year", y = "Number Of Encounters", fill = "Pod", 
       title = "Encounters Per Year Per Pod") + 
  theme_minimal()

plot1

#need to commit + push

library(leaflet)

pod_colors <- colorFactor("Set1", domain = orcas_clean$pod)

orcas_clean %>%
  filter(!is.na(begin_latitude), !is.na(begin_longitude)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~begin_longitude, lat = ~begin_latitude,
                   color = ~pod_colors(pod), radius = 4, stroke = FALSE,
                   fillOpacity = 0.6,
                   popup = ~paste0(pod, " pod<br>", date)) %>%
  addLegend(pal = pod_colors, values = ~pod, title = "Pods")


library(lubridate)

orcas_dates <- orcas %>%
  
  # make sure it's a date  
  mutate(date  = ymd(date),                              
    year  = year(date),
    month = month(date, label = TRUE, abbr = TRUE)) %>%
  
  # drop rows with no date
  filter(!is.na(date))                            

# count encounters for each year and month
orca_counts <- orcas_dates %>%
  count(year, month) %>%
  
  # fill months with no encounters as 0  
  complete(year, month, fill = list(n = 0)) 

# make heatmap
ggplot(orca_counts, aes(x = month, y = factor(year), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3) +
  scale_fill_gradient(low = "purple", high = "orange", name = "Encounters") +
  labs(
    title = "All Orca Encounters by Month",
    subtitle = "Center for Whale Research, 2017–2024",
    x = "Month",
    y = "Year") +
  theme_minimal()

