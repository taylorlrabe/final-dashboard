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
