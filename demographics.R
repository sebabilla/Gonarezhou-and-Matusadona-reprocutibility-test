#table 2 demographics
GNPdem <- raw %>% select(Park, `J1-Gender`:`J6-Times visited`) %>%
  filter(Park=="G") %>% mutate_all(as.factor) %>% summary() %>% t()
MNPdem <- raw %>% select(Park, `J1-Gender`:`J6-Times visited`) %>%
  filter(Park=="M") %>% mutate_all(as.factor) %>% summary() %>% t()
grid.arrange(tableGrob(GNPdem, cols = NULL),
             tableGrob(MNPdem, cols = NULL), nrow=1, top = "table 2 : GNP and MNP")

options(knitr.kable.NA = '-')
knitr::kable(GNPdem, caption = "ペーパーのTable 2")
knitr::kable(GNPdem)