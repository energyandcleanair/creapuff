require(tidyverse)
require(rcrea)

project_dir='G:/Shared drives/CREA-HIA/Projects/2026_Indonesia_100GW_solar_HIA'
hia_dir=file.path(project_dir, 'HIA')

hia <- read_csv(file.path(hia_dir, 'annual HIA.csv'))

#avoided impacts = RUPTL 2025 minus 100GW solar, summed over captive and utility plants
#economic costs are in mln USD/year
avoided <- hia %>%
  filter(outcome %in% c('deaths, total', 'economic costs'),
         scenario %in% c('RUPTL 2025', '100GW solar'),
         year>=2026) %>%
  group_by(outcome, scenario, year) %>%
  summarise(across(c(central, low, high), sum), .groups='drop') %>%
  pivot_longer(c(central, low, high), names_to='estimate') %>%
  pivot_wider(names_from=scenario, values_from=value, values_fill=0) %>%
  mutate(avoided=`RUPTL 2025` - `100GW solar`) %>%
  select(-`RUPTL 2025`, -`100GW solar`) %>%
  pivot_wider(names_from=estimate, values_from=avoided)

plot_avoided <- function(df, title, ylab, fill_color, filename) {
  p <- df %>%
    ggplot(aes(year, central)) +
    geom_col(fill=fill_color) +
    scale_y_continuous(expand=expansion(mult=c(0, .05)), labels=scales::comma) +
    labs(title=title,
         subtitle='Annual impacts in RUPTL 2025 minus 100GW solar scenario',
         x='', y=ylab) +
    theme_crea_new()

  quicksave(file.path(hia_dir, paste0(filename, '.png')), plot=p, preview=FALSE)
  write_csv(df, file.path(hia_dir, paste0(filename, '.csv')))
  p
}

avoided %>%
  filter(outcome=='deaths, total') %>%
  select(year, central, low, high) %>%
  plot_avoided('Air pollution-related deaths avoided by 100GW solar', 'deaths/year',
               rcrea::crea_palettes$dramatic['Dark.red'],
               'Avoided deaths by year, 100GW solar vs RUPTL 2025')

avoided %>%
  filter(outcome=='economic costs') %>%
  mutate(across(c(central, low, high), ~.x/1e3)) %>%
  select(year, central, low, high) %>%
  plot_avoided('Air pollution-related health costs avoided by 100GW solar', 'bln USD/year',
               rcrea::crea_palettes$dramatic['Dark.violet'],
               'Avoided health costs by year, 100GW solar vs RUPTL 2025')
