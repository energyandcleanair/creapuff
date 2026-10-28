require(tidyverse)
require(rcrea)

project_dir='G:/Shared drives/CREA-HIA/Projects/2026_Indonesia_100GW_solar_HIA'
emissions_dir=file.path(project_dir, 'emissions')

emis <- read_csv(file.path(emissions_dir, 'Cumulative emissions by scenario.csv'))

#pick the unit per pollutant that brings the largest value closest to the 1-999 range
units <- tibble(unit=c('Mt', 'kt', 't', 'kg'), divisor=c(1e6, 1e3, 1, 1e-3))

pollutant_units <- emis %>%
  group_by(pollutant) %>%
  summarise(max_t=max(emissions_t)) %>%
  cross_join(units) %>%
  mutate(max_scaled=max_t/divisor,
         distance=case_when(max_scaled<1 ~ log10(1/max_scaled),
                            max_scaled>=1000 ~ log10(max_scaled/999),
                            T ~ 0)) %>%
  group_by(pollutant) %>%
  slice_min(distance, n=1, with_ties=F) %>%
  select(pollutant, unit, divisor)

#order scenarios from highest to lowest SOx emissions
scenario_order <- emis %>% filter(pollutant=='SOx') %>% arrange(desc(emissions_t)) %>% pull(scenario)

plotdata <- emis %>%
  left_join(pollutant_units, by='pollutant') %>%
  mutate(value=emissions_t/divisor,
         pollutant_label=paste0(pollutant, ', ', unit),
         scenario=factor(scenario, levels=scenario_order))

scenario_colors <- rcrea::crea_palettes$change[c('Dark.red', 'Orange', 'Blue', 'Dark.blue')] %>%
  unname() %>% setNames(scenario_order)

p <- plotdata %>%
  ggplot(aes(scenario, value, fill=scenario)) +
  geom_col() +
  facet_wrap(~pollutant_label, scales='free_y') +
  scale_fill_manual(values=scenario_colors, guide='none') +
  scale_y_continuous(expand=expansion(mult=c(0, .05)), labels=scales::comma) +
  labs(title='Cumulative emissions by scenario', subtitle='2026 until end-of-life of all plants',
       x='', y='') +
  theme_crea_new() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

quicksave(file.path(emissions_dir, 'Cumulative emissions by scenario, scaled units.png'), plot=p, preview=FALSE)

plotdata %>%
  select(pollutant, scenario, unit, value, emissions_t) %>%
  arrange(pollutant, scenario) %>%
  write_csv(file.path(emissions_dir, 'Cumulative emissions by scenario, scaled units.csv'))
