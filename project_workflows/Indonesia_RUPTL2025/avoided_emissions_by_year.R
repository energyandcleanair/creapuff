require(tidyverse)
require(rcrea)

project_dir='G:/Shared drives/CREA-HIA/Projects/2026_Indonesia_100GW_solar_HIA'
emissions_dir=file.path(project_dir, 'emissions')

emis_long <- readRDS(file.path(emissions_dir, 'indonesia_RUPTL_emission_pathways_October_2026.RDS'))

#annual emissions by scenario and pollutant, operating plants only (retirement year included, consistent with cumulative totals)
annual <- emis_long %>%
  filter(COD<=year, year_retire>=year, year>=2026) %>%
  group_by(pollutant, scenario, year) %>%
  summarise(across(emissions_t, sum), .groups='drop')

#check against cumulative totals
annual %>% group_by(pollutant, scenario) %>% summarise(across(emissions_t, sum), .groups='drop') %>%
  left_join(read_csv(file.path(emissions_dir, 'Cumulative emissions by scenario.csv')) %>% rename(cumulative_t=emissions_t),
            by=c('pollutant', 'scenario')) %>%
  mutate(ratio=emissions_t/cumulative_t) %>% print()

avoided <- annual %>%
  filter(scenario %in% c('RUPTL 2025', '100GW solar')) %>%
  pivot_wider(names_from=scenario, values_from=emissions_t, values_fill=0) %>%
  mutate(avoided_t=`RUPTL 2025` - `100GW solar`)

#pick the unit per pollutant that brings the largest value closest to the 1-999 range
units <- tibble(unit=c('Mt', 'kt', 't', 'kg'), divisor=c(1e6, 1e3, 1, 1e-3))

pollutant_units <- avoided %>%
  group_by(pollutant) %>%
  summarise(max_t=max(abs(avoided_t))) %>%
  cross_join(units) %>%
  mutate(max_scaled=max_t/divisor,
         distance=case_when(max_scaled<1 ~ log10(1/max_scaled),
                            max_scaled>=1000 ~ log10(max_scaled/999),
                            T ~ 0)) %>%
  group_by(pollutant) %>%
  slice_min(distance, n=1, with_ties=F) %>%
  select(pollutant, unit, divisor)

plotdata <- avoided %>%
  left_join(pollutant_units, by='pollutant') %>%
  mutate(value=avoided_t/divisor,
         pollutant_label=paste0(pollutant, ', ', unit))

pollutant_colors <- rcrea::crea_palettes$dramatic[c('Dark.red', 'Dark.violet', 'Dark.purple', 'Black', 'Orange')] %>%
  unname() %>% setNames(sort(unique(plotdata$pollutant)))

p <- plotdata %>%
  ggplot(aes(year, value, fill=pollutant)) +
  geom_col() +
  facet_wrap(~pollutant_label, scales='free_y') +
  scale_fill_manual(values=pollutant_colors, guide='none') +
  scale_y_continuous(expand=expansion(mult=c(0, .05)), labels=scales::comma) +
  labs(title='Avoided emissions from 100GW solar',
       subtitle='Annual emissions in RUPTL 2025 minus 100GW solar scenario',
       x='', y='') +
  theme_crea_new()

quicksave(file.path(emissions_dir, 'Avoided emissions by year, 100GW solar vs RUPTL 2025.png'), plot=p, preview=FALSE)

plotdata %>%
  select(pollutant, year, unit, avoided=value, avoided_t, `RUPTL 2025_t`=`RUPTL 2025`, `100GW solar_t`=`100GW solar`) %>%
  arrange(pollutant, year) %>%
  write_csv(file.path(emissions_dir, 'Avoided emissions by year, 100GW solar vs RUPTL 2025.csv'))
