ntdf <- function(state, geography = "tract", county = NULL, geometry = FALSE, cache_table = TRUE, output = "wide", year = NULL){

	if (!require("pacman")) install.packages("pacman")
	pacman::p_load(tidyverse, tidycensus)

race_vars <- 
	c('totrace' = 'B03002_001',
	  'White' = 'B03002_003',
	  'Black' = 'B03002_004',
	  'Asian' = 'B03002_006',
	  'Latinx' = 'B03002_012')

acs_data <- 
	get_acs(
		geography = geography,
		variables = race_vars,
		state = state,
		county = county,
		geometry = geometry,
		cache_table = cache_table,
		output = output,
		year = year, 
		cb = TRUE
		) %>%
	select(-ends_with("M")) %>% 
	group_by(GEOID) %>% 
	mutate(pWhite = WhiteE/totraceE, 
		   pAsian = AsianE/totraceE, 
		   pBlack = BlackE/totraceE, 
		   pLatinx = LatinxE/totraceE, 
		   pOther = (totraceE - sum(WhiteE, AsianE, BlackE, LatinxE, na.rm = TRUE))/totraceE)

 nt(df = acs_data)

}