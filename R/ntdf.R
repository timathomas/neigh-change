ntdf <- function(
	state, 
	geography = "tract", 
	county = NULL, 
	geometry = FALSE, 
	cache_table = TRUE, 
	output = "wide", 
	year = 2018, 
	GEOID = "GEOID",
	WhiteE = "WhiteE",
	AsianE = "AsianE",
	BlackE = "BlackE",
	LatinxE = "LatinxE",
	totraceE = "totraceE"
){

race_vars <-
	c('totrace' = 'B03002_001',
	  'White' = 'B03002_003',
	  'Black' = 'B03002_004',
	  'Asian' = 'B03002_006',
	  'Latinx' = 'B03002_012')

acs_data <-
	tidycensus::get_acs(
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
	dplyr::select(-dplyr::ends_with("M")) %>%
	dplyr::group_by(GEOID) %>%
	dplyr::mutate(pWhite = WhiteE/totraceE,
		   pAsian = AsianE/totraceE,
		   pBlack = BlackE/totraceE,
		   pLatinx = LatinxE/totraceE,
		   pOther = (totraceE - sum(WhiteE, AsianE, BlackE, LatinxE, na.rm = TRUE))/totraceE)

 nt(df = acs_data)

}
