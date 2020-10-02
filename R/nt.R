# ==========================================================================
# Neighborhood Typology
# By: Tim Thomas - timothyathomas@gmail.com
# Measures drawn from:
# 	Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood
# 		Foreclosures, Racial/Ethnic Transitions, and Residential
# 		Segregation.” American Sociological Review 80:526–549.
#
# Process:
# 	1. Create a dataframe with the proportions for White, Black, Asian,
# 		Latinx, and Other.
# 	2. Run the `nt` function on that dataset to create a `NeighType` field
# 	3. Look at your neighborhood typology frequency and consider merging
# 		similar, or low frequency, neighborhoods into one or more common
# 		categories (e.g., some of the four category cases could be considered
# 		white-mixed--see the article to see how the authors consolidated
# 		categories).
# ==========================================================================

nt <- function(df, GEOID = "GEOID", totraceE = "toteraceE", pWhite = "pWhite", pBlack = "pBlack", pAsian = "pAsian", pLatinx = "pLatinx", pOther = "pOther"){

	df %>%
	dplyr::group_by(GEOID) %>%
			# create neighborhood typology
	dplyr::mutate(NeighType =
		dplyr::case_when(
			pWhite >=.9 ~ "All White",
			pBlack >=.9 ~ "All Black",
			pAsian >=.9 ~ "All Asian",
			pLatinx >=.9 ~ "All Latinx",
			pOther >=.9 ~ "All Other",

			# pBlack >=.7 ~ "Mostly Black",
			# pAsian >=.7 ~ "Mostly Asian",
			# pLatinx >=.7 ~ "Mostly Latinx",
			# pOther >=.7 ~ "Mostly Other",

			pWhite < .9 & pBlack < .1 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "White-Shared",
			pBlack < .9 & pWhite < .1 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "Black-Shared",
			pAsian < .9 & pBlack < .1 & pWhite < .1 & pLatinx < .1 & pOther < .1 ~ "Asian-Shared",
			pLatinx < .9 & pBlack < .1 & pAsian < .1 & pWhite < .1 & pOther < .1 ~ "Latinx-Shared",
			pOther < .9 & pBlack < .1 & pAsian < .1 & pLatinx < .1 & pWhite < .1 ~ "Other-Shared",

			pWhite >=.1 & pWhite < .9 & pBlack >=.1 & pBlack < .9 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "White-Black",
			pWhite >=.1 & pWhite < .9 & pLatinx >=.1 & pLatinx < .9 & pAsian < .1 & pBlack < .1 & pOther < .1 ~ "White-Latinx",
			pWhite >=.1 & pWhite < .9 & pAsian >=.1 & pAsian < .9 & pBlack < .1 & pLatinx < .1 & pOther < .1 ~ "White-Asian",
			pWhite >= .1 & pWhite < .9 & pOther >=.1 & pOther < .9 & pBlack < .1 & pLatinx < .1 & pAsian < .1 ~ "White-Other",

			pBlack >=.1 & pBlack < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatinx < .1 & pAsian < .1 ~ "Black-Other",
			pBlack >=.1 & pBlack < .9 & pLatinx >=.1 & pLatinx < .9 & pWhite < .1 & pAsian < .1 & pOther < .1 ~ "Black-Latinx",

			pAsian >=.1 & pAsian < .9 & pBlack >=.1 & pBlack < .9 & pWhite < .1 & pLatinx < .1 & pOther < .1 ~ "Asian-Black",
			pAsian >=.1 & pAsian < .9 & pLatinx >=.1 & pLatinx < .9 & pWhite < .1 & pBlack < .1 & pOther < .1 ~ "Asian-Latinx",
			pAsian >= .1 & pAsian < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatinx < .1 & pBlack < .1 ~ "Asian-Other",

			pLatinx >= .1 & pLatinx < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pAsian < .1 & pBlack < .1 ~ "Latinx-Other",

			pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pWhite < .1 & pOther < .1 ~ "Black-Asian-Latinx",
			pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pWhite < .1 & pLatinx < .1 ~ "Black-Asian-Other",
			pBlack >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 & pAsian < .1 ~ "Black-Latinx-Other",

			pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 & pBlack < .1 ~ "Asian-Latinx-Other",

			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatinx < .1 & pOther < .1 ~ "White-Black-Asian",
			pWhite >=.1 & pBlack >=.1 & pLatinx >=.1 & pAsian < .1 & pOther < .1 ~ "White-Black-Latinx",
			pWhite >=.1 & pBlack >=.1 & pOther >=.1 & pLatinx < .1 & pAsian < .1 ~ "White-Black-Other",
			pWhite >=.1 & pAsian >=.1 & pLatinx >=.1 & pBlack < .1 & pOther < .1 ~ "White-Asian-Latinx",
			pWhite >=.1 & pAsian >=.1 & pOther >=.1 & pLatinx < .1 & pBlack < .1 ~ "White-Asian-Other",
			pWhite >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "White-Latinx-Other",
			pWhite >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "White-Latinx-Other",

			pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 ~ "Black-Asian-Latinx-Other",
			pWhite >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pBlack < .1 ~ "White-Asian-Latinx-Other",
			pWhite >=.1 & pBlack >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 ~ "White-Black-Latinx-Other",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pLatinx < .1 ~ "White-Black-Asian-Other",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther < .1 ~ "White-Black-Asian-Latinx",

			pWhite >.1 & pWhite <=.7 & pBlack >.1 & pAsian >.1 & pLatinx >.1 & pOther >.1 ~ "Diverse",
			totraceE == 0 ~ "unpopulated tract")) %>%
		dplyr::ungroup()
	}
