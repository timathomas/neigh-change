ntcheck <- function(df, NeighType = "NeighType", n = "n")
	df %>% dplyr::group_by(NeighType) %>% dplyr::count() %>% dplyr::arrange(dplyr::desc(n)) %>% data.frame()
