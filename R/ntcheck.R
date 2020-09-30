ntcheck <- function(df) 
	df %>% group_by(NeighType) %>% count() %>% arrange(desc(n)) %>% data.frame()