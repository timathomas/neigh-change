# Urban Demographics

An R package with various functions to study urban change. So far, this package contains the fillowing: 

1. `nrt`: Neighborhood Racial Typologies

```
#### example
# Baltimore_nt <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
# cal <- ntdf(state = "CA")
# ny <- ntdf(state = "NY")
# glimpse(Baltimore_nt)
# 
# ps_nt <- ntdf(state = "WA", county = c("Snohomish", "King", "Pierce"), geometry = TRUE)
####


### 
# Check to see if there are duplicate tract assumptions. 
# Baltimore_nt %>% 
# st_set_geometry(NULL) %>% 
# mutate(val = 1) %>%
# spread(NeighType, val, fill = 0) %>% 
# mutate_at(vars(`All Black`:`White-Shared`), list(as.numeric)) %>% 
# select(13:ncol(.)) %>% 
# mutate(rowsum = rowSums(.)) %>% 
# filter(rowsum > 1) %>% 
# glimpse()

# ==========================================================================
# Note: 
# After running the above code, look at the counts and consider concatenating 
# of reducing the outlying neighborhood types into something more manageable. 
# ==========================================================================

# ntcheck(Baltimore_nt)
# ntcheck(cal)
# ntcheck(ny)
# ntcheck(ps_nt)
```
