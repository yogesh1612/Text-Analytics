library(tidyr)

tidyr::who

who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)

head(who1)

who1 %>% count(key)


