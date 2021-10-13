# 13102021
# By Phuong Thao
# data file that was created by the data collecter is a mess
# cannot analysing with that data
# using R to transformating the raw data

library(dplyr)
library(tidyr)
library(readxl)

model_info = read_xlsx("data/tinhbien_data.xlsx", sheet="model_info")
model = read_xlsx("data/model.xlsx", sheet=1)

head(model,5)

x <- model %>% 
  group_by(idMODEL, classification) %>% 
  count() %>% 
  pivot_wider(names_from = classification,
              values_from = n) %>% 
  inner_join(model_info) %>% 
  select(-id) %>% 
  tidyr::replace_na(list(intercr=0)) %>% # nếu k có dòng này thì khi sum intercr và mainpl sẽ cho giá trị NA ở những ô intercr = 0
  mutate(pl_numberT = sum(intercr, mainpl), .after=mainpl)

y <- model %>% 
  tidyr::replace_na(list(yield=0)) %>% 
  group_by(idMODEL, classification) %>% 
  summarise(yield_part = sum(yield)) %>% 
  pivot_wider(names_from = classification,
              values_from = yield_part) %>% 
  rename(intercr_yield = intercr,
         mainpl_yield = mainpl) 
  

# replace NA with 0
df <- left_join(x,y) %>%
  select(!comment) %>% 
  mutate_at(vars(-c(1:8)),~replace(.,is.na(.),0))

# export to .xlsx file
writexl::write_xlsx(df, "data/model_info.xlsx")

