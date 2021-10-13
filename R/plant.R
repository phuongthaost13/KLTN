library(dplyr)
library(readxl)

model = read_xlsx("data/model.xlsx", sheet=1)


plant <- model %>% 
  group_by(classification) %>% 
  distinct(plant) %>% 
  arrange(desc(classification))

write.csv(plant, "data/plant.csv", row.names = F)
