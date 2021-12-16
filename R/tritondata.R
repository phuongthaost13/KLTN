library(dplyr)
#mydata=seq(1,1000)
#mydata=as.data.frame(matrix(mydata,nrow = 100,ncol = 10,byrow=T))

#x = matrix(t(as.matrix(mydata)), nrow = 1)

dt = readxl::read_xlsx('data/tinhbien_data.xlsx', sheet="plant_wide_table")
df <- as_tibble(t(matrix(t(as.matrix(dt)), nrow=1))) %>% 
  mutate_all(.funs=tolower) %>% 
  rename(plant = V1) %>% 
  mutate(idMODEL = rep(1:99, each=8), .before = 1) %>% 
  mutate(classification = rep(c('mainPl', "mainPl",
                                rep('interCr',6)),
                              99)) %>% 
  tidyr::drop_na() %>% 
  

len = rep(1:99,each=8)
df=rbind(n,len)

writexl::write_xlsx(as.data.frame(df), "plant_wide_table.xlsx")

