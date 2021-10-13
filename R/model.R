library(dplyr)
library(tidyr)
library(readxl)
library(DataCombine) # FindReplace

dt = read_xlsx('data/tinhbien_data.xlsx', sheet = "model")

# rearrange data
slice_dt <- function(x) {
  dt %>% slice(seq(from=x, to=x+4*7, by=4))
}

naming <- c('plant','pl_amount','est_year','yield')

listofdt <- list()
for (i in 1:4) {
  x <- stringi::stri_trans_general(unlist(slice_dt(i)), "latin-ascii")
  listofdt <- c(listofdt, data.frame(x))
}

names(listofdt) = naming

# making full data frame
df <- as.data.frame(listofdt) %>% 
  mutate(idMODEL = rep(1:99, each=8), .before=1) %>% 
  mutate(classification= rep(c("mainPl", "mainPl", "interCr", "interCr", 
                               "interCr", "interCr", "interCr", "interCr"),99),
         .before= 'pl_amount') %>% 
  tibble::add_column(comments = NA) %>% 
  drop_na(plant) %>% 
  mutate_all(.funs = tolower)

# removing punctuation from words  
df$plant <-   gsub(",","",as.character(df$plant))

# convert columns with numeric data to numeric
num <- readxl::read_xlsx("data/model.xlsx", sheet = 1)
col_num <- c("idMODEL", "pl_amount", "est_year", "yield")
num[col_num] <- sapply(num[col_num], as.numeric)
sapply(num, class)

# bước cleaning phát hiện có một số cây trùng
# thay "tre" = "tre (mang)", "sau rieng khong hat" = "sau rieng"
replaces <- data.frame(from=c('tre','sau rieng khong hat'),
                       to=c('tre (mang)', 'sau rieng'))

num <- as.data.frame(num) #FindReplace only works with df, not with tibble

num <- FindReplace(data=num, Var = 'plant', replaceData = replaces,
                   from='from', to='to', exact = T)

# export to .xlsx file
writexl::write_xlsx(num, "data/model.xlsx")

