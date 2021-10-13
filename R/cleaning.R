# 13102021
# By Phuong Thao
# check if there are any errors in tinhbien data
# 13102021
# By Phuong Thao
# check if there are any errors in the dataset

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

model = read_xlsx("data/model.xlsx", sheet=1)
model_info = read_xlsx("data/model_info.xlsx", sheet=1)

# est_year
false.year = numeric()
summary(model$est_year)
false.year = c(which(model$est_year < 1900), false.year)


# profit
x <- plant_count %>% 
  left_join(model, by = c('plant', 'classification'))

# tiêu chí lọc: các model có lợi nhuận/ha lớn hơn 50 triệu
# các model không có giá trị năng suất nhưng lại có lợi nhuận
false.profit = unique(c(which(model_info$mainpl_profit_ha>50), 
                        which(model_info$intercr_profit_ha>50),
                        which(model_info$intercr_yield==0&model_info$intercr_inc>0),
                        which(model_info$mainpl_yield==0&model_info$mainpl_inc>0)))

false.profit = arrange(desc(data.frame(false.profit)))

#which(model_info$intercr_yield>0&model_info$intercr_inc==0) #ko cần thiết
#which(model_info$mainpl_yield>0&model_info$mainpl_inc==0) #ko cần thiết


# pl_amount
# lấy phần lợi nhuận chia cho tổng số cây để xem có gì bất thường k
plant_count <- model %>% 
  group_by(plant,classification) %>% 
  count() %>% 
  arrange(desc(classification)) #%>% 
  #tibble::rowid_to_column("ID")

ggplot(data=filter(plant_count, classification == "mainpl"))+
  geom_bar(mapping=aes(x=plant, y=n), stat = 'identity')

ggplot(data=filter(plant_count, classification == "intercr"))+
  geom_bar(mapping=aes(x=plant, y=n), stat = 'identity')

# từ biểu đồ, thấy rằng, cần thay "tre" = "tre (mang)"
# thay "sau rieng khong hat" = "sau rieng"
# quay lại model.R để thay ở file data gốc



