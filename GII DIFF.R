git init

# 加载包
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(here)
library(readxl)
library(tidyr)
#clean_name
install.packages("janitor")
library(janitor)

# 读取数据
gii <- read_excel(here("GII.xlsx")) 

# 查看数据结构
head(gii)
str(gii)
datatypelist<- gii %>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to = "All_variables", 
               values_to = "Variable_class")

# 选择需要的数据列
gii_clean <- gii %>%
  clean_names() %>%
  select(country_iso_code,country,year,value) %>%
  #将year列拆分为两列
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  # 生成新的一列计算两年间的差异
  mutate(gii_diff = value_2019 - value_2010)

# 分开两年的数据——因为分析2010和2019之间的差异，所以分开更好吗？
gii_2010 <- gii_clean %>% filter(year ==2010)
gii_2019 <- gii_clean %>% filter(year ==2019)
# QUESTION!!!!生成两个表格好，还是生成两列好？？？

# 读取世界地理空间数据-GeoJSON格式
world_geojson <- st_read(here("World_Countries_(Generalized)_9029012925078512962.geojson"))

# 查看数据结构
head(world_geojson)
str(world_geojson)

# 为了合并两个数据集，给gii生成countrycode
install.packages("countrycode")
library(countrycode)

# 将ISO 3位代码转换为ISO 2位代码
gii_clean <- gii_clean %>%
  mutate(ISO2 = countrycode(country_iso_code, "iso3c","iso2c"))

# merge gii_clean & world_geojson
merged_data <-world_geojson %>%
  left_join(gii_clean, by = c("ISO" = "ISO2"))
head(merged_data)

# draw the diff_map
ggplot(merged_data) +
  geom_sf(aes(fill = gii_diff)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  theme_minimal() +
  labs(title = "Difference in Gender Inequality Index (2010-2019)",
       fill = "GII Difference")
#QUESTION!!!——What about NA?
#应该按周把github仓库做一个份文件夹管理

# 内容推送到GitHub里
system("git init") # 初始化git仓库
system("git add .") # 添加所以文件至暂存区
system("git commit -m 'Add gii diff project'") # 提交更改
system("git status")
system("git push origin master")
system("git log")

