#INV_long$HCF <- as.numeric(as.character(INV_long$Year))
is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

library(dplyr)
library(stringr)
To_Export_SKU_s <- To_Export_SKU_s %>% 
  mutate_if(is_all_numeric,as.numeric)
       
To_Export_SKU_s$ProductSKU <- str_sub(To_Export_SKU_s$SKU, -3.-1)

#df1$Round_off <- round(df1$Score ,digit=2)
To_Export_SKU_s$`ADJ_D_days` <- round(To_Export_SKU_s$ADJ_D_days,digit=3)
To_Export_SKU_s$`ADJ_D_loss` <- round(To_Export_SKU_s$ADJ_D_loss,digit=3)
To_Export_SKU_s$`Adjusted Total Count` <- round(To_Export_SKU_s$`Adjusted Total Count`,digit=3)



#date <- as.Date(date, format = "%m/%d/%y")
To_Export_SKU_s$OOS...22 <- as.character(To_Export_SKU_s$OOS...22)
To_Export_SKU_s$OOS...24 <- as.character(To_Export_SKU_s$OOS...24)
To_Export_SKU_s$OOS...26 <- as.character(To_Export_SKU_s$OOS...26)
To_Export_SKU_s$OOS...28 <- as.character(To_Export_SKU_s$OOS...28)
To_Export_SKU_s$`IN STOCK...23` <- as.character(To_Export_SKU_s$`IN STOCK...23`)
To_Export_SKU_s$`IN STOCK...25` <- as.character(To_Export_SKU_s$`IN STOCK...25`)
To_Export_SKU_s$`IN STOCK...27` <- as.character(To_Export_SKU_s$`IN STOCK...27`)
To_Export_SKU_s$`IN STOCK...29` <- as.character(To_Export_SKU_s$`IN STOCK...29`)
To_Export_SKU_s$`First Date Audit` <- as.character(To_Export_SKU_s$`First Date Audit`)
To_Export_SKU_s$`Date Inbound` <- as.character(To_Export_SKU_s$`Date Inbound`)



To_Export_SKU_s$OOS...22 <- as.Date(To_Export_SKU_s$OOS...22, format = "%m/%d/%y")
To_Export_SKU_s$OOS...24 <- as.Date(To_Export_SKU_s$OOS...24, format = "%m/%d/%y")
To_Export_SKU_s$OOS...26 <- as.Date(To_Export_SKU_s$OOS...26, format = "%m/%d/%y")
To_Export_SKU_s$OOS...28 <- as.Date(To_Export_SKU_s$OOS...28, format = "%m/%d/%y")
To_Export_SKU_s$`IN STOCK...23` <- as.Date(To_Export_SKU_s$`IN STOCK...23`, format = "%m/%d/%y")
To_Export_SKU_s$`IN STOCK...25` <- as.Date(To_Export_SKU_s$`IN STOCK...25`, format = "%m/%d/%y")
To_Export_SKU_s$`IN STOCK...27` <- as.Date(To_Export_SKU_s$`IN STOCK...27`, format = "%m/%d/%y")
To_Export_SKU_s$`IN STOCK...29` <- as.Date(To_Export_SKU_s$`IN STOCK...29`, format = "%m/%d/%y")
To_Export_SKU_s$`First Date Audit` <- as.Date(To_Export_SKU_s$`First Date Audit`, format = "%m/%d/%y")
To_Export_SKU_s$`Date Inbound` <- as.Date(To_Export_SKU_s$`Date Inbound`, format = "%m/%d/%y")
