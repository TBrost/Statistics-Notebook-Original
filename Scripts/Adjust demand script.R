#Growth Rate
GrowthR <- 1.2
#Day in Year
CurrentD <- 279

#Create Binary for if item has been out of stock during year
To_Export_SKU_s$OOS_B <- ifelse(To_Export_SKU_s$`Days OOS 2021` >= 1, 1, 0)
#Create Binary for if item has been out of stock since year start.  >= (number of days since year start), 1, 0) MUST match "To_Export_SKU_s$`Days OOS 2021`"
To_Export_SKU_s$OOSYear_B <- ifelse(To_Export_SKU_s$`Days OOS 2021` >= CurrentD , 1, 0)
#Combine
To_Export_SKU_s$ADJ_D <- case_when(To_Export_SKU_s$`Days OOS 2021` >= 1 & To_Export_SKU_s$`OOSYear_B`== 1 ~ 2, 
                                To_Export_SKU_s$`Days OOS 2021` >= 1 & To_Export_SKU_s$`OOSYear_B`< 1 ~ 1, 
                                To_Export_SKU_s$`Days OOS 2021` < 1 ~ 0 )
#Ratio Calculation for adjustment, 1 for items in stock all year, primary ratio for those out of partially out of stock, secondary ratio for those out of stock all year
#Primary Ratio:((To_Export_SKU_s$`Days OOS 2021`/CurrentD ), (To_Export_SKU_s$`Days OOS 2021`/days in current year)
#Secondary Ratio:???   To_Export_SKU_s$`Current Days OOS` | Previous Demand per day/month over days/months this year??




#ADJ_D_days currently is a proportion of what percent of the year an item has been in stock
#I should flip flop 0 and 1 outputs here to match this idea

#if out of stock all year set prop to 1
#if out of stock for part of year set ratio to num days OOS / Total days YTD
#if NOT out of stock this year set to 0
To_Export_SKU_s$ADJ_D_days <- case_when(To_Export_SKU_s$`Days OOS 2021` >= 1 & To_Export_SKU_s$`OOSYear_B`== 1 ~ 1 , 
                                     To_Export_SKU_s$`Days OOS 2021` >= 1 & To_Export_SKU_s$`OOSYear_B`< 1 ~ ((CurrentD -To_Export_SKU_s$`Days OOS 2021`)/CurrentD ), 
                                     To_Export_SKU_s$`Days OOS 2021` < 1 ~ 0)



#ADJ_D_loss currently is an estimate of the demand value * 1 - ADJ_D_days
# IF your read this and are confused thats ok Idk how I got this equation in the first place but its correct. (you tested it dummy, the algebra thing)
# PropOfYearInStock * X = Demand YTD - > Demand YTD/ PropOfYearInStock = X, Prop of year OOS * X = Loss

# if GR is 1.2 then OOS all year items are getting ADJ_D of 120% growth from 2020
# if 1 set to LAST years demand * Growth rate ???????
#if <1 but >0 set to this year demand / 
#if 0 set to 0
#Get Final Lost Demand estimate
To_Export_SKU_s$`ADJ_D_loss` <- case_when(To_Export_SKU_s$ADJ_D_days > 0 & To_Export_SKU_s$ADJ_D_days < 1 ~ (To_Export_SKU_s$`2021` / To_Export_SKU_s$`ADJ_D_days`) * (1 - To_Export_SKU_s$`ADJ_D_days`), 
                                       To_Export_SKU_s$ADJ_D_days == 0 ~ 0, To_Export_SKU_s$ADJ_D_days >= 1 ~ (To_Export_SKU_s$`2020` + (To_Export_SKU_s$`2020` * GrowthR)))
#Get adjusted count from ratio and demand values
To_Export_SKU_s$`Adjusted Total Count` <- (To_Export_SKU_s$`ADJ_D_loss` + To_Export_SKU_s$`2021`)

