library("writexl")
write_xlsx(HCFINVMasterCopy,"/Users/frozendessertsupplies//HCFINVEDIT.xlsx")
write_xlsx(HCFINVGrouped,"/Users/frozendessertsupplies//HCFINVGroupedEDIT.xlsx")
write_xlsx(HCFINV,"/Users/frozendessertsupplies//HCFINVGroupedEDIT.xlsx")

library(googlesheets4)
HCFINVDemands <- gs4_create("HCFINVDemands", sheets = list(HCFINVGroupedSum = HCFINVGroupedSum, HCFINVSWCups = HCFINVSWCups, HCFINVDWCups = HCFINVDWCups, HCFINVRPCups = HCFINVRPCups, HCFINVLids = HCFINVLids, HCFINVSleeves = HCFINVSleeves))

#df %>%
#group_by(rowname) %>%
#summarize(col1=sum(col1), col2= paste(unique(col2),collapse=","), col3=paste(col2,collapse=","))

#col1=sum(col1),
#col2= paste(unique(col2),collapse=","),\


#All Columns
library(dplyr)
To_Export_SKU_s_Group <- To_Export_SKU_s %>%
  group_by(ProductSKU) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `July Demand`=sum(`July Demand`), `RFW Inv`=sum(`RFW Inv`), col7= paste(unique(`For Rent`),collapse=","), col8=sum(`2016`), col9=sum(`2017`), col10=sum(`2018`), col11=sum(`2019`), col12=sum(`2020`), col13=sum(`2021`), col14= paste(unique(Notes),collapse=","),
            
            col15= paste(unique(Playground),collapse=","), col16=sum(`On Shipment`), col17=sum(`On Order`), col18=sum(`Month's Inv`), col19=sum(MOQ), col20= paste(unique(`Current Days OOS`),collapse=","), col21= paste(unique(`OOS...22`),collapse=","),
            col22= paste(unique(`IN STOCK...23`),collapse=","), col23= paste(unique(`OOS...24`),collapse=","), col24= paste(unique(`IN STOCK...25`),collapse=","), col25= paste(unique(`OOS...26`),collapse=","), col26= paste(unique(`IN STOCK...27`),collapse=","),
            
            col27= paste(unique(`OOS...28`),collapse=","), col28= paste(unique(`IN STOCK...29`),collapse=","), col29= paste(unique(`First Date Audit`),collapse=","), col30= sum(`Days OOS 2021`),col31= paste(unique(`Adjusted Demand/Day 2021`),collapse=","), 
            col32= paste(unique(`44440`),collapse=","), col33= paste(unique(`Date Inbound`),collapse=","), col34= sum(`Qty Inbound`), col35= sum(`Forecast End Inv`), col36= sum(`H`), col37= paste(unique(`TI (Cases Per Layer)`),collapse=","), col38= paste(unique(`HI (Layers Per Pallet)`),collapse=","), 
            
            col39= paste(unique(`# Per Pallet`),collapse=","), col40= paste(unique(`High Level Category`),collapse=","), col41= paste(unique(`Detailed Category`),collapse=","), col42= paste(unique(`Color`),collapse=","), col43= paste(unique(`Size`),collapse=","),
            col44= paste(unique(`...45`),collapse=","), col45= sum(`CUFT.`), col14= sum(`Total CUFT.`), col46= paste(unique(`Supplier SKU`),collapse=","),  col47= sum(`Avg. Demand/Day in Stock`), col48= sum(`Invoice Cost`), col49= paste(unique(`OOS_B`),collapse=","),
            
            col50= paste(unique(`OOSYear_B`),collapse=","), `ADJ_D`= sum(`ADJ_D`), `ADJ_D_days`= mean(`ADJ_D_days`), `ADJ_D_loss`= sum(`ADJ_D_loss`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","), `Adjusted Total Count`= sum(`Adjusted Total Count`))

library(stringr)

HCFINVMerge <- rbind(HCFINVMasterCopy, To_Export_SKU_s)

#Main Columns
HCFINVGroupedSum<- To_Export_SKU_s%>%
  #filter(HCFINVGrouped, )
  group_by(ProductSKU) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),`Category`= paste(unique(`High Level Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= sum(`ADJ_D_days`), `Estimated Loss YTD`= sum(`ADJ_D_loss`),`Potential Total Demand`= sum(`Adjusted Total Count`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))

#Single Wall
#add size num
#HCFINVSWCups$SizeNum <- str_sub(HCFINVSWCups$Size, 1,2)
HCFINVSWCups<- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Detailed Category`, "Single Wall"), ]
  HCFINVSWCups$SizeNum <- str_sub(HCFINVSWCups$Size, 1,2) 
  #group by size
  HCFINVSWCups<- HCFINVSWCups %>%
    group_by(SizeNum) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
           `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`Proportion Days IS/YTD`), `Estimated Loss YTD`= sum(`Estimated Loss YTD`),`Potential Total Demand`= sum(`Potential Total Demand`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))

#Double Wall
#add size num, group by size
HCFINVDWCups<- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Detailed Category`, "Double Wall"), ]
  HCFINVDWCups$SizeNum <- str_sub(HCFINVDWCups$Size, 1,2) 
  
  
HCFINVDWCups <- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Detailed Category`, "Double Wall"), ]
  HCFINVDWCups$SizeNum <- str_sub(HCFINVDWCups$Size, 1,2) 
  HCFINVDWCups<- HCFINVDWCups %>%
    group_by(SizeNum) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
           `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`Proportion Days IS/YTD`), `Estimated Loss YTD`= sum(`Estimated Loss YTD`), `Potential Total Demand`= sum(`Potential Total Demand`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))

#Ripple
#add size num, group by size
HCFINVRPCups<- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Detailed Category`, "Ripple"), ]
  HCFINVRPCups$SizeNum <- str_sub(HCFINVRPCups$Size, 1,2)
  HCFINVRPCups<- HCFINVRPCups %>%
  group_by(SizeNum) %>%
    summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
              `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
              `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`Proportion Days IS/YTD`), `Estimated Loss YTD`= sum(`Estimated Loss YTD`), `Potential Total Demand`= sum(`Potential Total Demand`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))
  
#Sleeves
#add size num, group by Color
HCFINVSleeves<- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Detailed Category`, "Sleeves"), ]
HCFINVSleeves<- HCFINVSleeves %>%
  group_by(Color) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`Proportion Days IS/YTD`), `Estimated Loss YTD`= sum(`Estimated Loss YTD`), `Potential Total Demand`= sum(`Potential Total Demand`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))

  
#Lids
#add size num, group by Type
HCFINVLids<- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Category`, "Lids"), ]
HCFINVLids$`Detailed Category` <- case_when(HCFINVLids$`Detailed Category` == "Flip" ~ "Flip", HCFINVLids$`Detailed Category` == "Flat" ~ "Flat", HCFINVLids$`Detailed Category` == "Lid" ~ "FoamAroma", HCFINVLids$`Detailed Category` == "Solid" ~ "Flat", HCFINVLids$`Detailed Category` == "Dome" ~ "Dome", HCFINVLids$`Detailed Category` == "Sip" ~ "Sip")
HCFINVLids<- HCFINVLids %>%
  group_by(`Detailed Category`) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`Proportion Days IS/YTD`), `Estimated Loss YTD`= sum(`Estimated Loss YTD`), `Potential Total Demand`= sum(`Potential Total Demand`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))
#Seperate Clear lids
To_Export_lids<- To_Export_SKU_s[str_detect(To_Export_SKU_s$`High Level Category`, "Lids"), ]
To_Export_lids$`High Level Category` <- case_when(To_Export_lids$`High Level Category` == "Lids" ~ "Clear Lids")
To_ExportINVLids<- To_Export_lids %>%
  group_by(`Detailed Category`) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`ADJ_D_days`), `Estimated Loss YTD`= sum(`ADJ_D_loss`), `Potential Total Demand`= sum(`Adjusted Total Count`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))




#Spoons
#add size num, group by Type
HCFINVSpoons<- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Category`, "Spoons"), ]
HCFINVSpoons$`Detailed Category` <- case_when(HCFINVSpoons$`Detailed Category` == "Wood" ~ "Wood", HCFINVSpoons$`Detailed Category` == "Gelato" ~ "Gelato", HCFINVSpoons$`Detailed Category` == "Tasting" ~ "Tasting",HCFINVSpoons$`Detailed Category` == "Curve" ~ "Curve")
HCFINVSpoons<- HCFINVSpoons %>%
  group_by(`Detailed Category`) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`Proportion Days IS/YTD`), `Estimated Loss YTD`= sum(`Estimated Loss YTD`), `Potential Total Demand`= sum(`Potential Total Demand`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))

#FDS Cups
HCFINVFDSCups<- HCFINVGroupedSum[str_detect(HCFINVGroupedSum$`Category`, "Cups"), ]
HCFINVFDSCups$`Detailed Category` <- case_when(HCFINVFDSCups$`Category` == "Gelato Cups" ~ "Gelato Cups", HCFINVFDSCups$`Category` == "Drink Cups" ~ "Clear Cups")
HCFINVFDSCups<- HCFINVFDSCups %>%
  group_by(`Category`) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days IS/YTD`= mean(`Proportion Days IS/YTD`), `Estimated Loss YTD`= sum(`Estimated Loss YTD`), `Potential Total Demand`= sum(`Potential Total Demand`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))


To_Export_SKU_s$OOS...22 <- rbind(44197,
                     44238,
                     "",
                     44197,
                     44281,
                     44399,
                      "",                     
                     44042,
                     44349,
                     44259,
                     44259,
                     44259,
                     44433,
                     "",
                     "",
                     "",
                     44393,
                     44393,
                     44461,
                     44461,
                     44378,
                     "",
                     "",
                     "",
                     44305,
                     44340,
                     44383,
                     44326,
                     44421,
                     "",
                     44197,
                     44273,
                     44197)


To_Export_SKU_s[26, 13] = 13
To_Export_SKU_s[25, 13] = 37
To_Export_SKU_s[24, 13] = 16
To_Export_SKU_s[23, 13] = 11
To_Export_SKU_s[22, 13] = 1
