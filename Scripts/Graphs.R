library(reshape2)
library(ggplot2)
library(stringr)
library(dplyr)

HCFINVMasterCopy$ProductSKU <- str_sub(HCFINVMasterCopy$SKU, -3.-1)


#Main Columns
HCFINVGroupedSum<- HCFINVMasterCopy %>%
  #filter(HCFINVGrouped, )
  group_by(ProductSKU) %>%
  summarize(Supplier= paste(unique(Supplier),collapse=","), SKU= paste(unique(SKU),collapse=","), `Item Description`= paste(unique(`Item Description`),collapse=","), Tier= paste(unique(Tier),collapse=","), `BFW Inv`=sum(`BFW Inv`),
            `RFW Inv`=sum(`RFW Inv`), `2016`=sum(`2016`), `2017`=sum(`2017`), `2018`=sum(`2018`), `2019`=sum(`2019`), `2020`=sum(`2020`), `2021`=sum(`2021`), `Notes`= paste(unique(Notes),collapse=","), `Detailed Category`= paste(unique(`Detailed Category`),collapse=","),`Category`= paste(unique(`High Level Category`),collapse=","),
            `Size`= paste(unique(`Size`),collapse=","),  `Color`= paste(unique(Color),collapse=","),`Proportion Days OOS/YTD`= sum(`ADJ_D_days`), `Estimated Loss YTD`= sum(`ADJ_D_loss`),`Potential Total Demand`= sum(`Adjusted Total Count`), `ProductSKU`= paste(unique(`ProductSKU`),collapse=","))





INV_long <- melt(HCFINVMasterCopy,
  id.vars=c("SKU"),
  measure.vars=c("2016","2017","2018","2019", "2020", "2021"),
  variable.name="Year",value.name="Demand")
INV_long$Year <- as.numeric(as.character(INV_long$Year))
INV_longDemand <- INV_long

INV_longDemand$ProductSKU <- str_sub(INV_longDemand$SKU, -3.-1)
GroupedLongDemand<- INV_longDemand %>%
  #filter(HCFINVGrouped, )
  group_by(ProductSKU, Year) %>%
  summarize(`Demand`= sum(`Demand`))



INV_longCate <- melt(HCFINVMaster,
    id.vars=c("SKU"),
    measure.vars=("Detailed Category"),
    variable.name="Category2",value.name="Category")
INV_longCate$ProductSKU <- str_sub(INV_longCate$SKU, -3.-1)

GroupedLongCate<- INV_longCate %>%
  #filter(HCFINVGrouped, )
  group_by(ProductSKU) %>%
  summarize(`Detailed Category`= `Category`)
GroupedLongCate<- GroupedLongCate %>%
  #filter(HCFINVGrouped, )
  group_by(ProductSKU) %>%
  summarize(`Detailed Category`= `Detailed Category`)


INV_longM <- merge(INV_longDemand,INV_longCate,by="ProductSKU")

Mayplot<- ggplot(INV_longM %>% filter(SKU<= "HCF199999"& SKU>="HCF100008"), aes(x= Year, y=Demand, group=Catagory.1)) +
  geom_line(aes(color=Catagory.1)) +
  coord_cartesian(ylim=c(0, 250))+
  facet_wrap(~SKU)
Mayplot + scale_y_continuous(breaks=seq(0, 250, 50))


mylist <- list("HCF100008","HCF100012", "HCF100016", "HCF100020", "HCF100058", "HCF100064", "HCF100066", "HCF100067", "HCF100068", "HCF100076", "HCF100077", "HCF100086", "HCF100087", "HCF100136", "HCF100137", "HCF100146", "HCF100147", "HCF100408", "HCF100408F", "HCF1000412", "HCF100412F", "HCF100508", "HCF100508F", "HCF100512", "HCF100512F", "HCF100513", "HCF100514")

FINVLong <- INV_long %>% filter(SKU %in% mylist)
ggplot(FINVLong, aes(Year, Demand))
geom_line() +
  facet_grid(SKU ~ .)

ggplot(FINVLong %>% filter(SKU<= "HCF102127"& SKU>="HCF100008"), aes(x= Year, y=Demand)) +
  geom_line() +
  facet_wrap(~SKU)