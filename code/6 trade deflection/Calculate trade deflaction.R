library("splitstackshape")
library("xlsx")
library("foreign")
library("ggplot2")
library("scales")
library("gtable")
library("grid")
library("extrafontdb")
library("extrafont")
library("Rttf2pt1")
library("zoo")
library("gtalibrary")
library("lubridate")
library("data.table")
library("gridExtra")
rm(list = ls())


# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

setwd("GTA cloud")
# setwd("GTA cloud/0 report production/GTA 23")
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/")

#Settings
chapter.number=6
chapter.name="Trade deflection"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")

# Example table/figure name:
# paste(output.path,"/Figure ",chapter.number,".1 - A fine picture.png", sep="")


### THE GTA standard colour palette
gta_colour_palette()

# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
trump.tariffs <- read.csv("0 report production/GTA 23/help files/trade war - hs codes - usa.csv")


#########################################################################################################
###################### Looking at deflection to other markets more carefully ############################
#########################################################################################################

for(country in c("Germany", "France", "Brazil", "Japan")){
  eval(parse(text = paste0("load('data/ITC/G20 data/", country, ".Rdata')")))
  country <- gsub(pattern = " ", replacement = "_", country)
  correspondence <- subset(correspondence, exporter=="China" & is.na(`Product code`)==F)
  trade.data <- subset(trade.data, ID %in% correspondence$ID)
  
  # correspondence <- subset(correspondence, `Product code`>100)
  correspondence$`Product code` <- as.integer(correspondence$`Product code`)
  correspondence$hs8=substr(correspondence$`Product code`, 1,8)
  if(country=="Japan"){ 
    correspondence$hs8[nchar(correspondence$`Product code`)%%8!=1]=substr(correspondence$`Product code`[nchar(correspondence$`Product code`)%%8!=1], 1,7)
  } else {
    correspondence$hs8[nchar(correspondence$`Product code`)%%2==1]=substr(correspondence$`Product code`[nchar(correspondence$`Product code`)%%2==1], 1,7)
  }
  correspondence$hs6=substr(correspondence$hs8, 1,6)
  correspondence$hs6[nchar(correspondence$hs8)%%2==1]=substr(correspondence$hs8[nchar(correspondence$hs8)%%2==1], 1,5)
  
  correspondence$variable <- NA
  correspondence$variable[correspondence$hs6 %in% subset(trump.tariffs, state.act.id==27214)$hs10] <- "aluminum"
  correspondence$variable[correspondence$`Product code` %in% subset(trump.tariffs, state.act.id==27214)$hs10] <- "aluminum"
  correspondence$variable[correspondence$hs6 %in% subset(trump.tariffs, state.act.id==27158)$hs10] <- "steel"
  correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, state.act.id==28096)$hs10] <- "washers"
  correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, state.act.id==27215)$hs10] <- "solar cells"
  correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==57917)$hs10 & correspondence$exporter=="China"] <- "China round 1"
  correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==62073)$hs10 & correspondence$exporter=="China"] <- "China round 2"
  correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==63051)$hs10 & correspondence$exporter=="China"] <- "China round 3"
  correspondence$variable[is.na(correspondence$variable)] <- "not hit"

  trade.data <- merge(trade.data, correspondence, by=c("ID"), all.x = T)
  trade.data$date <- as.Date(paste(trade.data$year, trade.data$month, "15", sep = "-"))
  eval(parse(text = paste0("trade.data.", country, " <- trade.data")))
  rm(correspondence, trade.data)
  print(country)
}
trade.data <- rbind(trade.data.France, trade.data.Germany, trade.data.Brazil, trade.data.Japan)
trade.data <- subset(trade.data, year>2011)
# rm(trade.data.France, trade.data.Germany, trade.data.Brazil, trade.data.Japan)

### Exclude one ship purchase by Brazil (clear outlier)
trade.data <- subset(trade.data, !(importer=="Brazil" & value==2071125000))



####################################################################################################
####################### More narrow focus on trade deflection ######################################
####################################################################################################
load("data/ITC/G20 data/United States of America.Rdata")
correspondence <- subset(correspondence, exporter=="China")
correspondence$hs8=substr(correspondence$`Product code`, 1,8)
correspondence$hs8[nchar(correspondence$`Product code`)%%2==1]=substr(correspondence$`Product code`[nchar(correspondence$`Product code`)%%2==1], 1,7)
correspondence$hs6=substr(correspondence$hs8, 1,6)
correspondence$hs6[nchar(correspondence$hs8)%%2==1]=substr(correspondence$hs8[nchar(correspondence$hs8)%%2==1], 1,5)
correspondence$variable <- NA
correspondence$variable[correspondence$hs6 %in% subset(trump.tariffs, state.act.id==27214)$hs10] <- "aluminum"
correspondence$variable[correspondence$`Product code` %in% subset(trump.tariffs, state.act.id==27214)$hs10] <- "aluminum"
correspondence$variable[correspondence$hs6 %in% subset(trump.tariffs, state.act.id==27158)$hs10] <- "steel"
correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, state.act.id==28096)$hs10] <- "washers"
correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, state.act.id==27215)$hs10] <- "solar cells"
correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==57917)$hs10 & correspondence$exporter=="China"] <- "China round 1"
correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==62073)$hs10 & correspondence$exporter=="China"] <- "China round 2"
correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==63051)$hs10 & correspondence$exporter=="China"] <- "China round 3"
correspondence$variable[is.na(correspondence$variable)] <- "not hit"
trade.data <- subset(trade.data, ID %in% correspondence$ID)
trade.data <- merge(trade.data, correspondence, by=c("ID"), all.x = T)
trade.data$date <- as.Date(paste(trade.data$year, trade.data$month, "15", sep = "-"))

trade.data.US <- aggregate(value ~ `Product code` + hs8 + hs6, subset(trade.data, date>"2017-07-06" & date<"2018-07-06"), sum)
# trade.data.US <- aggregate(value ~ `Product code` + hs8 + hs6 + ID, subset(trade.data, date>"2017-07-06" & date<"2018-07-06"), sum)
trade.data.US <- subset(trade.data.US, value>100000000)
test <- aggregate(value ~ month + year + importer + exporter + date + hs6, data = subset(trade.data, variable == "China round 1" & `Product code` %in% trade.data.US$`Product code` & date>"2017-07-06"), sum)
test1 <- test
test1$year <- test1$year  + 1
test1 <- unique(subset(test1, select = c("value", "month", "importer", "year", "hs6")))
test <- merge(test, test1, by=c("month", "importer", "year", "hs6"), all.x = T) 
setnames(test, "value.x", "value")
setnames(test, "value.y", "prior.value")
test$yoy.growth <- test$value/test$prior.value - 1
test <- subset(test, is.na(prior.value)==F)
us.benchmark <- subset(test, select = c("date", "hs6", "yoy.growth"))
setnames(us.benchmark, "yoy.growth", "US.yoy.growth")
rm(test1, test)

trade.data.4 <- rbind(trade.data.France, trade.data.Germany, trade.data.Brazil, trade.data.Japan)
test <- aggregate(value ~ month + year + importer + exporter + date + hs6, data = subset(trade.data.4, variable == "China round 1" & hs6 %in% us.benchmark$hs6 & date>"2017-07-06"), sum)
test1 <- test
test1$year <- test1$year  + 1
test1 <- unique(subset(test1, select = c("value", "month", "importer", "exporter", "year", "hs6")))
test <- merge(test, test1, by=c("month", "importer", "exporter", "year", "hs6"), all.x = T) 
setnames(test, "value.x", "value")
setnames(test, "value.y", "prior.value")
test$yoy.growth <- test$value/test$prior.value - 1
test <- subset(test, is.na(prior.value)==F)
test$change <- (test$value - test$prior.value)/1000000

test <- merge(test, us.benchmark, by=c("date", "hs6"), all.x = T)
rm(test1)
test$date <- as.factor(test$date)

plot6.1 <- ggplot()+
  geom_point(data=test, aes(x=US.yoy.growth, y=change, colour=date))+
  facet_wrap(~ importer, ncol = 2, nrow=2, scales = "free")+
  geom_vline(xintercept = 0,size=0.5, colour="#666666")+
  geom_hline(yintercept = 0,size=0.5, colour="#666666")+
  gta_plot_wrapper(
    y.right.enable = T,
    y.left.name = "Change in Imports (in million USD)",
    x.bottom.name = "Year-on-year change in Chinese export share to the United States",
    x.bottom.limits = c(-1,0),
    colour.legend.col = 3,
    colour.legend.title = NULL,
    colour.palette = gta_colour$qualitative[c(1,2,4)]
  )+
  gta_theme()+
  theme()


plot6.1

gta_plot_saver(plot=plot6.1,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".1 G20 import patterns from CHN - 1st tranche (hs8 level) - combined graph", sep=""))




deflection.table <- unique(subset(test, select = c("importer", "month")))
deflection.table$number.HS6products <- NA
deflection.table$share.within <- NA
deflection.table$share.within.before <- NA
deflection.table$share.total <- NA
deflection.table$share.total.before <- NA

for(country in unique(deflection.table$importer)){
  for(time in subset(deflection.table, importer==country)$month){
    deflection.table$number.HS6products[deflection.table$importer==country & deflection.table$month==time] <- length(unique(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$hs6))
    
    deflection.table$share.within[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$value)/sum(subset(test, importer==country & month==time)$value),2)
    deflection.table$share.within.before[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$prior.value)/sum(subset(test, importer==country & month==time)$prior.value),2)
    
    deflection.table$share.total[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$value)/sum(subset(trade.data.4, importer==country & month==time & year==2018)$value),2)
    deflection.table$share.total.before[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$prior.value)/sum(subset(trade.data.4, importer==country & month==time & year==2017)$value),2)
  }
}

deflection.table <- deflection.table[order(deflection.table$importer),]
write.xlsx(x = deflection.table, file = paste0("0 report production/GTA 23/tables & figures/",output.path,"/", "Figure ",chapter.number,".1 Deflection matrix.xlsx"), row.names = F, sheetName = "Based on HS8")




##### Redo the whole matrix using a more aggregated assumption on the Trump tariffs (basically all products within an HS6 category)

test <- aggregate(value ~ month + year + importer + exporter + date + hs6, data = subset(trade.data.4, hs6 %in% us.benchmark$hs6 & date>"2017-07-06"), sum)
test1 <- test
test1$year <- test1$year  + 1
test1 <- unique(subset(test1, select = c("value", "month", "importer", "exporter", "year", "hs6")))
test <- merge(test, test1, by=c("month", "importer", "exporter", "year", "hs6"), all.x = T) 
setnames(test, "value.x", "value")
setnames(test, "value.y", "prior.value")
test$yoy.growth <- test$value/test$prior.value - 1
test <- subset(test, is.na(prior.value)==F)
test$change <- (test$value - test$prior.value)/1000000

test <- merge(test, us.benchmark, by=c("date", "hs6"), all.x = T)
rm(test1)
test$date <- as.factor(test$date)

plot6.2 <- ggplot()+
  geom_point(data=test, aes(x=US.yoy.growth, y=change, colour=date))+
  facet_wrap(~ importer, ncol = 2, nrow=2, scales = "free")+
  geom_vline(xintercept = 0,size=0.5, colour="#666666")+
  geom_hline(yintercept = 0,size=0.5, colour="#666666")+
  gta_plot_wrapper(
    y.right.enable = T,
    y.left.name = "Change in imports (in million USD)",
    x.bottom.name = "Year-on-year change in Chinese export share to the United States",
    x.bottom.limits = c(-1,0),
    colour.legend.col = 3,
    colour.legend.title = NULL,
    colour.palette = gta_colour$qualitative[c(1,2,4)]
  )+
  gta_theme()+
  theme()


plot6.2

gta_plot_saver(plot=plot6.2,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".2 G20 import patterns from CHN - 1st tranche (hs6 level) - combined graph", sep=""))





deflection.table <- unique(subset(test, select = c("importer", "month")))
deflection.table$number.HS6products <- NA
deflection.table$share.within <- NA
deflection.table$share.within.before <- NA
deflection.table$share.total <- NA
deflection.table$share.total.before <- NA

for(country in unique(deflection.table$importer)){
  for(time in subset(deflection.table, importer==country)$month){
    deflection.table$number.HS6products[deflection.table$importer==country & deflection.table$month==time] <- length(unique(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$hs6))
    
    deflection.table$share.within[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$value)/sum(subset(test, importer==country & month==time)$value),2)
    deflection.table$share.within.before[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$prior.value)/sum(subset(test, importer==country & month==time)$prior.value),2)
    
    deflection.table$share.total[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$value)/sum(subset(trade.data.4, importer==country & month==time & year==2018)$value),2)
    deflection.table$share.total.before[deflection.table$importer==country & deflection.table$month==time] <- round(100*sum(subset(test, importer==country & month==time & yoy.growth>0 & US.yoy.growth<0)$prior.value)/sum(subset(trade.data.4, importer==country & month==time & year==2017)$value),2)
  }
}
deflection.table <- deflection.table[order(deflection.table$importer),]
write.xlsx(x = deflection.table, file = paste0("0 report production/GTA 23/tables & figures/",output.path,"/", "Figure ",chapter.number,".1 Deflection matrix.xlsx"), row.names = F, sheetName = "Based on HS6", append = T)



sum(subset(trade.data.4, importer== "Brazil" & variable == "China round 1" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)/sum(subset(trade.data.4, importer== "Brazil" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)
sum(subset(trade.data.4, importer== "France" & variable == "China round 1" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)/sum(subset(trade.data.4, importer== "France" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)
sum(subset(trade.data.4, importer== "Germany" & variable == "China round 1" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)/sum(subset(trade.data.4, importer== "Germany" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)
sum(subset(trade.data.4, importer== "Japan" & variable == "China round 1" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)/sum(subset(trade.data.4, importer== "Japan" & hs6 %in% us.benchmark$hs6 & date>"2018-07-06")$value)






