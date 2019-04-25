# US China interventions timeline

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
library("tidyverse")
rm(list = ls())


# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/")


#Settings
chapter.number=2
chapter.name="Why put the tariff war in perspective"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

# Relevant Intervention id
trump.tariffs <- read.csv("0 report production/GTA 23/help files/trade war - hs codes - usa.csv",sep=",")

measures <- read.xlsx("0 report production/GTA 23/help files/trade war - timeline (us-china-measures).xlsx", sheetIndex = 1, rowIndex = c(1:9))

measures$variable <- c("China steel & aluminium", 
                      "US steel & aluminium", 
                      "China round 1", 
                      "US round 1",
                      "China round 2",
                      "US round 2",
                      "China round 3",
                      "US round 3")

measures$group <- c("Steel & Aluminium",
                    "Steel & Aluminium",
                    "Tariff round 1",
                    "Tariff round 1",
                    "Tariff round 2",
                    "Tariff round 2",
                    "Tariff round 3",
                    "Tariff round 3")



###### US MEASURES
load("data/ITC/G20 data/United States of America.Rdata")
correspondence <- subset(correspondence, exporter=="China")
correspondence$hs8=substr(correspondence$`Product code`, 1,8)
correspondence$hs8[nchar(correspondence$`Product code`)%%2==1]=substr(correspondence$`Product code`[nchar(correspondence$`Product code`)%%2==1], 1,7)
correspondence$hs6=substr(correspondence$hs8, 1,6)
correspondence$hs6[nchar(correspondence$hs8)%%2==1]=substr(correspondence$hs8[nchar(correspondence$hs8)%%2==1], 1,5)
correspondence$variable <- NA

correspondence.steel <- correspondence
correspondence.1 <- correspondence
correspondence.2 <- correspondence
correspondence.3 <- correspondence

correspondence.steel$variable[correspondence$`Product code` %in% subset(trump.tariffs, state.act.id==27214 | state.act.id ==27158)$hs10 | correspondence$hs6 %in% subset(trump.tariffs, state.act.id==27214 | state.act.id ==27158)$hs10] <- "China steel & aluminium"
# correspondence$variable[correspondence$hs6 %in% subset(trump.tariffs, state.act.id==27158)$hs10] <- "steel"
# correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, state.act.id==28096)$hs10] <- "washers"
# correspondence$variable[correspondence$hs8 %in% subset(trump.tariffs, state.act.id==27215)$hs10] <- "solar cells"
correspondence.1$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==57917)$hs10 & correspondence$exporter=="China"] <- "China round 1"
correspondence.2$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==62073)$hs10 & correspondence$exporter=="China"] <- "China round 2"
correspondence.3$variable[correspondence$hs8 %in% subset(trump.tariffs, intervention.id==63051)$hs10 & correspondence$exporter=="China"] <- "China round 3"
correspondence.3$variable[correspondence$`Product code` %in% subset(trump.tariffs, intervention.id==63051)$hs10 & correspondence$exporter=="China"] <- "China round 3"
# correspondence$variable[is.na(correspondence$variable)] <- "not hit"

correspondence <- rbind(correspondence, 
                        correspondence.steel,
                        correspondence.1,
                        correspondence.2,
                        correspondence.3)

trade.data <- subset(trade.data, ID %in% correspondence$ID & year == 2017)
trade.data <- merge(trade.data, correspondence, by=c("ID"), all.x = T)
trade.data$date <- as.Date(paste(trade.data$year, trade.data$month, "15", sep = "-"))

trade.data.US <- aggregate(value ~ variable, trade.data, sum)


###### CHINESE
load("data/ITC/G20 data/China.Rdata")
china.tariffs <- read.csv("0 report production/GTA 23/help files/trade war - hs codes - china.csv",sep=",")
correspondence <- subset(correspondence, exporter=="United States of America")
correspondence$hs8=substr(correspondence$`Product code`, 1,8)
correspondence$hs8[nchar(correspondence$`Product code`)%%2==1]=substr(correspondence$`Product code`[nchar(correspondence$`Product code`)%%2==1], 1,7)
correspondence$hs6=substr(correspondence$hs8, 1,6)
correspondence$hs6[nchar(correspondence$hs8)%%2==1]=substr(correspondence$hs8[nchar(correspondence$hs8)%%2==1], 1,5)
correspondence$variable <- NA

correspondence.steel <- correspondence
correspondence.1 <- correspondence
correspondence.2 <- correspondence
correspondence.3 <- correspondence

correspondence.steel$variable[correspondence$hs8 %in% subset(china.tariffs, state.act.id==30443)$hs8 | correspondence$hs6 %in% subset(china.tariffs, state.act.id==30443)$hs6 ] <- "US steel & aluminium"
# correspondence$variable[correspondence$`Product code` %in% subset(china.tariffs, state.act.id==30443)$hs10] <- "aluminum"
correspondence.1$variable[correspondence$hs8 %in% subset(china.tariffs, intervention.id==61778)$hs8 & correspondence$exporter=="United States of America"] <- "US round 1"
correspondence.2$variable[correspondence$hs8 %in% subset(china.tariffs, state.act.id==31381)$hs8 & correspondence$exporter=="United States of America"] <- "US round 2"
correspondence.3$variable[correspondence$hs8 %in% subset(china.tariffs, state.act.id==31839)$hs8 & correspondence$exporter=="United States of America"] <- "US round 3"
# correspondence.1$variable[is.na(correspondence$variable)] <- "not hit"

correspondence <- rbind(correspondence, 
                        correspondence.steel,
                        correspondence.1,
                        correspondence.2,
                        correspondence.3)

trade.data <- subset(trade.data, ID %in% correspondence$ID & year == 2017)
trade.data <- merge(trade.data, correspondence, by=c("ID"), all.x = T)
trade.data$date <- as.Date(paste(trade.data$year, trade.data$month, "15", sep = "-"))

trade.data.china <- aggregate(value ~ variable, trade.data, sum)




trade.data.both <- rbind(trade.data.US, trade.data.china)

measures <- merge(measures, trade.data.both, by=c("variable"), all.x=T)

measures <- measures[with(measures, order(Date)), ]
measures$Date <- as.Date(measures$Date)
row.names(measures) <- NULL

measures.xlsx <- measures[,c("Date","Case","Country","Action.taken","GTA.node","Tariffs.announced","group","value")]

names(measures.xlsx) <- c("Date","Case","Implementing Country","Description","Affected State Acts","Date announced","Tariff round","Value Affected")

write.xlsx(measures.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".1.xlsx", sep=""), row.names=F)
setnames(measures, "Country", "Implementing Country")
plot3.2 <- ggplot()+
  geom_bar(data=measures, aes(x=forcats::fct_inorder(group), y=value/1000000000, fill=`Implementing Country`), width=0.8,position_dodge(width=0.9),stat="identity")+
  labs(x="Tariff increases implemented during 2018",y="Value of bilateral trade affected (in billion USD)")+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,5)]))+
  scale_x_discrete(labels=c("Steel & Aluminium\nMarch/April",
                           "Tariff round\n1 July",
                           "Tariff round\n2 August",
                           "Tariff round\n3 September"))+
  scale_y_continuous(sec.axis =sec_axis(~.,name = "Value of bilateral trade affected (in billion USD)"))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(size=12),
        axis.title.y.left = element_text(size=12),
        axis.title.y.right = element_text(size=12))

plot3.2

gta_plot_saver(plot=plot3.2,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".1", sep=""))

