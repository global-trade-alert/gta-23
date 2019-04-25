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

# setwd("GTA cloud/0 report production/GTA 23")
setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

#Settings
chapter.number=5
chapter.name="Anticipation effects"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")

# Example table/figure name:
# paste(output.path,"/Figure ",chapter.number,".1 - A fine picture.png", sep="")


### THE GTA standard colour palette
gta_colour_palette()

load("data/ITC/G20 data/United States of America.Rdata")

trump.tariffs <- read.csv("0 report production/GTA 23/help files/trade war - hs codes - usa.csv")

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

trade.data <- merge(trade.data, correspondence, by=c("ID"), all.x = T)
trade.data$date <- as.Date(paste(trade.data$year, trade.data$month, "15", sep = "-"))


#####################################################################################
### Growth rates (rounds 1-3)
#####################################################################################
test <- aggregate(value ~ date + month + year + variable, 
                  subset(trade.data, (grepl("China", variable) & exporter=="China")), sum)
test <- merge(test, subset(subset(test, year==2016), select = c("month", "variable", "value")), by=c("month", "variable"), all.x = T)
setnames(test, "value.x", "value")
setnames(test, "value.y", "value.2016")
test$value.norm <- test$value / test$value.2016
test1 <- test
test1$year <- test1$year  + 1
test1 <- subset(test1, select = c("value", "month", "variable", "year"))
test <- merge(test, test1, by=c("month", "variable", "year"))
setnames(test, "value.x", "value")
setnames(test, "value.y", "prior.value")
test$yoy.growth <- test$value/test$prior.value - 1
test <- subset(test, year>2011)
test1 <- aggregate(yoy.growth ~ variable, test, sd)
setnames(test1, "yoy.growth", "st.dev")
test <- merge(test, test1, by="variable", all.x = T)
test$yoy.growth.std <- test$yoy.growth/test$st.dev

l1 <- ggplot(data = subset(test, variable=="China round 1"), aes(date, yoy.growth, colour=variable)) +
  geom_line(show.legend = F)+
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-04-03"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-07-06"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
# gta_theme()
l1

l2 <- ggplot(data = subset(test, variable=="China round 2"), aes(date, yoy.growth, colour=variable)) +
  geom_line(show.legend = F)+
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-06-15"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-08-23"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
# gta_theme()
l2

l3 <- ggplot(data = subset(test, variable=="China round 3"), aes(date, yoy.growth, colour=variable)) +
  geom_line(show.legend = F)+
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-08-17"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-09-24"), linetype=2, show.legend = F) +
  theme(axis.title.y = element_blank()) +
  xlab("Year")
# gta_theme()
l3


### Growth rate distributions (rounds 1-3)
r1 <- ggplot() +
  geom_density(data = subset(test, date<"2018-04-01" & variable=="China round 1"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>="2018-04-01" & date<"2018-07-01" & variable=="China round 1"), aes(x = yoy.growth, y = 0), colour = gta_colour$blue[1], size=2,show.legend = F) +
  geom_point(data = subset(test, date>="2018-07-01" & variable=="China round 1"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept =median(subset(test, date<"2018-04-01" & variable=="China round 1")$yoy.growth) + 1.645*unique(subset(test, variable=="China round 1")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept =median(subset(test, date<"2018-04-01" & variable=="China round 1")$yoy.growth) -1.645*unique(subset(test, variable=="China round 1")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
# gta_theme()
r1

r2 <- ggplot() +
  geom_density(data = subset(test, date<"2018-07-01" & variable=="China round 2"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>"2018-07-01" & date<"2018-08-23" & variable=="China round 2"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  geom_point(data = subset(test, date>="2018-08-23" & variable=="China round 2"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2018-07-01" & variable=="China round 2")$yoy.growth) + 1.645*unique(subset(test, variable=="China round 2")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2018-07-01" & variable=="China round 2")$yoy.growth) - 1.645*unique(subset(test, variable=="China round 2")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
# gta_theme()
r2

r3 <- ggplot() +
  geom_density(data = subset(test, date<"2018-08-01" & variable=="China round 3"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>"2018-08-01" & date<"2018-09-24" & variable=="China round 3"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  # geom_point(data = subset(test, date>="2018-09-24" & variable=="China round 3"), aes(yoy.growth, 0), colour="blue") +
  geom_vline(xintercept = median(subset(test, date<"2018-08-01" & variable=="China round 3")$yoy.growth) + 1.645*unique(subset(test, variable=="China round 3")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2018-08-01" & variable=="China round 3")$yoy.growth) - 1.645*unique(subset(test, variable=="China round 3")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title.y = element_blank()) +
  xlab("Year-on-year monthly growth rate")
# gta_theme()
r3

p1 <- grid.arrange(arrangeGrob(l1, r1, top = textGrob("First tranche", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   arrangeGrob(l2, r2, top = textGrob("Second tranche", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   arrangeGrob(l3, r3, top = textGrob("Third tranche", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   nrow = 3, 
                   left="Year-on-year monthly growth rate",
                   right="Distribution of year-on-year monthly growth rates")
p1


gta_plot_saver(plot = p1,
               path = paste("0 report production/GTA 23/tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".1 US import patterns - Chinese exports - combined graph", sep=""))

rm(l1, r1, l2, r2, l3, r3, test, test1, p1)


#####################################################################################
############################## Growth rates (steel) #################################
#####################################################################################
trade.data$exporter[trade.data$exporter %in% c("Canada", "Mexico")] <- "NAFTA"
trade.data$exporter[trade.data$exporter %in% c("France", "Germany", "Italy", "United Kingdom")] <- "European Union"
test <- aggregate(value ~ date + month + year + exporter, 
                  subset(trade.data, (variable=="steel" & exporter %in% c("China", "NAFTA", "European Union"))), sum)
test <- merge(test, subset(subset(test, year==2016), select = c("month", "exporter", "value")), by=c("month", "exporter"), all.x = T)
setnames(test, "value.x", "value")
setnames(test, "value.y", "value.2016")
test$value.norm <- test$value / test$value.2016
test1 <- test
test1$year <- test1$year  + 1
test1 <- subset(test1, select = c("value", "month", "exporter", "year"))
test <- merge(test, test1, by=c("month", "exporter", "year"))
setnames(test, "value.x", "value")
setnames(test, "value.y", "prior.value")
test$yoy.growth <- test$value/test$prior.value - 1
test <- subset(test, year>2011)
test1 <- aggregate(yoy.growth ~ exporter, test, sd)
setnames(test1, "yoy.growth", "st.dev")
test <- merge(test, test1, by="exporter", all.x = T)
test$yoy.growth.std <- test$yoy.growth/test$st.dev

l1 <- ggplot(data = subset(test, exporter=="China"), aes(date, yoy.growth, colour=exporter)) + 
  geom_line(show.legend = F)+ 
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2017-04-27"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-03-23"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
l1

l2 <- ggplot(data = subset(test, exporter=="European Union"), aes(date, yoy.growth, colour=exporter)) + 
  geom_line(show.legend = F)+ 
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2017-04-27"), linetype=2, show.legend = F) +
  # geom_vline(xintercept = as.Date("2018-03-23"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-06-01"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
l2

l3 <- ggplot(data = subset(test, exporter=="NAFTA"), aes(date, yoy.growth, colour=exporter)) + 
  geom_line(show.legend = F)+ 
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2017-04-27"), linetype=2, show.legend = F) +
  # geom_vline(xintercept = as.Date("2018-03-23"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-06-01"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
l3

r1 <- ggplot() +
  geom_density(data = subset(test, date<"2017-04-27" & exporter=="China"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>="2017-04-27" & date<"2018-03-23" & exporter=="China"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  geom_point(data = subset(test, date>="2018-03-23" & exporter=="China"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="China")$yoy.growth) + 1.645*unique(subset(test, exporter=="China")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="China")$yoy.growth) - 1.645*unique(subset(test, exporter=="China")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
r1

r2 <- ggplot() +
  geom_density(data = subset(test, date<"2017-04-27" & exporter=="European Union"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>="2017-04-27" & date<"2018-06-01" & exporter=="European Union"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  geom_point(data = subset(test, date>="2018-06-01" & exporter=="European Union"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="European Union")$yoy.growth) + 1.645*unique(subset(test, exporter=="European Union")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="European Union")$yoy.growth) - 1.645*unique(subset(test, exporter=="European Union")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
r2

r3 <- ggplot() +
  geom_density(data = subset(test, date<"2017-04-27" & exporter=="NAFTA"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>="2017-04-27" & date<"2018-06-01" & exporter=="NAFTA"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  geom_point(data = subset(test, date>="2018-06-01" & exporter=="NAFTA"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="NAFTA")$yoy.growth) + 1.645*unique(subset(test, exporter=="NAFTA")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="NAFTA")$yoy.growth) - 1.645*unique(subset(test, exporter=="NAFTA")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
r3

p1 <- grid.arrange(arrangeGrob(l1, r1, top = textGrob("China", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   arrangeGrob(l2, r2, top = textGrob("Largest 4 EU members", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   arrangeGrob(l3, r3, top = textGrob("NAFTA", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   nrow = 3,
                   left= textGrob("Year-on-year monthly growth rate", rot = 90, gp = gpar(fontsize=10, fonttype="Open Sans")),
                   right= textGrob("Distribution of year-on-year monthly growth rates", rot = -90, gp = gpar(fontsize=10, fonttype="Open Sans")))
p1 

gta_plot_saver(plot = p1,
               path = paste("0 report production/GTA 23/tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".2 US import patterns - Steel - combined graph", sep=""))
rm(l1, r1, l2, r2, l3, r3, test, test1, p1)

#####################################################################################
############################## Growth rates (aluminum) #################################
#####################################################################################
test <- aggregate(value ~ date + month + year + exporter, 
                  subset(trade.data, (variable=="aluminum" & exporter %in% c("China", "NAFTA", "European Union"))), sum)
test <- merge(test, subset(subset(test, year==2016), select = c("month", "exporter", "value")), by=c("month", "exporter"), all.x = T)
setnames(test, "value.x", "value")
setnames(test, "value.y", "value.2016")
test$value.norm <- test$value / test$value.2016
test1 <- test
test1$year <- test1$year  + 1
test1 <- subset(test1, select = c("value", "month", "exporter", "year"))
test <- merge(test, test1, by=c("month", "exporter", "year"))
setnames(test, "value.x", "value")
setnames(test, "value.y", "prior.value")
test$yoy.growth <- test$value/test$prior.value - 1
test <- subset(test, year>2011)
test1 <- aggregate(yoy.growth ~ exporter, test, sd)
setnames(test1, "yoy.growth", "st.dev")
test <- merge(test, test1, by="exporter", all.x = T)
test$yoy.growth.std <- test$yoy.growth/test$st.dev

l1 <- ggplot(data = subset(test, exporter=="China"), aes(date, yoy.growth, colour=exporter)) + 
  geom_line(show.legend = F)+ 
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2017-04-27"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-03-23"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
l1

l2 <- ggplot(data = subset(test, exporter=="European Union"), aes(date, yoy.growth, colour=exporter)) + 
  geom_line(show.legend = F)+ 
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2017-04-27"), linetype=2, show.legend = F) +
  # geom_vline(xintercept = as.Date("2018-03-23"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-06-01"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
l2

l3 <- ggplot(data = subset(test, exporter=="NAFTA"), aes(date, yoy.growth, colour=exporter)) + 
  geom_line(show.legend = F)+ 
  geom_point(size=1, show.legend = F) +
  geom_vline(xintercept = as.Date("2017-04-27"), linetype=2, show.legend = F) +
  # geom_vline(xintercept = as.Date("2018-03-23"), linetype=2, show.legend = F) +
  geom_vline(xintercept = as.Date("2018-06-01"), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
l3

r1 <- ggplot() +
  geom_density(data = subset(test, date<"2017-04-27" & exporter=="China"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>="2017-04-27" & date<"2018-03-23" & exporter=="China"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  geom_point(data = subset(test, date>="2018-03-23" & exporter=="China"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="China")$yoy.growth) + 1.645*unique(subset(test, exporter=="China")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="China")$yoy.growth) - 1.645*unique(subset(test, exporter=="China")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
r1

r2 <- ggplot() +
  geom_density(data = subset(test, date<"2017-04-27" & exporter=="European Union"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>="2017-04-27" & date<"2018-06-01" & exporter=="European Union"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  geom_point(data = subset(test, date>="2018-06-01" & exporter=="European Union"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="European Union")$yoy.growth) + 1.645*unique(subset(test, exporter=="European Union")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="European Union")$yoy.growth) - 1.645*unique(subset(test, exporter=="European Union")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
r2

r3 <- ggplot() +
  geom_density(data = subset(test, date<"2017-04-27" & exporter=="NAFTA"), aes(yoy.growth), show.legend = F) +
  geom_point(data = subset(test, date>="2017-04-27" & date<"2018-06-01" & exporter=="NAFTA"), aes(yoy.growth, 0), colour = gta_colour$blue[1], size=2, show.legend = F) +
  geom_point(data = subset(test, date>="2018-06-01" & exporter=="NAFTA"), aes(yoy.growth, 0), colour = gta_colour$red[1], size=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="NAFTA")$yoy.growth) + 1.645*unique(subset(test, exporter=="NAFTA")$st.dev), linetype=2, show.legend = F) +
  geom_vline(xintercept = median(subset(test, date<"2017-04-27" & exporter=="NAFTA")$yoy.growth) - 1.645*unique(subset(test, exporter=="NAFTA")$st.dev), linetype=2, show.legend = F) +
  theme(axis.title = element_blank())
r3

p1 <- grid.arrange(arrangeGrob(l1, r1, top = textGrob("China", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   arrangeGrob(l2, r2, top = textGrob("Largest 4 EU members", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   arrangeGrob(l3, r3, top = textGrob("NAFTA", gp = gpar(fontsize=10, fonttype="Open Sans")), nrow = 1),
                   nrow = 3,
                   left= textGrob("Year-on-year monthly growth rate", rot = 90, gp = gpar(fontsize=10, fonttype="Open Sans")),
                   right= textGrob("Distribution of year-on-year monthly growth rates", rot = -90, gp = gpar(fontsize=10, fonttype="Open Sans")))
p1

gta_plot_saver(plot = p1,
               path = paste("0 report production/GTA 23/tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".3 US import patterns - Aluminum - combined graph", sep=""))
rm(l1, r1, l2, r2, l3, r3, test, test1, p1)


