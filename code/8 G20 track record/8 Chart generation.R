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
rm(list = ls())


# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

setwd("GTA cloud/0 report production/GTA 23")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA 21")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23/")

#Settings
chapter.number=8
chapter.name="G20 track record"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("help files/GTA 23 cutoff and definitions.R")


## load master data
load("../../data/master_plus.Rdata")


### THE GTA standard colour palette
gta_colour_palette()


### drawing

implemented=master
implemented<-as.data.frame(implemented)

# data prep
implemented<-subset(implemented, is.na(date.implemented)==F)
implemented<-subset(implemented, date.implemented<=cutoff)

implemented$protect<-0
implemented$protect[implemented$gta.evaluation!="Green"]<-1


### G20 restriction
implemented<-subset(implemented, i.atleastone.G20==1)




## Figure 5-1 - new: May 2017, 2015, 2013, 2011, 2009.
implemented$quarter<-paste(year(implemented$date.implemented), "-Q", quarter(implemented$date.implemented), sep="")

figure81<-aggregate(intervention.id ~ quarter, data=subset(implemented, date.implemented<="2009-10-31" & date.published<="2009-10-31" & gta.evaluation!="Green"), function(x) length(unique(x)))
figure81$phase<-"31 October 2009"

figure81.11<-aggregate(intervention.id ~ quarter, data=subset(implemented, date.implemented<="2011-10-31" & date.published<="2011-10-31" & gta.evaluation!="Green"), function(x) length(unique(x)))
figure81.11$phase<-"31 October 2011  "
figure81<-rbind(figure81, figure81.11)

figure81.13<-aggregate(intervention.id ~ quarter, data=subset(implemented, date.implemented<="2013-10-31" & date.published<="2013-10-31" & gta.evaluation!="Green"), function(x) length(unique(x)))
figure81.13$phase<-"31 October 2013  "
figure81<-rbind(figure81, figure81.13)

figure81.15<-aggregate(intervention.id ~ quarter, data=subset(implemented, date.implemented<="2015-10-31" & date.published<="2015-10-31" & gta.evaluation!="Green"), function(x) length(unique(x)))
figure81.15$phase<-"31 October 2015  "
figure81<-rbind(figure81, figure81.15)

figure81.17<-aggregate(intervention.id ~ quarter, data=subset(implemented, date.implemented<="2017-10-31" & date.published<="2017-10-31" & gta.evaluation!="Green"), function(x) length(unique(x)))
figure81.17$phase<-"31 October 2017"
figure81<-rbind(figure81, figure81.17)

figure81.18<-aggregate(intervention.id ~ quarter, data=subset(implemented, date.implemented<="2018-10-31" & date.published<="2018-10-31" & gta.evaluation!="Green"), function(x) length(unique(x)))
figure81.18$phase<-"31 October 2018"
figure81<-rbind(figure81, figure81.18)


figure81=subset(figure81,! quarter %in% c("2018-Q4","2008-Q1","2008-Q2","2008-Q3"))


figure81.xlsx=reshape(figure81, idvar="quarter", timevar = "phase", direction="wide")
figure81.xlsx[is.na(figure81.xlsx)]=0

names(figure81.xlsx)=c("Quarter", paste("Reported by 31 October ", c(seq(2009,2017,2),2018), sep=""))
write.xlsx(figure81.xlsx, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".1 - Data for Figure 5.1.xlsx", sep=""), row.names=F)

p1 <- ggplot(data=figure81, aes(x=quarter, y=intervention.id, group=phase, colour=phase)) +
  geom_line(size=1) +
  gta_plot_wrapper(
    y.right.enable = T,
    colour.palette = gta_colour$qualitative,
    colour.legend.title = "Reported by ...",
    colour.legend.col = 5,
    x.bottom.name = "Implementation quarter",
    y.left.name = "Number of harmful interventions implemented",
    y.left.breaks = seq(0,450, by=50)
  ) +
  scale_x_discrete(breaks=c("2008-Q4","2009-Q2", "2009-Q4", "2010-Q2", "2010-Q4", "2011-Q2", "2011-Q4", "2012-Q2", "2012-Q4", "2013-Q2", "2013-Q4", "2014-Q2", "2014-Q4", "2015-Q2", "2015-Q4", "2016-Q2", "2016-Q4", "2017-Q2", "2017-Q4", "2018-Q2", "2018-Q4"))+
  gta_theme(x.bottom.angle = 90)+
  theme(axis.text.x.bottom = element_text(margin=margin(t=3), vjust=.5 ))

p1

gta_plot_saver(plot = p1,
               path = paste("tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".1 G20 interventions found by various cut-offs", sep=""))






## Figure 5-2: reported at cutoff date per year
gta_data_slicer(data.path="../../data/master_plus.Rdata",
                lag.adjustment = "10-31",
                implementing.country = "G20",
                keep.implementer = T,
                implementation.period = c("2009-01-01",cutoff),
                keep.implementation.na=F
                )

master.sliced$protect=as.numeric(master.sliced$gta.evaluation!="Green")
master.sliced$year=year(master.sliced$date.implemented)
figure82=aggregate(intervention.id ~ year + protect, master.sliced, function(x) length(unique(x)))
figure82$type="Liberalising"
figure82$type[figure82$protect==1]="Harmful"
figure82$protect=NULL

f82t=aggregate(intervention.id ~ year, master.sliced, function(x) length(unique(x)))
f82t$type="Total"
figure82=rbind(figure82, f82t)

rm(master.sliced, f82t)

## Interventions reported by cutoff
figure82.xlsx=reshape(figure82, idvar="type", timevar = "year", direction="wide")
names(figure82.xlsx)=c("Policy stance",2009:2018)
write.xlsx(figure82.xlsx, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".2 - Data for Figure 5.2.xlsx", sep=""), row.names=F)


figure82$label.position.y=figure82$intervention.id-50
figure82$label.position.x=figure82$year
figure82$label.position.x[figure82$year==2018 & figure82$type=="Harmful"]=2018.15
  
  
p1 <- ggplot(data=figure82, aes(x=year, y=intervention.id, group=type, colour=type)) +
  geom_line(size=1) +
  geom_text(data = subset(figure82,  type != "Total"), aes(x=label.position.x, y=label.position.y, label=intervention.id))+
  geom_point(size=3) +
    gta_plot_wrapper(
      y.right.enable = T,
      y.left.name = "Number of implemented interventions",
      x.bottom.name = "Reference year",
      colour.legend.title = NULL,
      colour.legend.col = 3,
      colour.palette = c(gta_colour$red[1], gta_colour$green[1], "black")
    )+
    gta_theme()+
  theme(axis.text.x.bottom = element_text(size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10))

p1

gta_plot_saver(plot=p1,
               path = paste("tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".2 - Same-year G20 interventions found by cut-off date", sep=""))






# 
# ### Figure 8-3: Intra-G20 groups
# implemented$group<-"Other G20"
# implemented$group[implemented$i.un %in% c("826", "840", "124", "251", "276", "381", "392", "36")]<-"G7 & Australia"
# implemented$group[implemented$i.un %in% c("76", "156", "699", "643", "710" )]<-"BRICS"
# implemented$i.year=year(implemented$date.implemented)
# 
# group.action<-aggregate(intervention.id ~ group + i.year, data=subset(implemented, protect==1 & i.atleastone.G20==1 & is.na(date.implemented)==F & date.implemented<=cutoff), function(x) length(unique((x))))
# setnames(group.action, old="intervention.id", new="frequency")
# year.action <- aggregate(frequency ~ i.year, data=group.action, function(x)sum(x))
# setnames(year.action, old="frequency", new="totalperyear")
# group.action<-merge(group.action, year.action, by="i.year", all.x=T)
# setnames(group.action, old="i.year", new="variable")
# group.action$value <- group.action$frequency / group.action$totalperyear
# #write.xlsx(group.action, file="tables & figures/Chapter 5 - G20 record on protectionism/Figure 5-3 - Intra-G20 group discriminatory interventions by year.xlsx", row.names=F)
# 
# 
# #figure83 <- read.xlsx(file="tables & figures/Chapter 5 - G20 record on protectionism/Figure 5-3 - Intra-G20 group discriminatory interventions by year.xlsx",1)
# #figure83 <- as.data.frame(figure83)
# 
# 
# 
# figure83 <- group.action
# figure83.x.value=figure83[,c("group","variable","frequency")]
# figure83.x.value=reshape(figure83.x.value, idvar="group",timevar="variable", direction="wide")
# names(figure83.x.value)=c("Country group", 2008:2018)
# write.xlsx(figure83.x.value, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".3 - Data for Figure 5.3.xlsx", sep=""), row.names=F, sheetName = "Intervention counts")
# 
# figure83.x.share=figure83[,c("group","variable","value")]
# figure83.x.share=reshape(figure83.x.share, idvar="group",timevar="variable", direction="wide")
# names(figure83.x.share)=c("Country group", 2008:2018)
# write.xlsx(figure83.x.share, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".3 - Data for Figure 5.3.xlsx", sep=""), row.names=F, sheetName = "Intervention shares", append = T)
# 
# 
# 
# # cairo_ps("tables & figures/Chapter 5 - G20 record on protectionism/Figure 5.3.eps", bg = gta_colour$panel.bg, width=11, height=11/1.8)
# 
# # Sort dataframe
# figure83.plot <- figure83
# figure83.plot$order[figure83.plot$group=="BRICS"] <- 2
# figure83.plot$order[figure83.plot$group=="Other G20"] <- 1
# figure83.plot$order[figure83.plot$group=="G7 & Australia"] <- 3
# 
# figure83.plot <- figure83.plot[with(figure83.plot, order(order)),]
# row.names(figure83.plot) <- NULL
# 
# library(plyr)
# figure83.plot <- ddply(figure83.plot, .(variable),
#                   transform, pos = cumsum(value) - (0.5 * value))
# figure83.plot$pos <- 1 - figure83.plot$pos
# 
# p1 <- ggplot(figure83.plot, aes(x=variable, y=value, fill=forcats::fct_inorder(group))) +
#   geom_bar(width = 0.5, stat = "identity")  +
#   geom_text(aes(label = frequency, y= pos), size = 3, colour="#FFFFFF") +
#   gta_plot_wrapper(
#     y.right.enable = T,
#     y.left.name = "Percentage of all G20 discriminatory measures\nimplemented in a given year\n",
#     y.left.breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
#     y.left.labels = percent,
#     y.left.expand = c(0.01,0.01),
#     x.bottom.breaks = c(2008:2018),
#     fill.palette = c(gta_colour$qualitative[c(5,3,1)]),
#     fill.legend.col = 3,
#     fill.legend.title = "Country Group"
#   ) +
#   gta_theme()
# 
# p1
# 
# gta_plot_saver(plot = p1,
#                path = paste("tables & figures/",output.path,"/", sep=""),
#                name = paste("Figure ",chapter.number,".3", sep=""))
# 
