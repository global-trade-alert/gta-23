library(xlsx)
library(gtalibrary)
library(tidyverse)
library(stringr)
rm(list = ls())


# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

setwd("GTA cloud")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA 21")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

#Settings
chapter.number=7
chapter.name="Look who's bashing"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

chapter.country=c("United States of America","China")
country.short=c("US","China")

instruments=gtalibrary::int.mast.types
instruments=as.character(subset(instruments, !mast.chapter.id %in% c("CAP", "FDI", "MIG"))$intervention.type)

# Figure 1: Number & trade share for import barriers only targetting X
gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                   affected.flows = c("inward", "outward subsidy"),
                   intervention.types = instruments,
                   keep.type = T,
                   coverage.period = c(2009, 2018),
                   exporters = chapter.country,
                   keep.exporters = T,
                   group.exporters = F,
                   nr.also.exporters=0,
                   trade.statistic = "share",
                   intra.year.duration = T
)

gta_data_slicer(gta.evaluation = c("Red", "Amber"),
                affected.flows =  c("inward", "outward subsidy"),
                intervention.types = instruments,
                keep.type = T,
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = F,
                affected.country = chapter.country,
                keep.affected = T,
                affected.also.nr = 0)

gta_intervention_duration(data.path='master.sliced[,c("intervention.id", "date.implemented", "date.removed")]',
                          is.data.frame=T)

intervention.duration=merge(intervention.duration, unique(master.sliced[,c("intervention.id", "affected.jurisdiction")]), by="intervention.id", all.x=T)

figure.61=aggregate(intervention.id ~ affected.jurisdiction + year,subset(intervention.duration, share>0) , function(x) length(unique(x)))
names(figure.61)=c("affected.jurisdiction","year","cumulative.count")

figure.61.shares=trade.coverage.estimates


figure.61.xlsx=figure.61
figure.61.xlsx=reshape(figure.61.xlsx, idvar="affected.jurisdiction",timevar = "year", direction="wide")
names(figure.61.xlsx)=gsub("cumulative.count.","",names(figure.61.xlsx))
figure.61.xlsx[is.na(figure.61.xlsx)]=0

write.xlsx(figure.61.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".1.xlsx", sep=""), row.names=F, sheetName = "Cumulative counts")
write.xlsx(figure.61.shares, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".1.xlsx", sep=""), row.names=F, sheetName = "Trade shares", append=T)



#FIGURE 61

figure.61.shares.plot = gather(figure.61.shares, year, value, 3:12)
figure.61.shares.plot$year <- as.numeric(rep(seq(2009,2018,1), each=2))

figure.61.shares.plot$`Importing country` <- NULL
names(figure.61.shares.plot) <- c("affected.jurisdiction","year","value")
figure.61.shares.plot$type <- "share"
figure.61.plot = figure.61
names(figure.61.plot) <- c("affected.jurisdiction","year","value")
figure.61.plot$type = "acts"
figure.61.plot <- rbind(figure.61.plot, figure.61.shares.plot)


plot6.1a <- ggplot()+
  geom_line(data=subset(figure.61.plot, affected.jurisdiction == "China" & type=="share"), aes(x=year, y=value*600/0.3, colour=type), size=1)+
  geom_line(data=subset(figure.61.plot, affected.jurisdiction == "China" & type == "acts"), aes(x=year, y=value, colour=type), size=1)+
  gta_plot_wrapper(
    y.right.enable = T,
    y.left.name="Number of foreign acts",
    y.right.name = "Share of Chinese exports affected",
    y.left.breaks = seq(0,600,100),
    y.right.transform = (1/600)*0.3,
    y.right.breaks = seq(0,1,0.05),
    x.bottom.breaks = seq(2008,2018,1),
    colour.legend.title = NULL,
    colour.palette = gta_colour$qualitative[c(1,3)],
    colour.labels = c("Number of foreign interventions targeting only China in effect (left hand side)","Share of Chinese exports facing interventions targeting only China (right hand side)")
  )+
  gta_theme(base.size = 18)+
  theme(axis.text.x.bottom = element_text(size=14),
        axis.text.y.left = element_text(size=14),
        axis.text.y.right = element_text(size=14))

plot6.1a

gta_plot_saver(plot=plot6.1a,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".1a", sep=""))


plot6.1b <- ggplot()+
  geom_line(data=subset(figure.61.plot, affected.jurisdiction == "United States of America" & type=="share"), aes(x=year, y=value*80/0.06, colour=type), size=1)+
  geom_line(data=subset(figure.61.plot, affected.jurisdiction == "United States of America" & type == "acts"), aes(x=year, y=value, colour=type), size=1)+
  gta_plot_wrapper(
    y.right.enable = T,
    y.left.name="Number of foreign acts",
    y.right.name = "Share of US exports affected",
    y.left.breaks = seq(0,80,10),
    y.right.transform = (1/80)*0.06,
    y.right.breaks = seq(0,1,0.01),
    x.bottom.breaks = seq(2008,2018,1),
    colour.legend.title = NULL,
    colour.palette = gta_colour$qualitative[c(1,3)],
    colour.labels = c("Number of foreign interventions targeting only US in effect (left hand side)","Share of US exports facing interventions targeting only China (right hand side)")
  )+
  gta_theme(base.size = 18)+
  theme(axis.text.x.bottom = element_text(size=14),
        axis.text.y.left = element_text(size=14),
        axis.text.y.right = element_text(size=14))

plot6.1b

gta_plot_saver(plot=plot6.1b,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".1b", sep=""))



# Figure 2: intervention type composition
# CHN
figure.62=as.data.frame(table(unique(subset(master.sliced, affected.jurisdiction=="China")[,c("intervention.id","intervention.type")])$intervention.type))
names(figure.62)=c("intervention.type","intervention.count")
figure.62=figure.62[order(-figure.62$intervention.count),]
figure.62=rbind(figure.62, data.frame(intervention.type="Other", intervention.count=sum(figure.62$intervention.count[6:nrow(figure.62)])))
figure.62=figure.62[c(1:5, nrow(figure.62)),]

figure.62.xlsx=figure.62
names(figure.62.xlsx)=c("Intervention type", "Number targeted at China")
write.xlsx(figure.62.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".2.xlsx", sep=""), row.names=F, sheetName = "China")

# USA
figure.62.us=as.data.frame(table(unique(subset(master.sliced, affected.jurisdiction=="United States of America")[,c("intervention.id","intervention.type")])$intervention.type))
names(figure.62.us)=c("intervention.type","intervention.count")
figure.62.us=figure.62.us[order(-figure.62.us$intervention.count),]
figure.62.us=rbind(figure.62.us, data.frame(intervention.type="Other", intervention.count=sum(figure.62.us$intervention.count[6:nrow(figure.62.us)])))
figure.62.us=figure.62.us[c(1:5, nrow(figure.62.us)),]

figure.62.us.xlsx=figure.62.us
names(figure.62.us.xlsx)=c("Intervention type", "Number targeted at USA")
write.xlsx(figure.62.us.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".2.xlsx", sep=""), row.names=F, sheetName = "USA", append=T)

row.names(figure.62) <- NULL


plot6.2a <- ggplot()+
  geom_bar(data=figure.62, aes(x="", y=intervention.count, fill=forcats::fct_inorder(factor(intervention.type))), stat="identity")+
  coord_polar("y", start=0)+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels = NULL)+
  scale_fill_manual(values=gta_colour$qualitative, labels=figure.62$intervention.type)+
  guides(fill = guide_legend(ncol=2, title=NULL))+
  gta_theme()+
  theme(panel.background = element_blank(),
        line = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot6.2a


gta_plot_saver(plot=plot6.2a,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".2a", sep=""))


plot6.2b <- ggplot()+
  geom_bar(data=figure.62.us, aes(x="", y=intervention.count, fill=forcats::fct_inorder(factor(intervention.type))), stat="identity")+
  coord_polar("y", start=0)+
  labs(x=NULL, y=NULL)+
  scale_y_continuous(labels=NULL)+
  scale_fill_manual(values=gta_colour$qualitative[c(1:4,8,6)],labels=figure.62.us$intervention.type)+
  guides(fill = guide_legend(ncol=2, title=NULL))+
  gta_theme()+
  theme(panel.background = element_blank(),
        line = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot6.2b


gta_plot_saver(plot=plot6.2b,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".2b", sep=""))



# Figure 3: trade shares
gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                   affected.flows =  c("inward", "outward subsidy"),
                   intervention.types = instruments,
                   keep.type = T,
                   coverage.period = c(2009, 2018),
                   exporters = chapter.country,
                   keep.exporters = T,
                   group.exporters = F,
                   trade.statistic = "share",
                   intra.year.duration = T
)

figure.63=trade.coverage.estimates

figure.63.xlsx=figure.63[,c(2:ncol(figure.63))]
names(figure.63.xlsx)=c("Affected exporter", 2009:2018)
write.xlsx(figure.63.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".3.xlsx", sep=""), row.names=F)



figure.63.plot <- gather(figure.63, year, value, 3:12)
figure.63.plot$year <- rep(seq(2009,2018),each=2)
figure.63.plot$label <- as.character(round(figure.63.plot$value,3))
figure.63.plot$label <- as.factor(str_pad(figure.63.plot$label, width=5, side="right", pad="0"))


plot6.3 <- ggplot()+
  geom_line(data=figure.63.plot, aes(x=year, y=value, colour=`Exporting country`), size=1)+
  geom_text(data=subset(figure.63.plot, `Exporting country` == "China"), aes(x=year, y=value, label=label), size=3, nudge_y = -0.05, nudge_x = 0.1)+
  geom_text(data=subset(figure.63.plot, `Exporting country` == "United States of America"), aes(x=year, y=value, label=label), size=3, nudge_y = +0.05, nudge_x = -0.1)+
  gta_plot_wrapper(
    y.right.enable = T,
    y.left.name="Share of exports affected",
    y.left.breaks=seq(0,1,0.1),
    y.left.limits = c(0,1),
    x.bottom.name="Year",
    x.bottom.breaks = seq(2009,2018,1),
    colour.palette = gta_colour$qualitative[c(1,3)],
    colour.legend.title = NULL,
    colour.legend.col = 2
  )+
  gta_theme()

plot6.3



gta_plot_saver(plot=plot6.3,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".3", sep=""))


# Figure 4: Fall in export shares given someone stops
partners=c("Argentina", "Australia", "Canada", "Indonesia", "Mexico", "Saudi Arabia", "Republic of Korea","Turkey","South Africa","Russia", "Japan", "Brazil", "India", "EU-28", "United States of America", "China")
partners=partners[order(partners)]


# CHN
figure.64=data.frame(removed.partner=c("Coverage estimate for 2018", partners),
                           share.2018=NA)

base.coverage=figure.63.xlsx[figure.63$`Exporting country`=="China",ncol(figure.63.xlsx)]

figure.64$share.2018[figure.64$removed.partner=="Coverage estimate for 2018"]=base.coverage
  
for(cty in partners){
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = c("inward", "outward subsidy"),
                     intervention.types = instruments,
                     keep.type = T,
                     coverage.period = c(2018,2018),
                     implementers = cty,
                     keep.implementer = F,
                     exporters = "China",
                     keep.exporters = T,
                     trade.statistic = "share",
                     intra.year.duration = T
  )
  figure.64$share.2018[figure.64$removed.partner==cty]=trade.coverage.estimates[,3]-base.coverage
  print(cty)
}

figure.64.xlsx=figure.64
names(figure.64.xlsx)=c("Removed implementing jurisdiction", "Change in trade coverage estimate")
write.xlsx(figure.64.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".4.xlsx", sep=""), row.names=F, sheetName = "China")


# USA
# CHN
figure.64.us=data.frame(removed.partner=c("Coverage estimate for 2018", partners),
                        share.2018=NA)

base.coverage=figure.63.xlsx[figure.63$`Exporting country`=="United States of America",ncol(figure.63.xlsx)]

figure.64.us$share.2018[figure.64.us$removed.partner=="Coverage estimate for 2018"]=base.coverage

for(cty in partners){
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = c("inward", "outward subsidy"),
                     intervention.types = instruments,
                     keep.type = T,
                     coverage.period = c(2018,2018),
                     implementers = cty,
                     keep.implementer = F,
                     exporters = "United States of America",
                     keep.exporters = T,
                     trade.statistic = "share",
                     intra.year.duration = T
  )
  figure.64.us$share.2018[figure.64.us$removed.partner==cty]=trade.coverage.estimates[,3]-base.coverage
  print(cty)
}

figure.64.us.xlsx=figure.64.us
names(figure.64.us.xlsx)=c("Removed implementing jurisdiction", "Change in trade coverage estimate")
write.xlsx(figure.64.us.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Data for Figure ",chapter.number,".4.xlsx", sep=""), row.names=F, sheetName = "USA", append=T)

figure.64.plot.china <- read.xlsx("0 report production/GTA 23/tables & figures/7 Look who's bashing/Data for Figure 7.4.xlsx",sheetIndex = 1)
figure.64.plot.us <- read.xlsx("0 report production/GTA 23/tables & figures/7 Look who's bashing/Data for Figure 7.4.xlsx",sheetIndex = 2)

figure.64.plot.china$type="China's exports"
figure.64.plot.us$type="USA's exports"

figure.64.plot <- rbind(figure.64.plot.china, figure.64.plot.us)

figure.64.plot <- subset(figure.64.plot, (type== "China's exports" & Removed.implementing.jurisdiction %in% c("EU-28","India","Brazil","Indonesia","United States of America")) | (type== "USA's exports" & Removed.implementing.jurisdiction %in% c("EU-28","India","Brazil","China","Canada","Indonesia")))

# This will handle the ordering
figure.64.plot <- figure.64.plot %>% 
  ungroup() %>%   
  arrange(type, Change.in.trade.coverage.estimate) %>%   
  mutate(.r = row_number()) 

figure.64.plot$Removed.implementing.jurisdiction=as.character(figure.64.plot$Removed.implementing.jurisdiction)
figure.64.plot$Removed.implementing.jurisdiction[figure.64.plot$Removed.implementing.jurisdiction=="United States of America"]="United States"


plot6.4 <- ggplot(subset(figure.64.plot, Removed.implementing.jurisdiction != "Coverage estimate for 2018"), aes(.r, Change.in.trade.coverage.estimate)) +  
  geom_bar(stat="identity", fill=gta_colour$blue[1]) +
  geom_text(data=figure.64.plot, aes(x=.r, y=0.005, label=round(Change.in.trade.coverage.estimate,3)), size=3)+
  scale_y_continuous(sec.axis = sec_axis(~., name="Reduction in export share affected by \nharmful measures if trading partner \neliminates all their discrimination"))+
  facet_wrap(~ type, scales = "free") +  
  scale_x_continuous(  
    breaks = figure.64.plot$.r,     
    labels = figure.64.plot$Removed.implementing.jurisdiction
  )+
  labs(x=NULL, y="Reduction in export share affected by \nharmful measures if trading partner \neliminates all their discrimination")+
  gta_theme(x.bottom.angle = 45) + 
  theme(axis.text.x.bottom = element_text(hjust=1))


plot6.4

gta_plot_saver(plot=plot6.4,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".4", sep=""))


  # Table 1: Share of US exports facing import barriers
gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                   affected.flows = c("inward"),
                   intervention.types = instruments,
                   keep.type = T,
                   coverage.period = c(2018, 2018),
                   importers=g20.members,
                   keep.importers = T,
                   group.importers = F,
                   implementers = g20.members,
                   keep.implementer = T,
                   exporters = chapter.country,
                   keep.exporters = T,
                   group.exporters = F,
                   trade.statistic = "share",
                   trade.data = "2017",
                   intra.year.duration = T
)

table.61=trade.coverage.estimates
write.xlsx(table.61, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Table ",chapter.number,".1.xlsx", sep=""), row.names=F)



