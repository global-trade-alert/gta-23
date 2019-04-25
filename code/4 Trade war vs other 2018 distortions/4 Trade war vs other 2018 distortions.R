library("xlsx")
library(gtalibrary)
rm(list = ls())


# font_import()
loadfonts(device="postscript")
loadfonts(device="win")
#
# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

#Settings
chapter.number=4
chapter.name="Trade war vs other 2018 distortions"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

# F1: For each year 2009-2018, 
# (i) total value of trade affected by US and Chinese tariff hikes (including trade defence actions) on each other only imposed in a given year, 
# (ii) total value of trade affected by world's tariff hikes (including trade defence actions) harming only one other party imposed in a given year, 
# (iii) total value of trade affected by world's tariff hikes (including trade defence actions) harming any number of trading partners and 
# (iv) total value of world imports harmed by any import distortion affecting any number of trading partners.

figure.31=data.frame(year=numeric(),
                     i=numeric(),
                     ii=numeric(),
                     iii=numeric(),
                     iv=numeric())

tariffs.et.al=as.character(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"])

for(yr in c(2009:2018)){
  yr.start=paste(yr, "-01-01",sep="")
  yr.end=paste(yr, "-12-31",sep="")
  
  f31=data.frame(year=yr,
                 i=NA,
                 ii=NA,
                 iii=NA,
                 iv=NA)
  
  ## i
  # (i) total value of trade affected by US and Chinese tariff hikes (including trade defence actions) on each other only imposed in a given year, 
  
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                  affected.flows = "inward",
                  coverage.period = c(yr, yr),
                  implementation.period = c(yr.start, yr.end),
                  importers = c("China","United States of America"),
                  keep.importers = T,
                  exporters = c("China","United States of America"),
                  keep.exporters = T,
                  nr.also.exporters=0,
                  intervention.types = tariffs.et.al,
                  keep.type = T,
                  group.type = T,
                  trade.statistic = "value",
                  # trade.data = "before implementation",
                  trade.data = "prior year",
                  intra.year.duration = F
  )
  # f31$i=trade.coverage.estimates[,3]
  f31$i=trade.coverage.estimates[,4]
  
  # (ii) total value of trade affected by world's tariff hikes (including trade defence actions) harming only one other party imposed in a given year, 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     nr.also.exporters=1,
                     intervention.types = tariffs.et.al,
                     keep.type = T,
                     group.type = T,
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = "prior year",
                     intra.year.duration = F
  )
  # f31$ii=trade.coverage.estimates[,3]
  f31$ii=trade.coverage.estimates[,4]
  
  
  # (iii) total value of trade affected by world's tariff hikes (including trade defence actions) harming any number of trading partners and 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     intervention.types = tariffs.et.al,
                     keep.type = T,
                     group.type = T,
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = "prior year",
                     intra.year.duration = F
  )
  # f31$iii=trade.coverage.estimates[,3]
  f31$iii=trade.coverage.estimates[,4]
  
  
  # (iv) total value of world imports harmed by any import distortion affecting any number of trading partners.
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = "prior year",
                     intra.year.duration = F
  )
  # f31$iv=trade.coverage.estimates[,3]
  f31$iv=trade.coverage.estimates[,4]
  
  
  
  figure.31=rbind(figure.31, f31)
  print(yr)
}



figure.31.plot <- figure.31

figure.31.plot$bilateral.tariff.hikes <- round(100*(figure.31.plot$i/figure.31.plot[nrow(figure.31.plot),c("i")]))
figure.31.plot$all.tariff.hikes.single.nation <- round(100*(figure.31.plot$ii/figure.31.plot[nrow(figure.31.plot),c("i")]))
figure.31.plot$all.tariff.hikes <- round(100*(figure.31.plot$iii/figure.31.plot[nrow(figure.31.plot),c("i")]))
figure.31.plot$all.import.restrictions <- round(100*(figure.31.plot$iv/figure.31.plot[nrow(figure.31.plot),c("i")]))

figure.31.xlsx=figure.31

figure.31.xlsx <- cbind(figure.31.xlsx, figure.31.plot[,c("bilateral.tariff.hikes","all.tariff.hikes.single.nation","all.tariff.hikes","all.import.restrictions")])

names(figure.31.xlsx)=c("Year", "Total trade affected by US/CHN tariff hikes (incl. TD) targeted at each other", 
                        "Total trade by affected by worldwide tariff hikes (incl.TD) targeted at a single nation",
                        "Total trade by affected by worldwide tariff hikes (incl.TD)",
                        "Total trade by affected by all import barriers worldwide", "US and China bilateral tariff hikes",
                        "All tariff hikes hurting a single nation",
                        "All tariff hikes",
                        "All import distortions")
write.xlsx(figure.31.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Table ",chapter.number,".1 - Data for Figure ",chapter.number,".1.xlsx", sep=""), row.names=F)



figure.31.plot <- gather(figure.31.plot,type, value, 6:9)

# Plot
plot3.1 <- ggplot()+
  geom_line(data=figure.31.plot, aes(x=year, y=value, colour=forcats::fct_inorder(type)), size=1)+
  geom_text(data=subset(figure.31.plot, type %in% c("all.tariff.hikes","all.import.restrictions")), aes(x=year, y=value, label=value), size=2.5, nudge_y = -0.1)+
  labs(x="Year",y="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)
")+
  scale_color_manual(labels=c("US and China bilateral tariff hikes",
                                "All tariff hikes hurting a single nation",
                                "All tariff hikes",
                                "All import distortions"),
                       values=gta_colour$qualitative)+
  scale_y_continuous(trans="log10", sec.axis = sec_axis(~., name="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)", breaks=c(1,10,100,1000)))+
  scale_x_continuous(breaks=seq(2009,2018,1))+
  guides(colour = guide_legend(title=NULL,ncol = 2))+
  gta_theme()
  
plot3.1

gta_plot_saver(plot=plot3.1,
             path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
             name=paste("Figure ",chapter.number,".1", sep=""))

# F2: For each year 2009-2018, 
# (i) total value of world imports affected by harmful intervention that affects one trading partner, 
# (ii) total value of world imports affected by harmful import distortions that affects any number of trading partners, and 
# (iii) total value of world exports affected by harmful export distortions. 


figure.32=data.frame(year=numeric(),
                     i=numeric(),
                     ii=numeric(),
                     iii=numeric())

for(yr in c(2009:2018)){
  yr.start=paste(yr, "-01-01",sep="")
  yr.end=paste(yr, "-12-31",sep="")
  
  f32=data.frame(year=yr,
                 i=NA,
                 ii=NA,
                 iii=NA)
  
  ## i
  # (i) total value of world imports affected by harmful intervention that affects one trading partner, 
  
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     nr.also.exporters = 1,
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = "prior year",
                     intra.year.duration = F
  )
  # f32$i=trade.coverage.estimates[,3]
  f32$i=trade.coverage.estimates[,4]
  
  # (ii) total value of world imports affected by harmful import distortions that affects any number of trading partners, and 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = "prior year",
                     intra.year.duration = F
  )
  # f32$ii=trade.coverage.estimates[,3]
  f32$ii=trade.coverage.estimates[,4]
  
  # (iii) total value of world exports affected by harmful export distortions. 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "outward subsidy",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = "prior year",
                     intra.year.duration = F
  )
  # f32$iii=trade.coverage.estimates[,3]
  f32$iii=trade.coverage.estimates[,4]
  
  figure.32=rbind(figure.32, f32)
  f32
  rm(f32)
  print(yr)
  
}

figure.32.0 <- figure.32
figure.32 <- figure.32.0

figure.32$iv <- figure.31$i
figure.32 <- figure.32[,c(1,5,2:4)]
names(figure.32) <- c("year", "i","ii","iii","iv")
figure.32.plot <- figure.32

figure.32.plot$bilateral.tariff.hikes <- 100*(figure.32.plot$i/figure.32.plot[nrow(figure.32.plot),c("i")])
figure.32.plot$all.tariff.hikes.single.nation <- 100*(figure.32.plot$ii/figure.32.plot[nrow(figure.32.plot),c("i")])
figure.32.plot$all.import.restrictions <- 100*(figure.32.plot$iii/figure.32.plot[nrow(figure.32.plot),c("i")])
figure.32.plot$all.export.incentives <- 100*(figure.32.plot$iv/figure.32.plot[nrow(figure.32.plot),c("i")])


figure.32.xlsx=figure.32

figure.32.xlsx <- cbind(figure.32.xlsx, figure.32.plot[,c("bilateral.tariff.hikes","all.tariff.hikes.single.nation","all.import.restrictions","all.export.incentives")])


names(figure.32.xlsx)=c("Year", 
                        "US-China tariff only",
                        "Total value of world imports affected by harmful import barriers that affects one trading partner",
                        "Total value of world imports affected by harmful import barriers that affect any number of trading partners",
                        "Total value of world exports affected by export incentives of competing exporters"
                        )
write.xlsx(figure.32.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Table ",chapter.number,".2 - Data for Figure ",chapter.number,".2.xlsx", sep=""), row.names=F)




figure.32.plot <- gather(figure.32.plot,type, value, 6:9)

# Plot
plot3.2 <- ggplot()+
  geom_line(data=figure.32.plot, aes(x=year, y=value, colour=forcats::fct_inorder(type)), size=1)+
  geom_text(data=subset(figure.32.plot, type %in% c("all.export.incentives")), aes(x=year, y=value, label=round(value,0)), size=2.5, nudge_y = 0.15)+
  geom_text(data=subset(figure.32.plot, type %in% c("all.import.restrictions")), aes(x=year, y=value, label=round(value,0)), size=2.5, nudge_y = -0.15)+
  labs(x="Year",y="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)
       ")+
  scale_color_manual(labels=c("US and China bilateral tariff hikes",
                              "All tariff hikes hurting a single nation",
                              "All import distortions",
                              "All exports incentives"),
                     values=gta_colour$qualitative)+
  scale_y_continuous(trans="log10",limits = c(0.5,10000),breaks=c(1,10,100,1000,10000), sec.axis = sec_axis(~., name="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)", breaks=c(1,10,100,1000,10000)))+
  scale_x_continuous(breaks=seq(2009,2018,1))+
  guides(colour = guide_legend(title=NULL,ncol = 2))+
  gta_theme()

plot3.2

gta_plot_saver(plot=plot3.2,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".2", sep=""))


