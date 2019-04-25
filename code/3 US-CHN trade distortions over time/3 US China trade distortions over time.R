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

setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA 21")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/")

## For replication
master.path="0 report production/GTA 23/0 GTA data used in final report/master_plus.Rdata"
replica.path="0 report production/GTA 23/0 GTA data used in final report/database replica - parts - base.Rdata"

#Settings
chapter.number=3
chapter.name="US-CHN trade distortions over time"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

# trade data
load("data/support tables/Final goods support table.Rdata")
trade=subset(final, Year>=2008)[,c("Reporter.un","Partner.un","Year", "Tariff.line", "Value")]
rm(final)
names(trade)=c("i.un","a.un","year","affected.product","trade.value")




# T1: US tariff measures (including trade defence) taken against China. And comparable measures taken against USA.

# T2: US safeguard duties on steel and aluminium and retaliatory actions taken by everyone else.

# F1: Stacked bar chart for annual average for 2013-16, annual 2017 and annual 2018 of Chinese exports to the USA affected by new harmful US measures introduced each year that
## Label 2013-16 as Obama II (2013-6). Bottom two entries in each stack should reveal extent (upper bound) of bilateral targeting to China.

figure2.1=data.frame(implementer=character(),
                     affected=character(),
                     year=character(),
                     i=character(),
                     ii=character(),
                     iii=character(),
                     iv=character()
)

for(yr in 2013:2018){
  # prep
  f21=data.frame(implementer=c("United States of America", "China"),
                 affected=c("China","United States of America"),
                 year=yr,
                 i=NA,
                 ii=NA,
                 iii=NA,
                 iv=NA
  )
  
  
  
  yr.start=paste(yr, "-01-01",sep="")
  yr.end=paste(yr, "-12-31",sep="")
  
  trade.yr=subset(trade, year==(yr-1) & a.un %in% c(156,840) & i.un %in% c(156,840))
  trade.yr$year=NULL
  names(trade.yr)=c("i.un","a.un","affected.product","trade.value")
  
  for(i in 1:2){
    
    # (i) were tariffs or trade defence that only affected China,
    gta_data_slicer(data.path=master.path,
                    gta.evaluation = c("Red", "Amber"),
                    affected.flows = "inward",
                    implementation.period = c(yr.start, yr.end),
                    implementing.country = as.character(f21$implementer[i]),
                    keep.implementer = T,
                    affected.country = as.character(f21$affected[i]),
                    keep.affected = T,
                    keep.others=F,
                    affected.also.nr = 0,
                    intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                    keep.type = T,
                    keep.implementation.na=F
    )
    
    f21.i=master.sliced
    rm(master.sliced)
    
    if(nrow(f21.i)>0){
      f21.i=cSplit(f21.i, which(names(f21.i)=="affected.product"), direction="long", sep=",")
      f21.i=merge(f21.i, trade.yr, by=c("i.un","a.un","affected.product"), all.x=T)
      f21.i$trade.value[is.na(f21.i$trade.value)]=0
      
      f21$i[i]=sum(unique(f21.i[,c("affected.product","trade.value")])$trade.value)
    }
    
    # (ii) were other harmful US policies that only affected China,
    gta_data_slicer(data.path=master.path,
                    gta.evaluation = c("Red", "Amber"),
                    affected.flows = "inward",
                    implementation.period = c(yr.start, yr.end),
                    implementing.country = as.character(f21$implementer[i]),
                    keep.implementer = T,
                    affected.country = as.character(f21$affected[i]),
                    keep.affected = T,
                    keep.others=F,
                    affected.also.nr = 0,
                    intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                    keep.type = F,
                    keep.implementation.na=F
    )
    f21.ii=master.sliced
    rm(master.sliced)
    
    if(nrow(f21.ii)>0){
      f21.ii=cSplit(f21.ii, which(names(f21.ii)=="affected.product"), direction="long", sep=",")
      f21.ii=merge(f21.ii, trade.yr, by=c("i.un","a.un","affected.product"), all.x=T)
      f21.ii$trade.value[is.na(f21.ii$trade.value)]=0
      
      f21$ii[i]=sum(unique(f21.ii[,c("affected.product","trade.value")])$trade.value)
      
    }
    
    
    # (iii) were tariffs or trade defence that affected China and other US trading partners,
    gta_data_slicer(data.path=master.path,
                    gta.evaluation = c("Red", "Amber"),
                    affected.flows = "inward",
                    implementation.period = c(yr.start, yr.end),
                    implementing.country = as.character(f21$implementer[i]),
                    keep.implementer = T,
                    affected.country = as.character(f21$affected[i]),
                    keep.affected = T,
                    keep.others=T,
                    intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                    keep.type = T,
                    keep.implementation.na=F
    )
    f21.iii=master.sliced
    f21.iii=subset(f21.iii, !intervention.id %in% f21.i$intervention.id)
    rm(master.sliced)
    
    if(nrow(f21.iii)>0){
      f21.iii=cSplit(f21.iii, which(names(f21.iii)=="affected.product"), direction="long", sep=",")
      f21.iii=merge(f21.iii, trade.yr, by=c("i.un","a.un","affected.product"), all.x=T)
      f21.iii$trade.value[is.na(f21.iii$trade.value)]=0
      
      f21$iii[i]=sum(unique(f21.iii[,c("affected.product","trade.value")])$trade.value)
    }
    
    # (iv) were other US policies that affected China and other US trading partners.
    gta_data_slicer(data.path=master.path,
                    gta.evaluation = c("Red", "Amber"),
                    affected.flows = "inward",
                    implementation.period = c(yr.start, yr.end),
                    implementing.country = as.character(f21$implementer[i]),
                    keep.implementer = T,
                    affected.country = as.character(f21$affected[i]),
                    keep.affected = T,
                    keep.others=T,
                    intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                    keep.type = F,
                    keep.implementation.na=F
    )
    f21.iv=master.sliced
    f21.iv=subset(f21.iv, !intervention.id %in% f21.ii$intervention.id)
    rm(master.sliced)
    
    if(nrow(f21.iv)>0){
      f21.iv=cSplit(f21.iv, which(names(f21.iv)=="affected.product"), direction="long", sep=",")
      f21.iv=merge(f21.iv, trade.yr, by=c("i.un","a.un","affected.product"), all.x=T)
      f21.iv$trade.value[is.na(f21.iv$trade.value)]=0
      
      f21$iv[i]=sum(unique(f21.iv[,c("affected.product","trade.value")])$trade.value)
      print(i)
    }
    
  }
  f21[is.na(f21)]=0
  figure2.1=rbind(figure2.1, f21)
  
  print(yr)
}

f1316=merge(merge(aggregate(i ~ implementer + affected, subset(figure2.1, year<=2016),mean),
                  aggregate(ii ~ implementer + affected, subset(figure2.1, year<=2016),mean), by=c("implementer", "affected"), all.x=T),
            merge(aggregate(iii ~ implementer + affected, subset(figure2.1, year<=2016),mean),
                  aggregate(iv ~ implementer + affected, subset(figure2.1, year<=2016),mean), by=c("implementer", "affected"), all.x=T),
            by=c("implementer", "affected"), all.x=T)
f1316$year="2013-16"

figure2.1=rbind(f1316, subset(figure2.1, year>2016))

figure2.1.xlsx=subset(figure2.1, implementer!="China")[,c(1,2,7,3:6)]

figure2.1.xlsx$v <- rowSums(figure2.1.xlsx[,c("i","ii","iii","iv")], na.rm = TRUE)

figure2.1.xlsx=figure2.1.xlsx[order(figure2.1.xlsx$implementer),]
names(figure2.1.xlsx)=c("Implementer","Affected country", "Period","Targeted tariffs or trade defence", "Targeted non-tariff/trade defence",
                        "Untargeted tariffs or trade defence", "Untargeted non-tariff/trade defence","All US policies harming Chinese exports")

write.xlsx(figure2.1.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Figure ",chapter.number,".1 - Data for Figure ",chapter.number,".1.xlsx", sep=""), row.names=F)


# plot here

figure2.1.plot <- gather(figure2.1.xlsx,type, value, c(4:8))

plot2.1 <- ggplot()+
  geom_bar(data=figure2.1.plot, aes(x=forcats::fct_inorder(type), y=value/1000000000, fill=Period), stat = "identity", position=position_dodge(0.8), width=0.7) +
  scale_fill_manual(labels=c("Obama II", "2017","2018"),values=c(gta_colour$qualitative[c(1,3,5)])) +
  scale_y_continuous(breaks=seq(0,400,50), sec.axis = sec_axis(~., name = "Billions of USD of Chinese exports affected",breaks=seq(0,400,50)))+
  scale_x_discrete(labels=c("Tariff increases \ntargeting \nChina",
                            "Other US\ndistortions targeting\nChina",
                            "Tariff increases\naffecting but\nnot targeting\nChina",
                            "Other US distortions\naffecting but\nnot targeting\nChina",
                            "All US policies\n harming \nChinese exports"))+
  xlab("Intervention type")+
  ylab("Billions of USD of Chinese exports affected")+
  guides(fill=guide_legend(title="Period", ncol=3))+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(hjust = 0.5))+
  theme(axis.text.x.bottom = element_text(size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10))

plot2.1

# plot2.1 <- ggplot()+
#   geom_bar(data=figure2.1.plot, aes(x=Period, y=value/1000000000, fill=forcats::fct_rev(type)), stat = "identity") +
#   scale_fill_manual(values=c(gta_colour$blue[2:1],gta_colour$red[1:2])) +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Value of chinese exports to the \nUSA affected (in billions)"))+
#   scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
#   xlab("Year")+
#   ylab("Value of chinese exports to the \nUSA affected (in Billions)")+
#   guides(fill=guide_legend(title=NULL, ncol=2))+
#   gta_theme()

gta_plot_saver(plot=plot2.1,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".1", sep=""))



# F2: Like F1 but for US exports to China.
figure2.2.xlsx=subset(figure2.1, implementer=="China")[,c(1,2,7,3:6)]

figure2.2.xlsx$v <- rowSums(figure2.2.xlsx[,c("i","ii","iii","iv")], na.rm = TRUE)

figure2.2.xlsx=figure2.2.xlsx[order(figure2.2.xlsx$implementer),]
names(figure2.2.xlsx)=c("Implementer","Affected country", "Period","Targeted tariffs or trade defence", "Targeted non-tariff/trade defence",
                        "Untargeted tariffs or trade defence", "Untargeted non-tariff/trade defence","All Chinese policies harming US exports")

write.xlsx(figure2.2.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Figure ",chapter.number,".2 - Data for Figure ",chapter.number,".2.xlsx", sep=""), row.names=F)

# plot here

figure2.2.plot <- gather(figure2.2.xlsx,type, value, 4:8)

plot2.2 <- ggplot()+
  geom_bar(data=figure2.2.plot, aes(x=forcats::fct_inorder(type), y=value/1000000000, fill=Period), stat = "identity", position=position_dodge(0.8), width=0.7) +
  scale_fill_manual(labels=c("Obama II", "2017","2018"),values=c(gta_colour$qualitative[c(1,3,5)])) +
  scale_y_continuous(breaks=seq(0,400,25), sec.axis = sec_axis(~., name = "Billions of USD of US exports affected",breaks=seq(0,400,25)))+
  scale_x_discrete(labels=c("Tariff increases \ntargeting\nthe USA",
                            "Other Chinese\ndistortions targeting\nthe USA",
                            "Tariff increases\n affecting but\nnot targeting\nthe USA",
                            "Other Chinese\ndistortions affecting \nbut not targeting\nthe USA",
                            "All Chinese policies \nharming US exports"))+
  xlab("Intervention type")+
  ylab("Billions of USD of US exports affected")+
  guides(fill=guide_legend(title="Period", ncol=3))+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(hjust = 0.5))+
  theme(axis.text.x.bottom = element_text(size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10))

plot2.2

# plot2.2 <- ggplot()+
#   geom_bar(data=figure2.2.plot, aes(x=Period, y=value/1000000000, fill=forcats::fct_rev(type)), stat = "identity") +
#   scale_fill_manual(values=c(gta_colour$blue[2:1],gta_colour$red[1:2])) +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Value of US exports to \nChina affected (in billions)"))+
#   scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
#   xlab("Year")+
#   ylab("Value of US exports to \nChina affected (in Billions)")+
#   guides(fill=guide_legend(title=NULL, ncol=2))+
#   gta_theme()

gta_plot_saver(plot=plot2.2,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".2", sep=""))


# F3: Bar chart with total amount of Chinese exports to the USA affected by US harmful policy interventions of any type in force at end of Obama administration (19 Jan 2017), at end of 31 December 2017, and on 31 October 2018.
## As percentage of total 2016 US imports from China
trade.yr=subset(trade, year==2017)
trade.yr$year=NULL
names(trade.yr)=c("i.un","a.un","affected.product","trade.value")

figure2.3=data.frame(administration=c("Obama II","Trump 1st year", "Trump 2nd year"),
                     end.date=c("2017-01-19", "2017-12-31",cutoff),
                     nr.interventions=NA,
                     trade.value=NA,
                     trade.share=NA)

for(i in 1:nrow(figure2.3)){
  # (i) were tariffs or trade defence that only affected China,
  gta_data_slicer(data.path=master.path,
                  gta.evaluation = c("Red", "Amber"),
                  affected.flows = "inward",
                  implementation.period = c("2008-11-01", as.character(figure2.3$end.date[i])),
                  implementing.country = "United States of America",
                  keep.implementer = T,
                  affected.country = "China",
                  keep.affected = T,
                  keep.others=F,
                  keep.implementation.na=F
  )
  
  
  if(nrow(master.sliced)>0){
    master.sliced=cSplit(master.sliced, which(names(master.sliced)=="affected.product"), direction="long", sep=",")
    master.sliced=merge(master.sliced, trade.yr, by=c("i.un","a.un","affected.product"), all.x=T)
    master.sliced$trade.value[is.na(master.sliced$trade.value)]=0
    
    figure2.3$trade.value[i]=sum(unique(master.sliced[,c("affected.product","trade.value")])$trade.value)
    figure2.3$nr.interventions[i]=length(unique(master.sliced$intervention.id))
    figure2.3$trade.share[i]=figure2.3$trade.value[i]/sum(trade.yr$trade.value[trade.yr$i.un==840 & trade.yr$a.un==156])
  }
  rm(master.sliced)
}
figure2.3[is.na(figure2.3)]=0


figure2.3.xlsx=figure2.3
figure2.3.xlsx$imports.from.china = c(sum(unique(trade$trade.value[trade$i.un==840&trade$a.un==156&trade$year==2016])),
                                      sum(unique(trade$trade.value[trade$i.un==840&trade$a.un==156&trade$year==2017])),
                                      sum(unique(trade$trade.value[trade$i.un==840&trade$a.un==156&trade$year==2017])))
figure2.3.xlsx$share.of.total <- figure2.3.xlsx$trade.value/figure2.3.xlsx$imports.from.china

# Manually inserted trade values as per request from simon, trade values from USTR
USTR.values <- c(462.6,
                 505.5,
                 505.5)
figure2.3.xlsx$USTR.values <- USTR.values
figure2.3.xlsx$USTR.shares <- (figure2.3.xlsx$trade.value/1000000000)/figure2.3.xlsx$USTR.values

names(figure2.3.xlsx)=c("US administration","Cut-off date","Number of harmful interventions imposed affecting China",
                        "Value of 2016 imports on affected tariff lines", "Share of 2016 US imports from China on affected tariff lines","US Imports from China","Share of total Chinese exports to the USA","USTR trade values","USTR related share of total imports")


write.xlsx(figure2.3.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Figure ",chapter.number,".3 - Data for Figure ",chapter.number,".3.xlsx", sep=""), row.names=F)

# plot here

plot2.3 <- ggplot()+
  geom_bar(data=figure2.3, aes(x=administration, y=trade.value/1000000000),fill=gta_colour$blue[1], stat = "identity", width=0.6) +
  geom_line(data=figure2.3, aes(x=administration, y=trade.share*500, group=1),colour=gta_colour$brown[2],size=1)+
  geom_text(data=figure2.3, aes(x=administration, y=trade.share*500, label=round(trade.share, digits = 3)), nudge_y = -20, size=3.5, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,500,50), limits = c(0,500),sec.axis = sec_axis(~.*(1/500), name = "Share of Chinese exports affected"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
  xlab("Period")+
  ylab("Chinese exports affected (billion US Dollars)")+
  guides(fill=guide_legend(title=NULL, ncol=2))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(size=12),
        axis.title.y.left = element_text(size=12),
        axis.title.y.right = element_text(size=12))

plot2.3

gta_plot_saver(plot=plot2.3,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".3", sep=""))


# Separat plot per request from simon, with trade values manually inserted from USTR

plot2.3.a <- ggplot()+
  geom_bar(data=figure2.3.xlsx, aes(x=`US administration`, y=`Value of 2016 imports on affected tariff lines`/1000000000),fill=gta_colour$blue[1], stat = "identity", width=0.6) +
  geom_line(data=figure2.3.xlsx, aes(x=`US administration`, y=`USTR related share of total imports`*400, group=1),colour=gta_colour$brown[2],size=1)+
  geom_text(data=figure2.3.xlsx, aes(x=`US administration`, y=`USTR related share of total imports`*400, label=round(`USTR related share of total imports`, digits = 3)), nudge_y = -20, size=2, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,400,50),sec.axis = sec_axis(~.*(1/400), name = "Share of Chinese exports affected"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
  xlab("Period")+
  ylab("Billions of US Dollars")+
  guides(fill=guide_legend(title=NULL, ncol=2))+
  gta_theme()

plot2.3.a

# not used
# gta_plot_saver(plot=plot2.3.a,
#               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
#               name=paste("Figure ",chapter.number,".3a", sep=""))



# F4: Like F1 but for US exports to China.
figure2.4=data.frame(administration=c("Obama II","Trump 1st year", "Trump 2nd year"),
                     end.date=c("2017-01-19", "2017-12-31",cutoff),
                     nr.interventions=NA,
                     trade.value=NA,
                     trade.share=NA)

for(i in 1:nrow(figure2.4)){
  # (i) were tariffs or trade defence that only affected China,
  gta_data_slicer(data.path=master.path,
                  gta.evaluation = c("Red", "Amber"),
                  affected.flows = "inward",
                  implementation.period = c("2008-11-01", as.character(figure2.4$end.date[i])),
                  implementing.country = "China",
                  keep.implementer = T,
                  affected.country = "United States of America",
                  keep.affected = T,
                  keep.others=F,
                  keep.implementation.na=F
  )
  
  
  if(nrow(master.sliced)>0){
    master.sliced=cSplit(master.sliced, which(names(master.sliced)=="affected.product"), direction="long", sep=",")
    master.sliced=merge(master.sliced, trade.yr, by=c("i.un","a.un","affected.product"), all.x=T)
    master.sliced$trade.value[is.na(master.sliced$trade.value)]=0
    
    figure2.4$trade.value[i]=sum(unique(master.sliced[,c("affected.product","trade.value")])$trade.value)
    figure2.4$nr.interventions[i]=length(unique(master.sliced$intervention.id))
    figure2.4$trade.share[i]=figure2.4$trade.value[i]/sum(trade.yr$trade.value[trade.yr$i.un==156 & trade.yr$a.un==840])
  }
  rm(master.sliced)
}
figure2.4[is.na(figure2.4)]=0

figure2.4.xlsx=figure2.4

figure2.4.xlsx$imports.from.us = c(sum(unique(trade$trade.value[trade$i.un==156&trade$a.un==840&trade$year==2016])),
                                   sum(unique(trade$trade.value[trade$i.un==156&trade$a.un==840&trade$year==2017])),
                                   sum(unique(trade$trade.value[trade$i.un==156&trade$a.un==840&trade$year==2017])))
figure2.4.xlsx$share.of.total <- figure2.4.xlsx$trade.value/figure2.4.xlsx$imports.from.us

# Manually inserted trade values as per request from simon, trade values from USTR
USTR.values <- c(115.6,
                 129.9,
                 129.9)
figure2.4.xlsx$USTR.values <- USTR.values
figure2.4.xlsx$USTR.shares <- (figure2.4.xlsx$trade.value/1000000000)/figure2.4.xlsx$USTR.values

names(figure2.4.xlsx)=c("US administration","Cut-off date","Number of harmful interventions imposed by China and affecting USA",
                        "Value of 2016 US exorts to China on affected tariff lines", "Share of 2016 US exports to China on affected tariff lines","Total Chinese Imports from US","Share of total imports from US","USTR trade values","USTR related share of total imports")

write.xlsx(figure2.4.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Figure ",chapter.number,".4 - Data for Figure ",chapter.number,".4.xlsx", sep=""), row.names=F)

# plot here
plot2.4 <- ggplot()+
  geom_bar(data=figure2.4, aes(x=administration, y=trade.value/1000000000),fill=gta_colour$blue[1], stat = "identity", width=0.6) +
  geom_line(data=figure2.4, aes(x=administration, y=trade.share*150, group=1),colour=gta_colour$brown[2],size=1)+
  geom_text(data=figure2.4, aes(x=administration, y=trade.share*150, label=round(trade.share, digits = 3)), nudge_y = -20, size=3.5, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,150,25), limits = c(0,150),sec.axis = sec_axis(~.*(1/150), name = "Share of US exports affected"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
  xlab("Period")+
  ylab("US exports affected (billion US Dollars)")+
  guides(fill=guide_legend(title=NULL, ncol=2))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(size=12),
        axis.title.y.left = element_text(size=12),
        axis.title.y.right = element_text(size=12))


plot2.4

gta_plot_saver(plot=plot2.4,
               path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".4", sep=""))


# Separat plot per request from simon, with trade values manually inserted from USTR

plot2.4.a <- ggplot()+
  geom_bar(data=figure2.4.xlsx, aes(x=`US administration`, y=`Value of 2016 US exports to China on affected tariff lines`/1000000000),fill=gta_colour$blue[1], stat = "identity", width=0.6) +
  geom_line(data=figure2.4.xlsx, aes(x=`US administration`, y=`USTR related share of total imports`*125, group=1),colour=gta_colour$brown[2],size=1)+
  geom_text(data=figure2.4.xlsx, aes(x=`US administration`, y=`USTR related share of total imports`*125, label=round(`USTR related share of total imports`, digits = 3)), nudge_y = -13, size=2, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,400,25),sec.axis = sec_axis(~.*(1/125), name = "Share of Chinese exports affected"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
  xlab("Period")+
  ylab("Billions of US Dollars")+
  guides(fill=guide_legend(title=NULL, ncol=2))+
  gta_theme()

plot2.4.a

# not used
# gta_plot_saver(plot=plot2.4.a,
#                path=paste("0 report production/GTA 23/tables & figures/",output.path, sep=""),
#                name=paste("Figure ",chapter.number,".4a", sep=""))


