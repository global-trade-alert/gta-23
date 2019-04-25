library("splitstackshape")
library("xlsx")
library("foreign")
library("ggplot2")
library("grid")
library("gtable")
library("extrafontdb")
library("extrafont")
library("Rttf2pt1")
library("rworldmap")
library("gtalibrary")
rm(list = ls())

loadfonts(device="postscript")
loadfonts(device="win")
# font_import()

# setwd("GTA cloud/0 report production/GTA 23")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA 21")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23/")

#Settings
chapter.number=10
chapter.name="Which countries hit"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("help files/GTA 23 cutoff and definitions.R")




### THE GTA standard colour palette
gta_colour_palette()


gta_data_slicer(data.path = "../../data/master_plus.Rdata",
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = T,
                implementing.country = "G20",
                keep.implementer = T,
                gta.evaluation = c("amber","red"),
                in.force.today = T )

both=master.sliced
rm(master.sliced)



# dates
both$i.quarter<-quarter(both$date.implemented)
both$i.year<-year(both$date.implemented)

both$implementing.jurisdiction<-as.character(both$implementing.jurisdiction)
both$implementing.jurisdiction[both$implementing.jurisdiction=="United Kingdom of Great Britain and Northern Ireland"]<-"United Kingdom"
both$implementing.jurisdiction[both$implementing.jurisdiction=="United States of America"]<-"United States"
both$implementing.jurisdiction[both$implementing.jurisdiction=="Russian Federation"]<-"Russia"
both$implementing.jurisdiction[both$implementing.jurisdiction=="Republic of Korea"]<-"South Korea"

both$affected.jurisdiction<-as.character(both$affected.jurisdiction)
both$affected.jurisdiction[both$affected.jurisdiction=="United Kingdom of Great Britain and Northern Ireland"]<-"United Kingdom"
both$affected.jurisdiction[both$affected.jurisdiction=="United States of America"]<-"United States"
both$affected.jurisdiction[both$affected.jurisdiction=="Russian Federation"]<-"Russia"
both$affected.jurisdiction[both$affected.jurisdiction=="Republic of Korea"]<-"South Korea"

# Figure 10-1: Map of hits
figure101<-aggregate(intervention.id ~ a.un, data=both, function(x) length(unique(x)))
# sudan<-figure101[1,]
# sudan[1,1]<-728
# sudan[1,2]<-figure101$intervention.id[figure101$a.un==729]
# figure101<-rbind(figure101, sudan)

map1 <- gta_plot_map(data = figure101, countries = "a.un", value = "intervention.id", legend.title = "Number of times harmed \nby protectionist interventions\nimplemented by G20 that \nare currentlyin force", colour.low = gta_colour$blue[4], colour.high = gta_colour$blue[1], range.split=seq(0,4500,1000), legend.labels=c("0-999","1000-1999","2000-2999","3000+"))

map1

gta_plot_saver(plot=map1,
               path = paste("tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".1 - Global incidence of protectionism", sep=""))


# Figure 10-2: G20 vs WB country groups - number of hits
gta.jurisdiction.group <- read.csv(file = "../../data/database replica/gta_jurisdiction_group.csv")
gta.jurisdiction.group <- subset(gta.jurisdiction.group, id %in% c(21,10,11,12,13,14,15))
setnames(gta.jurisdiction.group, old = "id", new = "jurisdiction_group_id")
gta.jurisdiction.group.member <- read.csv(file = "../../data/database replica/gta_jurisdiction_group_member.csv")
gta.jurisdiction.group.member <- subset(gta.jurisdiction.group.member, jurisdiction_group_id %in% c(21,10,11,12,13,14,15))
gta.jurisdiction.group <- merge(gta.jurisdiction.group, gta.jurisdiction.group.member, by="jurisdiction_group_id", all.x=T)
gta.jurisdiction <- read.csv(file = "../../data/database replica/gta_jurisdiction.csv")
setnames(gta.jurisdiction, old = "id", new = "jurisdiction_id")
gta.jurisdiction <- subset(gta.jurisdiction, jurisdiction_id %in% gta.jurisdiction.group$jurisdiction_id)
gta.jurisdiction.group <- merge(gta.jurisdiction.group, gta.jurisdiction, by="jurisdiction_id", all.x=T)
gta.jurisdiction.group <- subset(gta.jurisdiction.group, select = c("name.x", "un_code"))
setnames(gta.jurisdiction.group, old = "name.x", new = "country.group")
setnames(gta.jurisdiction.group, old = "un_code", new = "a.un")
rm(gta.jurisdiction, gta.jurisdiction.group.member)

imp <- merge(both, gta.jurisdiction.group, by="a.un", all.x=T)

figure102<-aggregate(intervention.id~ implementing.jurisdiction+country.group, data=imp, function(x) length(unique(x)))
setnames(figure102, old="intervention.id", new="hits")
figure102$country.group <- as.character(figure102$country.group)
figure102$country.group[figure102$country.group=="Latin America and the Caribbean"]<-"Latin America & Caribbean"
figure102$country.group[figure102$country.group=="Middle East and North Africa"]<-"Middle East & North Africa"
figure102$country.group[figure102$country.group=="Europe and Central Asia"]<-"Europe & Central Asia"
figure102$country.group <- as.factor(figure102$country.group)

#figure102$trend[is.na(figure102$trend)==T]<-0
figure102$imp<-paste(" ", figure102$implementing.jurisdiction)

figure102.xlsx=figure102[,c("imp","country.group","hits")]
figure102.xlsx=reshape(figure102.xlsx, idvar="imp",timevar="country.group", direction="wide")

names(figure102.xlsx)=c("G20 member", gsub("hits\\.","", names(figure102.xlsx)[2:ncol(figure102.xlsx)]))

write.xlsx(figure102.xlsx, file=paste("tables & figures/",output.path,"/Figure ",chapter.number,".2 - Data for Figure 10.2.xlsx", sep=""), row.names = F)


limit<-500
(
  p1<- ggplot(figure102, aes(y=implementing.jurisdiction, x=country.group))+
    geom_tile(aes(fill=hits), colour="white") +
    scale_fill_gradient2(limits=c(0,limit),mid="white", low="darkorange", midpoint=1, high=gta_colour$red[1], na.value=gta_colour$red[1], name=paste("Protectionist   \ninterventions   \nimplemented   "))+
    theme(axis.text=element_text(family="", size=11, colour="black")) +
    coord_fixed(ratio=1) +
    theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="right")+
    labs(y="Implementing G20 member", x="Affected region")+
    gta_theme(x.bottom.angle = 45)+
    theme(legend.position = "right",
          aspect.ratio = 0.5,
          plot.background = element_blank(),
          panel.background = element_rect(fill="white",colour="grey",size=0.2),
          axis.line = element_blank(),
          axis.text.x.bottom = element_text(hjust=1))
)


# draw it
# png(paste("tables & figures/",output.path,"/Figure ",chapter.number,".2 - G20 vs country groups.png", sep=""), width=1000, height=600, res=76)
# p1
# dev.off()
# 
# cairo_ps(paste("tables & figures/",output.path,"/Figure ",chapter.number,".2 - G20 vs country groups.eps", sep=""), bg = "white", width=10.4, height=6.25, family="Open Sans")
# p1
# dev.off()

gta_plot_saver(plot=p1, path=paste("tables & figures/",output.path,"/", sep=""), name=paste("Figure ",chapter.number,".2 - G20 vs country groups", sep=""))



# Figure 10-3: G20 hitting LDC per year protectionst
country_iso <- read.csv(file = "../../R help files/country_iso_un.csv", sep = ";")
ldc <- subset(country_iso, LDC==1)

figure103<-aggregate(intervention.id~ implementing.jurisdiction+i.year, data=subset(both, a.un %in% ldc$UN), function(x) length(unique(x)))
figure103 <- subset(figure103, i.year>2008)
#figure103<-aggregate(intervention.id~ implementing.jurisdiction+affected.jurisdiction + protect + i.atleastone.G20 + a.atleastone.G20, data=both[both$date.implemented<=cutoff], function(x) length(unique(x)))
setnames(figure103, old="intervention.id", new="hits")


figure103$imp<-paste(" ", figure103$implementing.jurisdiction)


figure103.xlsx=figure103[,c("imp","i.year","hits")]
figure103.xlsx=reshape(figure103.xlsx, idvar="imp",timevar="i.year", direction = "wide")
figure103.xlsx[is.na(figure103.xlsx)]=0
names(figure103.xlsx)=c("G20 member", 2009:2018)

write.xlsx(figure103.xlsx, file=paste("tables & figures/",output.path,"/Figure ",chapter.number,".3 - Data for Figure 10.3.xlsx", sep=""), row.names = F)

limit<-25
(
  p1<-ggplot(figure103, aes(y=implementing.jurisdiction, x=as.factor(i.year)))
  + geom_tile(aes(fill=hits), colour="white")
  + scale_fill_gradient(limits=c(0,limit), low="white", high=gta_colour$red[1], na.value=gta_colour$red[1], name=paste("Protectionist   \ninterventions   \nimplemented   "))
  + theme(axis.text.x = element_text(angle = 90, hjust = .9, vjust=0.2),
          axis.text=element_text(family="Open Sans", size=11, colour="black"),
          aspect.ratio = 0.7)
  + coord_fixed(ratio=1)
  + theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="bottom")
  + labs(y="Implementing G20 member", x="year")
)



(
  p2<-ggplot(figure103, aes(y=imp, x=as.factor(i.year)))
  + geom_tile(aes(fill=hits), colour="white")
  + scale_fill_gradient(limits=c(0,limit), low="white", high=gta_colour$red[1], na.value=gta_colour$red[1], name=paste("Protectionist   \ninterventions   \nimplemented   "))
  + theme(axis.text.x = element_text(angle = 90, hjust = .9, vjust=0.2),
          axis.text=element_text(family="Open Sans", size=11, colour="black"),
          axis.text.y = element_text(hjust=0),
          aspect.ratio = 0.7)
  + coord_fixed(ratio=1)
  + theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="bottom")
  + labs(y="Implementing G20 member", x="year")
)

g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)


# draw it
png(paste("tables & figures/",output.path,"/Figure ",chapter.number,".3 - Frequency of harm by each G20 to the LDCs.png", sep=""), width=1000, height=600, res=76)
grid.draw(g)
dev.off()

cairo_ps(paste("tables & figures/",output.path,"/Figure ",chapter.number,".3 - Frequency of harm by each G20 to the LDCs.eps", sep=""), bg = "white", width=10.4, height=6.25, family="Open Sans")
grid.draw(g)
dev.off()
