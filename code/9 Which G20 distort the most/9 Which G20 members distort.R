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
library("gtalibrary")
library("lubridate")
library("tidyverse")
rm(list = ls())

# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

# setwd("GTA cloud/0 report production/GTA 23")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA 21")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23/")

#Settings
chapter.number=9
chapter.name="Which G20 distort the most"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("help files/GTA 23 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

gta_data_slicer(data.path = "../../data/master_plus.Rdata",
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = F,
                implementing.country = "G20",
                in.force.today = "any",
                keep.implementer = T,
                gta.evaluation = c("amber","red"))

implemented=master.sliced
rm(master.sliced)

# dates
implemented$i.quarter<-quarter(implemented$date.implemented)
implemented$i.year<-year(implemented$date.implemented)

implemented$phase="2018"
implemented$phase[implemented$date.implemented<=break.date]="pre-2018"

implemented$implementing.jurisdiction<-as.character(implemented$implementing.jurisdiction)
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="United Kingdom of Great Britain and Northern Ireland"]<-"United Kingdom"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="United States of America"]<-"United States"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="Russian Federation"]<-"Russia"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="Republic of Korea"]<-"South Korea"



# Figure 6-1: Protectionism by the G20
figure91.2018<-aggregate(intervention.id ~ implementing.jurisdiction, data=subset(implemented, phase == "2018"), function(x) length(unique(x)))
figure91.pre2018<-aggregate(intervention.id ~ implementing.jurisdiction, data=subset(implemented, phase == "pre-2018"), function(x) length(unique(x)))
figure91.total<-aggregate(intervention.id ~ implementing.jurisdiction, data=implemented, function(x) length(unique(x)))
setnames(figure91.2018, "intervention.id","2018")
setnames(figure91.pre2018, "intervention.id","pre-2018")
setnames(figure91.total, "intervention.id","total")
figure91 <- cbind(figure91.2018, figure91.pre2018, figure91.total)
figure91 <- figure91[,c("implementing.jurisdiction","pre-2018","2018","total")]
figure91 <- gather(figure91, phase, value, 2:3)

figure91 <- figure91[with(figure91, order(c(-total))),]
row.names(figure91) <- NULL

figure91.xlsx=figure91
names(figure91.xlsx)=c("G20 members","Total","Phase", "Number of implemented harmful interventions")
write.xlsx(figure91.xlsx, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".1 - Data for Figure 6.1.xlsx", sep=""), row.names=F)

p1=ggplot(data=figure91, aes(y=value, x=forcats::fct_inorder(implementing.jurisdiction)))+
  geom_bar(aes(fill=phase), stat = "identity", width=.8)+
  geom_text(data=subset(figure91, phase =="pre-2018"), aes(x=implementing.jurisdiction, y=40, label=value), colour="#FFFFFF", size=2.8)+
  geom_text(data=subset(figure91, phase =="2018"), aes(x=implementing.jurisdiction, y=total+40, label=value), colour=gta_colour$brown[2], size=2.8)+
  scale_fill_manual(values=c(gta_colour$brown[2], gta_colour$blue[2]))+
  scale_y_continuous()+
    gta_theme(x.bottom.angle = 90)+
  theme(axis.text.x.bottom = element_text(hjust=1))+
  labs(x="\nG20 member" , y="Number of discriminatory interventions \nimplemented Nov 2008 - October 2018",
       fill="")+
  theme(legend.box.margin = margin(t=0),
        axis.text.x.bottom = element_text(size=10),
        axis.text.y.left = element_text(size=10))

p1
# JF: did not work for me...
    # gta_plot_wrapper(y.right.enable = T,
    #                y.left.name = "Number of discriminatory\ninterventions implemented\nNov 2008 - October 2018",
    #                x.bottom.labels = figure91[with(figure91, order(-intervention.id)), ]$implementing.jurisdiction,
    #                x.bottom.breaks = seq(1,length(figure91$implementing.jurisdiction),1),
    #                x.bottom.name = "G20 member")+

gta_plot_saver(plot=p1,
               path = paste("tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".1", sep=""))




# Figure 6-2: Protectionist Tile
figure92<-aggregate(intervention.id~ implementing.jurisdiction+i.atleastone.G20 + i.year +i.quarter, data=implemented, function(x) length(unique(x)))
setnames(figure92, old="intervention.id", new="hits")
figure92$hit.gprop<-figure92$hits/max(figure92$hits)
figure92$date<-paste(figure92$i.year, figure92$i.quarter, sep="-Q")


figure92$imp<-paste(" ", figure92$implementing.jurisdiction)
figure92=subset(figure92, date!="2018-Q4")



figure92.xlsx=figure92[,c("imp","date","hits")]
names(figure92.xlsx)=c("G20 member", "Quarter", "Number of newly implemented harmful interventions")
write.xlsx(figure92.xlsx, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".2  - Data for Figure 6.2.xlsx", sep=""), row.names=F)


limit<-15
  (

p1 <- ggplot(figure92, aes(y=implementing.jurisdiction, x=date)) + geom_tile(aes(fill=hits), colour="white") +
  scale_fill_gradient(limits=c(0,limit), low="white", high=gta_colour$red[1], na.value=gta_colour$red[1], name=paste("Protectionist   \ninterventions   \nimplemented   \nby quarter   "))
+ theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=13, colour="black"))
+ coord_fixed(ratio=1.25)
+ theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="bottom")
+ labs(x="Quarter", y="G20 member")
)

(
p2 <- ggplot(figure92, aes(y=imp, x=date))
  + geom_tile(aes(fill=hits), colour="white")
  + scale_fill_gradient(limits=c(0,limit), low="white", high=gta_colour$red[1], na.value=gta_colour$red[1], name=paste("Protectionist   \ninterventions   \nimplemented   \nby quarter   "))
  + theme(axis.text.x = element_text(angle = 90, vjust = .5),
          axis.text=element_text(family="Open Sans", size=13, colour="black"),
          axis.text.y = element_text(hjust=0))
  + coord_fixed(ratio=1.25)
  + theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="bottom")
  + labs(x="Quarter", y="G20 member")
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
png(paste("tables & figures/",output.path,"/Figure ",chapter.number,".2 - G20 quarterly resort to protectionism.png", sep=""), width=1000, height=800, res=76)
grid.draw(g)
dev.off()

cairo_ps(paste("tables & figures/",output.path,"/Figure ",chapter.number,".2 - G20 quarterly resort to protectionism.eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
grid.draw(g)
dev.off()


### Figure 6.3

gta_data_slicer(data.path = "../../data/master_plus.Rdata",
                implementation.period = c("2008-11-01",cutoff),
                implementing.country = "G20",
                keep.implementer = T,
                gta.evaluation = c("amber","red"),
                lag.adjustment = "12-31",
                keep.implementation.na=F)

implemented=master.sliced
rm(master.sliced)

# dates
implemented$i.quarter<-quarter(implemented$date.implemented)
implemented$i.year<-year(implemented$date.implemented)

implemented$implementing.jurisdiction<-as.character(implemented$implementing.jurisdiction)
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="United Kingdom of Great Britain and Northern Ireland"]<-"United Kingdom"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="United States of America"]<-"United States"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="Russian Federation"]<-"Russia"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="Republic of Korea"]<-"South Korea"


figure93<-aggregate(intervention.id ~ i.year + implementing.jurisdiction, data = implemented, function(x) length(unique(x)))

figure93$intervention.id[figure93$i.year==2018]<-figure93$intervention.id[figure93$i.year==2018]/(303/365)

for(cty in unique(figure93$implementing.jurisdiction)){
figure93$index[figure93$implementing.jurisdiction==cty]<-figure93$intervention.id[figure93$implementing.jurisdiction==cty]/mean(figure93$intervention.id[figure93$implementing.jurisdiction==cty])
}


figure93$imp<-paste(" ", figure93$implementing.jurisdiction)


figure93.xlsx=figure93[,c("imp", "i.year","index")]
figure93.xlsx=reshape(figure93.xlsx, idvar="imp",timevar = "i.year", direction="wide")
names(figure93.xlsx)=c("G20 member", 2009:2018)
figure93.xlsx[is.na(figure93.xlsx)]=0
write.xlsx(figure93.xlsx, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".3 - Data for Figure 6.3.xlsx", sep=""), row.names=F)


limit<-3
(
p1 <- ggplot(subset(figure93, tolower(implementing.jurisdiction) %in% tolower(c(g20.member.names, "united states"))), aes(y=implementing.jurisdiction, x=as.factor(i.year))) + geom_tile(aes(fill=index), colour="white") +
  scale_fill_gradient(limits=c(0,limit), low="white", high=gta_colour$red[1], na.value=gta_colour$red[1], name=paste("Protectionist activity   \nreported by 31 December of same year     \ncompared to country average   "))
  # scale_fill_gradient2(limits=c(0,limit),mid="darkorange", low="white", midpoint=1, high=red, na.value=red, name=paste("Protectionist activity   \nreported by 31 December of same year     \ncompared to country average   "))
+ theme(axis.text.x = element_text(angle = 90, vjust=.5),
        axis.text=element_text(family="Open Sans", size=11, colour="black"))
+ coord_fixed(ratio=.75)
+ theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="bottom")
+ labs(x="year", y="G20 member")
)

(
p2 <- ggplot(subset(figure93, tolower(implementing.jurisdiction) %in% tolower(c(g20.member.names, "united states"))), aes(y=imp, x=as.factor(i.year))) + geom_tile(aes(fill=index), colour="white") +
     scale_fill_gradient(limits=c(0,limit), low="white", high=gta_colour$red[1], na.value=gta_colour$red[1], name=paste("Protectionist activity   \nreported by 31 December of same year     \ncompared to country average   "))
  # scale_fill_gradient2(limits=c(0,limit),mid="darkorange", low="white", midpoint=1, high=red, na.value=red, name=paste("Protectionist activity   \nreported by 31 December of same year     \ncompared to country average   "))
+ theme(axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text=element_text(family="Open Sans", size=11, colour="black"),
        axis.text.y = element_text(hjust=0))
+ coord_fixed(ratio=.75)
+ theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="bottom")
+ labs(x="year", y="G20 member")
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
png(paste("tables & figures/",output.path,"/Figure ",chapter.number,".3 - G20 reporting-lag adjusted annual heat map.png", sep=""), width=600, height=800, res=76)
grid.draw(g)
dev.off()

cairo_ps(paste("tables & figures/",output.path,"/Figure ",chapter.number,".3 - G20 reporting-lag adjusted annual heat map.eps", sep=""), bg = "white", width=6.25, height=10, family="Open Sans")
grid.draw(g)
dev.off()
