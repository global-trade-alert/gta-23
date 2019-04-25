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

rm(list = ls())

# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

## setup
# setwd("GTA cloud/0 report production/GTA 23")
load("../../data/master_plus.Rdata")
gtalibrary::gta_colour_palette()
source("help files/GTA 23 cutoff and definitions.R")

# 
# OLD code 
# g20<-subset(master, date.implemented<=cutoff & i.un %in% c("32", "36", "76", "124", "156", "251", "276", "699", "360", "381", "392", "484", "410", "643", "682", "710", "792", "826", "840") & is.na(date.implemented)==F)
# g20$protect<-0
# g20$protect[g20$gta.evaluation!="Green"]<-1
# 
# bars<-as.data.frame(unique(g20$i.un))
# setnames(bars, old="unique(g20$i.un)", new="i.un")
# bars$protect<-"0,1"
# bars<-as.data.frame(cSplit(bars, 2, direction="long", sep=","))
# bars$year<-"2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018"
# bars<-as.data.frame(cSplit(bars, 3, direction="long", sep=","))
# 
# bars$measures<-apply(bars, 1, function(x) length(unique(subset(g20, g20$i.un==x[1] & g20$protect==x[2] & year(g20$date.implemented)<=x[3] & g20$date.implemented<=cutoff)$Ref)))

## bar chart data: cumulative number of implemented interventions for each G20 member

master$protect=as.numeric(master$gta.evaluation!="Green")
master$year=year(master$date.implemented)
bars.yr=aggregate(intervention.id ~ i.un + year + protect, subset(master, i.un %in% g20.members & is.na(date.implemented)==F) , function(x) length(unique(x)))

bars=expand.grid(unique(bars.yr$i.un),c(2009:2018), c(0,1))
names(bars)=c("i.un","year", "protect")
bars$measures=apply(bars, 1, function(x) sum(subset(bars.yr, i.un==x[1] & year<=x[2] & protect==x[3])$intervention.id))


write.xlsx(bars, file="tables & figures/annex - p. 3 & 4 - bottom - bar charts/Data for bar charts.xlsx", row.names = F)

## Argentina
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==32 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==32 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==32 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==32 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Argentina_bottom_protectionist.png", width=800, height=800/1.8, res=76, type="cairo")
grid.draw(g)
dev.off()


cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Argentina_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Argentina
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==32 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==32 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==32 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==32 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Argentina_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Argentina_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()



## Australia
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==36 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==36 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==36 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==36 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Australia_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Australia_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Australia
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==36 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==36 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==36 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==36 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Australia_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Australia_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()





## Brazil
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==76 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==76 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==76 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==76 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Brazil_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Brazil_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Brazil
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==76 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==76 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==76 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==76 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Brazil_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Brazil_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Canada
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==124 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==124 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==124 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==124 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Canada_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Canada_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Canada
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==124 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==124 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==124 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==124 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Canada_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Canada_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## China
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==156 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==156 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==156 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==156 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/China_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/China_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## China
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==156 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==156 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==156 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==156 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/China_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/China_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## France
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==251 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==251 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==251 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==251 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/France_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/France_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## France
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==251 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==251 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==251 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==251 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/France_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/France_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Germany
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==276 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==276 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==276 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==276 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Germany_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Germany_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Germany
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==276 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==276 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==276 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==276 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Germany_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Germany_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()





## India
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==699 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==699 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==699 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==699 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/India_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/India_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## India
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==699 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==699 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==699 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==699 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/India_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/India_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Indonesia
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==360 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==360 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==360 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==360 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Indonesia_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Indonesia_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Indonesia
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==360 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==360 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==360 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==360 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Indonesia_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Indonesia_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Italy
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==381 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==381 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==381 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==381 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Italy_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Italy_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Italy
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==381 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==381 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==381 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==381 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Italy_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Italy_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Japan
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==392 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==392 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==392 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==392 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Japan_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Japan_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Japan
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==392 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==392 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==392 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==392 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Japan_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Japan_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Mexico
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==484 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==484 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==484 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==484 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Mexico_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Mexico_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Mexico
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==484 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==484 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==484 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==484 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Mexico_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Mexico_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Southkorea
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==410 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==410 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==410 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==410 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southkorea_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southkorea_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Southkorea
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==410 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==410 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==410 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==410 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southkorea_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southkorea_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Russia
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==643 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==643 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==643 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==643 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Russia_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Russia_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Russia
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==643 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==643 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==643 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==643 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Russia_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Russia_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Saudiarabia
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==682 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==682 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==682 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==682 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Saudiarabia_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Saudiarabia_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Saudiarabia
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==682 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==682 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==682 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==682 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Saudiarabia_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Saudiarabia_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Southafrica
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==710 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==710 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==710 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==710 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southafrica_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southafrica_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Southafrica
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==710 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==710 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==710 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==710 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southafrica_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Southafrica_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## Turkey
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==792 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==792 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==792 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==792 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Turkey_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Turkey_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## Turkey
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==792 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==792 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==792 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==792 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Turkey_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/Turkey_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()





## UK
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==826 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==826 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==826 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==826 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/UK_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/UK_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## UK
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==826 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==826 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==826 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==826 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/UK_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/UK_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()




## USA
## protectionist

(
  p1 <- ggplot(data=subset(bars, i.un==840 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==840 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==840 & protect==1), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[2])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==840 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/USA_bottom_protectionist.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/USA_bottom_protectionist.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()

## USA
## liberalising

(
  p1 <- ggplot(data=subset(bars, i.un==840 & protect==0), aes(y=measures, x=year))+
    geom_bar(stat="identity", fill=blue[4])+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==840 & protect==1)$measures)/100+.5)*100)))+
    labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year\n",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
)

(
  p2 <- ggplot(data=subset(bars, i.un==840 & protect==0), aes(y=measures, x=year))+
    geom_text(aes(label=measures), position=position_dodge(width=0.9), vjust=-0.5)+
    geom_bar(stat="identity", fill=blue[4])+
    scale_x_continuous(breaks=2009:2018)+
    scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==840 & protect==1)$measures)/100+.5)*100)))+
    labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
         fill="")+
    theme(legend.position="bottom", legend.justification="left",
          panel.background = element_rect(fill =panel.bg), 
          legend.key=element_rect(fill="white"),
          legend.text=element_text(size=11),
          axis.text=element_text(family="Open Sans", size=11, colour="black"))
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
png("tables & figures/annex - p. 3 & 4 - bottom - bar charts/USA_bottom_liberalising.png", width=800, height=800/1.8, res=76)
grid.draw(g)
dev.off()

cairo_ps("tables & figures/annex - p. 3 & 4 - bottom - bar charts/USA_bottom_liberalising.eps", bg = "white", width=11, height=11/1.8, family="Open Sans")
grid.draw(g)
dev.off()



