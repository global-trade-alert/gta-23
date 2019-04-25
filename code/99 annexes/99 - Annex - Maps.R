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
library("rworldmap")
library("data.table")
library("gtalibrary")
rm(list = ls())

# font_import()
loadfonts(device="cairo_ps")
loadfonts(device="win")

# setup
# setwd("GTA cloud/0 report production/GTA 23")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23/")
load("../../data/master_plus.Rdata")
gtalibrary::gta_colour_palette()
source("help files/GTA 23 cutoff and definitions.R")
g20.members=c("32", "36", "76", "124", "156", "251", "276", "699", "360", "381", "392", "484", "410", "643", "682", "710", "792", "826", "840")

op <- par(family = "HersheyGothicEnglish")
par(op)

#### FOR MAPS ONLY: change UN IDs for India, Italy and France

# master$a.un[master$a.un==251]<-250
# master$i.un[master$i.un==251]<-250
#
# master$a.un[master$a.un==381]<-380
# master$i.un[master$i.un==381]<-380
#
# master$a.un[master$a.un==699]<-356
# master$i.un[master$i.un==699]<-356
#
# master$a.un[master$a.un==729]<-728
# master$i.un[master$i.un==729]<-728




conversion <- read.csv("../../R help files/country_iso_un.csv", sep=";")
setnames(conversion, old="UN", new="i.un")


world.0 <- gtalibrary::world.geo


conversion$name=as.character(conversion$name)
conversion$name[conversion$name=="United Kingdom of Great Britain and Northern Ireland"]<-"the United Kingdom"
conversion$name[conversion$name=="United States of America"]<-"the United States"
conversion$name[conversion$name=="Republic of Korea"]<-"South Korea"
conversion$name[conversion$name=="Russian Federation"]<-"Russia"

#################### MAPS  ###########################
for(cty in g20.members){



map.annex<-aggregate(intervention.id ~ a.un, data=subset(master, gta.evaluation!="Green" & currently.in.force=="Yes" & i.un==cty), function(x) length(unique(x)))

world <- world.0

data <- map.annex

data[,c("UN","value")] <- data[,c("a.un","intervention.id")]
data$UN <- gta_un_code_vector(data$UN)

# merge data with map data
world = merge(world, data[,c("UN","value")], by="UN", all.x=T)

###### IMPORTANT, sort for X (id) again
world <-  world[with(world, order(X)),]
world$value[is.na(world$value) == T] <- 0

marked.country <- gta_un_code_vector(cty)

map1 = ggplot() +
  geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.15, color = "white") +
  geom_polygon(data=subset(world, UN == marked.country), aes(x=long, y=lat, group = group), fill=gta_colour$turquoise[4], size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_y_continuous(limits=c(-55,85))+
  scale_x_continuous(limits=c(-169,191))+
  labs(x="", y="") +
  scale_fill_gradientn(colours = c(gta_colour$blue.shades(4),"#dadada"), values=c(1,0.21,0.11,0.01,0), breaks=c(0, seq(20,60,20), max(data$value)), position="bottom", labels=c("0","1-19","20-39","40-59","60 or more")) + # Set color gradient
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10)),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
        legend.text.align = 0
  ) +
  guides(fill=guide_legend(title=paste("Number of times harmed by a\nprotectionist intervention\nimposed by ",unique(conversion$name[conversion$i.un==cty])," and\ncurrently in force", sep=""), label.position = "top"),
         ymax=guide_legend(titel="size"))


map1

gta_plot_saver(plot=map1,
               path="tables & figures/annex - p. 2 - maps/",
               name=paste0("map_",unique(conversion$name[conversion$i.un==cty]),"_top"),
               width = 21,
               height = 12)






map.annex<-aggregate(intervention.id ~ i.un, data=subset(master, gta.evaluation!="Green" & currently.in.force=="Yes" & a.un==cty), function(x) length(unique(x)))

world <- world.0

data <- map.annex

data[,c("UN","value")] <- data[,c("i.un","intervention.id")]
data$UN <- gta_un_code_vector(data$UN)

# merge data with map data
world = merge(world, data[,c("UN","value")], by="UN", all.x=T)

###### IMPORTANT, sort for X (id) again
world <-  world[with(world, order(X)),]
world$value[is.na(world$value) == T] <- 0

marked.country <- gta_un_code_vector(cty)

map2 = ggplot() +
  geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.15, color = "white") +
  geom_polygon(data=subset(world, UN == marked.country), aes(x=long, y=lat, group = group), fill=gta_colour$turquoise[4], size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  labs(x="", y="") +
  scale_y_continuous(limits=c(-55,85))+
  scale_x_continuous(limits=c(-169,191))+
  scale_fill_gradientn(colours = c(gta_colour$blue.shades(4),"#dadada"), values=c(1,0.21,0.11,0.01,0), breaks=c(0, seq(20,60,20), max(data$value)), position="bottom", labels=c("0","1-19","20-39","40-59","60 or more")) + # Set color gradient
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10)),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
        legend.text.align = 0

  ) +
  guides(fill=guide_legend(title=paste("Discriminatory interventions\nharming ",unique(conversion$name[conversion$i.un==cty])," which are\ncurrently in force", sep=""), label.position = "top"),
         ymax=guide_legend(titel="size"))


map2

gta_plot_saver(plot=map2,
               path="tables & figures/annex - p. 2 - maps/",
               name=paste0("map_",unique(conversion$name[conversion$i.un==cty]),"_bottom"),
               width = 21,
               height = 12)
print(cty)


}




