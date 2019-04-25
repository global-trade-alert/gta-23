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
library("data.table")
library("gtalibrary")
rm(list = ls())

# font_import()
loadfonts(device="postscript")
loadfonts(device="win")
# loadfonts()

## setup
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud/0 report production/GTA 23")
# setwd("0 report production/GTA 23")
setwd("GTA cloud/0 report production/GTA 23")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23")
load("../../data/master_plus.Rdata")
gtalibrary::gta_colour_palette()
source("help files/GTA 23 cutoff and definitions.R")

break.date = "2017-12-31"

mt=read.csv("../../data/database replica/gta_measure_type.csv")


implemented <- subset(master, is.na(date.implemented)==F)
implemented.TL=cSplit(implemented, which(colnames(implemented)=="affected.product"), direction="long", sep=", ")


#######
g20<-subset(implemented, date.implemented<=cutoff & i.un %in% g20.members & is.na(date.implemented)==F)
g20$protect<-0
g20$protect[g20$gta.evaluation!="Green"]<-1
g20$prior<-0
g20$prior[g20$date.implemented<=break.date]<-1
g20$tariff<-as.numeric(g20$mast.chapter=="TARIFF")
g20$ad<-as.numeric(g20$mast.chapter=="D")
g20$murky<-as.numeric(grepl(paste(subset(mt, is_murky==1)$name, collapse="|"), g20$intervention.type, ignore.case=T))

g20.TL<-subset(implemented.TL, date.implemented<=cutoff & i.un %in% g20.members & is.na(date.implemented)==F)
g20.TL$protect<-0
g20.TL$protect[g20.TL$gta.evaluation!="Green"]<-1
g20.TL$prior<-0
g20.TL$prior[g20.TL$date.implemented<=break.date]<-1
g20.TL$tariff<-as.numeric(g20.TL$mast.chapter=="TARIFF")
g20.TL$ad<-as.numeric(g20.TL$mast.chapter=="D")
g20.TL$murky<-as.numeric(grepl(paste(subset(mt, is_murky==1)$name, collapse="|"), g20.TL$intervention.type, ignore.case=T))



hits<-aggregate(intervention.id ~ i.un + protect + murky + tariff + prior + currently.in.force, data=g20, function(x) length(unique(x)))
products<-aggregate(affected.product ~ i.un + protect + currently.in.force, data=subset(g20.TL, prior==1), function(x) length(unique(x)))
products.imp<-aggregate(affected.product ~ i.un + protect, data=subset(g20.TL, prior==1), function(x) length(unique(x)))
products.prior<-aggregate(affected.product ~ i.un + protect + currently.in.force, data=subset(g20.TL, prior==0), function(x) length(unique(x)))
products.prior.imp<-aggregate(affected.product ~ i.un + protect, data=subset(g20.TL, prior==0), function(x) length(unique(x)))

# "Share of all interventions implemented since November 2008 that are harmful"
# "Share of harmful interventions that are 'murky' (not tariffs or trade defence)"
# "Share of tariff lines affected by surviving harmful interventions ",
# "Share of tariff lines affected by all implemented harmful interventions",
# "Share of harmful interventions still in force",
# "Share of all interventions since 1 January 2012 that are harmful"
current<-as.data.frame(unique(g20$i.un))
setnames(current, old="unique(g20$i.un)", new="i.un")
current$c<-"c1.p, c2.p, c3.p, c4.p, c5.p, c1.l, c2.l, c3.l, c4.l, c5.l"
current<-as.data.frame(cSplit(current,2,direction="long", sep=","))
current$i.un<-as.numeric(current$i.un)
current$score[current$c=="c1.p"]<-apply(subset(current, c=="c1.p"),1, function(x) sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==1)$intervention.id)/sum(subset(hits, prior==0  & i.un==as.numeric(x[1]))$intervention.id))
current$score[current$c=="c2.p"]<-apply(subset(current, c=="c2.p"),1, function(x) sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==1 & murky==1)$intervention.id)/sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==1)$intervention.id))
current$score[current$c=="c3.p"]<-apply(subset(current, c=="c3.p"),1, function(x) sum(subset(products, i.un==as.numeric(x[1]) & protect==1 & currently.in.force=="Yes")$affected.product)/5205)
current$score[current$c=="c4.p"]<-apply(subset(current, c=="c4.p"),1, function(x) sum(subset(products.imp, i.un==as.numeric(x[1]) & protect==1)$affected.product)/5205)
current$score[current$c=="c5.p"]<-apply(subset(current, c=="c5.p"),1, function(x) sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==1 & currently.in.force=="Yes")$intervention.id)/sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==1)$intervention.id))


# 
# "Share of all interventions implemented since November 2008 that are liberalising",
# "Share of liberalising interventions that are tariff cuts",
# "Share of tariff lines benefiting from surviving liberalising interventions ",
# "Share of tariff lines benefiting from all implemented liberalising interventions",
# "Share of liberalising interventions still in force",
# "Share of all interventions since 1 January 2012 that are liberalising"))

current$score[current$c=="c1.l"]<-apply(subset(current, c=="c1.l"),1, function(x) sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==0)$intervention.id)/sum(subset(hits, prior==0  & i.un==as.numeric(x[1]))$intervention.id))
current$score[current$c=="c2.l"]<-apply(subset(current, c=="c2.l"),1, function(x) sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==0 & tariff==1)$intervention.id)/sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==0)$intervention.id))
current$score[current$c=="c3.l"]<-apply(subset(current, c=="c3.l"),1, function(x) sum(subset(products, i.un==as.numeric(x[1]) & protect==0 & currently.in.force=="Yes")$affected.product)/5205)
current$score[current$c=="c4.l"]<-apply(subset(current, c=="c4.l"),1, function(x) sum(subset(products.imp, i.un==as.numeric(x[1]) & protect==0)$affected.product)/5205)
current$score[current$c=="c5.l"]<-apply(subset(current, c=="c5.l"),1, function(x) sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==0 & currently.in.force=="Yes")$intervention.id)/sum(subset(hits, prior==0  & i.un==as.numeric(x[1]) & protect==0)$intervention.id))

conversion <- read.csv("../../R help files/country_iso_un.csv", sep=";")
setnames(conversion, old="UN", new="i.un")
current<-merge(current, unique(conversion[, c("i.un", "name")]), by="i.un", all.x=T)
current$name<-as.character(current$name)
current$name[current$name=="United Kingdom of Great Britain and Northern Ireland"]<-"UK"
current$name[current$name=="United States of America"]<-"US"
current$name[current$name=="Republic of Korea"]<-"South Korea"
current$name[current$name=="Russian Federation"]<-"Russia"
current$name<-paste(current$name," 2018" ,sep="")

current$score[is.na(current$score)==T]<-0



## G20 avg
g20.avg<-as.data.frame(0)
setnames(g20.avg, old="0", new="i.un")
g20.avg$c<-"c1.p, c2.p, c3.p, c4.p, c5.p, c1.l, c2.l, c3.l, c4.l, c5.l"
g20.avg<-as.data.frame(cSplit(g20.avg,2,direction="long", sep=","))
g20.avg$score<-apply(g20.avg, 1, function(x) mean(current$score[current$c==x[2]]))

g20.avg$name<-"G20 mean 2018"


## pre cutoff
pre.2018<-as.data.frame(unique(g20$i.un))
setnames(pre.2018, old="unique(g20$i.un)", new="i.un")
pre.2018$c<-"c1.p, c2.p, c3.p, c4.p, c5.p, c1.l, c2.l, c3.l, c4.l, c5.l"
pre.2018<-as.data.frame(cSplit(pre.2018,2,direction="long", sep=","))
pre.2018$i.un<-as.numeric(pre.2018$i.un)
pre.2018$score[pre.2018$c=="c1.p"]<-apply(subset(pre.2018, c=="c1.p"),1, function(x) sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==1)$intervention.id)/sum(subset(hits, prior==1 & i.un==as.numeric(x[1]))$intervention.id))
pre.2018$score[pre.2018$c=="c2.p"]<-apply(subset(pre.2018, c=="c2.p"),1, function(x) sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==1 & murky==1)$intervention.id)/sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==1)$intervention.id))
pre.2018$score[pre.2018$c=="c3.p"]<-apply(subset(pre.2018, c=="c3.p"),1, function(x) sum(subset(products.prior, i.un==as.numeric(x[1]) & protect==1 & currently.in.force=="Yes")$affected.product)/5205)
pre.2018$score[pre.2018$c=="c4.p"]<-apply(subset(pre.2018, c=="c4.p"),1, function(x) sum(subset(products.prior.imp, i.un==as.numeric(x[1]) & protect==1)$affected.product)/5205)
pre.2018$score[pre.2018$c=="c5.p"]<-apply(subset(pre.2018, c=="c5.p"),1, function(x) sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==1 & currently.in.force=="Yes")$intervention.id)/sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==1)$intervention.id))





pre.2018$score[pre.2018$c=="c1.l"]<-apply(subset(pre.2018, c=="c1.l"),1, function(x) sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==0)$intervention.id)/sum(subset(hits, prior==1 & i.un==as.numeric(x[1]))$intervention.id))
pre.2018$score[pre.2018$c=="c2.l"]<-apply(subset(pre.2018, c=="c2.l"),1, function(x) sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==0 & tariff==1)$intervention.id)/sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==0)$intervention.id))
pre.2018$score[pre.2018$c=="c3.l"]<-apply(subset(pre.2018, c=="c3.l"),1, function(x) sum(subset(products.prior, i.un==as.numeric(x[1]) & protect==0 & currently.in.force=="Yes")$affected.product)/5205)
pre.2018$score[pre.2018$c=="c4.l"]<-apply(subset(pre.2018, c=="c4.l"),1, function(x) sum(subset(products.prior.imp, i.un==as.numeric(x[1]) & protect==0)$affected.product)/5205)
pre.2018$score[pre.2018$c=="c5.l"]<-apply(subset(pre.2018, c=="c5.l"),1, function(x) sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==0 & currently.in.force=="Yes")$intervention.id)/sum(subset(hits, prior==1 & i.un==as.numeric(x[1]) & protect==0)$intervention.id))

conversion <- read.csv("../../R help files/country_iso_un.csv", sep=";")
setnames(conversion, old="UN", new="i.un")
pre.2018<-merge(pre.2018, unique(conversion[, c("i.un", "name")]), by="i.un", all.x=T)
pre.2018$name<-as.character(pre.2018$name)
pre.2018$name[pre.2018$name=="United Kingdom of Great Britain and Northern Ireland"]<-"UK"
pre.2018$name[pre.2018$name=="United States of America"]<-"US"
pre.2018$name[pre.2018$name=="Republic of Korea"]<-"South Korea"
pre.2018$name[pre.2018$name=="Russian Federation"]<-"Russia"

pre.2018$name<-paste(pre.2018$name," pre-2018" ,sep="")
pre.2018$score[is.na(pre.2018$score)==T]<-0


## G20 avg
g20.avg.pre<-as.data.frame(0)
setnames(g20.avg.pre, old="0", new="i.un")
g20.avg.pre$c<-"c1.p, c2.p, c3.p, c4.p, c5.p, c1.l, c2.l, c3.l, c4.l, c5.l"
g20.avg.pre<-as.data.frame(cSplit(g20.avg.pre,2,direction="long", sep=","))
g20.avg.pre$score<-apply(g20.avg.pre, 1, function(x) mean(pre.2018$score[current$c==x[2]]))

g20.avg.pre$name<-"G20 mean pre-2018"




### final
lines<-rbind(current, pre.2018, g20.avg, g20.avg.pre)

lines$criterion[lines$c=="c1.p"]<-"Share of harmful in all implemented interventions"
lines$criterion[lines$c=="c2.p"]<-"Share of harmful interventions that are 'murky' (not tariffs or trade defence)"
lines$criterion[lines$c=="c3.p"]<-"Share of tariff lines affected by surviving harmful interventions"
lines$criterion[lines$c=="c4.p"]<-"Share of tariff lines affected by all implemented harmful interventions"
lines$criterion[lines$c=="c5.p"]<-"Share of harmful interventions still in force"
lines$criterion[lines$c=="c1.l"]<-"Share of liberalising in all implemented interventions"
lines$criterion[lines$c=="c2.l"]<-"Share of liberalising interventions that are tariff cuts"
lines$criterion[lines$c=="c3.l"]<-"Share of tariff lines benefiting from surviving liberalising interventions"
lines$criterion[lines$c=="c4.l"]<-"Share of tariff lines benefiting from all implemented liberalising interventions"
lines$criterion[lines$c=="c5.l"]<-"Share of liberalising interventions still in force"

lines$type[lines$c=="c1.p"]<-"protect"
lines$type[lines$c=="c2.p"]<-"protect"
lines$type[lines$c=="c3.p"]<-"protect"
lines$type[lines$c=="c4.p"]<-"protect"
lines$type[lines$c=="c5.p"]<-"protect"
lines$type[lines$c=="c1.l"]<-"liberalising"
lines$type[lines$c=="c2.l"]<-"liberalising"
lines$type[lines$c=="c3.l"]<-"liberalising"
lines$type[lines$c=="c4.l"]<-"liberalising"
lines$type[lines$c=="c5.l"]<-"liberalising"

lines$id[lines$c=="c1.p"]<-1
lines$id[lines$c=="c2.p"]<-2
lines$id[lines$c=="c3.p"]<-3
lines$id[lines$c=="c4.p"]<-4
lines$id[lines$c=="c5.p"]<-5
lines$id[lines$c=="c1.l"]<-1
lines$id[lines$c=="c2.l"]<-2
lines$id[lines$c=="c3.l"]<-3
lines$id[lines$c=="c4.l"]<-4
lines$id[lines$c=="c5.l"]<-5


write.xlsx(lines, file="tables & figures/annex - p. 3 & 4 - top - criterion charts/Data for Track Record charts.xlsx", row.names=F)


## conversion to us
conversion$comtrade<-as.character(conversion$comtrade)
conversion$comtrade[conversion$i.un==840]<-"United States"



# plots A-F
countries<-c("32", "36", "76", "124", "156", "251")

for(cty in countries) {
  p.data<-subset(lines, i.un %in% c(cty, "0"))

  p1 <- ggplot(data=subset(p.data, type=="protect"), aes(y=id, x=score, col=name, shape=name))+
    geom_point(size=7)+
    scale_shape_manual(values=c(17,17,15,15))+
    scale_colour_manual(values=c(gta_colour$blue[4], gta_colour$blue[2], gta_colour$brown[4], gta_colour$brown[2]))+
    guides(col = guide_legend(override.aes = list(size=6, colour=c(gta_colour$blue[4], gta_colour$blue[2], gta_colour$brown[4], gta_colour$brown[2]), shape=c(17,17,15,15))))+
    labs(x=expression(paste("More protectionist policy stance ", symbol('\256'))), y="",
         colour="")+
    guides(shape = FALSE)+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(labels=c("Share of harmful\nin all implemented interventions",
                              "Share of harmful interventions\nthat are 'murky'\n(not tariffs or trade defence)",
                              "Share of tariff lines\naffected by surviving\nharmful interventions",
                              "Share of tariff lines\naffected by all implemented\nharmful interventions",
                              "Share of harmful interventions\nstill in force"), sec.axis = sec_axis(~., labels=c("Share of harmful\nin all implemented interventions",
                                                                                                                  "Share of harmful interventions\nthat are 'murky'\n(not tariffs or trade defence)",
                                                                                                                  "Share of tariff lines\naffected by surviving\nharmful interventions",
                                                                                                                  "Share of tariff lines\naffected by all implemented\nharmful interventions",
                                                                                                                  "Share of harmful interventions\nstill in force")))+
    gta_theme(base.size=12, base.family="Open Sans")+
theme(axis.text.y.left = element_text(size=8.5),
      axis.text.y.right = element_text(size=8.5),
      axis.text.x.bottom = element_text(size=8.5))
  
    # gta_plot_saver(plot=p1,
    #                path="tables & figures/annex - p. 3 & 4 - top - criterion charts/",
    #                name = paste0(conversion$comtrade[conversion$i.un==cty],"_top_protectionist"))
    # draw it
    png(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_protectionist"),".png", sep=""), width=3000, height=2400, res=300)
    p1
    dev.off()
    
    # cairo_ps(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_protectionist"),".eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
    # p1
    # dev.off()
    # 


# liberalising
  p1 <- ggplot(data=subset(p.data, type=="liberalising"), aes(y=id, x=score, col=name, shape=name))+
  geom_point(size=7)+
  scale_shape_manual(values=c(17,17,15,15))+
  scale_colour_manual(values=c(gta_colour$blue[4], gta_colour$blue[2], gta_colour$brown[4], gta_colour$brown[2]))+
  guides(col = guide_legend(override.aes = list(size=6, colour=c(gta_colour$blue[4], gta_colour$blue[2], gta_colour$brown[4], gta_colour$brown[2]), shape=c(17,17,15,15))))+
    labs(x=expression(paste("More liberal policy stance ", symbol('\256'))), y="",
         colour="")+
    guides(shape = FALSE)+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(labels=c("Share of liberalising\nin all implemented interventions",
                              "Share of liberalising interventions\nthat are tariff cuts",
                              "Share of tariff lines\nbenefiting from\nsurviving\nliberalising interventions",
                              "Share of tariff lines\nbenefiting from\nall implemented\nliberalising interventions",
                              "Share of liberalising\ninterventions\nstill in force"), sec.axis = sec_axis(~., labels=c("Share of liberalising\nin all implemented interventions",
                                                                                                                        "Share of liberalising interventions\nthat are tariff cuts",
                                                                                                                        "Share of tariff lines\nbenefiting from\nsurviving\nliberalising interventions",
                                                                                                                        "Share of tariff lines\nbenefiting from\nall implemented\nliberalising interventions",
                                                                                                                        "Share of liberalising\ninterventions\nstill in force")))+
    gta_theme(base.size=12, base.family="Open Sans")+
    theme(axis.text.y.left = element_text(size=8.5),
          axis.text.y.right = element_text(size=8.5),
          axis.text.x.bottom = element_text(size=8.5))
  
    # gta_plot_saver(plot=p1,
    #                path="tables & figures/annex - p. 3 & 4 - top - criterion charts/",
    #                name = paste0(conversion$comtrade[conversion$i.un==cty],"_top_liberalising"))
  
    
    # draw it
    png(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_liberalising"),".png", sep=""), width=3000, height=2400, res=300)
    p1
    dev.off()
    
    # cairo_ps(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_liberalising"),".eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
    # p1
    # dev.off()

}


# plots G-Z
countries<-c("276", "699", "360", "381", "392", "484", "410", "643", "682", "710", "792", "826", "840")


for(cty in countries) {
  p.data<-subset(lines, i.un %in% c(cty, "0"))
  
  p1 <- ggplot(data=subset(p.data, type=="protect"), aes(y=id, x=score, col=name, shape=name))+
    geom_point(size=7)+
    scale_shape_manual(values=c(15,15,17,17))+
    scale_colour_manual(values=c(gta_colour$brown[4], gta_colour$brown[2],gta_colour$blue[4], gta_colour$blue[2]))+
    guides(col = guide_legend(override.aes = list(size=6, colour=c(gta_colour$brown[4], gta_colour$brown[2],gta_colour$blue[4], gta_colour$blue[2]), shape=c(15,15,17,17))))+
    labs(x=expression(paste("More protectionist policy stance ", symbol('\256'))), y="",
         colour="")+
    guides(shape = FALSE)+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(labels=c("Share of harmful\nin all implemented interventions",
                              "Share of harmful interventions\nthat are 'murky'\n(not tariffs or trade defence)",
                                "Share of tariff lines\naffected by surviving\nharmful interventions",
                                "Share of tariff lines\naffected by all implemented\nharmful interventions",
                                "Share of harmful interventions\nstill in force"), sec.axis = sec_axis(~., labels=c("Share of harmful\nin all implemented interventions",
                                                                                                                    "Share of harmful interventions\nthat are 'murky'\n(not tariffs or trade defence)",
                                                                                                                    "Share of tariff lines\naffected by surviving\nharmful interventions",
                                                                                                                    "Share of tariff lines\naffected by all implemented\nharmful interventions",
                                                                                                                    "Share of harmful interventions\nstill in force")))+
      gta_theme(base.size=12, base.family="Open Sans")+
    theme(axis.text.y.left = element_text(size=8.5),
          axis.text.y.right = element_text(size=8.5),
          axis.text.x.bottom = element_text(size=8.5))
  # 
  # gta_plot_saver(plot=p1,
  #                path="tables & figures/annex - p. 3 & 4 - top - criterion charts/",
  #                name = paste0(conversion$comtrade[conversion$i.un==cty],"_top_protectionist"))
  # draw it
  png(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_protectionist"),".png", sep=""), width=3000, height=2400, res=300)
  p1
  dev.off()
  
  # cairo_ps(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_protectionist"),".eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
  # p1
  # dev.off()
  # 
  
  
  
  # liberalising
  p1 <- ggplot(data=subset(p.data, type=="liberalising"), aes(y=id, x=score, col=name, shape=name))+
    geom_point(size=7)+
    scale_shape_manual(values=c(15,15,17,17))+
    scale_colour_manual(values=c(gta_colour$brown[4], gta_colour$brown[2],gta_colour$blue[4], gta_colour$blue[2]))+
    guides(col = guide_legend(override.aes = list(size=6, colour=c(gta_colour$brown[4], gta_colour$brown[2],gta_colour$blue[4], gta_colour$blue[2]), shape=c(15,15,17,17))))+
    labs(x=expression(paste("More liberal policy stance ", symbol('\256'))), y="",
         colour="")+
    guides(shape = FALSE)+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(labels=c("Share of liberalising\nin all implemented interventions",
                                "Share of liberalising interventions\nthat are tariff cuts",
                                "Share of tariff lines\nbenefiting from\nsurviving\nliberalising interventions",
                                "Share of tariff lines\nbenefiting from\nall implemented\nliberalising interventions",
                                "Share of liberalising\ninterventions\nstill in force"), sec.axis = sec_axis(~., labels=c("Share of liberalising\nin all implemented interventions",
                                                                                                                          "Share of liberalising interventions\nthat are tariff cuts",
                                                                                                                          "Share of tariff lines\nbenefiting from\nsurviving\nliberalising interventions",
                                                                                                                          "Share of tariff lines\nbenefiting from\nall implemented\nliberalising interventions",
                                                                                                                          "Share of liberalising\ninterventions\nstill in force")))+
      gta_theme(base.size=12, base.family="Open Sans")+
      theme(axis.text.y.left = element_text(size=8.5),
            axis.text.y.right = element_text(size=8.5),
            axis.text.x.bottom = element_text(size=8.5))
  
  # gta_plot_saver(plot=p1,
  #                path="tables & figures/annex - p. 3 & 4 - top - criterion charts/",
  #                name = paste0(conversion$comtrade[conversion$i.un==cty],"_top_liberalising"))
  # 
  # draw it
  png(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_liberalising"),".png", sep=""), width=3000, height=2400, res=300)
  p1
  dev.off()
  
  cairo_ps(paste("tables & figures/annex - p. 3 & 4 - top - criterion charts/",paste0(conversion$comtrade[conversion$i.un==cty],"_top_liberalising"),".eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
  p1
  dev.off()
  # 


}




