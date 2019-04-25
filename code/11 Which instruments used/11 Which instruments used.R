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
rm(list = ls())

loadfonts(device="postscript")
loadfonts(device="win")

# setwd("GTA cloud/0 report production/GTA 23")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23/")


#Settings
chapter.number=11
chapter.name="Which instruments used"
output.path=paste(chapter.number, chapter.name, sep=" ")
source("help files/GTA 23 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

gta_data_slicer(data.path = "../../data/master_plus.Rdata",
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = F,
                implementing.country = "G20",
                keep.implementer = T,
                in.force.today = "Any",
                gta.evaluation = c("amber","red"))

implemented=master.sliced
rm(master.sliced)


implemented$phase="2018"
implemented$phase[implemented$date.implemented<=break.date]="pre-2018"

# Figure 11.1: Top 10 instruments new types
topinst<-aggregate(intervention.id ~ mast.chapter, data=implemented, function(x) length(unique(x)))
setnames(topinst, old="intervention.id", new="Total")
topinst$rank<-rank(-topinst$Total, ties.method="first")
topinst<-subset(topinst, rank<11)

implemented$mast.chapter<-as.character(implemented$mast.chapter)
implemented$mast.chapter[!implemented$mast.chapter %in% unique(topinst$mast.chapter)]<-"Other"

top.stack<-aggregate(intervention.id ~ mast.chapter + phase, data = subset(implemented, phase=="pre-2018"), function(x) length(unique(x)))
top.stack$share<-top.stack$intervention.id/sum(top.stack$intervention.id)

top.stack2<-aggregate(intervention.id ~ mast.chapter + phase, data = subset(implemented, phase!="pre-2018"), function(x) length(unique(x)))
top.stack2$share<-top.stack2$intervention.id/sum(top.stack2$intervention.id)

top.stack<-rbind(top.stack, top.stack2)

setnames(top.stack, old="intervention.id", new="Total")
top.stack<-merge(top.stack, aggregate(intervention.id ~ mast.chapter, data = implemented, function(x) length(unique(x)))[,c("mast.chapter", "intervention.id")], by="mast.chapter", all.x=T)
setnames(top.stack, old="intervention.id", new="entire")



# top.stack$intervention.type<-as.character(top.stack$intervention.type)
# top.stack$intervention.type[top.stack$intervention.type=="Public procurement localisation"]<-"Public procurement\nlocalisation"
# top.stack$intervention.type[top.stack$intervention.type=="Tax or social insurance relief"]<-"Tax or social\ninsurance relief"

top.stack$rank<-rank(top.stack$entire, ties.method = "average")
top.stack$rank[top.stack$phase=="pre-2018"]<-top.stack$rank[top.stack$phase=="pre-2018"]+.1

top.stack$mast.chapter <- reorder(top.stack$mast.chapter, -top.stack$rank)

top.stack$phase[top.stack$phase=="pre-2018"]<-"pre-2018"

setnames(top.stack, "mast.chapter","mast.chapter.id")
mast.names <- gtalibrary::int.mast.types
top.stack <- merge(top.stack, mast.names[,c("mast.chapter.id","mast.chapter.name")], by="mast.chapter.id", all.x=T)
top.stack <- unique(top.stack)

figure111.xlsx=top.stack[order(-top.stack$entire),c("mast.chapter.id","mast.chapter.name", "phase", "Total","entire")]



figure111.xlsx$entire=NULL
figure111.xlsx=subset(figure111.xlsx, mast.chapter.id!="Other")
names(figure111.xlsx)=c("Mast Chapter","Name", "Phase", "Number of times used")
write.xlsx(figure111.xlsx, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".1  - Data for Figure 11.1.xlsx", sep=""), row.names=F)

# top.stack$position<-top.stack$Total/2
# top.stack$position[top.stack$phase=="pre-2018"]<-top.stack$Total[top.stack$phase=="pre-2018"]/2+top.stack$entire[top.stack$phase=="pre-2018"]-top.stack$Total[top.stack$phase=="pre-2018"]
top.stack$position[top.stack$phase=="pre-2018"] <- 75
top.stack$position[top.stack$phase!="pre-2018"] <- top.stack$entire[top.stack$phase!="pre-2018"]+75

row.names(top.stack) <- NULL
top.stack$mast.chapter.name <- as.character(top.stack$mast.chapter.name)
top.stack$mast.chapter.name[top.stack$mast.chapter.name=="P: Export-related measures (incl. subsidies)"]="P: Export-related measures (incl. export subsidies)"


  p1 <- ggplot()+
    geom_bar(data=subset(top.stack, mast.chapter.id!="Other"), aes(x=forcats::fct_inorder(mast.chapter.name), y=Total, fill=phase), stat = "identity", width=.65)+
    geom_text(data=subset(top.stack, mast.chapter.id!="Other"), aes(x= mast.chapter.name, y = position, label = Total, family=""), size = 3.5, colour=c(rep(c("#FFFFFF",gta_colour$brown[1]),5),
                                                                                                                                                        rep(c(gta_colour$brown[1],"#FFFFFF"),1),
                                                                                                                                                        rep(c("#FFFFFF",gta_colour$brown[1]),4))) +
    labs(x="Type of policy intervention and where appropriate MAST chapter label" , y="Number of harmful\ninterventions implemented",
         fill="")+
    scale_fill_manual(values=c(gta_colour$brown[2], gta_colour$blue[2]))+
    scale_x_discrete(labels= c("L: Subsidies \n(excl. export subsidies)", "P: Export-related measures\n(incl. export subsidies)", "D: Contingent trade-\nprotective measures", "Tariff measures", "M: Government procurement \nrestrictions", "I: Trade-related \ninvestment measures", "E: Non-automatic \nlicensing, quotas etc.", "FDI measures", "Instrument unclear", "Migration measures" ))+
    scale_y_continuous(breaks=seq(0,2700,200), limit=c(0,2700), sec.axis = sec_axis(~.,breaks=seq(0,2700,200), name = "Number of harmful\ninterventions implemented"))+
    guides(guide_legend(title = NULL))+
    gta_theme(x.bottom.angle = 45)+
    theme(axis.text.x.bottom = element_text(hjust=0.9 , size=10),
          axis.title.y.left = element_text(size=10),
          axis.title.y.right = element_text(size=10),
          legend.text = element_text(size=8),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent", color="transparent"),
          legend.position = c(.9,.9),
          legend.background = element_rect(fill="transparent"),
          legend.margin = margin(t=-10,b=0,r=0,l=0))

p1

  gta_plot_saver(plot=p1,
               path = paste("tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".1 Top 10 instruments used by G20 for discrimination", sep=""))




