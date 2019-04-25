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
library("gridExtra")
library("gtalibrary")

rm(list = ls())

loadfonts(device="postscript")
loadfonts(device="win")
#setwd("GTA cloud/0 report production/GTA 23")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23/")


#Settings
chapter.number=12
chapter.name="Which sectors hit"
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

implemented$phase<-"pre-2018"
implemented$phase[implemented$date.implemented>=break.date]<-"2018"

implemented$implementing.jurisdiction<-as.character(implemented$implementing.jurisdiction)
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="United Kingdom of Great Britain and Northern Ireland"]<-"United Kingdom"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="United States of America"]<-"United States"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="Russian Federation"]<-"Russia"
implemented$implementing.jurisdiction[implemented$implementing.jurisdiction=="Republic of Korea"]<-"South Korea"


implemented.splitted <- cSplit(implemented, which(colnames(implemented)=="affected.sector"), direction="long", sep=", ")

sect.2digit <-subset(implemented.splitted, affected.sector<100)
sect.3digit <-subset(implemented.splitted, affected.sector>99)
sect.2digit$affected.sector <- substr(sect.2digit$affected.sector, 1,1)
sect.3digit$affected.sector <- substr(sect.3digit$affected.sector, 1,2)
implemented.splitted <- rbind(sect.2digit, sect.3digit)
implemented.splitted$affected.sector <- as.numeric(implemented.splitted$affected.sector)
rm(sect.2digit, sect.3digit)
implemented.splitted <- as.data.frame(unique(implemented.splitted))

sector.hits<-aggregate(intervention.id ~affected.sector, data=implemented.splitted, function(x) length(unique(x)))
setnames(sector.hits, old="affected.sector", new="cpc")
sector.hits<-subset(sector.hits, ! cpc %in% c(0,10,20,30,40,50,60,70,80,90))

sector.hits$rank<-rank(-sector.hits$intervention.id, ties.method="first")
# sector.hits$rank[sector.hits$protect==0]<-rank(-sector.hits$intervention.id[sector.hits$protect==0], ties.method="first")


sector.hits<-aggregate(intervention.id ~affected.sector + phase, data=subset(implemented.splitted, affected.sector %in% unique(sector.hits$cpc[sector.hits$rank<11])), function(x) length(unique(x)))
setnames(sector.hits, old="affected.sector", new="cpc")
sector.hits<-subset(sector.hits, ! cpc %in% c(0,10,20,30,40,50,60,70,80,90))

cpc<-read.xlsx(file="../../R help files/cpc_titles.xlsx", 1)
sector.hits$protect<-NULL
setnames(sector.hits, old="intervention.id", new="Number.of.hits")
sector.hits<-merge(sector.hits, cpc, by="cpc", all.x=T)
setnames(sector.hits, old="description", new="sector.title")
setnames(sector.hits, old="cpc", new="CPC number")

sector.hits$sector.title <- as.character(sector.hits$sector.title)
sector.hits$sector.title[sector.hits$sector.title=="Electricity, town gas, steam and hot water"]<-"Electricity, gas etc."
sector.hits$sector.title[sector.hits$sector.title=="Meat, fish, fruits, vegetables, oils and fats"]<-"Agricultural products"
sector.hits$sector.title[sector.hits$sector.title=="Other chemical products; man-made fibres"]<-"Other chemicals"
sector.hits$sector.title[sector.hits$sector.title=="Fabricated metal products, except machinery and equipment"]<-"Fabricated\nmetal products"
sector.hits$sector.title[sector.hits$sector.title=="Electrical machinery and apparatus"]<-"Electrical machinery"
sector.hits$sector.title[sector.hits$sector.title=="Special-purpose machinery"]<-"Special-purpose\nmachinery"
sector.hits$sector.title[sector.hits$sector.title=="General-purpose machinery"]<-"General-purpose\nmachinery"
sector.hits$sector.title[sector.hits$sector.title=="Products of agriculture, horticulture and market gardening"]<-"Agricultural\nproducts"

sector.hits$sector.title <- as.factor(sector.hits$sector.title)

top=sector.hits
top=top[order(-top$Number.of.hits),]
unique(top$sector.title)

sector.hits$sector.title <- factor(sector.hits$sector.title, levels = unique(top$sector.title))


write.xlsx(sector.hits, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".1  - Data for Figure 12.1.xlsx", sep=""), row.names=F)

entires <- aggregate(Number.of.hits ~ sector.title, sector.hits, function(x) sum(x))
sector.hits <- merge(sector.hits, entires, by="sector.title", all.x = T)
setnames(sector.hits, "Number.of.hits.y", "Total")
setnames(sector.hits, "Number.of.hits.x", "Number.of.hits")
sector.hits$position[sector.hits$phase=="pre-2018"] <- 40
sector.hits$position[sector.hits$phase!="pre-2018"] <- sector.hits$Total[sector.hits$phase!="pre-2018"]+40

sector.hits <- sector.hits[with(sector.hits, order(-Total)),]
row.names(sector.hits) <- NULL

p1 <- ggplot() +
  geom_bar(data=sector.hits, aes(x=forcats::fct_inorder(sector.title), y = Number.of.hits, fill=phase), stat="identity", width=0.8)+
  geom_text(data= subset(sector.hits, phase=="pre-2018"), aes(x=sector.title, y = position, label = Number.of.hits, family=""), size = 3.5, colour="#FFFFFF") +
  geom_text(data= subset(sector.hits, phase=="2018"), aes(x=sector.title, y = position, label = Number.of.hits, family=""), size = 3.5, colour=gta_colour$turquoise[1]) +
  scale_fill_manual(labels=c("2018", "pre-2018"), values=c(gta_colour$qualitative[c(6,1)])) +
  scale_y_continuous(breaks=seq(0,1500,200), expand = c(0.0,0.0), limits = c(0,1300), sec.axis = sec_axis(~.,name="Number of times a sector has been\nhit by G20 protectionist policy\ninterventions since November 2008",breaks=seq(0,1500,200))) +
  ylab("Number of times a sector has been\nhit by G20 protectionist policy\ninterventions since November 2008")+
  xlab("")+
  # scale_x_discrete(limits= unique(top$sector.title))+
  gta_theme(x.bottom.angle=45) +
  theme(axis.text.x.bottom = element_text(hjust=0.9),
        legend.box.margin = margin(t=-20))

p1

gta_plot_saver(plot=p1,
               path = paste("tables & figures/",output.path,"/", sep=""),
               name = paste("Figure ",chapter.number,".1 - Top 10 sectors hit by G20", sep=""))



## Figure 12-2: Tile G20 sectoral hits
sector.hits<-aggregate(intervention.id ~affected.sector, data=implemented.splitted, function(x) length(unique(x)))
setnames(sector.hits, old="affected.sector", new="cpc")
sector.hits<-subset(sector.hits, ! cpc %in% c(0,10,20,30,40,50,60,70,80,90))
sector.hits$rank<-rank(-sector.hits$intervention.id, ties.method="first")

sector.hits<-aggregate(intervention.id ~affected.sector + phase, data=subset(implemented.splitted, affected.sector %in% unique(sector.hits$cpc[sector.hits$rank<11])), function(x) length(unique(x)))
setnames(sector.hits, old="affected.sector", new="cpc")
sector.hits<-subset(sector.hits, ! cpc %in% c(0,10,20,30,40,50,60,70,80,90))

setnames(sector.hits, old="intervention.id", new="Number.of.hits")
sector.hits<-merge(sector.hits, cpc, by="cpc", all.x=T)
setnames(sector.hits, old="description", new="sector.title")
setnames(sector.hits, old="cpc", new="CPC number")

sector.hits$sector.title <- as.character(sector.hits$sector.title)
sector.hits$sector.title[sector.hits$sector.title=="Fabricated metal products, except machinery and equipment"]<-"Metal products"
sector.hits$sector.title[sector.hits$sector.title=="Meat, fish, fruits, vegetables, oils and fats"]<-"Meat, fruits, & vegetables"
sector.hits$sector.title[sector.hits$sector.title=="Other chemical products; man-made fibres"]<-"Other chemicals"
sector.hits$sector.title[sector.hits$sector.title=="Products of agriculture, horticulture and market gardening"]<-"Agricultural products"
sector.hits$sector.title[sector.hits$sector.title=="Grain mill products, starches and starch products; other food products"]<-"Grain mill products"
sector.hits$sector.title[sector.hits$sector.title=="Products of agriculture, horticulture and market gardening"]<-"Agricultural\nproducts"
sector.hits$sector.title <- as.factor(sector.hits$sector.title)




gta.jurisdiction.group <- read.csv(file = "../../data/database replica/gta_jurisdiction_group.csv")
gta.jurisdiction.group <- subset(gta.jurisdiction.group, id %in% c(6,21,10,11,12,13,14,15))
setnames(gta.jurisdiction.group, old = "id", new = "jurisdiction_group_id")
gta.jurisdiction.group.member <- read.csv(file = "../../data/database replica/gta_jurisdiction_group_member.csv")
gta.jurisdiction.group.member <- subset(gta.jurisdiction.group.member, jurisdiction_group_id %in% c(6,21,10,11,12,13,14,15))
gta.jurisdiction.group <- merge(gta.jurisdiction.group, gta.jurisdiction.group.member, by="jurisdiction_group_id", all.x=T)
gta.jurisdiction <- read.csv(file = "../../data/database replica/gta_jurisdiction.csv")
setnames(gta.jurisdiction, old = "id", new = "jurisdiction_id")
gta.jurisdiction <- subset(gta.jurisdiction, jurisdiction_id %in% gta.jurisdiction.group$jurisdiction_id)
gta.jurisdiction.group <- merge(gta.jurisdiction.group, gta.jurisdiction, by="jurisdiction_id", all.x=T)
gta.jurisdiction.group <- subset(gta.jurisdiction.group, select = c("name.x", "un_code"))
setnames(gta.jurisdiction.group, old = "name.x", new = "country.group")
setnames(gta.jurisdiction.group, old = "un_code", new = "i.un")
rm(gta.jurisdiction, gta.jurisdiction.group.member)


# correcting for double memberships
gta.jurisdiction.group=subset(gta.jurisdiction.group, ! (country.group=="Europe and Central Asia" & i.un %in% subset(gta.jurisdiction.group, country.group=="EU-28")$i.un))
gta.jurisdiction.group=subset(gta.jurisdiction.group, ! (country.group!="EU-28" & i.un==470))
## should be NULL:
nrow(subset(as.data.frame(table(gta.jurisdiction.group$i.un)), Freq>1)$Var1)


implemented.splitted <- merge(implemented.splitted, gta.jurisdiction.group, by="i.un", all.x=T)

sec.tile<-aggregate(intervention.id ~ country.group + affected.sector, data=subset(implemented.splitted, affected.sector %in% unique(sector.hits$'CPC number')), function(x) length(unique(x)))
setnames(sec.tile, old="affected.sector", new="cpc")

sec.tile$description="lalala"
sec.tile$description[sec.tile$cpc==1]="Products of agriculture"
sec.tile$description[sec.tile$cpc==21]="Meat, fish, fruits,\nvegetables, oils and fats"
sec.tile$description[sec.tile$cpc==23]="Grain mill products\nand starches"
sec.tile$description[sec.tile$cpc==34]="Basic chemicals"
sec.tile$description[sec.tile$cpc==35]="Man-made fibres,\nother chemicals"
sec.tile$description[sec.tile$cpc==41]="Basic metals"
sec.tile$description[sec.tile$cpc==42]="Fabricated metal\nproducts"
sec.tile$description[sec.tile$cpc==43]="General-purpose\nmachiner"
sec.tile$description[sec.tile$cpc==44]="Special-purpose\nmachinery"
sec.tile$description[sec.tile$cpc==49]="Transport\nequipment"
sec.tile$description[sec.tile$cpc==17]="Electricity & Gas"
sec.tile$description[sec.tile$cpc==46]="Electrical machinery"


#layout the names
sec.tile$description<-as.character(sec.tile$description)
# sec.tile$description[sec.tile$description=="Agricultural products"]<-"Agricultural\nproducts"


limit<-200
st<-sec.tile
st$intervention.id[st$intervention.id>limit]<-limit

st$country.group=as.character(st$country.group)
st$country.group[st$country.group=="Latin America and the Caribbean"]="Latin America and\nthe Caribbean"
st$country.group[st$country.group=="Middle East and North Africa"]="Middle East and\nNorth Africa"
st$country.group[st$country.group=="Europe and Central Asia"]="Europe and\nCentral Asia"


st$imp<-paste(" ", st$country.group)

write.xlsx(st, file=paste("tables & figures/",output.path,"/Table ",chapter.number,".2  - Data for Figure 12.2.xlsx", sep=""), row.names=F)

(
  p1 <- ggplot(st, aes(y=imp, x=description)) +
    geom_tile(aes(fill=intervention.id), colour="white") +
    scale_fill_gradient(limits=c(0,limit), low="white", high=gta_colour$red[1], na.value="white", name=paste("Protectionist   \ninterventions   \nimplemented   \nagainst sector   \n"))+
    gta_theme(x.bottom.angle = 45)+
    coord_fixed(ratio=.85)+
    theme(panel.background = element_blank(), panel.border=element_rect(0, colour="grey"), legend.position="right", axis.text.x.bottom = element_text(hjust=1))+
    labs(x="Sector", y="Region")
)


# draw it
png(paste("tables & figures/",output.path,"/Figure ",chapter.number,".2 - Regional breakdown of harm done by G20 to 10 sectors of the world economy.png", sep=""), width=3000, height=2400, res=300)
p1
dev.off()

cairo_ps(paste("tables & figures/",output.path,"/Figure ",chapter.number,".2 - Regional breakdown of harm done by G20 to 10 sectors of the world economy.eps", sep=""), bg = "white", width=10, height=8, family="Open Sans")
p1
dev.off()
