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


# F1: the total value of exports from China to the USA affected by all trade distortions imposed by the USA in a given year
all.us.chn=data.frame(implementer=character(),
                     affected=character(),
                     year=character(),
                     trade.value=numeric()
)

for(yr in 2009:2018){
  # prep
  all.tc=data.frame(implementer=c("United States of America", "China"),
             affected=c("China","United States of America"),
             year=yr,
             trade.value=NA
  )



  yr.start=paste(yr, "-01-01",sep="")
  yr.end=paste(yr, "-12-31",sep="")

  trade.yr=subset(trade, year==(yr-1))
  trade.yr$year=NULL
  names(trade.yr)=c("i.un","a.un","affected.product","trade.value")

  for(i in 1:2){

      gta_data_slicer(gta.evaluation = c("Red", "Amber"),
                    affected.flows = "inward",
                    implementation.period = c(yr.start, yr.end),
                    implementing.country = as.character(all.tc$implementer[i]),
                    keep.implementer = T,
                    affected.country = as.character(all.tc$affected[i]),
                    keep.affected = T,
                    keep.others=F,
                    keep.implementation.na = F,
                    in.force.today = "any"
    )

    all.tc.i=master.sliced
    rm(master.sliced)

    if(nrow(all.tc.i)>0){
    all.tc.i=cSplit(all.tc.i, which(names(all.tc.i)=="affected.product"), direction="long", sep=",")
    all.tc.i=merge(all.tc.i, trade.yr, by=c("i.un","a.un","affected.product"), all.x=T)
    all.tc.i$trade.value[is.na(all.tc.i$trade.value)]=0

    all.tc$trade.value[i]=sum(unique(all.tc.i[,c("affected.product","trade.value")])$trade.value)
    }
  }
  all.tc[is.na(all.tc)]=0
  all.us.chn=rbind(all.us.chn, all.tc)

  print(yr)
}


# F2: Like F1 but for US exports to China.
all.us.chn.xlsx=all.us.chn
all.us.chn.xlsx=reshape(all.us.chn.xlsx, idvar=c("implementer", "affected"), timevar = "year", direction="wide")
names(all.us.chn.xlsx)=gsub("trade.value.","",names(all.us.chn.xlsx))

write.xlsx(all.us.chn.xlsx, file=paste("0 report production/GTA 23/tables & figures/",output.path,"/Total US-CHN trade affected by all import barriers.xlsx", sep=""), row.names=F)

