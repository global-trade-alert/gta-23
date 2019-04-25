library("xlsx")
library("gtalibrary")
rm(list = ls())


## setup
#setwd("0 report production/GTA 23")
# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")



path="0 report production/GTA 23/tables & figures/annex - p. 1 - title tables/"


chapters=c("D","E","F","G","I","L","M","P","TARIFF","X")

for(cty in g20.member.names){
  if(cty=="South Korea"){cty="Republic of Korea"}
    gta_trade_coverage(coverage.period = c(2009,2018),
                     gta.evaluation = c("red","amber"),
                     exporters=cty,
                     keep.exporters = T,
                     implementer.role = c("importer","3rd country"),
                     mast.chapters = chapters,
                     keep.mast = T,
                     group.mast = F,
                     in.force.today="Any",
                     output.path = paste(path,cty, ".xlsx",sep=""))
  print(cty)
}


