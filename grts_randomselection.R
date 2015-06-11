#Create stratified random points for wetland analysis using grts

library(spsurvey)
library(sp)

att = read.dbf("wet_and_sanswetland_cmp")
att$poly_type = as.character(att$poly_type)
att$poly_type[is.na(att$poly_type)] = "dry"
att$poly_type = as.factor(att$poly_type)

design = list("wetland"=list(panel=c(Panel=2000),seltype="Equal"),
              "mosaic"=list(panel=c(Panel=2000),seltype="Equal"),
              "dry"=list(panel=c(Panel=2000),seltype="Equal"))
grts(design, type.frame = "area", in.shape="wet_and_sanswetland_cmp", stratum="poly_type", att.frame = att)
