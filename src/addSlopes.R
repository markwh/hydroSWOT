# addSlopes.R
# 11/21/2016

library(ProjectTemplate)
load.project()

gageloc <- read.dbf("data/GageLoc-GageLoc.dbf", as.is = TRUE) %>% 
  setNames(tolower(names(.))) %>% 
  transmute(reach = reachcode,
            gage = as.numeric(source_fea))

hs_reach <- hswot %>% 
  left_join(gageloc, by = c("xs" = "gage"))

slopes <- read.dbf("data/elevslope.dbf") %>% 
  setNames(tolower(names(.))) %>% 
  transmute(reach = reachcode,
            slope = slope)


flowline <- read.dbf("data/NHDPlusMS/NHDPlus07/NHDPlusAttributes/PlusFlowAR.dbf")

str(slopes, 1)
