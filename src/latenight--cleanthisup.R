# just trying something.

head(hswot)

hswot %>% 
  sample_n(10000) %>% 
  plot(logQ ~ logW, .)


hsdat <- hswot %>% 
  filter(w_m > 0, q_m3s > 0) %>% 
  group_by(xs) %>% 
  mutate(sdW = sd(logW),
         sdh = sd(h_m),
         n = n()) %>% 
  ungroup() %>% 
  filter(n > 30) %>% 
  na.omit()

lm1 <- lm(logQ ~ logW, hsdat)
summary(lm1)

lm2 <- lm(logQ ~ logW + sdW + sdh, hsdat)
summary(lm2)


library(streamstats)
ws1 <- delineateWatershed(xlocation = -72.9249, ylocation = 42.3170, crs = 4326,
                          includeparameters = "true")

library(foreign)
foo <- read.dbf("data/elevslope.dbf")
str(foo, 1)
install.packages("mdb-tools")
install.packages("Hmisc")
library("Hmisc")
bar <- mdb.get("data/USGS_Streamgages-NHD_Locations.mdb") # need mdbtools gnu package
str(bar, 1)
bar$gages$SITE.NO %>% head
bar$gages$NHD2GAGE.D %>% head
bar$gages %>% glimpse
class(bar$gages$OBJECTID)
glimpse(bar$gages_Shape_Index) # no cid here!


bar2 <- read.dbf("data/GageLoc-GageLoc.dbf")
str(bar2, 1)

bar2 %>% 
  setNames(tolower(names(.))) %>% 
  select(comid, reachcode, source_fea) %>% 
  summary() # Now we're cookin'.


