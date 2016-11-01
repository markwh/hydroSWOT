# maps.R
# Mark Hagemann
# 10/31/2016
# mapping hydroswot data

# Where do these come from?

blocs <- hswot %>% 
  transmute(lat = dec_lat_va,
            lon = dec_long_va,
            xs = site_no) %>% 
  unique() %>% 
  left_join(bhats, by = "xs") %>% 
  filter(!is.na(n))

library(leaflet)

blocs %>% 
  sample_n(400) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~log(n))
dim(blocs)
summary(blocs)
