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

# AHG strength

ahgMapData <- q2s %>% 
  mutate(site_no = as.numeric(site_no)) %>% 
  left_join(blocs, by = c("site_no" = "xs")) %>%
  sample_n(100)

pal <- colorQuantile(
  palette = "Blues",
  domain = ahgMapData$Q2,
  n = 7
)

leaflet(ahgMapData) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~Q2 * 10, 
                   color = ~pal(Q2), 
                   popup = ~format(Q2, digits = 3, scientific = FALSE),
                   stroke = FALSE, fillOpacity = 0.5)

  
  
  
  df <- read.csv(textConnection(
    "Name,Lat,Long
    Samurai Noodle,47.597131,-122.327298
    Kukai Ramen,47.6154,-122.327157
    Tsukushinbo,47.59987,-122.326726"
  ))
  
  leaflet(df) %>% addTiles() %>%
    addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name))