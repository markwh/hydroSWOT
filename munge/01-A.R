# Reading in data

hswot <- read.csv("data/SWOT_ADCP_Dataset.txt", sep = "\t")
fieldref <- read.csv("data/FieldDefinitions.txt", sep = "\t")
fr <- function(field)
  dplyr::filter(fieldref, grepl(field, Field))

swotcols <- c("dec_lat_va", "dec_long_va", "drain_area_va", 
              "contrib_drain_area_va", "alt_va", "alt_datum_cd",
              "alt_va", "alt_datum_va", "alt_datum_cd", "q_meas_dt",
              "q_meas_td", "stream_width_va")

unkcols <- c("q_meas_no", "q_va", "stag_va", "stage_diff_va", "stage_diff_du",
             "mean_idx_vel_va", "std_second_area_va")

# get distribution of b
bdata <- hswot %>% 
  transmute(q_m3s = q_va * 0.028317,
            w_m = stream_wdth_va / 3.28084,
            logQ = log(q_m3s),
            logW = log(w_m),
            xs = site_no, 
            xsname = station_nm) %>% 
  filter(w_m > 0, q_m3s > 0) %>% 
  tbl_df()

getb <- function(logW, logQ)
  cor(logW, logQ) * sd(logW) / sd(logQ)

bhats <- bdata %>% 
  group_by(xsname, xs) %>% 
  summarize(n = n(), bhat = getb(logW, logQ)) %>% 
  filter(bhat > -1, bhat < 2) %>% 
  filter(n > 10)

bhats %>% 
  `[[`("bhat") %>% 
  # hist() %>%
  density() %>% 
  plot()

# Where do these come from?

blocs <- hswot %>% 
  transmute(lat = dec_lat_va,
            lon = dec_long_va,
            xs = site_no) %>% 
  left_join(bhats, by = "xs")

library(leaflet)

blocs %>% 
  sample_n(40) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~n * 10 / mean(n))

# distribution of AHG errors

# distribution of Ao as function of Wo, H'(Wo) 



