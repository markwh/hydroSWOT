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
  plot(xlim = c(0, 1))

# Where do these come from?
trimb <- filter(bhats, bhat > 0, bhat < 1)
meanb <- mean(trimb$bhat)
varb <- var(trimb$bhat)

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

# distribution of AHG errors
ahgMods <- bdata %>% 
  group_by(xsname)


# distribution of Ao as function of Wo, H'(Wo) 
ahgMods <- bdata %>% 
  group_by(xsname, xs) %>%
  mutate(n = n()) %>% 
  ungroup() %>% 
  dplyr::filter(n > 10) %>%
  split(f = .$xs) %>% 
  lapply(lm, formula = logW ~ logQ)

ahgMods.a <- lapply(ahgMods, augment) %>% 
  bind_rows(.id = "station") %>% 
  mutate(Q = exp(logQ), W = exp(logW), 
         What = exp(.hat),
         Wresid = W - What)




ahgMods.t <- lapply(ahgMods, tidy) %>% 
  bind_rows(.id = "station") %>% 
  select(station, term, estimate) %>%
  spread(key = term, value = estimate) %>% 
  setNames(station, a, b)
  tbl_df()

# Get standard deviation
dim(ahgMods.a)
hist(ahgMods.a$.resid, xlim = c(-1, 1), breaks = 200)
car::qqPlot(ahgMods.a$.resid) # heavy tails
qqplot(qcauchy(ppoints(500)), ahgMods.a$.resid)

## Summary
ahgMods.sum <- ahgMods.a %>% 
  group_by(station) %>% 
  summarize(sdLogW = sd(.resid))
