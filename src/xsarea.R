# xsarea.R
# Mark Hagemann
# 10/31/2016
# cross-sectional area computation


makedate <- function(dmy_hms, tz) {
  mapply()
}
xsdat <- hswot %>%  
  transmute(datetime = datetime,
            q_m3s = q_va * 0.028317,
            w_m = stream_wdth_va / 3.28084,
            logQ = log(q_m3s),
            logW = log(w_m),
            xs = site_no, 
            xsname = station_nm, 
            area = xsec_area_va) %>% 
  filter(w_m > 0, q_m3s > 0) %>% 
  tbl_df() %>% 
  group_by(xs) %>% 
  mutate(Ao = min(area),
         dA = area - Ao) %>% 
  ungroup()

glimpse(xsdat)


# Area as a function of W

plot(area ~ logW, sample_n(xsdat, 10000), log = "y")


# Add dA, Ao

