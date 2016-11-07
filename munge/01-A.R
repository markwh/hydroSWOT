# Reading in data

hswot <- read.csv("data/SWOT_ADCP_Dataset.txt", sep = "\t") %>% 
  mutate(q_meas_td = fixtzs(q_meas_td)) %>% 
  group_by(q_meas_td) %>% 
  mutate(datetime = mdy_hms(q_meas_dt, tz = q_meas_td[1])) %>% 
  ungroup() %>% 
  group_by(site_no) %>% 
  arrange(datetime) %>% 
  ungroup()

fieldref <- read.csv("data/FieldDefinitions.txt", sep = "\t")
fr <- function(field)
  dplyr::filter(fieldref, grepl(field, Field))

swotcols <- c("dec_lat_va", "dec_long_va", "drain_area_va", 
              "contrib_drain_area_va", "alt_va", "alt_datum_cd",
              "alt_va", "alt_datum_va", "alt_datum_cd", "q_meas_dt",
              "q_meas_td", "stream_width_va")

unkcols <- c("q_meas_no", "q_va", "stag_va", "stage_diff_va", "stage_diff_du",
             "mean_idx_vel_va", "std_second_area_va")


### Data for inspecting cross-sections

xsdat <- hswot %>%  
  filter(!is.na(xsec_area_va),
         xsec_area_va > 0) %>%
  transmute(datetime = datetime,
            q_m3s = q_va * 0.028317,
            w_m = stream_wdth_va / 3.28084,
            logQ = log(q_m3s),
            logW = log(w_m),
            h_m = stage_va / 3.28084,
            xs = site_no, 
            xsname = station_nm, 
            area = xsec_area_va) %>% 
  # filter(w_m > 0, q_m3s > 0, !is.na(h_m)) %>% 
  filter(!is.na(h_m)) %>%
  tbl_df() %>% 
  group_by(xs) %>% 
  mutate(Ao = min(area),
         dA = area - Ao, 
         n = n()) %>% 
  ungroup()
