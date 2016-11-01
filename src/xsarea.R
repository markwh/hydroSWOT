# xsarea.R
# Mark Hagemann
# 10/31/2016
# cross-sectional area computation

xsdat <- hswot %>%  
  transmute(q_m3s = q_va * 0.028317,
            w_m = stream_wdth_va / 3.28084,
            logQ = log(q_m3s),
            logW = log(w_m),
            xs = site_no, 
            xsname = station_nm, 
            area = xsec_area_va) %>% 
  filter(w_m > 0, q_m3s > 0) %>% 
  tbl_df()

