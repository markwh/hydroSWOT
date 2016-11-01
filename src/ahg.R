# ahg.R
# Mark Hagemann
# 10/31/2016
# computing AHG relationships


# simple data frame for computing ahg
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

# distribution of AHG errors
ahgMods <- bdata %>% 
  group_by(xsname)

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
