# bamsim.R
# Mark Hagemann
# 7/26/2017
# Analysis for BAM simulation

#' Things I need: 
#' - distribution of sd(logQ) across all stations
#'     - already done, see varlogQ.R 
#'     - Use qsdlm1 (relies on var(logW) only)
#'     - sd(logQ) = 0.7664 + 0.8872 sd(logW)
#'     - RMSE is 0.362

mean(varqdat$lqsd, na.rm = TRUE)
sd(varqdat$lqsd, na.rm = TRUE)


# mean log area as a function of mean log Q --------------

glimpse(xsdat)

xsstats <- xsdat %>% 
  group_by(xs) %>% 
  summarize(meanlogA = mean(log(area_m2), na.rm = TRUE),
            sdlogA = sd(log(area_m2), na.rm = TRUE),
            meanlogQ = mean(logQ, na.rm = TRUE),
            sdlogQ = sd(logQ, na.rm = TRUE))
pairs(xsstats[-1])

xslm1 <- lm(meanlogA ~ meanlogQ + sdlogQ, xsstats)
xslm2 <- lm(sdlogA ~ meanlogQ + sdlogQ, xsstats)

summary(xslm1)
summary(xslm2)

# xs area as function of mean xs area, log deviation in flow

xsdat2 <- xsdat %>%
  filter(q_m3s > 0) %>% 
  group_by(xs) %>% 
  transmute(logAdev = log(area_m2) - mean(log(area_m2), na.rm = TRUE),
            logQdev = log(q_m3s) - mean(log(q_m3s), na.rm = TRUE)) %>% 
  ungroup()

xsdat2 %>% 
  sample_n(40000) %>% 
  select(-xs) %>% 
  pairs

summary(xsdat2)

xslm3 <- lm(logAdev ~ logQdev, xsdat2)

summary(xslm3)


# b stats from logA stats -------------------------------------------------

bstats <- bdata %>% 
  group_by(xsname, xs) %>% 
  summarize(n = n(), bhat = getb(logW, logQ),
            meanlogA = mean(log(area_m2), na.rm = TRUE),
            sdlogA = sd(log(area_m2), na.rm = TRUE)) %>% 
  filter(bhat > -1, bhat < 2) %>% 
  filter(n > 10)

pairs(bstats[4:6])

ablm1 <- lm(bhat ~ meanlogA + sdlogA, bstats)
ablm2 <- lm(bhat ~ sdlogA, bstats)

#' logWc from logQ stats -- oh right, can't without AMHG data
#' But can get mean(logW) from logQ stats
#' 

wcdat <- xsdat %>% 
  filter(q_m3s > 0) %>% 
  group_by(xs) %>% 
  summarize(mean_logQ = mean(logQ),
            mean_logW = mean(logW))
plot(mean_logW ~ mean_logQ, wcdat)
wclm1 <- lm(mean_logW ~ mean_logQ, wcdat)
summary(wclm1)


# Covariance matrix for hydro variables -----------------------------------

cordat <- xsdat %>% 
  filter(q_m3s > 0) %>% 
  transmute(xs, 
            logQ = log(q_m3s),
            logA = log(area_m2),
            logW = log(w_m))
cordat %>% 
  group_by(xs) %>% 
  summarize(rhoqa = cor(logQ, logA),
            rhoqw = cor(logQ, logW),
            rhowa = cor(logW, logA)) %>% 
  summary()
  

# logW variance as function of logQ variance ------------------------------

xsstats <- xsdat %>% 
  group_by(xs) %>% 
  summarize(meanlogW = mean(log(w_m), na.rm = TRUE),
            sdlogW = sd(log(w_m), na.rm = TRUE),
            meanlogQ = mean(logQ, na.rm = TRUE),
            sdlogQ = sd(logQ, na.rm = TRUE))
pairs(xsstats[-1])

lm(sdlogW ~ sdlogQ + meanlogQ, xsstats) %>% summary
lm(sdlogW ~ sdlogQ, xsstats) %>% summary
