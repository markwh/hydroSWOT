# xsarea.R
# Mark Hagemann
# 10/31/2016
# cross-sectional area computation

# Area as a function of W
plot(area ~ logW, sample_n(xsdat, 10000), log = "y")

# Distribution of Ao
aos <- unique(xsdat$Ao)
hist(log10(aos))

# Ao as a function of W, dA statistics

aodat <- xsdat %>% 
  filter(n > 20) %>% 
  group_by(xs, xsname) %>% 
  summarize(lwbar = mean(logW),
            lwsd = sd(logW),
            sdabar = mean(sqrt(dA)),
            sdasd = sd(sqrt(dA)),
            lAo = log(Ao[1]),
            ha25 = dHdW(h_m, W = w_m, lwr_p = 0, upr_p = 0.25),
            ha50 = dHdW(h_m, W = w_m, lwr_p = .25, upr_p = .50),
            ha75 = dHdW(h_m, W = w_m, lwr_p = .5, upr_p = .75),
            n = n()) %>% 
  ungroup()
summary(aodat)

pairs(aodat[-1:-2])
aodat %>% 
  filter(n > 50) %>% 
  select(lwbar : ha75, n) %>% 
  pairs()

# model lAo ~ lwbar + lwsd

alm1 <- lm(lAo ~ lwbar + lwsd, data = filter(aodat))
summary(alm1)
AIC(alm1)
alm2 <- lm(lAo ~ lwbar, data = aodat)
summary(alm2)
AIC(alm2)

png(filename = "Ao_fit.png", width = 600, height = 300)
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
visreg::visreg(alm1)
par(op)
dev.off()

# Look at a single xs data

# xs1 <- sample(xsdat$xs, 1)
xs1 <- filter(aodat, n > 50)$xs[which.min(filter(aodat, n > 50)$ha75)]
ad1 <- filter(xsdat, xs == xs1)
with(ad1, dHdW(h_m, w_m, 0, 1))
plot(h_m ~ w_m, ad1)
ad1$h_m


# incorporating work done in reports/heightAnomalies.Rmd

cstats$cpstat[1:50] %>% plot(log = "y")
fromcache("cstats")
badHstas <- cstats$xs[1:25]

aodat_nobad <- xsdat %>% 
  filter(!(xs %in% badHstas),
         n > 20) %>% 
  group_by(xs, xsname) %>% 
  summarize(lwbar = mean(logW),
            lwsd = sd(logW),
            sdabar = mean(sqrt(dA)),
            sdasd = sd(sqrt(dA)),
            lAo = log(Ao[1]),
            # ha25 = dHdW(h_m, W = w_m, lwr_p = 0, upr_p = 0.5),
            ha50 = dHdW(h_m, W = w_m, lwr_p = 0, upr_p = .50),
            ha75 = dHdW(h_m, W = w_m, lwr_p = .5, upr_p = .75),
            n = n()) %>% 
  ungroup()

summary(aodat_nobad)

pairs(aodat_nobad[-1:-2])

head(arrange(aodat_nobad, ha25))

aodat_nobad[which.min(aodat_nobad$ha25),]

plothw(2237700)

# doesn't look like dh/dw tells us anything.


