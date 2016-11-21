# xsarea.R
# Mark Hagemann
# 10/31/2016
# cross-sectional area computation

# Area as a function of W
plot(area ~ logW, sample_n(xsdat, 10000), log = "y")

# Distribution of Ao
aos <- unique(xsdat$Ao)
hist(log10(aos))


pairs(aodat[-1:-2])
aodat %>% 
  filter(n > 50) %>% 
  select(lwbar : ha75, n) %>% 
  pairs()

# model lAo ~ lwbar + lwsd
alm1 <- lm(lAo ~ lwbar + lwsd, data = aodat)
summary(alm1)
AIC(alm1)
alm2 <- lm(lAo ~ lwbar, data = aodat)
summary(alm2)
AIC(alm2)

## Save model 

alm <- alm1
cache("alm")

## Save plot
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

summary(aodat_nobad)
pairs(aodat_nobad[-1:-2])
head(arrange(aodat_nobad, ha25))
aodat_nobad[which.min(aodat_nobad$ha25),]
plothw(2237700)

# doesn't look like dh/dw tells us anything.


