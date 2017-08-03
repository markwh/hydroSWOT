# ahg.R
# Mark Hagemann
# 10/31/2016
# computing AHG relationships

# b 
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

gcvs <- lapply(ahgMods, gcv)
vars <- lapply(ahgMods, function(mod) var(mod$model$logW))
q2s <- mapply(function(x, y) data.frame(Q2 = 1 - x / y), 
              x = gcvs, y = vars, SIMPLIFY = FALSE) %>% 
  bind_rows(.id = "site_no")
hist(q2s$Q2)
arrange(q2s, desc(Q2)) %>% 
  head()

# Get standard deviation
dim(ahgMods.a)
hist(ahgMods.a$.resid, xlim = c(-1, 1), breaks = 200)
car::qqPlot(ahgMods.a$.resid) # heavy tails
qqplot(qcauchy(ppoints(500)), ahgMods.a$.resid)

# fit a cauchy distribution
fitdist(ahgMods.a$.resid, distr = "cauchy")
qqplot(qcauchy(ppoints(500), location = -0.0008, scale = 0.069258), ahgMods.a$.resid)
qqline(ahgMods.a$.resid, distribution = function(p) qcauchy(p, location = -0.0008, scale = 0.069258))

## Summary
ahgMods.sum <- ahgMods.a %>% 
  group_by(station) %>% 
  summarize(lwsd = sd(.resid))

wsddat <- ahgMods.sum %>% 
  transmute(xs = as.numeric(station),
            sdLogW.r = lwsd) %>% 
  inner_join(aodat_nobad, by = "xs") %>% 
  dplyr::select(sdLogW.r, lwbar:ha75)
pairs(wsddat)

plot(sdLogW.r ~ lwsd, wsddat, log = "xy")
abline(0, 1)

# is b affected by things we can measure?

bpreddat <- bhats %>% 
  ungroup() %>% 
  dplyr::select(xs, bhat) %>% 
  inner_join(aodat_nobad, by = "xs") %>% 
  dplyr::select(bhat, lwbar:ha75, -lAo, n)
pairs(bpreddat)
summary(bpreddat)
cache("bpreddat")

# Yes! lwsd. 

blm1 <- lm(bhat ~ lwsd, bpreddat)
summary(blm1)
AIC(blm1)

blm2 <- lm(bhat ~ poly(lwsd, degree = 2), bpreddat)
summary(blm2)
gcv(blm2)
AIC(blm2)

blm <- blm1
blm$data <- bpreddat
cache("blm")

png(filename = "b_fit.png", width = 600, height = 300)
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
visreg::visreg(blm2)
par(op)
dev.off()
