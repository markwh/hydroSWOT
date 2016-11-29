# varlogQ.R
# Get hyperpriors on variance of logQ


# Hyperprior on var(logQ)
glimpse(varqdat)
pairs(varqdat[3:9]) # lwsd, maybe. But need to check log-transform madness.

plot(lqsd ~ lwsd, varqdat)
plot(lqsd^2 ~ lwsd^2, varqdat)
plot(lqsd ~ lwsd, varqdat, log = "xy")
plot(lqsd^2 ~ lwsd^2, varqdat, log = "xy")
plot(lqsd ~ orthog(hsd, lwsd), varqdat)


qsdlm1 <- lm(lqsd ~ lwsd, varqdat)
summary(qsdlm1)

qsdlm2 <- lm(lqsd ~ lwsd + hsd, varqdat)
summary(qsdlm2)
visreg::visreg(qsdlm2)

qsdlm3 <- lm(lqsd ~ lwsd + orthog(hsd, lwsd), varqdat)
summary(qsdlm3)

qsdlm4 <- lm(lqsd ~ (lwsd + hsd)^2, varqdat)
summary(qsdlm4)

qsdlm5 <- lm(lqsd ~ poly(lwsd, 2) + poly(hsd, 2), varqdat)
summary(qsdlm5)


AIC(qsdlm1)
AIC(qsdlm2)
AIC(qsdlm5)
gcv(qsdlm1)
sqrt(gcv(qsdlm2)) # This is RMSE of prediction -- becomes prior sd. 
gcv(qsdlm5)

visreg::visreg(qsdlm3)
visreg::visreg(qsdlm5)

## Save the best model

qsdlm <- qsdlm2
cache("qsdlm")

png(filename = "qsd_fit.png", width = 600, height = 300)
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
visreg::visreg(qsdlm2)
par(op)
dev.off()