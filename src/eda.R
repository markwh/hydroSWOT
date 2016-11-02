
summary(hswot)

# How many datasets represented?

length(unique(hswot$site_no))

# What's the distribution on n per dataset?

hswot %>% 
  group_by(site_no) %>% 
  summarize(n = n()) %>% 
  `[[`("n") %>% 
  log10() %>% 
  hist()

# How many timezones per site?
hswot %>% 
  group_by(site_no) %>% 
  summarize(ntz = length(unique(q_meas_td))) %>% 
  arrange(desc(ntz))
hswot %>% 
  filter(site_no == 3081500) %>% 
  select(q_meas_no : q_va)
  # glimpse()
  # select(1:10)

# Some stations have ill-behaved stage measurements. 
hswot %>% 
  filter(site_no == 4232730) %>% 
  glimpse

library(dataRetrieval)
seneca <- readNWISuv(siteNumbers = "04232730", parameterCd = "00065", startDate = "2009-07-27", endDate = "2009-08-01")
plot(seneca$X_00065_00011)

hswot %>% 
  group_by(site_no) %>% 
  summarize(hrange = max(stage_va) - min(stage_va), n = n()) %>% 
  arrange(desc(hrange)) %>% 
  print(n = 40)

# uses xsdat from xsarea.R

ploth <- function(siteno, ...) {
  hswot %>% 
    filter(site_no == siteno) %>% 
    # `[[`("stage_va") %>% 
    plot(stage_va ~ datetime, ., main = station_nm[1], 
         xlab = "Date", ylab = "Stage (ft)", ...)
}

plothw <- function(siteno, ...) {
  xsdat %>% 
    filter(xs == siteno) %>% 
    # `[[`("stage_va") %>% 
    plot(h_m ~ w_m, ., main = xsname[1], 
         xlab = "Width (m)", ylab = "Stage (m)", ...)
}

plothw(9529300)

# Trying anomalyDetection package
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
?AnomalyDetectionTs

adet <- function(x) try(AnomalyDetectionTs(x, plot = TRUE))
hsanoms <- hswot %>% 
  select(datetime, stage_va) %>% 
  split(hswot$site_no) %>% 
  lapply(adet)

sapply(hsanoms, is, "try-error") %>% `!` %>% sum
