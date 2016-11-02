
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
  glimpse()
  select(1:10)
