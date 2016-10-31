# pullData.R
# Mark Hagemann
# 10/31/2016
# Obtaining data from archive on web. Populates data/ folder as expected in src files.

temp <- tempfile()
url <- "https://www.sciencebase.gov/catalog/file/get/57435ae5e4b07e28b660af55"
download.file(url, temp, mode = "wb")
con <- unzip(temp, exdir = "data")
