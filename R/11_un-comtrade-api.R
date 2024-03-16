# 11 - A simple R package for the UN Comtrade API ----
# URL: https://stefan.angrick.me/a-simple-r-package-for-the-un-comtrade-api

# R Wikipedia: https://en.wikipedia.org/wiki/R_%28programming_language%29

# Using the UN Comtrade data API with R
# https://comtrade.un.org/data/Doc/api/ex/r

# UN Comtrade: https://comtradeplus.un.org/

# Sample code on their homepage: https://comtrade.un.org/data/Doc/api/ex/r

# Hilary Parker: Writing an R package from scratch: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

download.file(url = "https://media.portblue.net/resources/150429_uncomtrade-r-package/comtrade.tar.gz", destfile = "comtrade.tar.gz")
library(tools)
md5sum("comtrade.tar.gz")

# MD5 checksum Wikipedia: https://en.wikipedia.org/wiki/MD5


install.packages("comtrade.tar.gz", repos = NULL, type = "source")
library(comtrade)
(s1 <- get.Comtrade(r = "842", p = "124,484"))

# END