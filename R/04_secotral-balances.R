# 04 - Visualise macroeconomic sectoral balances using R ----
# URL: https://stefan.angrick.me/visualise-macroeconomic-sectoral-balances-using-r


# ECB: https://www.ecb.europa.eu/home/html/index.en.html
# ECB Forum on Central Banking: https://www.ecb.europa.eu/pub/conferences/ecbforum/html/index.en.html

# Cian Allen Paper: https://www.ecb.europa.eu/pub/conferences/ecbforum/YE_competition//shared/pdf/2018/EFCB_2018_Cian_Allen_Paper.en.pdf
# Angrick BOJ Paper: https://grips.repo.nii.ac.jp/records/1522

# Michael Pettis: https://carnegieendowment.org/chinafinancialmarkets/
# Martin Wolf Talk: https://carnegieendowment.org/chinafinancialmarkets/


# OECD Net lending/borrowing by sector: https://data.oecd.org/natincome/net-lending-borrowing-by-sector.htm
# OECD: https://www.oecd.org/
# OECD National Accounts at a Glance: https://stats.oecd.org/Index.aspx?DataSetCode=NAAG
# Eurostat: https://ec.europa.eu/eurostat
# Eurostat Financial transactions - quarterly data: https://ec.europa.eu/eurostat/web/products-datasets/product?code=nasq_10_f_tr
# Eurostat Non-financial transactions - quarterly data: https://ec.europa.eu/eurostat/web/products-datasets/product?code=nasq_10_nf_tr

# 04_sectors.R
# Eurostat: https://ec.europa.eu/eurostat/data/database
# Board of Governors FED: https://www.federalreserve.gov/releases/z1/current/html/s2a.htm
# St. Louis Fed: https://research.stlouisfed.org/
# BOJ: https://www.stat-search.boj.or.jp/index_en.html
# Cabinet Office: https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/toukei_top.html

# Capital account data: https://fred.stlouisfed.org/series/ADSLBAQ027S
# BOJ Flow of Funds: https://www.boj.or.jp/en/statistics/sj/index.htm

# Sectoral Balances: https://media.portblue.net/resources/181231_sectoral-balances/sectoral.balances.html


## Instructions for running the R script ----

### 1. ----
# OECD R package
# - CRAN: https://cran.r-project.org/package=OECD
# - GitHub: https://github.com/expersso/OECD

# eurostat R package
# - CRAN: https://cran.r-project.org/package=eurostat
# - GitHub: https://github.com/rOpenGov/eurostat
# - Web: https://ropengov.github.io/eurostat/

# pdfetch R package
# - CRAN: https://cran.r-project.org/package=pdfetch
# - GitHub: https://github.com/abielr/pdfetch

# estatapi R package
# - CRAN: https://cran.r-project.org/package=estatapi
# - GitHub: https://github.com/yutannihilation/estatapi
# - Web: https://yutannihilation.github.io/estatapi/

### 2. ----

# BOJ Time-Series Data Search: https://www.stat-search.boj.or.jp/index_en.html

# Data Selection by exact series codes: https://www.stat-search.boj.or.jp/ssi/cgi-bin/famecgi2?cgi=$nme_s050_en


### 3. ----

# Hiroaki Yutani: https://yutani.rbind.io/
# estatapi: https://github.com/yutannihilation/estatapi
# English README: https://github.com/yutannihilation/estatapi/blob/master/README.en.md
# s-Stat portal: https://www.e-stat.go.jp/api/
# e-Stat's Japanese-language website: https://www.e-stat.go.jp/mypage/user/preregister
# appId: https://www.e-stat.go.jp/mypage/login
# latest GDP data: https://www.esri.cao.go.jp/en/sna/sokuhou/sokuhou_top.html

library(estatapi)
Sys.getenv("appID")


### 4. ----

# R-script: https://media.portblue.net/resources/181231_sectoral-balances/sectors.r
# HTML page: https://media.portblue.net/resources/181231_sectoral-balances/sectoral.balances.html
# R Markdown: https://media.portblue.net/resources/181231_sectoral-balances/sectoral.balances.Rmd


## Further reading ----

# Cian poster: https://www.ecb.europa.eu/pub/conferences/ecbforum/YE_competition//shared/pdf/2018/Allen_Cian_Poster.pdf

# BOE Paper: https://www.bankofengland.co.uk/financial-stability-paper/2011/growing-fragilities-balance-sheets-in-the-great-moderation

# BOE Paper 2: https://www.bankofengland.co.uk/working-paper/2016/a-dynamic-model-of-financial-balances-for-the-uk

# ECB Paper: https://www.ecb.europa.eu/pub/pdf/scpops/ecbocp105.pdf

# END



