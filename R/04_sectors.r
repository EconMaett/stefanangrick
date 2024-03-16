### Housekeeping ###############################################################
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings

# Libraries
library("reshape2")      # To reshape data
library("OECD")          # To obtain data from OECD
library("eurostat")      # To obtain data from Eurostat
library("pdfetch")       # To obtain data from FRED
library("estatapi")      # To obtain data from Japanese government (e-Stat)
library("countrycode")   # To match country codes
library("ggplot2")       # ggplot plots
library("ggthemes")      # ggplot themes
library("RColorBrewer")  # For additional colour palettes
library("plotly")        # For interactive plots
library("xts")           # Advanced time series manipulations
library("zoo")           # Time series data management

# Parameters
maq          <- 4        # Moving average over maq quarters
use.estat    <- FALSE    # Set to TRUE to use e-Stat API
colour.blind <- FALSE    # Use colourblind-safe ColorBrewer palette for plots

sector.names <- c("Households", "Government", "Corporate", "Foreign")
df           <- setNames(data.frame(matrix(NA, ncol = 8, nrow = 0)),
                         c("Geo", "Time", sector.names, "Freq", "Source"))

### Get OECD data ##############################################################
# Net lending/borrowing by sector: NAAG
# http://stats.oecd.org/Index.aspx?DataSetCode=NAAG
# Table 2. Income
# https://data.oecd.org/natincome/net-lending-borrowing-by-sector.htm
# http://www.oecd-ilibrary.org/economics/national-accounts-at-a-glance_22200444
odata <- get_dataset("NAAG",
                     filter= "AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EUU+EMU+OTO+NMEC+BRA+CHN+IND+IDN+RUS+ZAF.B9S1S+B9S11_S12S+B9S13S+B9S14_S15S",
                     pre_formatted = TRUE)

od <- odata
od <- od[(od$LOCATION != "EMU"),]
od <- od[(od$LOCATION != "EUU"),]
od$LOCATION    <- countrycode(od$LOCATION, 'iso3c', 'iso2c')
od$TIME_FORMAT <- NULL
od$OBS_STATUS  <- NULL
od <- dcast(od, "LOCATION + obsTime ~ INDICATOR", value.var = "obsValue")

# Sector percentages
od$Geo        <- od$LOCATION
od$Time       <- od$obsTime
od$Households <- od$B9S14_S15S
od$Government <- od$B9S13S
od$Corporate  <- od$B9S11_S12S
od$Total      <- od$B9S1S
od$Foreign    <- -od$Total
od$LOCATION   <- od$obsTime <- od$B9S11_S12S <- od$B9S13S <- od$B9S14_S15S <-
  od$B9S1S <- od$Total <- NULL
od$Time       <- as.Date(paste0(od$Time, "-01-01"))

# Impute missing values
onena <- which(apply(od[, sector.names], 1, function(x) sum(is.na(x))) == 1)

for (i in onena) {
  od[i, which(is.na(od[i,]))] <- -rowSums(od[i, sector.names], na.rm = TRUE)
}

# Add metadata and merge into main set
od$Freq   <- "Annual data"
od$Source <- "OECD"
od        <- od[, colnames(df)]
df        <- rbind(df, od)

### Get Eurostat data ##########################################################
# Quarterly non-financial transactions: nasq_10_nf_tr
# http://ec.europa.eu/eurostat/web/products-datasets/product?code=nasq_10_nf_tr
# B9 = Net lending (+) /net borrowing (-)
# CP_MEUR = Current prices, million euro
edata <- data.frame(get_eurostat("nasq_10_nf_tr", time_format = "num"))
ed    <- subset(edata, (unit == "CP_MEUR") & (direct == "PAID") &
                  (sector %in% c("S11", "S12", "S13", "S14_S15", "S2")) &
                  (na_item == "B9") & (s_adj == "NSA"))

ed$Geo  <- ed$geo
ed$Time <- as.Date(as.yearqtr(ed$time))
ed      <- ed[, c("Geo", "Time", "sector", "values")]
ed      <- dcast(ed, "Geo + Time ~ sector", value.var = "values")

# Sector percentages
ed$Households <- ed$S14_S15          # Households
ed$Government <- ed$S13              # Government
ed$Corporate  <- ed$S11 + ed$S12     # Non-financial + financial corp
ed$Foreign    <- ed$S2               # Foreign
ed            <- ed[, c("Geo", "Time", "Households", "Government",
                        "Corporate", "Foreign")]

# Quarterly GDP: namq_10_gdp
# http://ec.europa.eu/eurostat/web/products-datasets/product?code=namq_10_gdp
egdpdata <- data.frame(get_eurostat("namq_10_gdp", time_format = "num"))
egd      <- subset(egdpdata, (unit == "CP_MEUR") & (s_adj == "NSA") &
                     (na_item == "B1GQ"))

egd$Geo       <- egd$geo
egd$Time      <- as.Date(as.yearqtr(egd$time))
egd$GDP       <- egd$values
egd           <- egd[, c("Geo", "Time", "GDP")]

# Merge both tables and calculate figures as % of GDP
ed            <- merge(x = ed, y = egd, by = c("Geo", "Time"), all.x = TRUE)
ed$Households <- ed$Households / ed$GDP * 100
ed$Government <- ed$Government / ed$GDP * 100
ed$Corporate  <- ed$Corporate / ed$GDP * 100
ed$Foreign    <- ed$Foreign / ed$GDP * 100
ed$GDP        <- NULL

# Convert country identifiers to ISO2 format
ed$Geo       <- countrycode(ed$Geo, "eurostat", "iso2c",
                            custom_match = c("EA19" = "EA19",
                                             "EU28" = "EU28",
                                             "EU27_2020" = "EU27"))
ed           <- ed[order(ed$Geo, ed$Time),]
rownames(ed) <- NULL

# Impute missing values and calculate moving average
onena <- which(apply(ed[, sector.names], 1, function(x) sum(is.na(x))) == 1)

for (i in onena) {
  ed[i, which(is.na(ed[i,]))] <- -rowSums(ed[i, sector.names], na.rm = TRUE)
}

ed[, sector.names] <- rollmean(ed[, sector.names],
                               k = maq, align = "right", fill = NA)

# Add to global set
ed$Freq   <- paste0("Quarterly data (", maq, "-quarter moving average)")
ed$Source <- "Eurostat"
ed        <- ed[, colnames(df)]
df        <- rbind(df, ed)

# Annual non-financial stransactions: nasa_10_nf_tr
# http://ec.europa.eu/eurostat/web/products-datasets/product?code=nasa_10_nf_tr
# B9 = Net lending (+) /net borrowing (-)
# CP_MEUR = Current prices, million euro
edataa <- data.frame(get_eurostat("nasa_10_nf_tr", time_format = "num"))
eda    <- subset(edataa, (unit == "CP_MEUR") & (direct == "PAID") &
                   (sector %in% c("S11", "S12", "S13", "S14_S15", "S2")) &
                   (na_item == "B9"))
eda$Geo        <- eda$geo
eda$Time       <- as.Date(paste0(eda$time, "-01-01"))
eda            <- eda[, c("Geo", "Time", "sector", "values")]
eda            <- dcast(eda, "Geo + Time ~ sector", value.var = "values")

# Sector percentages
eda$Households <- eda$S14_S15          # Households
eda$Government <- eda$S13              # Government
eda$Corporate  <- eda$S11 + eda$S12    # Non-financial + financial corp
eda$Foreign    <- eda$S2               # Foreign
eda            <- eda[, c("Geo", "Time", "Households", "Government",
                          "Corporate", "Foreign")]
# Annual GDP: nama_10_gdp
# http://ec.europa.eu/eurostat/web/products-datasets/product?code=nama_10_gdp
egdpdata <- data.frame(get_eurostat("nama_10_gdp", time_format = "num"))
egda     <- subset(egdpdata, (unit == "CP_MEUR") & (na_item == "B1GQ"))

egda$Geo       <- egda$geo
egda$Time      <- as.Date(as.yearqtr(egda$time))
egda$GDP       <- egda$values
egda           <- egda[, c("Geo", "Time", "GDP")]

# Merge both tables and calculate figures as % of GDP
eda <- merge(x = eda, y = egda, by = c("Geo", "Time"), all.x = TRUE)
eda$Households <- eda$Households / eda$GDP * 100
eda$Government <- eda$Government / eda$GDP * 100
eda$Corporate  <- eda$Corporate / eda$GDP * 100
eda$Foreign    <- eda$Foreign / eda$GDP * 100
eda$GDP        <- NULL

# Convert country identifiers to ISO2 format
eda$Geo       <- countrycode(eda$Geo, "eurostat", "iso2c",
                             custom_match = c("EA19" = "EA19",
                                              "EU28" = "EU28",
                                              "EU27_2020" = "EU27"))
eda           <- eda[order(eda$Geo, eda$Time),]
rownames(eda) <- NULL

# Impute missing values
onena <- which(apply(eda[, sector.names], 1, function(x) sum(is.na(x))) == 1)

for (i in onena) {
  eda[i, which(is.na(eda[i,]))] <- -rowSums(eda[i, sector.names], na.rm = TRUE)
}

# Add to global set
eda$Freq   <- "Annual data"
eda$Source <- "Eurostat"
eda        <- eda[, colnames(df)]
df         <- rbind(df, eda)

### Get FRED data for US #######################################################
# Financial Accounts of the United States - Z.1
# S.2.a Selected Aggregates for Total Economy and Sectors
# Capital account
# https://www.federalreserve.gov/apps/fof/DisplayTable.aspx?t=s.2.a
freddata <- pdfetch_FRED(c("ADSLBAQ027S", "HNOLACQ027S", "NNBLACQ027S",
                           "NCBLACQ027S", "BOGZ1FA795000905Q",
                           "BOGZ1FA315000905Q", "BOGZ1FA205000905Q",
                           "RWLBACQ027S", "GDP"))
fed      <- freddata

# Sector percentages
fed$GDP        <- fed$GDP * 10^3
fed$Households <- fed$HNOLACQ027S / fed$GDP * 100
fed$Government <- (fed$BOGZ1FA315000905Q +
                     fed$BOGZ1FA205000905Q) / fed$GDP * 100
fed$Corporate  <- (fed$NNBLACQ027S + fed$NCBLACQ027S +
                     fed$BOGZ1FA795000905Q) / fed$GDP * 100
fed$Foreign    <- fed$RWLBACQ027S / fed$GDP * 100

# Impute missing values and calculate moving average
fed                 <- na.approx(fed)
fed                 <- data.frame(Geo = "US", Time = index(fed),
                                  coredata(fed[, c("Households", "Government",
                                                   "Corporate", "Foreign")]))

onena <- which(apply(fed[, sector.names], 1, function(x) sum(is.na(x))) == 1)

for (i in onena) {
  fed[i, which(is.na(fed[i,]))] <- -rowSums(fed[i, sector.names], na.rm = TRUE)
}

fed[, sector.names] <- rollmean(fed[, sector.names],
                                k = maq, align = "right", fill = NA)

# Add to global set
fed$Freq   <- paste0("Quarterly data (", maq, "-quarter moving average)")
fed$Source <- "FRED"
fed        <- fed[, colnames(df)]
df         <- rbind(df, fed)

### Get BOJ/Cabinet Office data for Japan ######################################
# Flow of funds data for SNA68, SNA93, and SNA08
# Open http://www.stat-search.boj.or.jp/index_en.html
# Go to "Search by exact series code" and download
bojdata <- read.csv(grep("^nme", list.files(), value = TRUE)[1],
                    stringsAsFactors = FALSE, strip.white = TRUE,
                    na.strings = c("", "NA"))
bd       <- bojdata
bd       <- bd[(grep("19| 20", bd[,1]))[1]:nrow(bd),]
colnames(bd)[which(colnames(bd) == "Series.code")] <- "Time"
bd$Time  <- as.Date(paste0(bd$Time, "/01"))
bd$Time  <- bd$Time - months(2)
bd       <- bd[(rowSums(is.na(bd[, -1])) != ncol(bd[, -1])),]
bd[, -1] <- sapply(bd[, -1], FUN = function(x) as.numeric(as.character(x)))
bd[, -1] <- bd[, -1] * 100 * 10^6

# GDP, SNA68, 1955-2001
# https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/2001/qe011/gdemenuea.html
download.file("https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/2001/qe011/__icsFiles/afieldfile/2012/02/28/gaku_mg01168.csv",
              mode = "wb", destfile = "sna68.csv")

if (R.Version()$os == "linux-gnu") {
  system("iconv -f SHIFT-JIS -t UTF-8 sna68.csv > sna68utf8.csv")
  sna68gdp <- read.csv("sna68utf8.csv", header = FALSE, na.strings = "",
                       stringsAsFactors = FALSE)
} else {
  sna68gdp <- read.csv("sna68.csv", header = FALSE, na.strings = "",
                       stringsAsFactors = FALSE, encoding='cp932')
}
sna68gdp <- sna68gdp[9:nrow(sna68gdp),12]
sna68gdp <- sna68gdp[!is.na(sna68gdp)]
sna68gdp <- as.numeric(gsub(",", "", as.character(trimws(sna68gdp)))) * 10^9
sna68gdp <- data.frame("Time" = seq(as.Date("1955-01-01"),
                                    as.Date("2001-01-01"), by = "quarter"),
                       "GDPSNA68" = sna68gdp)

# GDP, SNA93, 1994-2016
# https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/2016/qe164_2/gdemenuea.html
download.file("https://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2016/qe164_2/tables/gaku-mg1642.csv",
              mode = "wb", destfile = "sna93.csv")

if (R.Version()$os == "linux-gnu") {
  system("iconv -f SHIFT-JIS -t UTF-8 sna93.csv > sna93utf8.csv")
  sna93gdp <- read.csv("sna93utf8.csv", header = FALSE, na.strings = "",
                       stringsAsFactors = FALSE)
} else {
  sna93gdp <- read.csv("sna93.csv", header = FALSE, na.strings = "",
                       stringsAsFactors = FALSE, encoding='cp932')
}
sna93gdp <- sna93gdp[8:nrow(sna93gdp),2]
sna93gdp <- sna93gdp[!is.na(sna93gdp)]
sna93gdp <- as.numeric(gsub(",", "", as.character(trimws(sna93gdp)))) * 10^9
sna93gdp <- data.frame("Time" = seq(as.Date("1994-01-01"),
                                    as.Date("2016-12-01"), by = "quarter"),
                       "GDPSNA93" = sna93gdp)

# GDP, SNA08, 2017-2018 (latest file as of 2021-02-14)
# https://www.esri.cao.go.jp/en/sna/data/sokuhou/files/2020/qe203_2/gdemenuea.html
download.file("https://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2020/qe203_2/tables/gaku-mg2032.csv",
              mode = "wb", destfile = "sna08.csv")

if (R.Version()$os == "linux-gnu") {
  system("iconv -f SHIFT-JIS -t UTF-8 sna08.csv > sna08utf8.csv")
  sna08gdp <- read.csv("sna08utf8.csv", header = FALSE, na.strings = "",
                       stringsAsFactors = FALSE)
} else {
  sna08gdp <- read.csv("sna08.csv", header = FALSE, na.strings = "",
                       stringsAsFactors = FALSE, encoding='cp932')
}
sna08gdp <- sna08gdp[8:nrow(sna08gdp),2]
sna08gdp <- sna08gdp[!is.na(sna08gdp)]
sna08gdp <- as.numeric(gsub(",", "", as.character(trimws(sna08gdp)))) * 10^9
sna08gdp <- data.frame("Time" = seq(as.Date("1994-01-01"), by = "quarter",
                                    length.out = length(sna08gdp)),
                       "GDPSNA08" = sna08gdp)

# Get latest nominal GDP data from e-Stat (optional)
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00100409&kikan=00100&tstat=000001014470&cycle=2&tclass1=000001014471&tclass2=000001018314&result_page=1&second=1&second2=1

if (use.estat == TRUE) {
  # Set appId
  # appId <- "XXXXX"
  source("estat.appid.r")

  estatdata <- estat_getStatsData(appId = appId, statsDataId = "0003109741")
  sna08gdp  <- data.frame(estatdata)
  sna08gdp  <- sna08gdp[(sna08gdp$cat01_code == "11"),]
  sna08gdp$Time     <- as.Date(paste0(substr(sna08gdp$time_code, 1, 4), "-",
                                      substr(sna08gdp$time_code, 7, 8), "-01"))
  sna08gdp$GDPSNA08 <- sna08gdp$value * 10^9
  sna08gdp  <- sna08gdp[, c("Time", "GDPSNA08")]
}

# Merge GDP into main set
bd <- merge(x = bd, y = sna68gdp, by = "Time", all.x = TRUE)
bd <- merge(x = bd, y = sna93gdp, by = "Time", all.x = TRUE)
bd <- merge(x = bd, y = sna08gdp, by = "Time", all.x = TRUE)

# Sector percentages
sna08.start <- which(!is.na(bd$FF.FOF_FFAF500L700))[1]
sna93.start <- which(!is.na(bd$FF.FOF_93FFAF500L700))[1]
sna68.start <- which(!is.na(bd$FF.FFFA300L390))[1]

bd$Households <- bd$FF.FOF_FFAF430L700 + bd$FF.FOF_FFAF440L700
bd$Government <- bd$FF.FOF_FFAF420L700
bd$Corporate  <- bd$FF.FOF_FFAF410L700 + bd$FF.FOF_FFAF100L700
bd$Foreign    <- bd$FF.FOF_FFAF500L700
bd$GDP        <- bd$GDPSNA08

bd$Households[sna68.start:(sna08.start-1)] <-
  bd$FF.FFFA270L390[sna68.start:(sna08.start-1)]
bd$Government[sna68.start:(sna08.start-1)] <-
  bd$FF.FFFA220L390[sna68.start:(sna08.start-1)] +
  bd$FF.FFFA230L390[sna68.start:(sna08.start-1)]
bd$Corporate[sna68.start:(sna08.start-1)]  <-
  bd$FF.FFFA260L390[sna68.start:(sna08.start-1)] +
  bd$FF.FFFA100L390[sna68.start:(sna08.start-1)]
bd$Foreign[sna68.start:(sna08.start-1)]    <-
  bd$FF.FFFA300L390[sna68.start:(sna08.start-1)]
bd$GDP[sna68.start:(sna08.start-1)]    <-
  bd$GDPSNA68[sna68.start:(sna08.start-1)]

bd$Households <- bd$Households / bd$GDP * 100
bd$Government <- bd$Government / bd$GDP * 100
bd$Corporate  <- bd$Corporate / bd$GDP * 100
bd$Foreign    <- bd$Foreign / bd$GDP * 100

# Impute missing values and calculate moving average
onena <- which(apply(bd[, sector.names], 1, function(x) sum(is.na(x))) == 1)

for (i in onena) {
  bd[i, which(is.na(bd[i,]))] <- -rowSums(bd[i, sector.names], na.rm = TRUE)
}

bd[, -1]    <- rollmean(bd[, -1], k = maq, align = "right", fill = NA)

# Add to global set
bd$Geo     <- "JP"
bd$Freq    <- paste0("Quarterly data (", maq, "-quarter moving average)")
bd$Source  <- "BOJ, CAO"
bd         <- bd[, colnames(df)]
df         <- rbind(df, bd)

### Plots ######################################################################
# Remove rows where all data are NA
df <- df[which(rowSums(is.na(df[, sector.names])) != ncol(df[, sector.names])),]

# Round values
df[, sector.names] <- round(df[, sector.names], digits = 3)

# Add additional identifiers to data set
df$Countryname <- countrycode(df$Geo, "iso2c", "country.name",
                              custom_match = c("EA19" = "Euro Area-19",
                                               "EU28" = "European Union-28",
                                               "EU27" = "European Union-27"))
df$id <- as.factor(paste0(df$Geo, "_", substr(df$Freq, 1, 1), "_",
                          gsub(", ", "_", tolower(df$Source))))

# Plots
if (colour.blind != TRUE) {
  secpal <- gdocs_pal()(4)
} else {
  secpal <- brewer.pal(4, "PuOr")
}

names(secpal) <- c("Households", "Government", "Corporate", "Foreign")

ggplot.list <- list()
plotly.list <- list()

dir.create(file.path(getwd(), "charts"), showWarnings = FALSE)

for (i in sort(unique(df$id))) {
  dfc <- subset(df, id == as.character(i))
  print(i)

  # Ggplot
  dfd <- melt(dfc, id = colnames(dfc)[!is.element(colnames(dfc), sector.names)])

  gp <- ggplot(dfd, aes(x = Time, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = unique(dfd$Countryname), y = "% of GDP",
         x = unique(dfd$Freq),
         caption = paste("Source:", unique(dfd$Source))) +
    scale_x_date(date_labels = "%Y") +
    theme(legend.position = "bottom") +
    scale_fill_manual("", values = secpal, breaks = names(secpal))

  ggsave(paste0("charts/", i, ".png"), plot = gp, width = 8, height = 4.5)
  ggsave(paste0("charts/", i, ".pdf"), plot = gp, width = 8, height = 4.5)

  # Plotly
  pp <- plot_ly()

  for (r in rev(names(secpal))) {
    pp <- add_trace(pp, y = dfc[, r], x = dfc$Time,
                    type = "bar", name = r, marker = list(color = secpal[r]))
  }

  pp <- layout(pp,
               autosize = FALSE,
               barmode = 'relative',
               title = unique(dfc$Countryname),
               yaxis = list(title = "% of GDP"),
               xaxis = list(title = paste0(unique(dfc$Freq), ", Source: ",
                                           unique(dfc$Source))),
               barmode = "stack",
               plot_bgcolor = "rgba(235,235,235,1)",
               paper_bgcolor = "rgba(255,255,255,1)",
               legend = list(orientation = 'v'))
  pp <- config(pp, displaylogo = FALSE)

  # Add plots to list
  local({
    i <- i
    ggplot.list[[i]] <<- gp
    plotly.list[[i]] <<- pp
  })
}

### Cleanup ####################################################################
last.updated <- Sys.time()
write.csv(df, "sectors.data.csv", row.names = FALSE)
save.image("sectors.rda")
