### Asia Bond Markets Data from Asia Bonds Online ##############################
rm(list = ls(all = TRUE))
library("RCurl")     # For downloading data from a URL
library("XML")       # For reading HTML tables
library("zoo")       # For interpolation
library("reshape2")  # For reshaping data
library("ggplot2")   # For static ggplot plot
library("plotly")    # For interactive plotly plot
library("ggthemes")  # For sensible colour schemes

### Data #######################################################################
# Size of LCY Bond Market in % of GDP / Size of LCY Bond Market
u <- "https://asianbondsonline.adb.org/spreadsheets/RG-LCY_in_GDP_Local.xls"
theurl        <- getURL(u)
tables        <- readHTMLTable(theurl)
lcybonds      <- tables[[1]]
lcybonds$Date <- as.Date(paste(1, lcybonds$Date), "%d %b %Y")

# FCY Bonds Outstanding
u <- "https://asianbondsonline.adb.org/spreadsheets/RG-FCY_Bonds_Outstanding.xls"
theurl        <- getURL(u)
tables        <- readHTMLTable(theurl)
fcybonds      <- tables[[1]]
fcybonds$Date <- as.Date(paste(1, fcybonds$Date), "%d %b %Y")

# Merge tables
ab <- merge(x = lcybonds, y = fcybonds, all = TRUE)
fullgrid  <- expand.grid("Market" = unique(ab$Market), "Date" = unique(ab$Date))
ab <- merge(x = fullgrid, y = ab, all = TRUE)

# Subset data
ab <- ab[, c("Market", "Date",
             "Government (in USD Billions)",
             "Corporate (in USD Billions)",
             "TOT FCY Govt Bonds Outstanding USD billion",
             "TOT FCY Corp Bonds Outstanding USD billion",
             "GDP (in USD Billions)")]
colnames(ab) <- c("Market", "Date",
                  "LCY Govt", "LCY Corp", "FCY Govt", "FCY Corp", "GDP")
ab <- ab[order(ab$Market, ab$Date),]
rownames(ab) <- NULL
ab[, -c(1,2)] <- sapply(ab[, -c(1,2)], function(x) as.numeric(as.character(x)))

# Interpolate missing data
for (ct in unique(ab$Market)) {
  ab[(ab$Market == ct), -c(1,2)] <- sapply(ab[(ab$Market == ct), -c(1,2)],
                                           na.approx, rule = 1:2, na.rm = FALSE)
}

### Transform data #############################################################
# Aggregate four main statistics
ab        <- melt(ab, id = c("Market", "Date"))
ab        <- dcast(ab, Date~variable, function(x) sum(x, na.rm = TRUE))
startdate <- max(unlist(sapply(ab, function(x) which(x == 0))))
ab        <- ab[(startdate+1):nrow(ab),]
ab["LCY Govt"] <- ab["LCY Govt"] / ab$GDP * 100
ab["LCY Corp"] <- ab["LCY Corp"] / ab$GDP * 100
ab["FCY Govt"] <- ab["FCY Govt"] / ab$GDP * 100
ab["FCY Corp"] <- ab["FCY Corp"] / ab$GDP * 100
ab$GDP   <- NULL
ab[, -1] <- sapply(ab[, -1], round, digits = 1)
ab.m     <- melt(ab, id = "Date")

### Plot #######################################################################
# ggplot
g <- ggplot(ab.m, aes(x = Date, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual("", values = gdocs_pal()(4)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  labs(title = "Asian bond market size (bonds outstanding)", y = "% of GDP",
       caption = "Source: AsianBondsOnline") +
  theme(legend.position = "bottom")
g
ggsave("ab_ggplot.png", g, width = 7.68, height = 5, dpi = 300)

# plotly
p <- plot_ly(ab, x = ~Date, y = ~`FCY Corp`, type = "bar", name = "FCY Corp",
        marker = list(color = gdocs_pal()(4)[4])) %>%
  add_trace(y = ~`FCY Govt`, name = "FCY Govt",
            marker = list(color = gdocs_pal()(4)[3])) %>%
  add_trace(y = ~`LCY Corp`, name = "LCY Corp",
            marker = list(color = gdocs_pal()(4)[2])) %>%
  add_trace(y = ~`LCY Govt`, name = "LCY Govt",
            marker = list(color = gdocs_pal()(4)[1])) %>%
  layout(title = "Asian bond market size (bonds outstanding)",
         yaxis = list(title = "% of GDP"),
         xaxis = list(title = "Date<br />Source: AsianBondsOnline"),
         barmode = "stack",
         plot_bgcolor = "rgba(235,235,235,1)",
         paper_bgcolor = "rgba(255,255,255,1)",
         legend = list(orientation = 'h'))
p
htmlwidgets::saveWidget(as_widget(p), "ab_plotly.html")
