### BOJ balance sheet ##########################################################
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings

library("pdfetch")    # To obtain data from St. Louis FED
library("xts")        # Extended time series
library("broom")      # To prepare xts objects for ggplot-ing
library("ggplot2")    # For plotting
library("ggthemes")   # Plot themes
library("plotly")     # For interactive plot
library("lubridate")  # Advanced date manipulation

# Open CSV containing BOJ accounts
# See http://www.stat-search.boj.or.jp/ssi/cgi-bin/famecgi2?cgi=$nme_a000_en&lstSelection=BS01
# Download the full set of accounts. This will give you a file named nme_*.csv

boj       <- read.csv(grep("^nme", list.files(), value = TRUE)[1],
                      stringsAsFactors = FALSE, strip.white = TRUE,
                      na.strings = c("", "NA"))
boj       <- boj[(grep("19| 20", boj[,1])[1]:nrow(boj)),]
colnames(boj)[which(colnames(boj) == "Series.code")] <- "Date"
boj$Date  <- as.Date(paste0(boj$Date, "/01"))
boj       <- boj[(rowSums(is.na(boj[, -1])) != ncol(boj[, -1])),]
boj[, -1] <- sapply(boj[, -1], FUN = function(x) as.numeric(as.character(x)))

# Prepare balance sheet
bs     <- boj
bs$afr <- rowSums(boj[, c("BS01.MABJMA1", "BS01.MABJMA2", "BS01.MABJMA12")],
                  na.rm = TRUE)
bs$agv <- boj[, c("BS01.MABJMA5")]
bs$apr <- rowSums(boj[, c("BS01.MABJMA001", "BS01.MABJMA002", "BS01.MABJMAA",
                          "BS01.MABJMA4", "BS01.MABJMAB", "BS01.MABJMA003",
                          "BS01.MABJMA004", "BS01.MABJMA.01", "BS01.MABJMA7")],
                  na.rm = TRUE)
bs$aot <- boj[, c("BS01.MABJMTA")] -
  rowSums(bs[, grep("^a", colnames(bs))], na.rm = TRUE)

bs$lbn <- boj[, c("BS01.MABJML1")]
bs$lrs <- rowSums(boj[, c("BS01.MABJML11", "BS01.MABJML4")], na.rm = TRUE)
bs$lfr <- boj[, c("BS01.MABJML5")]
bs$lgv <- boj[, c("BS01.MABJML3")]
bs$lot <- boj[, c("BS01.MABJMTA")] -
  rowSums(bs[, grep("^l", colnames(bs))], na.rm = TRUE) -
  rowSums(boj[, c("BS01.MABJML7", "BS01.MABJMC1", "BS01.MABJMC2")],
          na.rm = TRUE)

# We include reverse repos ("BS01.MABJML5") with "Other liabilities"
# We ignore net own capital ("BS01.MABJML7", "BS01.MABJMC1", "BS01.MABJMC2")

# Invert liabilities
bs[, grep("^l", colnames(bs))] <- -bs[, grep("^l", colnames(bs))]

# Convert to xts
bs <- xts(bs[, -1], order.by = bs$Date)

# Prepare for plotting
bs          <- bs*10^8/10^12
bs          <- bs[, (!colnames(bs) %in% colnames(boj))]
write.csv(data.frame("Date" = index(bs), coredata(bs)), file = "bojbs.csv",
          row.names = FALSE)

bs.labs     <- c("afr" = "Foreign reserves",
                 "aot" = "Other assets",
                 "apr" = "Claims on private sector",
                 "agv" = "Claims on government",
                 "lbn" = "Notes",
                 "lrs" = "Bank reserves",
                 "lgv" = "Government deposits",
                 "lfr" = "Foreign liabilities",
                 "lot" = "Other liabilites")
bs.names    <- names(bs.labs)
bs.cols     <- setNames(gdocs_pal()(length(bs.names)), bs.names)
bs.order    <- c(grep("^a", bs.names, value = TRUE),
                 rev(grep("^l", bs.names, value = TRUE)))
bs.l        <- tidy(bs)
bs.l$series <- factor(bs.l$series, levels = bs.order)

# ggplot area plot
boj.area <- ggplot(bs.l, aes(x = index, y = value, fill = series)) +
  geom_area() +
  scale_fill_manual("", breaks = bs.names, labels = bs.labs, values = bs.cols) +
  labs(title = paste0("Bank of Japan balance sheet"),
       y = "JPY trillion (Liabilities/Assets)", x = "Date",
       caption = "Source: Bank of Japan") +
  theme(axis.title.x = element_blank())
boj.area
ggsave("boj.area.png", boj.area, width = 8, height = 4.5, dpi = 300)
ggsave("boj.area.svg", boj.area, width = 8, height = 4.5, dpi = 300)

# plotly bar chart
pd               <- plotly::plot_ly()
bs.m             <- apply.monthly(bs, FUN = mean)
day(index(bs.m)) <- 1
bs.order         <- c(rev(grep("^a", bs.names, value = TRUE)),
                      grep("^l", bs.names, value = TRUE))
for (i in bs.order) {
  pd <- add_trace(pd, y = data.frame(bs.m)[, i], x = index(bs.m), type = "bar",
                  name = bs.labs[i], marker = list(color = bs.cols[i]))
}
pd <- layout(pd, barmode = 'relative',
             title = "Bank of Japan balance sheet",
             yaxis = list(title = "JPY trillion (Liabilities/Assets)"),
             xaxis = list(title = "Date<br />Source: Bank of Japan"),
             barmode = "stack",
             plot_bgcolor = "rgba(235,235,235,1)",
             paper_bgcolor = "rgba(255,255,255,1)",
             legend = list(orientation = 'v'))
pd
htmlwidgets::saveWidget(as_widget(pd), "boj.bars.html")

### Keep the data for later ####################################################
save.image("boj.balance.sheet.rda")
