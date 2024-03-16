### ECB balance sheet ##########################################################
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings
Sys.setenv(TZ = "Europe/London")                 # Set time zone
options(tz = "Europe/London")

library("ecb")        # To obtain data from ECB
library("ISOweek")    # To properly convert weeks on Windows
library("reshape2")   # To reshape dataset
library("xts")        # Extended time series
library("broom")      # To prepare xts objects for ggplot-ing
library("ggplot2")    # For plotting
library("ggthemes")   # Plot themes
library("plotly")     # For interactive plot
library("lubridate")  # Advanced date manipulation

# Get data from ECB's Statistical Data Warehouse
# http://sdw.ecb.europa.eu/browse.do?node=9691294
# https://www.ecb.europa.eu/pub/annual/balance/html/index.en.html
# https://www.ecb.europa.eu/press/pr/wfs/2018/html/index.en.html
# https://www.ecb.europa.eu/press/pr/wfs/2018/html/ecb.fst180925.en.html

a01 <- get_data("ILM.W.U2.C.A010000.Z5.Z0Z")
a02 <- get_data("ILM.W.U2.C.A020000.U4.Z06")
a03 <- get_data("ILM.W.U2.C.A030000.U2.Z06")
a04 <- get_data("ILM.W.U2.C.A040000.U4.EUR")
a05 <- get_data("ILM.W.U2.C.A050000.U2.EUR")
a06 <- get_data("ILM.W.U2.C.A060000.U2.EUR")
a07 <- get_data("ILM.W.U2.C.A070000.U2.EUR")
a08 <- get_data("ILM.W.U2.C.A080000.U2.EUR")
a09 <- get_data("ILM.W.U2.C.A110000.Z5.Z01")
at  <- get_data("ILM.W.U2.C.T000000.Z5.Z01")

l01 <- get_data("ILM.W.U2.C.L010000.Z5.EUR")
l02 <- get_data("ILM.W.U2.C.L020000.U2.EUR")
l03 <- get_data("ILM.W.U2.C.L030000.Z5.EUR")
l04 <- get_data("ILM.W.U2.C.L040000.Z5.EUR")
l05 <- get_data("ILM.W.U2.C.L050000.U2.EUR")
l06 <- get_data("ILM.W.U2.C.L060000.U4.EUR")
l07 <- get_data("ILM.W.U2.C.L070000.U2.Z06")
l08 <- get_data("ILM.W.U2.C.L080000.U4.Z06")
l09 <- get_data("ILM.W.U2.C.L090000.U4.XDR")
l10 <- get_data("ILM.W.U2.C.L120000.Z5.Z01")
l11 <- get_data("ILM.W.U2.C.L140000.Z5.Z01")  # Net
l12 <- get_data("ILM.W.U2.C.L150000.Z5.Z01")  # Net

ecb      <- rbind(a01, a02, a03, a04, a05, a06, a07, a08, a09, at,
                  l01, l02, l03, l04, l05, l06, l07, l08, l09, l10, l11, l12)
ecb$Date <- ISOweek2date(paste0(ecb$obstime, "-1"))
ecb      <- dcast(ecb, "Date~bs_item", value.var = "obsvalue")

# Prepare balance sheet
bs     <- ecb
bs$afr <- rowSums(bs[, c("A010000", "A020000", "A040000")], na.rm = TRUE)
bs$agv <- rowSums(bs[, c("A070000", "A080000")], na.rm = TRUE)
bs$apr <- rowSums(bs[, c("A030000", "A050000", "A060000")], na.rm = TRUE)
bs$aot <- bs[, c("A110000")]
bs$lbn <- bs[, c("L010000")]
bs$lrs <- bs[, c("L020000")]
bs$lfr <- rowSums(bs[, c("L060000", "L080000", "L090000")], na.rm = TRUE)
bs$lgv <- bs[, c("L050000")]
bs$lot <- rowSums(bs[, c("L030000", "L040000", "L070000", "L120000")],
                  na.rm = TRUE)

# We ignore net own capital ("L140000", "L150000")

# Invert liabilities
bs[, grep("^l", colnames(bs))] <- -bs[, grep("^l", colnames(bs))]

# Convert to xts
bs <- xts(bs[, -1], order.by = bs$Date)

# Prepare for plotting
bs          <- bs * 10^6/10^12
bs          <- bs[, (!colnames(bs) %in% colnames(ecb))]
write.csv(data.frame("Date" = index(bs), coredata(bs)), file = "ecb.bs.csv",
          row.names = FALSE)

bs.labs     <- c("afr" = "Foreign reserves",
                 "aot" = "Other assets",
                 "apr" = "Claims on private sector",
                 "agv" = "Claims on government",
                 "lbn" = "Bank notes",
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
ecb.area <- ggplot(bs.l, aes(x = index, y = value, fill = series)) +
  geom_area() +
  scale_fill_manual("", breaks = bs.names, labels = bs.labs, values = bs.cols) +
  labs(title = paste0("Eurosystem consolidated balance sheet"),
       y = "EUR trillion (Liabilities/Assets)", x = "Date",
       caption = "Source: European Central Bank") +
  theme(axis.title.x = element_blank())
ecb.area
ggsave("ecb.area.png", ecb.area, width = 8, height = 4.5, dpi = 300)
ggsave("ecb.area.svg", ecb.area, width = 8, height = 4.5, dpi = 300)

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
             title = "Eurosystem consolidated balance sheet",
             yaxis = list(title = "EUR trillion (Liabilities/Assets)"),
             xaxis = list(title = "Date<br />Source: European Central Bank"),
             barmode = "stack",
             plot_bgcolor = "rgba(235,235,235,1)",
             paper_bgcolor = "rgba(255,255,255,1)",
             legend = list(orientation = 'v'))
pd
htmlwidgets::saveWidget(as_widget(pd), "ecb.bars.html")

### Keep the data for later ####################################################
save.image("ecb.balance.sheet.rda")

