### Fed balance sheet ##########################################################
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

# Get data from FRED
# See https://fred.stlouisfed.org/release/tables?rid=20&eid=2216
fed    <- pdfetch_FRED(c("WALCL", "WGCAL", "WASDRAL", "WACL", "WSHOTS",
                         "WSHOFDSL", "WSHOMCB", "WAML1L", "WACBS", "WFCDA",
                         "WLTLECL", "WLFN", "WLRRA", "WLDBTDL", "WLODLL",
                         "WLTGAL", "WLFOL"))

# Prepare balance sheet
bs     <- fed
bs$afr <- rowSums(fed[, c("WFCDA", "WACBS", "WGCAL", "WASDRAL", "WACL")])
bs$agv <- rowSums(fed[, c("WSHOTS")])
bs$apr <- rowSums(fed[, c("WSHOFDSL", "WSHOMCB", "WAML1L")])
bs$aot <- rowSums(fed[, c("WALCL")]) - rowSums(bs[, grep("^a", colnames(bs))])

bs$lbn <- rowSums(fed[, c("WLFN")])
bs$lrs <- rowSums(fed[, c("WLDBTDL", "WLODLL")])
bs$lfr <- rowSums(fed[, c("WLFOL")])
bs$lgv <- rowSums(fed[, c("WLTGAL")])
bs$lot <- rowSums(fed[, c("WLTLECL")]) - rowSums(bs[, grep("^l", colnames(bs))])

# We include reverse repos ("WLRRA") with "Other liabilities"
# We ignore net own capital ("WCTCL")

# Invert liabilities
bs[, grep("^l", colnames(bs))] <- -bs[, grep("^l", colnames(bs))]

# Prepare for plotting
bs          <- bs/10^6
bs          <- bs[, (!colnames(bs) %in% colnames(fed))]
write.csv(data.frame("Date" = index(bs), coredata(bs)), file = "fedbs.csv",
          row.names = FALSE)
bs          <- bs[complete.cases(bs),]

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
fed.area <- ggplot(bs.l, aes(x = index, y = value, fill = series)) +
  geom_area() +
  scale_fill_manual("", breaks = bs.names, labels = bs.labs, values = bs.cols) +
  labs(title = paste0("US Federal Reserve System balance sheet"),
       y = "USD trillion (Liabilities/Assets)", x = "Date",
       caption = "Source: St. Louis FRED") +
  theme(axis.title.x = element_blank())
fed.area
ggsave("fed.area.png", fed.area, width = 8, height = 4.5, dpi = 300)
ggsave("fed.area.svg", fed.area, width = 8, height = 4.5, dpi = 300)

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
             title = "Federal Reserve System balance sheet",
             yaxis = list(title = "USD trillion (Liabilities/Assets)"),
             xaxis = list(title = "Date<br />Source: St. Louis FRED"),
             barmode = "stack",
             plot_bgcolor = "rgba(235,235,235,1)",
             paper_bgcolor = "rgba(255,255,255,1)",
             legend = list(orientation = 'v'))
pd
htmlwidgets::saveWidget(as_widget(pd), "fed.bars.html")

### Keep the data for later ####################################################
save.image("fed.balance.sheet.rda")
