### BOE balance sheet ##########################################################
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings
Sys.setenv(TZ = "Europe/London")                 # Set time zone
options(tz = "Europe/London")

library("pdfetch")    # To obtain data from Bank of England
library("xts")        # Extended time series
library("broom")      # To prepare xts objects for ggplot-ing
library("ggplot2")    # For plotting
library("ggthemes")   # Plot themes
library("plotly")     # For interactive plot
library("lubridate")  # Advanced date manipulation

# Get data from BOE
# See https://www.bankofengland.co.uk/boeapps/database/index.asp?first=yes&SectionRequired=B&HideNums=-1&ExtraInfo=false&Travel=NIxSTx

# Bank of England Weekly Report
boew <- pdfetch_BOE(c("RPWB75A", "RPWB74A", "RPWB65A",
                      "RPWB55A", "RPWB56A", "RPWB9R6", "RPWB9R8", "RPWB58A",
                      "RPWBV79", "RPWB59A",
                      "RPWB62A", "RPWB63A", "RPWB57A",
                      "RPWB66A", "RPWB67A", "RPWB68A", "RPWBL59", "RPWB69A",
                      "RPWZ4TJ", "RPWZ4TK", "RPWZ4TL", "RPWZ4TM", "RPWZ4TN",
                      "RPWB72A"),
                    from = "2006-05-24")

# Asset Purchase Facility
APF  <- pdfetch_BOE(c("YWWB58C", "YWWB99D", "YWWB9T9", "YWWB8X9", "YWWZJ5N"),
                    from = "2006-05-24")

# Bank of England Consolidated Balance Sheet, quarterly, 5 quarter lag
boeq <- pdfetch_BOE(c("RPQB56A", "RPQB58A", "RPQB9R8", "RPQBV79", "RPQB9R6",
                      "RPQB55A", "RPQB62A", "RPQZ6MO", "RPQZ6MS", "RPQB59A",
                      "RPQZ6MT", "RPQB75A",
                      "RPQB68A", "RPQB67A", "RPQBL59", "RPQB66A", "RPQZ4TJ",
                      "RPQZ4TK", "RPQB69A", "RPQZ4TL", "RPQZ4TM", "RPQB72A",
                      "RPQZ6MW", "RPQZ4TN", "RPQZ6MX"),
                    from = "2006-05-24")

# Merge in data on Asset Purchase Facility and quarterly series
APF  <- merge.xts(APF, seq(as.Date("2006-05-24"), today(), "day"))
APF  <- na.approx(APF, na.rm = FALSE)
boew <- merge.xts(boew, APF, join = "left")

boeq <- merge.xts(boeq, seq(as.Date("2006-05-24"), today(), "day"))
boeq <- na.approx(boeq, na.rm = FALSE)
boew <- merge.xts(boew, boeq, join = "left")
first.qtr  <- index(boew)[first(which(!is.na(boew[, "RPQB75A"])))]
last.qtr   <- index(boew)[last(which(!is.na(boew[, "RPQB75A"])))]
mid.qtr    <- index(boew)[mean(which(index(boew) %in% c(first.qtr, last.qtr)))]
max.assets <- max(boew[, "RPQB75A"], na.rm = TRUE) * 10^6/10^9

# Prepare balance sheet
bs     <- boew["2006-05-24/"]
bs$afr <- rowSums(bs[, c("RPWZ4TN", "RPQZ6MX")], na.rm = TRUE)
bs$RPWB75A["2014-10-01/"] <- bs$RPQB75A["2014-10-01/"]
bs$RPWZ4TM["/2013-09-18"] <- bs$YWWB9T9["/2013-09-18"]
bs$agv <- rowSums(bs[, c("RPWB72A", "RPWZ4TL", "RPWZ4TM")], na.rm = TRUE)
bs$apr <- rowSums(cbind(bs[, c("RPWZ4TM")], -bs[, c("YWWB9T9")]), na.rm = TRUE)
bs$agv <- rowSums(cbind(bs[, c("agv")], -bs[, c("apr")]), na.rm = TRUE)
bs$apr <- rowSums(bs[, c("apr", "RPWB69A", "RPWB65A")], na.rm = TRUE)
bs$aot <- bs[, c("RPWB75A")] - rowSums(bs[, c("afr", "agv", "apr")],
                                       na.rm = TRUE)
bs$aot[(bs$aot < 0)] <- 0
bs$lbn <- bs[, c("RPWB55A")]
bs$lrs <- bs[, c("RPWB56A")]
bs$lfr <- rowSums(bs[, c("RPWB59A", "RPQZ6MT")], na.rm = TRUE)
bs$lgv <- 0
bs$lot <- bs[, c("RPWB75A")] -
  rowSums(bs[, c("lbn", "lrs", "lfr", "lgv", "RPQZ6MS")], na.rm = TRUE)

# We ignore net own capital ("RPQZ6MS")

# Invert liabilities
bs[, grep("^l", colnames(bs))] <- -bs[, grep("^l", colnames(bs))]

# Prepare for plotting
bs          <- bs * 10^6/10^9
bs          <- bs[, (!colnames(bs) %in% colnames(boew))]
write.csv(data.frame("Date" = index(bs), coredata(bs)), file = "boe.bs.csv",
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
bs$lgv      <- NULL
bs.l        <- tidy(bs)
bs.l$series <- factor(bs.l$series, levels = bs.order)

# ggplot area plot
boe.area <- ggplot(bs.l, aes(x = index, y = value, fill = series)) +
  geom_area() +
  scale_fill_manual("", breaks = bs.names, labels = bs.labs, values = bs.cols) +
  labs(title = paste0("Bank of England balance sheet"),
       y = "GBP billion (Liabilities/Assets)", x = "Date",
       caption = "Source: Bank of England") +
  theme(axis.title.x = element_blank()) +
  geom_vline(xintercept = first.qtr, colour = "#232323") +
  geom_vline(xintercept = last.qtr, colour = "#232323") +
  annotate("text", label = "Interpolated using\nquarterly data", size = 3.5,
           x = mid.qtr, y = max.assets)
boe.area
ggsave("boe.area.png", boe.area, width = 8, height = 4.5, dpi = 300)
ggsave("boe.area.svg", boe.area, width = 8, height = 4.5, dpi = 300)

# plotly bar chart
pd               <- plotly::plot_ly()
bs.m             <- apply.monthly(bs, FUN = mean)
day(index(bs.m)) <- 1
day(first.qtr)   <- 1
day(last.qtr)    <- 1
bs.order         <- c(rev(grep("^a", bs.names, value = TRUE)),
                      grep("^l", bs.names, value = TRUE))
bs.order         <- bs.order[(bs.order != "lgv")]

lines <- list(list(type = "line", line = list(color = "#232323", width = 1),
                   xref = "x", yref = "y",
                   x0 = first.qtr - days(15), x1 = first.qtr - days(15),
                   y0 = -round(max.assets, -2), y1 = round(max.assets, -2)),
              list(type = "line", line = list(color = "#232323", width = 1),
                   xref = "x", yref = "y",
                   x0 = last.qtr + days(15), x1 = last.qtr + days(15),
                   y0 = -round(max.assets, -2), y1 = round(max.assets, -2)))
an    <- list(x = mid.qtr, y = max.assets, xref = "x", yref = "y",
              text = "Interpolated using quarterly data", showarrow = FALSE,
              font = list(color = "#000000", size = 14))

for (i in bs.order) {
  pd <- add_trace(pd, y = data.frame(bs.m)[, i], x = index(bs.m), type = "bar",
                  name = bs.labs[i], marker = list(color = bs.cols[i]))
}
pd <- layout(pd, barmode = 'relative',
             title = "Bank of England balance sheet",
             yaxis = list(title = "GBP billion (Liabilities/Assets)"),
             xaxis = list(title = "Date<br />Source: Bank of England"),
             barmode = "stack",
             plot_bgcolor = "rgba(235,235,235,1)",
             paper_bgcolor = "rgba(255,255,255,1)",
             legend = list(orientation = 'v'),
             shapes = lines,
             annotations = an)
pd
htmlwidgets::saveWidget(as_widget(pd), "boe.bars.html")

### Keep the data for later ####################################################
save.image("boe.balance.sheet.rda")

