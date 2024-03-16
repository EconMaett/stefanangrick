### Bank statistics ############################################################
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings

library("xlsx")      # To open Excel files
library("xts")       # Extended time series
library("broom")     # To prepare data for plotting
library("ggplot2")   # For plotting
library("ggthemes")  # Plot themes
library("plotly")    # To save an interactive version of the plot

### Bank share #################################################################
download.file("http://www.bis.org/statistics/totcredit/totcredit.xlsx",
              destfile = "totcredit.xlsx", mode = "wb")
bisdf <- read.xlsx2("totcredit.xlsx", sheetName = "Quarterly Series",
                    stringsAsFactors = FALSE, header = FALSE)

# Remove aggregates and keep only figures for banks and totals
bisdf      <- bisdf[, -grep(".*aggregate.*", bisdf[1, ])]
total.cols <- grep(".*P:A:M:770:A$", bisdf[which(bisdf[, 1] == "Period"),])
banks.cols <- grep(".*P:B:M:770:A$", bisdf[which(bisdf[, 1] == "Period"),])

# Clean up data frame
colnames(bisdf) <- gsub("\\.", " ", bisdf[(which(bisdf[, 1] == "Period")-1),])
colnames(bisdf)[1] <- "Date"
bisdf <- bisdf[-c(1:(which(bisdf[, 1] == "Period")+1)),]
bisdf <- as.data.frame(sapply(bisdf, FUN = function(x)
  as.numeric(as.character(x))))
bisdf$Date <- as.Date(as.POSIXct(bisdf$Date * (60*60*24), origin = "1899-12-30",
                                 tz = ""), format = "%Y-%m-%d")

# Convert to xts and divide bank shares by totals
bisb  <- xts(bisdf[, banks.cols], order.by = bisdf$Date)
bist  <- xts(bisdf[, total.cols], order.by = bisdf$Date)
bisdf <- bisb/bist * 100
bisp  <- bisdf["1990/", c("United States", "China", "Japan", "Germany",
                          "United Kingdom", "France", "Canada", "Korea")]

# Plotting
p <- ggplot(tidy(bisp), aes(x = index, y = value, color = series)) +
  geom_point(alpha = 0.4, stroke = 0) +
  geom_smooth(span = 0.25, se = FALSE) +
  scale_color_gdocs("") +
  labs(title = "Bank credit / Total credit to non-financial sector",
       y = "Percent", x = "Date", caption = "Source: BIS")
p
ggsave("bankshare.png", p, width = 8, height = 4.5, dpi = 300)

# Convert to plotly object
pl <- ggplotly(p)
htmlwidgets::saveWidget(as_widget(pl), "bankshare.html")

### Keep the data for later ####################################################
save.image("bankshare.rda")
