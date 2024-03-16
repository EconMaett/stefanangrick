### Visualising government bond markets ########################################
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings

library("pdfetch")   # To obtain data from St. Louis FED
library("xts")       # Extended time series
library("reshape2")  # To reshape data sets
library("ggplot2")   # For plotting
library("ggthemes")  # Plot themes
library("plotly")    # For 3D plot

### US government bond markets #################################################
gbu <- pdfetch_FRED(c("DGS30", "DGS20", "DGS10", "DGS7", "DGS5", "DGS3",
                      "DGS2", "DGS1", "DGS6MO", "DGS3MO", "DGS1MO"))
names(gbu) <- c("30Y", "20Y", "10Y", "7Y", "5Y", "3Y", "2Y", "1Y",
                "6M", "3M", "1M")

# Uncomment this if you want to interpolate NA values
#gbu <- na.approx(gbu, na.rm = FALSE)

# Static plot
u <- ggplot(melt(fortify(gbu), id = "Index"),
            aes(x = Index, y = value, color = variable)) +
  geom_line(size = 1) +
  scale_color_gdocs("", breaks = names(gbu)) +
  labs(title = "US Treasury yields",
       y = "Percent", x = "Date", caption = "Source: St. Louis FRED")
u
ggsave("ustreasuries-yields.png", u, width = 8, height = 4.5, dpi = 300)

## 3D plot
p <- plot_ly(z = ~gbu, y = index(gbu), x = names(gbu),
             colors = gdocs_pal()(3)) %>%
  add_surface() %>%
  layout(title = "US Treasury yields<br />Source: St. Louis FRED",
         scene = list(
           xaxis = list(title = "Maturity", gridcolor = "rgb(255, 255, 255)",
                        zerolinecolor = "rgb(255, 255, 255)",
                        showbackground = TRUE,
                        backgroundcolor = "rgb(240, 240,240)"),
           yaxis = list(title = "Date", gridcolor = "rgb(255, 255, 255)",
                        zerolinecolor = "rgb(255, 255, 255)",
                        showbackground = TRUE,
                        backgroundcolor = "rgb(230, 230,230)"),
           zaxis = list(title = "Percent", gridcolor = "rgb(255, 255, 255)",
                        zerolinecolor = "rgb(255, 255, 255)",
                        showbackground = TRUE,
                        backgroundcolor = "rgb(220, 220,220)")
         ))
p
htmlwidgets::saveWidget(as_widget(p), "ustreasuries-yields.html")

### Keep the data for later ####################################################
save.image("ustreasuries.rda")
