library("shiny")
library("shinyjs")
library("ggplot2")
library("ggthemes")
library("lubridate")
library("reshape2")

# Load data
dat <- read.csv("www/data.csv", header = TRUE, stringsAsFactors = FALSE)
dat$date <- as.Date(dat$date)
day(dat$date) <- 1
dat <- dat[complete.cases(dat), ]

# Function to get prep data frame and calculate final row as specified
prep_dat <- function(dat, grw_rate = NULL, shr_bounds = NULL, neg_rate = NULL) {
  # Calculate shares of each balance in percent
  dat$pos_shr <- dat$pos_bal * 100 / dat$tot_bal
  dat$zer_shr <- dat$zer_bal * 100 / dat$tot_bal
  dat$neg_shr <- dat$neg_bal * 100 / dat$tot_bal
  
  # Duplicate final row
  final_row      <- dat[nrow(dat), ]
  final_row$date <- final_row$date + months(1)
  
  # Set values for final row if provided, otherwise keep defaults  
  if (!is.null(grw_rate)) {
    final_row$tot_bal <- tail(dat$tot_bal, 1) * (100 + grw_rate) / 100
    final_row$pos_bal <- tail(dat$pos_bal, 1) * (100 + grw_rate) / 100
    final_row$zer_bal <- tail(dat$zer_bal, 1) * (100 + grw_rate) / 100
    final_row$neg_bal <- tail(dat$neg_bal, 1) * (100 + grw_rate) / 100
  }
  
  if (!is.null(shr_bounds)) {
    final_row$pos_shr <- shr_bounds[1]
    final_row$zer_shr <- shr_bounds[2] - shr_bounds[1]
    
    final_row$neg_shr <- 100 - final_row$zer_shr - final_row$pos_shr
    
    final_row$pos_bal <- final_row$tot_bal * final_row$pos_shr / 100
    final_row$zer_bal <- final_row$tot_bal * final_row$zer_shr / 100
    final_row$neg_bal <- final_row$tot_bal * final_row$neg_shr / 100
  }
  
  if (!is.null(neg_rate)) {
    final_row$neg_rate <- neg_rate
  }
  
  # Attach final row to data frame
  dat <- rbind(dat, final_row)
  
  # Calculate average rate and total interest income
  dat$avg_rate <- dat$pos_rate * dat$pos_shr / 100 +
                     dat$zer_rate * dat$zer_shr / 100 +
                     dat$neg_rate * dat$neg_shr / 100
  
  dat$int_inc <- dat$tot_bal * dat$avg_rate / 100
  
  # Return data frame
  return(dat)
}

# Function to reshape and subset data with specific stacking order
subset_dat <- function(dat, var_subset = NULL) {
  dat <- melt(dat, id.vars = "date")
  dat <- subset(dat, variable %in% var_subset)
  dat$variable <- factor(dat$variable, var_subset)
  return(dat)
}

# Set global values for bounds and policy rate
bound_lower <- as.numeric(round(tail(prep_dat(dat), 1)$pos_shr, 1))
bound_upper <- as.numeric(round(tail(prep_dat(dat), 1)$pos_shr + 
                                  tail(prep_dat(dat), 1)$zer_shr, 1))
neg_rate    <- tail(dat$neg_rate, 1)
grw_rate    <- 0

# Shiny UI code
ui <- fluidPage(
  useShinyjs(),
  tags$h1("Bank of Japan current account tiering"),
  fluidRow(
    column(5, sliderInput(inputId = "shr_bounds_input",
                          label = "Bounds of zero balance:",
                          min = 0, max = 100, step = 0.1, round = FALSE,
                          width = "95%",
                          value = c(bound_lower, bound_upper))),
    column(3, numericInput(inputId = "pol_rate_input",
                           label = "Policy rate:",
                           min = -3, max = 1, value = neg_rate, step = 0.1)),
    column(3,numericInput(inputId = "grw_rate_input",
                          label = "Growth rate of current account balance:",
                          min = -100, max = 100, value = grw_rate, step = 0.1)),
    column(1,
           #submitButton(text = "Apply"),
           actionButton(inputId = "reset_input", label = "Reset"))),
  #fluidRow(
  #  textOutput("shr_bounds_output"),
  #  textOutput("pol_rate_output"),
  #  textOutput("grw_rate_output")),
  fluidRow(column(6, plotOutput("intinc_plot")),
           column(6, plotOutput("rates_plot"))),
  fluidRow(column(6, plotOutput("absolute_plot")),
           column(6, plotOutput("shares_plot"))),
  tags$p("This app is in no way officially related to or endorsed by the Bank of Japan.")
)

# Server code
server <- function(input, output) {
  output$shr_bounds_output <- renderText({
    paste0(input$shr_bounds_input, collapse = ", ")
  })
  output$pol_rate_output <- renderText({
    input$pol_rate_input
  })
  output$grw_rate_output <- renderText({
    input$grw_rate_input
  })
  output$absolute_plot <- renderPlot({
    ggplot(subset_dat(prep_dat(dat, grw_rate = input$grw_rate_input,
                               shr_bounds = input$shr_bounds_input,
                               neg_rate = input$pol_rate_input),
                      var_subset = c("neg_bal", "zer_bal", "pos_bal"))) + 
      geom_bar(position = "stack", stat = "identity",
               aes(fill = variable, y = value / 10000, x = date)) +
      scale_fill_manual("", labels = c("pos_bal" = "Positive",
                                       "zer_bal" = "Zero",
                                       "neg_bal" = "Negative"),
                        values = ggthemes::gdocs_pal()(3)[c(2, 1, 3)]) +
      geom_vline(xintercept = as.numeric(max(dat$date)) + 13.5, linetype = 1) +
      labs(title = "Current account balance", y = "JPY trillion", x = "") +
      theme_gray(base_size = 14) +
      theme(legend.position = "bottom", axis.title.x = element_blank())
  })
  output$shares_plot <- renderPlot({
    ggplot(subset_dat(prep_dat(dat, grw_rate = input$grw_rate_input,
                               shr_bounds = input$shr_bounds_input,
                               neg_rate = input$pol_rate_input),
                      var_subset = c("neg_shr", "zer_shr", "pos_shr"))) + 
      geom_bar(position = "stack", stat = "identity",
               aes(fill = variable, y = value, x = date)) +
      scale_fill_manual("", labels = c("pos_shr" = "Positive",
                                       "zer_shr" = "Zero",
                                       "neg_shr" = "Negative"),
                        values = ggthemes::gdocs_pal()(3)[c(2, 1, 3)]) +
      geom_vline(xintercept = as.numeric(max(dat$date)) + 13.5, linetype = 1) +
      labs(title = "Shares of total", y = "%", x = "") +
      theme_gray(base_size = 14) +
      theme(legend.position = "bottom", axis.title.x = element_blank())
  })
  output$intinc_plot <- renderPlot({
    ggplot(subset_dat(prep_dat(dat, grw_rate = input$grw_rate_input,
                               shr_bounds = input$shr_bounds_input,
                               neg_rate = input$pol_rate_input),
                      var_subset = c("int_inc"))) + 
      geom_line(size = 1, aes(color = variable, y = value / 10000, x = date)) +
      geom_point(shape = 21, stroke = 2, fill = "white",
                 aes(color = variable, y = value / 10000, x = date)) +
      scale_color_manual("", labels = c("int_inc" = "Total interest income"),
                         values = ggthemes::gdocs_pal()(1)) +
      geom_vline(xintercept = as.numeric(max(dat$date)) + 13.5, linetype = 1) +
      labs(title = "Total interest income", y = "JPY trillion", x = "") +
      theme_gray(base_size = 14) +
      theme(legend.position = "bottom", axis.title.x = element_blank())
  })
  output$rates_plot <- renderPlot({
    ggplot(subset_dat(prep_dat(dat, grw_rate = input$grw_rate_input,
                               shr_bounds = input$shr_bounds_input,
                               neg_rate = input$pol_rate_input),
                      var_subset = c("avg_rate"))) + 
      geom_line(size = 1, aes(color = variable, y = value, x = date)) +
      geom_point(shape = 21, stroke = 2, fill = "white",
                 aes(color = variable, y = value, x = date)) +
      scale_color_manual("", labels = c(
        "cal_rate" = "Uncollateralised overnight call rate",
        "avg_rate" = "Average rate on current account balances"),
        values = ggthemes::gdocs_pal()(3)[2:3]) +
      geom_vline(xintercept = as.numeric(max(dat$date)) + 13.5, linetype = 1) +
      labs(title = "Interest rate", y = "%", x = "") +
      theme_gray(base_size = 14) +
      theme(legend.position = "bottom", axis.title.x = element_blank())
  })
  observeEvent(input$reset_input, {
    reset("shr_bounds_input")
    reset("pol_rate_input")
    reset("grw_rate_input")
  })
}

shinyApp(ui = ui, server = server)
