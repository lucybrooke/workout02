#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# functions

#' @title future value
#' @description calculates the future value of money with compound interest
#' @param amount initial invested amount
#' @param rate annual rate of return
#' @param years number of years
#' @return total future value

future_value <- function(amount, rate, years){
  fv <- amount*(1 + rate)^years
  return(fv)
}


#' @title future value of annuity
#' @description calculates the future value of money deposited annually with compound interest
#' @param contrib contributed amount
#' @param rate annual rate of return
#' @param years number of years
#' @return total future value of annuity

annuity <- function(contrib, rate, years){
  fva <- contrib*((((1 + rate)^years) - 1)/rate)
  return(fva)
}


#' @title future value of growing annuity
#' @description calculates the future value of money (in increasing amounts) deposited annually with compound interest
#' @param contrib contributed amount
#' @param rate annual rate of return
#' @param years number of years
#' @param growth number of years
#' @return total future value of growing annuity

growing_annuity <- function(contrib, rate, years, growth){
  br_n <- ((1 + rate)^years) - ((1 + growth)^years)
  br_d <- (rate - growth)
  fvga <- contrib*(br_n/br_d)
  return(fvga)
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investment Scenarios"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(4,
         sliderInput("initial",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000,
                     step = 500),
      sliderInput("annual",
                  "Annual Contribution",
                  min = 0,
                  max = 50000,
                  value = 2000,
                  step = 500)
      ),
      column(4, 
             sliderInput("return",
                         "Return Rate (in %)",
                         min = 0,
                         max = 20,
                         value = 5,
                         step = 0.1),
             sliderInput("growth",
                         "Growth Rate (in %)",
                         min = 0,
                         max = 20,
                         value = 2,
                         step = 0.1)
      ),
      column(4,
             sliderInput("years",
                         "Years",
                         min = 0,
                         max = 50,
                         value = 20,
                         step = 1),
             selectInput("facet",
                         "Facet?",
                         choices = c("No", "Yes"))
             )
   ),
      # Show a plot of the generated distribution
      mainPanel(
        h4("Timelines"),
        plotOutput("saveplot"),
        
        h4("Balances"),
        tableOutput("df_balances")
      )
  
    )



library(ggplot2)
library(tibble)
library(dplyr)
library(stats)

server <- function(input, output) {
   
   output$saveplot <- renderPlot({
     
     decrate <- input$return/100
     decgrowthr <- input$growth/100
     
     rgs_no <- vector('numeric', length(input$years))
     rgs_an <- vector('numeric', length(input$years))
     rgs_ga <- vector('numeric', length(input$years))
     
     for(i in 0:input$years){
       rgs_no[i] <- future_value(amount = input$initial, rate = decrate, years = i)
       rgs_an[i] <- annuity(contrib = input$annual, rate = decrate, years = i)
       rgs_fix <- rgs_an + rgs_no
       rgs_ga[i] <- growing_annuity(contrib = input$annual, rate = decrate, years = i, growth = decgrowthr)
       rgs_grow <- rgs_ga + rgs_no
     }
     
     hys_no <- vector('numeric', length(input$years))
     hys_an <- vector('numeric', length(input$years))
     hys_ga <- vector('numeric', length(input$years))
     
     for(i in 0:input$years){
       hys_no[i] <- future_value(amount = input$initial, rate = decrate, years = i)
       hys_an[i] <- annuity(contrib = input$annual, rate = decrate, years = i)
       hys_fix <- hys_an + hys_no
       hys_ga[i] <- growing_annuity(contrib = input$annual, rate = decrate, years = i, growth = decgrowthr)
       hys_grow <- hys_ga + hys_no
     }
     
     if_no <- vector('numeric', length(input$years))
     if_an <- vector('numeric', length(input$years))
     if_ga <- vector('numeric', length(input$years))
     
     
     for(i in 0:input$years){
       if_no[i] <- future_value(amount = input$initial, rate = decrate, years = i)
       if_an[i] <- annuity(contrib = input$annual, rate = decrate, years = i)
       if_fix <- if_an + if_no
       if_ga[i] <- growing_annuity(contrib = input$annual, rate = decrate, years = i, growth = decgrowthr)
       if_grow <- if_ga + if_no
     }
     
     #contribution type
     no_contrib <- c("No Contribution")
     no_contrib <- rep(no_contrib, times = input$years + 1)
     
     fix_contrib <- c("Fixed Contribution")
     fix_contrib <- rep(fix_contrib, times = input$years + 1)
     
     gro_contrib <- c("Growing Contribution")
     gro_contrib <- rep(gro_contrib, times = input$years + 1)
     
     n_contrib_type <- c(no_contrib, fix_contrib, gro_contrib)
     
     # final vector
     f_contrib_type <- rep(n_contrib_type, times = 3)
      
     #type of savings account
     x <- c("Regular")
     f_reg_type <- rep(x, times = 3*(input$years + 1))
     
     y <- c("High Yield")
     f_hys_type <- rep(y, times = 3*(input$years + 1))
     
     z <- c("Index")
     f_ind_type <- rep(z, times = 3*(input$years + 1))
     
     #final vector
     f_savings_type <- c(f_reg_type, f_hys_type, f_ind_type)
     
     # making the year column/final vector
     f_year <- rep(c(0:input$years), times = 9)
     
     # adding value at year 0 to index 1 
     rgs_no_win <- c(input$initial, rgs_no)
     rgs_fix_win <- c(input$initial, rgs_fix)
     rgs_grow_win <- c(input$initial, rgs_grow)
     
     hys_no_win <- c(input$initial, hys_no)
     hys_fix_win <- c(input$initial, hys_fix)
     hys_grow_win <- c(input$initial, hys_grow)
     
     if_no_win <- c(input$initial, if_no)
     if_fix_win <- c(input$initial, if_fix)
     if_grow_win <- c(input$initial, if_grow)
     
     reg_nfg <- c(rgs_no_win, rgs_fix_win, rgs_grow_win)
     
     hys_nfg <- c(hys_no_win, hys_fix_win, hys_grow_win)
     
     if_nfg <- c(if_no_win, if_fix_win, if_grow_win)
     
     f_balance <- c(reg_nfg, hys_nfg, if_nfg)
     
     save_scenarios <- data.frame(year = f_year, balance = f_balance, contribution = f_contrib_type, type_savings = f_savings_type)
     
     # filtered data frame for plotting
     reg_save_scenarios <- save_scenarios %>%
       filter(type_savings == "Regular") 
     head(reg_save_scenarios)
     
     hy_save_scenarios <- save_scenarios %>%
       filter(type_savings == "High Yield")
     
     if_save_scenarios <- save_scenarios %>%
       filter(type_savings == "Index")
     
  if(input$facet == "No"){
    return(ggplot() +
      geom_line(data = reg_save_scenarios, aes(x = year, y = balance, color = contribution)) +
      geom_line(data = hy_save_scenarios, aes(x = year, y = balance, color = contribution)) +
      geom_line(data = if_save_scenarios, aes(x = year, y = balance, color = contribution)) +
      labs(title = "Three Modes of Investing") +
      theme_bw())
  }
   if(input$facet == "Yes"){
       return(ggplot() +
         geom_line(data = reg_save_scenarios, aes(x = year, y = balance, color = contribution)) +
         geom_line(data = hy_save_scenarios, aes(x = year, y = balance, color = contribution)) +
         geom_line(data = if_save_scenarios, aes(x = year, y = balance, color = contribution)) +
         facet_grid(.~contribution) +
         labs(title = "Three Modes of Investing") +
         theme_bw())
     }
      
   })
   
  df_balances <- reactive({
    decrate <- input$return/100
    decgrowthr <- input$growth/100
    
    mode_one <- c()
    
    for(i in 0:input$years){
      mode_one[i] <- future_value(amount = input$initial, rate = decrate, years = i)
    }
  
 
    mtwo_fv <- c()
    mtwo_a <- c()
    mode_two <- c()
    
    for(i in 0:input$years){
      mtwo_fv[i] <- future_value(amount = input$initial, rate = decrate, years = i)
      mtwo_a[i] <- annuity(contrib = input$annual, rate = decrate, years = i)
      mode_two <- mtwo_a + mtwo_fv
    }

    
    m3_fv <- c()
    m3_ga <- c()
    mode_three <- c()
    
    for(i in 0:input$years){
      m3_fv[i] <- future_value(amount = input$initial, rate = decrate, years = i)
      m3_ga[i] <- growing_annuity(contrib = input$annual, rate = decrate, years = i, growth = decgrowthr)
      mode_three <- m3_fv + m3_ga
    }
    
    mode_one <- c(input$initial, mode_one)
    mode_two <- c(input$initial, mode_two)
    mode_three <- c(input$initial, mode_three)
    
    modes <- c(mode_one, mode_two, mode_three)
    cyears <- input$years + 1
    cont_names <- c(rep("No Contribution", times = cyears), rep("Fixed Contribution", times = cyears), rep("Growing Contribution", times = cyears))
    year_vect <- c(rep(0:input$years, times = 3))
    df_balances <- data.frame("year" = year_vect, "Balance" = modes, "Contribution Type" = cont_names)
    df_balances <- reshape(df_balances, idvar = "year", direction = "wide", timevar = "Contribution.Type")
    names(df_balances)[2] <- "No Contribution"
    names(df_balances)[3] <- "Fixed Contribution"
    names(df_balances)[4] <- "Growing Contribution"
    return(df_balances)
  })
  
output$df_balances <- renderTable({
  df_balances <- df_balances()
})
}

# Run the application 
shinyApp(ui = ui, server = server)

