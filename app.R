# Coursera Data Science Specialization Course 9 Project 3 Script----------------
# Dice Roll! App


# The purpose of this script is to complete the basic requirements behind the
# project 3 peer-graded assignment which is part of the Developing Data Products
# course from Johns Hopkins University within the Data Science Specialization on
# Coursera.
#
# The instructions say to create an interactive application using Shiny. This
# project will meet the objective by creating a game where the user can place
# wagers on the outcome of a roll of two dice.


library(shiny)
library(tidyverse)
#library(DT)


# User Interface----------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel("Dice Roll!"),

    # Specify layout style
    sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
            
            # Sliders for wagers
            sliderInput("wager2",
                        "Wager on 2:",
                        value = 10,
                        min = 0,
                        max = 100),
            sliderInput("wager3",
                         "Wager on 3:",
                         value = 10,
                         min = 0,
                         max = 100),
            sliderInput("wager4",
                         "Wager on 4:",
                         value = 10,
                         min = 0,
                         max = 100),
            
            actionButton("roll", "Roll Dice!")
            
        ),

        # Table of the wager details
        mainPanel(
            
            # Output wager details
            tableOutput("total.wager"),
            
            # Show the dice outcomes
            tableOutput("dice.outcome")
            
        )
        
    )
)


# dice.values <- c(0, 0)


# RollButton <- function() {
#     dice.values2 <- sample(1:6, 2, replace = TRUE)
# }

# Server Logic------------------------------------------------------------------

server <- function(input, output) {
    
    # Reactive expression to create data frame summary
    WagerValues <- reactive({
        data.frame(
            Name = c("Total Amount Wagered"),
            Value = as.character(c(
                sum(input$wager2, input$wager3, input$wager4))),
            stringsAsFactors = FALSE)
    })
    
    # Show the wager details in an HTML table
    output$total.wager <- renderTable({
        WagerValues()
    })
    
    # # Get dice outcomes when Roll button is clicked
    # observeEvent(input$roll, {
    #     dice.values2 <- sample(1:6, 2, replace = TRUE)
    #     return(dice.values)
    # }, ignoreNULL = FALSE)
    
    # Get dice outcomes when Roll button is clicked
    RollButton <- eventReactive(input$roll, {
        
        dice.values <- sample(1:6, 2, replace = TRUE)
        dice.sum <- sum(dice.values)
        
        outcome <- c(2:12)
        wagers <- c(rep(0, 4), input$wager2, input$wager3, input$wager4, rep(0, 4))
        wager.table <- data.frame(outcome, wagers)
        
        win.wager <- filter(wager.table, outcome == dice.sum)[[2]]
        payout <- 5 * win.wager
        losses <- sum(select(wager.table, wagers)) - win.wager
        net.gain <- payout - losses
        c(dice.values, dice.sum, net.gain)
    
    }, ignoreNULL = FALSE)
    
    # Show the dice values
    output$dice.outcome <- renderText({
        RollButton()
    })
    

    
}

# Run Application---------------------------------------------------------------

shinyApp(ui = ui, server = server)

