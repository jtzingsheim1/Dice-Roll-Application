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


# Server Logic------------------------------------------------------------------

server <- function(input, output) {
    
    # Setup shared variables
    vars <- reactiveValues()
        vars$tokens <- 1000  # Initial number of tokens
        vars$die1 <- 0  # Initial die setting
        vars$die2 <- 0  # Initial die setting

    # # Reactive expression to create data frame summary
    # WagerValues <- reactive({
    #     data.frame(
    #         Name = c("Total Amount Wagered"),
    #         Value = as.character(c(
    #             sum(input$wager2, input$wager3, input$wager4))),
    #         stringsAsFactors = FALSE)
    # })
    # 
    # # Show the wager details in an HTML table
    # output$total.wager <- renderTable({
    #     WagerValues()
    # })
    
    # Function to run when roll button is clicked
    RollButton <- eventReactive(input$roll, {
        
        # Roll the dice
        vars$die1 <- sample(1:6, 1)
        vars$die2 <- sample(1:6, 1)
        dice.sum <- sum(vars$die1, vars$die2)

        # Lookup wagers placed and find winning wager
        wagers <- c(rep(0, 5), input$wager2, input$wager3, input$wager4, rep(0, 4))
        win.wager <- wagers[dice.sum]
        
        # Calculate payout, losses, net gain, and update token quantity
        payout <- 10 * win.wager
        losses <- sum(wagers) - win.wager
        net.gain <- payout - losses
        vars$tokens <- vars$tokens + net.gain
        
        # Function returns
        c(dice.sum, net.gain)

    })
    
    # Show the dice values
    output$dice.outcome <- renderText({
        c(vars$die1, vars$die2, RollButton(), vars$tokens)
    })
    
}

# Run Application---------------------------------------------------------------

shinyApp(ui = ui, server = server)

