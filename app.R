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

        # Main panel for outputs
        mainPanel(

            # Details of the roll results
            textOutput("dice.values"),
            textOutput("results1"),
            textOutput("results2"),
            textOutput("results3")
            
            
            
        )
        
    )
)


# Server Logic------------------------------------------------------------------

server <- function(input, output) {
    
    # Setup shared variables
    vars <- reactiveValues()
        vars$tokens <- 1000  # Initial number of tokens
        vars$die1 <- 0
        vars$die2 <- 0
        vars$dice.sum <- 0
        vars$win.wager <- 0
        vars$pay.ratio <- 10
        vars$winnings <- 0
        vars$losses <- 0
        vars$net.gain <- 0

    # Function to run when roll button is clicked
    RollButton <- eventReactive(input$roll, {
        
        # Roll the dice
        vars$die1 <- sample(1:6, 1)
        vars$die2 <- sample(1:6, 1)
        vars$dice.sum <- sum(vars$die1, vars$die2)

        # Lookup wagers placed and find winning wager
        wagers <- c(rep(0, 5), input$wager2, input$wager3, input$wager4, rep(0, 4))
        vars$win.wager <- wagers[vars$dice.sum]
        
        # Calculate payout, losses, net gain, and update token quantity
        vars$winnings <- vars$pay.ratio * vars$win.wager
        vars$losses <- sum(wagers) - vars$win.wager
        vars$net.gain <- vars$winnings - vars$losses
        vars$tokens <- vars$tokens + vars$net.gain
        
    })
    
    # Render the dice results output
    output$dice.values <- renderText({
        RollButton()
        paste("Die 1:", vars$die1, "Die 2:", vars$die2)
    })
    
    output$results1 <- renderText({
        RollButton()
        paste("The sum is", vars$dice.sum, "and you wagered", vars$win.wager,
              "on this outcome.")
    })
    
    output$results2 <- renderText({
        RollButton()
        paste("This outcome pays", vars$pay.ratio, "to one, so you win",
              vars$winnings, "tokens.")
    })
    
    output$results3 <- renderText({
        RollButton()
        paste("You also wagered", vars$losses, "on numbers that did not win, so
              your net gain for this round is", vars$net.gain)
    })
    
}

# Run Application---------------------------------------------------------------

shinyApp(ui = ui, server = server)

