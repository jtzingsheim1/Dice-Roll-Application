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

# Set configuration variables
slider.def <- 0
slider.min <- 0
slider.max <- 100

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
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager3",
                         "Wager on 3:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager4",
                         "Wager on 4:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager5",
                        "Wager on 5:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager6",
                        "Wager on 6:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager7",
                        "Wager on 7:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager8",
                        "Wager on 8:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager9",
                        "Wager on 9:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager10",
                        "Wager on 10:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager11",
                        "Wager on 11:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),
            sliderInput("wager12",
                        "Wager on 12:",
                        value = slider.def,
                        min = slider.min,
                        max = slider.max),

            # Button to roll the dice
            actionButton("roll", "Roll Dice!")
            
        ),

        # Main panel for outputs
        mainPanel(
            
            # Display the payout table
            tableOutput("payout.table"),
            
            # Display stop message or dice results as applicable
            uiOutput("roll.output")
            
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
        vars$wager.total <- 0
        vars$win.wager <- 0
        vars$payouts <- as.integer(c(45, 20, 13, 9, 6, 4, 6, 9, 13, 20, 45))
        vars$pay.ratio <- 0
        vars$winnings <- 0
        vars$losses <- 0
        vars$net.gain <- 0
        
    # Render a table of the payouts offered for each outcome
    output$payout.table <- renderTable({
        data.frame(
            "Outcome" = 2:12,
            "Payout Multiple" = vars$payouts)
    })
    
    output$roll.output <- renderUI({
        if (vars$wager.total > vars$tokens) {
            textOutput("wager.message")
        } else {
            tagList(
                textOutput("dice.values"),
                textOutput("results1"),
                textOutput("results2"),
                textOutput("results3"),
                textOutput("results4"))
        }
        
    })
    
    output$wager.message <- renderText({
        "Sorry, you cannot wager more tokens than you currently have, reduce
        your bets to continue."
    })

    # Function to run when roll button is clicked
    RollButton <- eventReactive(input$roll, {

        # Roll the dice
        vars$die1 <- sample(1:6, 1)
            print(paste("First die:", vars$die1))
        vars$die2 <- sample(1:6, 1)
            print(paste("Second die:", vars$die2))
        vars$dice.sum <- sum(vars$die1, vars$die2)        
            print(paste("Dice sum:", vars$dice.sum))
        
        # Lookup wagers placed and find winning wager
        wagers <- c(input$wager2, input$wager3, input$wager4, input$wager5,
                    input$wager6, input$wager7, input$wager8, input$wager9,
                    input$wager10, input$wager11, input$wager12)
            print(wagers)
        vars$wager.total <- sum(wagers)
            print(paste("Total wagered:", vars$wager.total))
        vars$win.wager <- wagers[vars$dice.sum - 1]
            print(paste("Winning wager:", vars$win.wager))
        
        # Calculate payout, losses, net gain, and update token quantity
        vars$pay.ratio <- vars$payouts[vars$dice.sum - 1]
        vars$winnings <- vars$pay.ratio * vars$win.wager
        vars$losses <- sum(wagers) - vars$win.wager
        vars$net.gain <- vars$winnings - vars$losses
        vars$tokens <- vars$tokens + vars$net.gain
        
    })
    
    # Render the dice results output
    output$dice.values <- renderText({
        RollButton()
        paste("First Die:", vars$die1, "Second Die:", vars$die2)
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
              your net gain is", vars$net.gain)
    })
    
    output$results4 <- renderText({
        RollButton()
        paste("After this round you have", vars$tokens, "remaining, good luck!")
    })
    
}

# Run Application---------------------------------------------------------------

shinyApp(ui = ui, server = server)

