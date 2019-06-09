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

    # Assemble values and table of the payouts offered for each outcome
    Payouts <- reactive({
        print("Assembling payout values and table")
        # Set the payout values
        values <- as.integer(c(45, 20, 13, 9, 6, 4, 6, 9, 13, 20, 45))
        # Assemble into table
        table <- data.frame(
            "Outcome" = 2:12,
            "Payout" = values)
        list(values = values, table = table)
    })
    
    # Render a table of the payouts offered for each outcome
    output$payout.table <- renderTable({
        print("Rendering payout table")
        Payouts()$table
    })

    # Check for valid wagers before continuing
    output$roll.output <- renderUI({
        print("Attempting to prepare roll output")
        if (RollButton()$warn.flag) {
            print("Setting output style to warning message")
            textOutput("wager.message")
        } else {
            print("Setting output style to dice results")
            tagList(
                textOutput("dice.values"),
                tableOutput("roll.results"))
        }
    })
    
    # # Check for valid wagers before continuing
    # output$roll.output <- renderUI({
    #     print("Attempting to prepare roll output")
    #     if (RollButton()$warn.flag) {
    #         print("Setting output style to warning message")
    #         textOutput("wager.message")
    #     } else {
    #         print("Setting output style to dice results")
    #         tagList(
    #             textOutput("dice.values"),
    #             textOutput("results1"),
    #             textOutput("results2"),
    #             textOutput("results3"),
    #             textOutput("results4"))
    #     }
    # })
    
    # Function to run when roll button is clicked
    RollButton <- eventReactive(input$roll, {

        print("Roll Button Clicked")
        
        # Lookup wagers placed
        wagers <- c(input$wager2, input$wager3, input$wager4, input$wager5,
                    input$wager6, input$wager7, input$wager8, input$wager9,
                    input$wager10, input$wager11, input$wager12)
            #print("Printing wagers vector below")
            #print(wagers)
        wager.total <- sum(wagers)
            #print(paste("Total wagered:", wager.total))
        
        # Check for sufficient tokens
        #print("Checking for sufficient tokens")
        if (wager.total > vars$tokens) {
            print("Total wager exceeds total tokens, raise flag and stop")
            warn.flag <- TRUE
            list(wager.total = wager.total, warn.flag = warn.flag)
        } else {
            
            print("Sufficient tokens to place wager, roll dice")
            warn.flag <- FALSE  # Reset flag for warning message
            
            # Roll the dice
            die1 <- sample(1:6, 1)
                #print(paste("First die:", die1))
            die2 <- sample(1:6, 1)
                #print(paste("Second die:", die2))
            dice.sum <- sum(die1, die2)        
                print(paste("Dice sum:", dice.sum))
                
            # Lookup winning wager and payout value
            win.wager <- wagers[dice.sum - 1]
                #print(paste("Winning wager:", win.wager))
            payout <- Payouts()$values[dice.sum - 1]
                #print(paste("Payout:", payout))
            
            # Calculate winnings, losses, net gain, and update token quantity
            winnings <- payout * win.wager
                #print(paste("Winnings:", winnings))
            losses <- wager.total - win.wager
                #print(paste("Losses:", losses))
            net.gain <- winnings - losses
                print(paste("Net gain:", net.gain))
            vars$tokens <- vars$tokens + net.gain
                print(paste("Tokens:", vars$tokens))
            
            # Return all objects as a list
            #print("Returning list of RollButton variables, end of Roll Button")
            list(die1 = die1, die2 = die2, dice.sum = dice.sum,
                 wager.total = wager.total, win.wager = win.wager,
                 payout = payout, winnings = winnings, losses = losses,
                 net.gain = net.gain, warn.flag = warn.flag)
            
        }

    })

    output$wager.message <- renderText({
        "Sorry, you cannot wager more tokens than you currently have, reduce
        your bets to continue."
    })
       
    # Render the dice results output
    output$dice.values <- renderText({
        paste("First Die:", RollButton()$die1, "Second Die:", RollButton()$die2)
    })
    
    # Assemble table of the results of the dice roll
    output$roll.results <- renderTable({
        print("Assembling table of the results of the dice roll")
        var.names <- c("Dice Sum", "Wager on Outcome", "Outcome Payout",
                       "Amount Won", "Amount Lost", "Net Gain",
                       "Remaining Tokens")
        variables <- as.integer(c(RollButton()$dice.sum, RollButton()$win.wager,
                       RollButton()$payout, RollButton()$winnings,
                       RollButton()$losses, RollButton()$net.gain, vars$tokens))
        data.frame(var.names, variables)
    }, colnames = FALSE)
    

    
    # output$results1 <- renderText({
    #     paste("The sum is", RollButton()$dice.sum, "and you wagered",
    #           RollButton()$win.wager, "on this outcome.")
    # })
    # 
    # output$results2 <- renderText({
    #     paste("This outcome pays", RollButton()$pay.ratio, "to one, so you win",
    #           RollButton()$winnings, "tokens.")
    # })
    # 
    # output$results3 <- renderText({
    #     paste("You also wagered", RollButton()$losses, "on numbers that did not win, so
    #           your net gain is", RollButton()$net.gain)
    # })
    # 
    # output$results4 <- renderText({
    #     paste("After this round you have", vars$tokens, "remaining, good luck!")
    # })
    
}


# Run Application---------------------------------------------------------------

shinyApp(ui = ui, server = server)

