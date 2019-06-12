# Coursera Data Science Specialization Course 9 Project 3 Script----------------
# Dice Roll! Application


# The purpose of this script is to satisfy the requirements of the project 3
# peer-graded assignment which is part of the Developing Data Products course
# from Johns Hopkins University within the Data Science Specialization on
# Coursera.
#
# The instructions say to create an interactive application using Shiny. This
# project will meet the objective by creating a game where the user can place
# wagers on the outcome of a roll of two dice.


library(shiny)


# Set configuration variables
slider.def <- 0  # Slider default value
slider.min <- 0  # Slider minimum
slider.max <- 100  # Slider maximum
img.sz <- "100px"  # Image sizes


# User Interface----------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel("Dice Roll!"),
    
    # Set layout style to sidebar
    sidebarLayout(
        
        sidebarPanel(
            
            fluidRow(
                
                # Left side of sidebar
                column(6,

                    # Display instructions as help text
                    helpText("This is a game where you can place wagers on the
                    outcome of a roll of two dice. You are wagering on the sum
                    of a single roll of the dice. If you have a wager on the
                    winning outcome then you win a multiple of that amount, and
                    the multiple varies for each outcome according to the table
                    below. You lose any amounts wagered on outcomes that do not
                    occur. Place your wagers by using the sliders to the right
                    and when you are ready click the Roll Dice! button below. Be
                    careful, the dice are fair, but the payouts are not! Have
                    fun and good luck!"),
                    
                    br(),
                    
                    # Display the payout table
                    tableOutput("payout.table"),
                    
                    # Display current token and wager details
                    tableOutput("tokens.wagers"),
                    
                    # Button to roll the dice
                    actionButton("roll", "Roll Dice!")),
                
                # Right side of sidebar
                column(6,
                    
                    # Sliders to place wagers
                    sliderInput("wager2", "Wager on 2:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager3", "Wager on 3:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager4", "Wager on 4:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager5", "Wager on 5:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager6", "Wager on 6:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager7", "Wager on 7:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager8", "Wager on 8:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager9", "Wager on 9:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager10", "Wager on 10:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager11", "Wager on 11:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max),
                    sliderInput("wager12", "Wager on 12:",
                                value = slider.def,
                                min = slider.min,
                                max = slider.max)
                    )
                )
            ),
        
        # Main panel where output will be displayed
        mainPanel(uiOutput("roll.output"))

        )
    )


# Server Logic------------------------------------------------------------------

server <- function(input, output) {
    
    # Setup shared variables
    variables <- reactiveValues()
        variables$tokens <- 1000  # Initial number of tokens
        variables$round <- 0  # Counter for number of dice rolls
        
    # Assemble values and table of the payouts offered for each outcome
    Payouts <- reactive({
        print("Assembling payout values and table")
        # Set the payout multiples
        values <- as.integer(c(45, 20, 13, 9, 6, 4, 6, 9, 13, 20, 45))
        # Assemble into table
        table <- data.frame(
            "Outcome" = 2:12,
            "Payout Multiplier" = values)
        list(values = values, table = table)  # Returns as list
    })
    
    # Render a table of the payouts offered for each outcome
    output$payout.table <- renderTable({
        print("Rendering payout table")
        Payouts()$table
    })
    
    # Monitor the total wager amount
    Wagers <- reactive({
        print("Updating amount wagered")
        values <- c(input$wager2, input$wager3, input$wager4, input$wager5,
                    input$wager6, input$wager7, input$wager8, input$wager9,
                    input$wager10, input$wager11, input$wager12)
        total <- sum(values)
        print(paste("Current total wager:", total))
        list(values = values, total = total)  # Returns as list
    })
    
    # Render a table of the current tokens and total wager
    output$tokens.wagers <- renderTable({
        print("Rendering tokens and wagers table")
        var.names <- c("Total Tokens", "Total Wagered", "Tokens Available")
        tokens.available <- variables$tokens - Wagers()$total
        variables <- as.integer(c(variables$tokens, Wagers()$total,
                                  tokens.available))
        data.frame(var.names, variables)
    }, colnames = FALSE)

    # Function to run when roll button is clicked
    RollButton <- eventReactive(input$roll, {
        
        print("Begin RollButton routine")
        
        # Check for sufficient tokens before rolling dice
        if (Wagers()$total > variables$tokens) {
            
            print("Total wager exceeds total tokens, raise flag and stop")
            warn.flag <- TRUE  # Set flag for output style

        } else {
            
            print("Sufficient tokens to place wager, proceed")
            warn.flag <- FALSE  # Reset flag for output style
            variables$round <- variables$round + 1  # Increment to trigger roll

        }

        list(warn.flag = warn.flag)  # Returns as list
                
    })
    
    RollTheDice <- eventReactive(variables$round, {

        print("Rolling the dice")
        
        # Roll the dice
        die1 <- sample(1:6, 1)
        die2 <- sample(1:6, 1)
        dice.sum <- sum(die1, die2)        
        print(paste("Dice sum:", dice.sum))
        
        # Lookup winning wager and payout values
        win.wager <- Wagers()$values[dice.sum - 1]
        payout <- Payouts()$values[dice.sum - 1]

        # Calculate winnings, losses, net gain, and update token quantity
        winnings <- payout * win.wager
        losses <- Wagers()$total - win.wager
        net.gain <- winnings - losses
        print(paste("Net gain:", net.gain))
        variables$tokens <- variables$tokens + net.gain
        print(paste("Tokens:", variables$tokens))
        
        # Return all objects as a list
        list(die1 = die1, die2 = die2, dice.sum = dice.sum,
             win.wager = win.wager, payout = payout, winnings = winnings,
             losses = losses, net.gain = net.gain)
    })
    
    # Determine the output format based on wager validity
    output$roll.output <- renderUI({
        print("Attempting to prepare roll output")
        if (RollButton()$warn.flag) {
            print("Setting output style to warning message")
            h3(textOutput("wager.message"))
        } else {
            print("Setting output style to dice results")
            list(fluidRow(
                column(1,
                       imageOutput("die1", width = img.sz, height = img.sz)),
                column(1,
                       imageOutput("die2", width = img.sz, height = img.sz))),
                fluidRow(br(), br(), br(),
                         tableOutput("roll.results")))
        }
    })
    
    # Render the warning message for insufficient tokens
    output$wager.message <- renderText({
        print("Rendering wager warning message")
        "Sorry, you cannot wager more tokens than you currently have, reduce
        your bets to continue."
    })
     
    # Render the image for the first die
    output$die1 <- renderImage({
        filename <- paste0("die", RollTheDice()$die1, ".png")
        filepath <- normalizePath(file.path('./die_images', filename))
        list(src = filepath, width = img.sz, height = img.sz)
    }, deleteFile = FALSE)
    
    # Render the image for the second die
    output$die2 <- renderImage({
        filename <- paste0("die", RollTheDice()$die2, ".png")
        filepath <- normalizePath(file.path('./die_images', filename))
        list(src = filepath, width = img.sz, height = img.sz)
    }, deleteFile = FALSE)
     
    # Render a table of the results from the dice roll
    output$roll.results <- renderTable({
        print("Assembling table of the results of the dice roll")
        var.names <- c("Dice Sum", "Wager on Outcome", "Payout Multiplier",
                       "Amount Won", "Amount Lost", "Net Gain",
                       "Remaining Tokens")
        variables <- as.integer(c(RollTheDice()$dice.sum,
                                  RollTheDice()$win.wager,
                                  RollTheDice()$payout,
                                  RollTheDice()$winnings,
                                  RollTheDice()$losses,
                                  RollTheDice()$net.gain,
                                  variables$tokens))
        data.frame(var.names, variables)
    }, colnames = FALSE)

}


# Run Application---------------------------------------------------------------

shinyApp(ui = ui, server = server)

