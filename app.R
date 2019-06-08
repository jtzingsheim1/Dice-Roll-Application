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
                        value = 0,
                        min = 0,
                        max = 100),
            sliderInput("wager3",
                         "Wager on 3:",
                         value = 0,
                         min = 0,
                         max = 100),
            sliderInput("wager4",
                         "Wager on 4:",
                         value = 0,
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


dice.values <- c(0, 0)

# RollDice <- function() {
#     sample(1:6, 2, replace = TRUE)
# }

# Server Logic------------------------------------------------------------------

server <- function(input, output) {
    
    # Reactive expression to create data frame summary
    wager.values <- reactive({
        data.frame(
            Name = c("Total Amount Wagered"),
            Value = as.character(c(
                sum(input$wager2, input$wager3, input$wager4))),
            stringsAsFactors = FALSE)
    })
    
    # Show the wager details in an HTML table
    output$total.wager <- renderTable({
        wager.values()
    })
    
    # Get dice outcomes when Roll button is clicked
    RollDice <- eventReactive(input$roll, {
        dice.values2 <- sample(1:6, 2, replace = TRUE)
    }, ignoreNULL = FALSE)
    
    # Show the dice values
    output$dice.outcome <- renderText({
        c(dice.values, RollDice(), dice.values2)
    })
    

    
}

# Run Application---------------------------------------------------------------

shinyApp(ui = ui, server = server)

