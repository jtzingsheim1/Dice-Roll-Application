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
library(DT)


# User Interface----------------------------------------------------------------

# Define UI for application that draws a histogram
ui.dice <- fluidPage(

    # Application title
    titlePanel("Dice Roll!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("wager2",
                        "Wager on 2:",
                        value = 0,
                        min = 0,
                        max = 100,
                        step = 1),
            sliderInput("wager3",
                         "Wager on 3:",
                         value = 0,
                         min = 0,
                         max = 100),
            sliderInput("wager4",
                         "Wager on 4:",
                         value = 0,
                         min = 0,
                         max = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("total.wager")
        )
    )
)


# Server Logic------------------------------------------------------------------

server.dice <- function(input, output) {

    output$total.wager <- renderText({
        sum(input$wager2, input$wager3, input$wager4)
    })
}

# Run Application---------------------------------------------------------------

shinyApp(ui = ui.dice, server = server.dice)

