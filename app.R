#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(data.table)

matches <- fread("matches.csv")
matches$tourney_date <- lubridate::ymd(matches$tourney_date)
matches$year <- lubridate::year(matches$tourney_date)
matches
matches <- matches %>% 
    select(year, tourney_date, tourney_name, surface, round, best_of, winner_name, loser_name, score) %>%
    filter(year >= 2000) %>% 
    arrange(desc(tourney_date))
matches
ratings <- fread("ratings.csv")
ratings <- ratings %>% arrange(Player)
playerList <- ratings$Player

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),

    # Application title
    titlePanel(
        h1("Tennis Head to Head", align = "left")),
    
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                dateRangeInput("dates", h4("Date Range"), start = "2000-01-01"),
                h4("Select Players"),
                selectInput(inputId = "player1", label = "Player 1", choices = playerList, selected = playerList[[1]]),
                selectInput(inputId = "player2", label = "Player 2", choices = playerList, selected = playerList[[2]])),
            fluidRow(
                h4("Get Elo Win Probabilities"),
                actionButton("winProb", "Predict"))),
        mainPanel(
            fluidRow(
                column(10,
                dataTableOutput("h2h"))))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$h2h <- renderDataTable({
        matches %>% dplyr::filter((winner_name == input$player1 & loser_name == input$player2) |
                                      (loser_name == input$player1 & winner_name == input$player2)) %>% 
            filter(dplyr::between(tourney_date, input$dates[1], input$dates[2]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
