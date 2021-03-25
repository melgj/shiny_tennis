
library(data.table)
library(shiny)
library(shinythemes)
library(dplyr)


matches <- fread("atp_matches.csv")
matches$tourney_date <- lubridate::ymd(matches$tourney_date)
matches$year <- lubridate::year(matches$tourney_date)

matches <- matches %>% 
    dplyr::select(year, tourney_date, tourney_name, surface, round, best_of, winner_name, loser_name, score) %>%
    dplyr::arrange(desc(tourney_date))

ratings <- fread("atp_ratings.csv")
ratings <- ratings %>%
    dplyr::select(Player, Rating) %>% 
    dplyr::arrange(Player)

playerList <- ratings$Player

probP1 <- function(p1_rtng, p2_rtng){
    dij <- p2_rtng - p1_rtng
    return(1 / (1 + 10 ^ (dij/400)))
}

ui <- fluidPage(
    theme = shinytheme("cerulean"),

 
    titlePanel(
        h1("Tennis Head-to-Head", align = "left")),
    
    sidebarLayout(
        sidebarPanel(
                dateRangeInput("dates", h4("Date Range Head-to-Head"), start = "2000-01-01"),
                h4("Select Players"),
                selectInput(inputId = "player1", label = "Player 1", 
                            choices = playerList, selected = playerList[[1]]),
                selectInput(inputId = "player2", label = "Player 2", 
                            choices = playerList, selected = playerList[[2]]),
                h4("Calculate Elo Win Probabilities"),
                actionButton("winProb", "PREDICT"),
                tableOutput("preds")),
        mainPanel(
            column(10,
                   tableOutput("winLoss")
            ),
            column(10,
                   dataTableOutput("h2h")))
    )
)


server <- function(input, output) {
    output$winLoss <- renderTable({
        matches %>% dplyr::filter((winner_name == input$player1 & loser_name == input$player2) |
                                      (loser_name == input$player1 & winner_name == input$player2)) %>% 
            dplyr::filter(dplyr::between(tourney_date, input$dates[1], input$dates[2])) %>% 
            group_by(winner_name) %>% 
            summarise(Wins = n()) %>% 
            rename(Player = winner_name) %>% 
            arrange(desc(Wins))
        
    })
    output$h2h <- renderDataTable({
        matches %>% dplyr::filter((winner_name == input$player1 & loser_name == input$player2) |
                                      (loser_name == input$player1 & winner_name == input$player2)) %>% 
            dplyr::filter(dplyr::between(tourney_date, input$dates[1], input$dates[2]))
    })
    
    data <- eventReactive(input$winProb,{
    
                p1Elo <- round(ratings$Rating[ratings$Player == input$player1],2)
                p2Elo <- round(ratings$Rating[ratings$Player == input$player2],2)
                
                p1Prob <- probP1(p1Elo, p2Elo)
                p2Prob <- 1 - p1Prob
                
                probTable <- data.table(Player = c(input$player1, input$player2),
                                        `Elo Rating` = c(p1Elo, p2Elo),
                                        `Win Probability` = c(p1Prob, p2Prob))
    })
    
    output$preds <- renderTable({
        data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

