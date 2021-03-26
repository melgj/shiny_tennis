
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(PlayerRatings)
library(lubridate)
library(reshape2)
library(ggplot2)
library(plotly)


matches <- read_csv("atp_matches.csv")
matches$tourney_date <- lubridate::ymd(matches$tourney_date)
matches$Year <- lubridate::year(matches$tourney_date)

matches <- matches %>% 
    dplyr::select(Year, tourney_date, tourney_name, surface, round, best_of, winner_name, loser_name, score) %>%
    dplyr::rename(`Event Date` = tourney_date,
                  `Event Name` = tourney_name,
                  Surface = surface,
                  Round = round,
                  `Best Of` = best_of,
                  Winner = winner_name,
                  Loser = loser_name,
                  `Match Score` = score) 

rndLevels <- c("R128","R64","R32","R16","RR","QF","SF","BR","F")

matches$Round <- factor(matches$Round, rndLevels, ordered = T)

matches <- matches %>% 
    dplyr::arrange(desc(`Event Date`), desc(Round))

Base_Date <- min(matches$`Event Date`)
End_Date <- max(matches$`Event Date`)

match_hist <- matches %>% 
    mutate(Period = lubridate::interval(Base_Date, matches$`Event Date`) %/% weeks(1)+1,
           Result = 1) %>% 
    arrange(`Event Date`, Round) %>% 
    select(Period, Winner, Loser, Result)

sElo <- steph(match_hist, init = c(1500,300), history = TRUE)

ratings <- as_tibble(sElo$ratings) %>% 
    filter(Lag <= 25, Rating >= 1500, Games >= 30) %>% 
    arrange(Player)

playerList <- ratings$Player

# Elo Player Timeline for plotting

timeline <- as_tibble(sElo$history, rownames = "Player")

timeline <- timeline %>% 
    filter(Player %in% playerList) %>% 
    select(Player, ends_with("Rating")) %>% 
    select(Player, last_col(offset = 99):last_col())

eloTl <- melt(timeline, id.vars = "Player",variable.name = "Time", 
             value.name = "Rating")

# Build UI

ui <- fluidPage(
    theme = shinytheme("cerulean"),

    titlePanel(
        ("Tennis Head-to-Head")),
    
    sidebarLayout(
        sidebarPanel(
                dateRangeInput("dates", h4("Date Range Head-to-Head"),
                               start = "2000-01-01", end = End_Date),
                h4("Select Players"),
                selectInput(inputId = "player1", label = "Player 1", 
                            choices = playerList, selected = playerList[[9]]),
                selectInput(inputId = "player2", label = "Player 2", 
                            choices = playerList, selected = playerList[[10]]),
                h4("Elo Timeline"),
                plotOutput("tPlot"),
                h4("Calculate Elo Win Probabilities"),
                actionButton("winProb", "CALCULATE"),
                tableOutput("preds")),
        mainPanel(
            tableOutput("winLoss"),
            fluidRow(
            column(8,
            dataTableOutput("h2h"))
            )
        )
    ),
    
    tags$footer(h6("Match data sourced from Jeff Sackmann Tennis Abstract https://github.com/JeffSackmann"))
)

# Build server function

server <- function(input, output) {
    output$winLoss <- renderTable({
        matches %>% dplyr::filter((Winner == input$player1 & Loser == input$player2) |
                                      (Loser == input$player1 & Winner == input$player2)) %>% 
            dplyr::filter(dplyr::between(`Event Date`, input$dates[1], input$dates[2])) %>% 
            group_by(Winner) %>% 
            summarise(Wins = n()) %>% 
            rename(Player = Winner) %>% 
            arrange(desc(Wins))
        
    })
    output$h2h <- renderDataTable({
        matches %>% dplyr::filter((Winner == input$player1 & Loser == input$player2) |
                                      (Loser == input$player1 & Winner == input$player2)) %>% 
            dplyr::filter(dplyr::between(`Event Date`, input$dates[1], input$dates[2]))
            
    })
    
    data <- eventReactive(input$winProb,{
        
        matchup <- tibble(Time = 1,
                           Player1 = input$player1,
                           Player2 = input$player2)
        
        predProb <- predict(sElo, matchup, trat = c(1500, 300), gamma = 0)
        
        plyr1Elo <- ratings$Rating[ratings$Player == input$player1]
        plyr2Elo <- ratings$Rating[ratings$Player == input$player2]
        
        matchPred <- tibble(Player = c(input$player1, input$player2),
                            Elo = c(plyr1Elo, plyr2Elo),
                            `Win Probability` = c(round(predProb, 2), round(1 - predProb, 2)))
                
    })
    
    output$preds <- renderTable({
        data()
    })
    
    output$tPlot <- renderPlot({
        eloTl %>% filter(Player %in% c(input$player1, input$player2)) %>% 
            ggplot(aes(x = Time,
                   y = Rating,
                   col = Player,
                   group = Player)) +
            labs(title = "Elo Timeline", 
                 x = "Time", y = "Rating") +
            scale_x_discrete(labels = NULL) +
            ylim(1500,2100) +
            geom_hline(yintercept = c(1600, 1700, 1800, 1900, 2000, 2100), col = "grey", lty = 2) +
            geom_line(lwd = 1)
            #geom_smooth(lty = 2, lwd = 0.2, col = "red")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

