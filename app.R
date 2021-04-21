library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(lubridate)
library(reshape2)
library(ggplot2)

tdy <- Sys.Date()

atpHistory <- read_csv(paste0("atp_shiny_timeline_", tdy, ".csv" ), col_names = T)
atpRatings <- read_csv(paste0("atp_SR_ratings_", tdy, ".csv"), col_names = T)
matches <- read_csv(paste0("atp_shiny_matches_", tdy, ".csv"), col_names = TRUE)

atpHistory
atpRatings$Rating <- round(atpRatings$Rating, 2)
atpRatings$Rating

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
End_Date <- Sys.Date()

playerList <- unique(atpHistory$Player)

# Elo Player Timeline for plotting

eloTimeline <- melt(atpHistory, id.vars = "Player",variable.name = "Time", 
             value.name = "Rating")

# Elo Win Probability Function

probP1 <- function(p1_rtng, p2_rtng){
    dij <- p2_rtng - p1_rtng
    return(1 / (1 + 10 ^ (dij/400)))
}

# Build UI

ui <- fluidPage(
    theme = shinytheme("cerulean"),

    titlePanel(
        ("Tennis Head-to-Head")),
    
    sidebarLayout(
        sidebarPanel(
                dateRangeInput("dates", h4("Date Range Head-to-Head"),
                               start = Base_Date, end = End_Date),
                h4("Select Players"),
                selectInput(inputId = "player1", label = "Player 1", 
                            choices = playerList, selected = "Rafael Nadal"),
                selectInput(inputId = "player2", label = "Player 2", 
                            choices = playerList, selected = "Novak Djokovic"),
                h4("Calculate Elo Win Probabilities"),
                actionButton("winProb", "CALCULATE"),
                tableOutput("preds"),
                h4("Elo Timeline"),
                plotOutput("tPlot")),
        mainPanel(
            tableOutput("winLoss"),
            fluidRow(
            DT::dataTableOutput("h2h", width = "100%")
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
    
    output$h2h <- DT::renderDataTable({
        matches %>% dplyr::filter((Winner == input$player1 & Loser == input$player2) |
                                      (Loser == input$player1 & Winner == input$player2)) %>% 
            dplyr::filter(dplyr::between(`Event Date`, input$dates[1], input$dates[2]))
            
    })
    
    data <- eventReactive(input$winProb,{
        matchup <- tibble(Time = 1,
                           Player1 = input$player1,
                           Player2 = input$player2)
        
        plyr1Elo <- atpRatings$Rating[atpRatings$Player == input$player1]
        plyr2Elo <- atpRatings$Rating[atpRatings$Player == input$player2]
        plyr1Prob <- probP1(plyr1Elo, plyr2Elo)
        
        matchPred <- tibble(Player = c(input$player1, input$player2),
                            Elo = c(plyr1Elo, plyr2Elo),
                            `Win Probability` = c(plyr1Prob, 1 - plyr1Prob))
    })
    
    output$preds <- renderTable({
        data()
    })
    
    output$tPlot <- renderPlot({
        eloTimeline %>% filter(Player %in% c(input$player1, input$player2)) %>% 
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
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

