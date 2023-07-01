# The purpose of this script is to use the fitted models to predict results

# Load the required libraries
library(tidyverse)

MatchSimulation <- function(HomeTeam = "Galway",
                            AwayTeam = "Derry",
                            Season = 2023,
                            NeutralVenue = TRUE,
                            NumberSims = 100000){
  temp <- tibble(TeamFactor = as.factor(c(HomeTeam, AwayTeam)),
                 OppositionFactor = as.factor(c(AwayTeam, HomeTeam)),
                 Season = Season,
                 HomeFactor = as.factor(c(!NeutralVenue, FALSE))) %>%
    mutate(SimMeanGoals = predict(fit_Goals, type = "response", newdata = .),
           SimMeanPoints = predict(fit_Points, type = "response", newdata = .))
  
  Sim <- tibble(ID <- c(1:NumberSims),
                Team = HomeTeam,
                Opposition = AwayTeam,
                GoalsFor = rpois(NumberSims, temp$SimMeanGoals[1]),
                PointsFor = rpois(NumberSims, temp$SimMeanPoints[1]),
                ScoreFor = 3*GoalsFor + PointsFor,
                GoalsAgainst = rpois(NumberSims, temp$SimMeanGoals[2]),
                PointsAgainst = rpois(NumberSims, temp$SimMeanPoints[2]),
                ScoreAgainst = 3*GoalsAgainst + PointsAgainst,
                NetScore = ScoreFor - ScoreAgainst,
                Results = if_else(NetScore > 0, "Win", if_else(NetScore == 0, "Draw", "Lose")))
  
  Sim %>%
    ggplot(aes(x = NetScore, fill = Results)) +
    geom_vline(xintercept = 0) +
    geom_histogram(binwidth = 1, colour = "white") +
    geom_vline(xintercept = mean(Sim$NetScore), linetype = 2) +
    theme_bw() +
    coord_flip() +
    scale_fill_manual(values = c("grey", "#C70039", "#72D54F")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = glue::glue("{HomeTeam} vs. {AwayTeam}"),
         subtitle = "Net Score after 100,000 simulations",
         y = "Number of Simulations") +
    theme(legend.position = "none")
}



MatchSimulation("Kerry", "Tyrone", Season = 2023)
MatchSimulation("Dublin", "Mayo", Season = 2023)
MatchSimulation("Derry", "Cork", Season = 2023)
MatchSimulation("Armagh", "Monaghan", Season = 2023)
