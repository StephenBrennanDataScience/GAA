# The purpose of this script is to read in the GAA match data from google sheets and
# format clean it

# Load the required libraries
library(tidyverse)
library(lubridate)
library(googlesheets4)

# ---- Read in the data from the online source ----

# Define the URL where the data are located
kURL <- "https://docs.google.com/spreadsheets/d/1y5VpAqogmLXSVOBYKaGLKX2YOaAOZOJK2SYIN2SpgrA/edit#gid=887483570"

# In order to authenticate the googlesheets4 library, read in a test sheet and
# select 1 as the options for using my email
read_sheet(kURL, "2009")
1

# Define a function to read the data for a specific year
read_gaa_data <- function(Year){
  
  # Load the data
  RawData <- read_sheet(kURL,
                        sheet = as.character(Year),
                        range = "A:P",
                        col_types = "cccccccccccccccc")
  
  # Format and select the desired columns
  RawData %>%
    filter(Date != "NA") %>%
    mutate(Season = Year,
           Date = as_date(Date, format = "%d/%m/%Y"),
           Grade = Grade,
           Team1 = `Team 1`,
           Goals1 = as.numeric(G...6),
           Points1 = as.numeric(P...7),
           Team2 = `Team 2`,
           Goals2 = as.numeric(G...12),
           Points2 = as.numeric(P...13),
           Team1Home = (`Home?` == "Y")) %>%
    select(Season, Grade, Date, Team1, Goals1, Points1, Team2, Goals2, Points2, Team1Home)
}

# Read in 2009
GAAData <- read_gaa_data(2009)

# Read in the folowing years and continuously append
for (i in c(2010:2023)){
  GAAData <- GAAData %>%
    bind_rows(read_gaa_data(i))
}


# ---- Convert the data into long format ----

# Define correct team names
TeamList <- c("Antrim", "Armagh", "Carlow", "Cavan", "Clare", "Cork",
              "Derry", "Donegal", "Down", "Dublin", "Fermanagh",
              "Galway", "Kerry", "Kildare", "Kilkenny", "Laois",
              "Leitrim", "Limerick", "London", "Longford", "Louth",
              "Mayo", "Meath", "Monaghan", "New York", "Offaly",
              "Roscommon", "Sligo", "Tipperary", "Tyrone",
              "Waterford", "Westmeath", "Wexford", "Wicklow")

# Format names correctly and filter out non-names
GAAData <- GAAData %>%
  mutate(Team1 = str_to_title(Team1),
         Team2 = str_to_title(Team2)) %>%
  filter(Team1 %in% TeamList,
         Team2 %in% TeamList)
  

# Define the match ID
GAAData$ID <- c(1:nrow(GAAData))

# Define the data from Team1's perspective
Team1Data <- GAAData %>%
  mutate(Team = Team1,
         GoalsFor = Goals1,
         PointsFor = Points1,
         ScoreFor = 3*GoalsFor + PointsFor,
         Opposition = Team2,
         GoalsAgainst = Goals2,
         PointsAgainst = Points2,
         ScoreAgainst = 3*GoalsAgainst + PointsAgainst,
         Home = Team1Home) %>%
  select(ID, Date, Season, Grade, Team, Opposition, Home, GoalsFor, PointsFor, ScoreFor, GoalsAgainst, PointsAgainst, ScoreAgainst)

# Define the data from Team2's perspective
Team2Data <- GAAData %>%
  mutate(Team = Team2,
         GoalsFor = Goals2,
         PointsFor = Points2,
         ScoreFor = 3*GoalsFor + PointsFor,
         Opposition = Team1,
         GoalsAgainst = Goals1,
         PointsAgainst = Points1,
         ScoreAgainst = 3*GoalsAgainst + PointsAgainst,
         Home = FALSE) %>%
  select(ID, Date, Season, Grade, Team, Opposition, Home, GoalsFor, PointsFor, ScoreFor, GoalsAgainst, PointsAgainst, ScoreAgainst)

# Combine into one data frame
MatchData <- rbind(Team1Data, Team2Data)
rm(Team1Data, Team2Data)
