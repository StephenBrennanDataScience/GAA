# The purpose of this script is to attempt to model the points and goals scored
# by each team

# Load the required packages
library(tidyverse)
library(fitdistrplus)

# Load the raw data
RawMatchData <- MatchData

# Remove NAs
MatchData <- RawMatchData %>%
  filter(!is.na(ScoreFor))

# ---- Data Preparation ----

# Define correct team names
TeamList <- c("Antrim", "Armagh", "Carlow", "Cavan", "Clare", "Cork",
              "Derry", "Donegal", "Down", "Dublin", "Fermanagh",
              "Galway", "Kerry", "Kildare", "Kilkenny", "Laois",
              "Leitrim", "Limerick", "London", "Longford", "Louth",
              "Mayo", "Meath", "Monaghan", "New York", "Offaly",
              "Roscommon", "Sligo", "Tipperary", "Tyrone",
              "Waterford", "Westmeath", "Wexford", "Wicklow")

# Format the data for modelling
MatchData <- MatchData %>%
  mutate(TeamFactor = factor(Team, levels = TeamList),
         OppositionFactor = factor(Opposition, levels = TeamList),
         HomeFactor = as_factor(Home))

# Split the data into training (pre-2021), and test (2021+)
TrainingData <- MatchData %>%
  filter(Season <= 2023,
         Season >= 2021)

TestData <- MatchData %>%
  filter(Season >= 2023)


# ---- Model the goals scored ----

fit_Goals <- glm(data = TrainingData,
                   family = poisson(link = "log"),
                   formula = GoalsFor ~ Season + TeamFactor + OppositionFactor + HomeFactor)

summary(fit_Goals)

TestData <- TestData %>%
  mutate(GoalsForFitted = predict(fit_Goals, type = "response", newdata = TestData),
         GoalsForResidual = GoalsFor - GoalsForFitted)

chisq.test(x = TestData$GoalsFor, y = TestData$GoalsForFitted)


# ---- Plot the modelled parameters ----

# Extract the coefficients
df <- tibble(Name = names(fit_Goals$coefficients)) %>%
  cbind(summary(fit_Goals)$coefficients %>%  as_tibble())

# Plot the percentage difference average goals scored compared to the base (Antrim)
df %>%
  filter(str_detect(Name, "TeamFactor"),
         Name != "TeamFactorNew York") %>%
  mutate(Name = str_remove(Name, "TeamFactor")) %>%
  rbind(c("Antrim", 0, NA, NA, NA), .) %>%
  mutate(ResponseEstimate = exp(as.numeric(Estimate)) - 1,
         ErrorUp = exp(as.numeric(Estimate) + as.numeric(`Std. Error`)) - 1,
         ErrorDown = exp(as.numeric(Estimate) - as.numeric(`Std. Error`)) - 1) %>%
  ggplot(aes(x = ResponseEstimate, y = Name)) +
  geom_segment(aes(x = ErrorDown, xend = ErrorUp, yend = Name)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title = "Model parameters for predicted goals scored",
       subtitle = "Presented as the percentage difference relative to the chosen base Team, Antrim",
       x = "Percentage difference in predicted goals compared with Antrim",
       y = element_blank())

# Plot the percentage difference average goals conceded compared to the base (Antrim)
df %>%
  filter(str_detect(Name, "OppositionFactor")) %>%
  mutate(Name = str_remove(Name, "OppositionFactor")) %>%
  rbind(c("Antrim", 0, NA, NA, NA), .) %>%
  mutate(ResponseEstimate = exp(as.numeric(Estimate)) - 1,
         ErrorUp = exp(as.numeric(Estimate) + as.numeric(`Std. Error`)) - 1,
         ErrorDown = exp(as.numeric(Estimate) - as.numeric(`Std. Error`)) - 1) %>%
  ggplot(aes(x = ResponseEstimate, y = Name)) +
  geom_segment(aes(x = ErrorDown, xend = ErrorUp, yend = Name)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title = "Model parameters for predicted goals conceded",
       subtitle = "Presented as the percentage difference relative to the chosen base Team, Antrim",
       x = "Percentage difference in predicted goals compared with Antrim",
       y = element_blank())


# ---- Model the points scored ----

fit_Points <- glm(data = TrainingData,
                 family = poisson(link = "log"),
                 formula = PointsFor ~ Season + TeamFactor + OppositionFactor + HomeFactor)

summary(fit_Points)

TestData <- TestData %>%
  mutate(PointsForFitted = predict(fit_Points, type = "response", newdata = TestData),
         PointsForResidual = PointsFor - PointsForFitted)

chisq.test(x = TestData$PointsFor, y = TestData$PointsForFitted)

# ---- Plot the modelled parameters ----

# Extract the coefficients
df <- tibble(Name = names(fit_Points$coefficients)) %>%
  cbind(summary(fit_Points)$coefficients %>%  as_tibble())

# Plot the percentage difference average points scored compared to the base (Antrim)
df %>%
  filter(str_detect(Name, "TeamFactor")) %>%
  mutate(Name = str_remove(Name, "TeamFactor")) %>%
  rbind(c("Antrim", 0, NA, NA, NA), .) %>%
  mutate(ResponseEstimate = exp(as.numeric(Estimate)) - 1,
         ErrorUp = exp(as.numeric(Estimate) + as.numeric(`Std. Error`)) - 1,
         ErrorDown = exp(as.numeric(Estimate) - as.numeric(`Std. Error`)) - 1) %>%
  ggplot(aes(x = ResponseEstimate, y = Name)) +
  geom_segment(aes(x = ErrorDown, xend = ErrorUp, yend = Name)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title = "Model parameters for predicted points scored",
       subtitle = "Presented as the percentage difference relative to the chosen base Team, Antrim",
       x = "Percentage difference in predicted points compared with Antrim",
       y = element_blank())

# Plot the percentage difference average points conceded compared to the base (Antrim)
df %>%
  filter(str_detect(Name, "OppositionFactor")) %>%
  mutate(Name = str_remove(Name, "OppositionFactor")) %>%
  rbind(c("Antrim", 0, NA, NA, NA), .) %>%
  mutate(ResponseEstimate = exp(as.numeric(Estimate)) - 1,
         ErrorUp = exp(as.numeric(Estimate) + as.numeric(`Std. Error`)) - 1,
         ErrorDown = exp(as.numeric(Estimate) - as.numeric(`Std. Error`)) - 1) %>%
  ggplot(aes(x = ResponseEstimate, y = Name)) +
  geom_segment(aes(x = ErrorDown, xend = ErrorUp, yend = Name)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title = "Model parameters for predicted points conceded",
       subtitle = "Presented as the percentage difference relative to the chosen base Team, Antrim",
       x = "Percentage difference in predicted points compared with Antrim",
       y = element_blank())

