require(RSQLite)
require(baseballr)
require(pitchRx)
require(mlbgameday)
require(ggplot2)
require(devtools)
require(tidyverse)
require(broom)
require(mgcv)
require(modelr)
require(lme4)
require(factoextra)

masterTableTaken <- dbGetQuery(conn, "SELECT * FROM masterTable") %>% filter(description == 'called_strike' | description == 'ball')

# Creates the model to analyze strike percentages
strikePercentageMaster <-
  gam(type == "S" ~ s(plate_x, plate_z),
      family = binomial,
      data = masterTableTaken)

# Predicts the strike percentage of each pitch given location over a smooth surface
predictedStrikeMaster <-
  strikePercentageMaster %>% augment(type.predict = "response")

# Creates a grid of all possible points to predict
GridMaster <-
  masterTableTaken %>% data_grid(plate_x = seq_range(plate_x, n = 250),
                                    plate_z = seq_range(plate_z, n = 250))

# Predicts the strike percentage of pitches given the grid location
predictedStrikeGridMaster <-
  strikePercentageMaster %>% augment(type.predict = "response", newdata = GridMaster)

# Plots the pitches
tilePlotMaster <-
  k_zone_plot %+% predictedStrikeGridMaster + geom_tile(aes(fill = .fitted), alpha = 0.7) +
  scale_fill_gradient(low = "gray", high = "red", name = "Estimated Strike Percentage")

tilePlotMaster

# Adds the strike probability column to the season data
masterTableTaken <-
  masterTableTaken %>% mutate(strike_prob = predict(strikePercentageMaster, newdata = .,
                                                  type = "response"))
# Filters the taken data
masterTableTaken <-
  masterTableTaken %>% filter(description == "called_strike" |
                              description == "ball")
