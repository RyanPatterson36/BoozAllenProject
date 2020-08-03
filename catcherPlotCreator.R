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
require(visreg)
require(scales)
require(plotly)

conn <- dbConnect(SQLite(), 'baseball.db')

playerName <- "A. J. Ellis"
seasonData <- "seasonData2017"

query <-
  paste0("SELECT * FROM ",seasonData , " WHERE catcher_name = '", playerName, "'")

#Filter data so that it only contains pitches that the batter did not swing at
playerTaken <-
  dbGetQuery(conn, query) %>% filter(description == 'called_strike' | description == 'ball')


#Create the model to determine a pitch's strike probability given its location
playerStrikeMod <-
  gam(type == "S" ~ s(plate_x, plate_z),
      family = binomial,
      data = playerTaken)

#Create a grid of points for the player to encompass all possible points
playerPitchGrid <- playerTaken %>%
  data_grid(plate_x = seq_range(plate_x, 150),
            plate_z = seq_range(plate_z, 150))

#Predict the strike probability of a pitch given the grid locations
playerPitchGridMod <-
  playerStrikeMod %>% augment(type.predict = "response", newdata = playerPitchGrid)

title <- paste0(playerName, "'s Strike Probability Chart")

#Create the plot

playerPlot <- k_zone_plot %+% playerPitchGridMod + geom_tile(aes(fill = .fitted), alpha = 0.7) +
  scale_fill_gradient(low = "gray", high = "red", name = "Estimated Strike Percentage") + ggtitle(title)

ggplotly(playerPlot)

