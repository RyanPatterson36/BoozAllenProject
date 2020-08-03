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
require(caret)
require(xgboost)


conn <- dbConnect(SQLite(), 'baseball.db')

treeData <- dbGetQuery(conn, "SELECT * FROM masterTableTaken")
treeData <-
  treeData %>% drop_na(plate_x, plate_z, release_pos_x, release_pos_z)

set.seed(36)

training.samples <-
  treeData$type %>% createDataPartition(p = 0.8, list = F)

train.data <- treeData[training.samples,]
test.data <- treeData[-training.samples,]

set.seed(36)

treeModel <-
  train(
    type ~ plate_x + plate_z + release_pos_x + release_pos_z,
    data = train.data,
    method = "xgbTree",
    trControl = trainControl("cv", number = 5)
  )
predicted.classes <- model %>% predict(test.data)
mean(predicted.classes == test.data$type)

treeData <-
  treeData %>% mutate(predictedCall = predict(treeModel, newdata = ., type.predict = "response"))

treeData <-
  treeData %>% mutate(strikeStolen2 = ifelse(predictedCall == "B" &
                                               type == "S", 1, 0))

treeData <-
  treeData %>% mutate(strikeLost2 = ifelse(predictedCall == "S" &
                                             type == "B", 1, 0))

dbWriteTable(conn, "masterTableTakenXGBTree", treeData, overwrite = T)

catcherStrikesStolenMasterXGBTree <- dbGetQuery(
  conn,
  "SELECT Catcher_Name,
  sum(strikeStolen) AS numberOfStolenStrikes,

  sum(strikeStolen2) AS numberOfStolenStrikes2,

  sum(strikeLost) AS numberOfLostStrikes,
  sum(strikeLost2) AS numberOfLostStrikes2,


  sum(strikeStolen) - sum(strikeLost) AS differenceOfStrikes,

  sum(strikeStolen2) - sum(strikeLost2) AS differenceOfStrikes2,

  ((sum(strikeStolen) - sum(strikeLost))*.125) AS totalRunDifference,

    ((sum(strikeStolen2) - sum(strikeLost2))*.125) AS totalRunDifference2,


  round((((sum(strikeStolen) - sum(strikeLost))*.125) / 10.02), 4) AS framingWAR,
    round((((sum(strikeStolen2) - sum(strikeLost2))*.125) / 10.02), 4) AS framingWAR2,


  round((sum(strikeStolen) - sum(strikeLost))/count(*), 4) AS strikesStolenPerPitchCaught,

  round((sum(strikeStolen2) - sum(strikeLost2))/count(*), 4) AS strikesStolenPerPitchCaught2,

  count(*) AS totalPitchesCaught

  FROM
    masterTableTakenXGBTree
  WHERE
    Catcher_Name IS NOT NULL
  GROUP BY
    Catcher_Name
  HAVING
    count(*) > 500
  ORDER BY
    differenceOfStrikes DESC"
)
