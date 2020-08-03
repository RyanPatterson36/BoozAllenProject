require(RSQLite)


conn <- dbConnect(SQLite(), 'baseball.db')

#testData <- dbGetQuery(conn, "SELECT * FROM seasonData2017Taken")

testData <- scrape_statcast_savant(start_date = Sys.Date() - 7, end_date = Sys.Date())
testData <- testData %>% filter(description == "called_strike" | description == "ball")

testData <- testData %>% mutate(strike_prob = predict(strikePercentage2019, newdata = ., type = "response"))
testData <- testData %>% mutate(strikeStolen = ifelse(strike_prob < .5 & type == "S", 1, 0))
testData <- testData %>% mutate(strikeLost = ifelse(strike_prob >= .5 & type == "B", 1, 0))
testData <- testData %>% mutate(strikeStolenWeight = ifelse(strikeStolen == 1, 1 - strike_prob, 0))
testData <- testData %>% mutate(strikeLostWeight = ifelse(strikeLost == 1, strike_prob, 0))
#testData <- testData %>% mutate(expectedStrikeWeight = ifelse(strikeStolen == 0 & strikeLost == 0 & type == "S", 1 - strike_prob, 0))
#testData <- testData %>% mutate(expectedBallWeight = ifelse(strikeStolen == 0 & strikeLost == 0 & type == "B", strike_prob, 0))
dbWriteTable(conn, "testTable", testData, overwrite = TRUE)

test <- dbGetQuery(
  conn,
  "SELECT Catcher_Name, 
  sum(strikeStolen) AS numberOfStolenStrikes,
  
  sum(strikeLost) AS numberOfLostStrikes, 
  
  sum(strikeStolen) - sum(strikeLost) AS differenceOfStrikes,
  
  round(sum(strikeStolenWeight) - sum(strikeLostWeight), 3) AS weightedDifferenceOfStrikes,
  
  ((sum(strikeStolen) - sum(strikeLost))*.125) AS totalRunDifference,
  
  round((sum(strikeStolenWeight) - sum(strikeLostWeight)) * .125, 3)  AS weightedTotalRunDifference,
  
  round((((sum(strikeStolen) - sum(strikeLost))*.125) / 10.02), 4) AS framingWAR,
  
  round((((sum(strikeStolenWeight) - sum(strikeLostWeight)) * .125) / 10.02), 4) AS weightedFramingWAR,
  
  round((sum(strikeStolen) - sum(strikeLost))/count(*), 4) AS strikesStolenPerPitchCaught, 
  
  count(*) AS totalPitchesCaught
  
  FROM  
    testTable
  WHERE 
    Catcher_Name IS NOT NULL
  GROUP BY 
    Catcher_Name 
  HAVING 
    count(*) > 100
  ORDER BY 
    differenceOfStrikes DESC"
)

