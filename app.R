require(RSQLite)
require(ggplot2)
require(tidyverse)
require(broom)
require(mgcv)
require(modelr)
require(lme4)
require(shiny)
require(shinydashboard)
require(DT)
require(plotly)
require(factoextra)
require(formattable)
require(kableExtra)
require(shinyWidgets)


# Creates the connection to the database
conn <- dbConnect(SQLite(), 'baseballShiny.db')


# Creates the list of names to be used in the drop down choice tab
catcherNames2017 <- dbGetQuery(conn, "SELECT Catcher_Name, count(*) FROM seasonData2017Taken GROUP BY 1 HAVING COUNT(*) > 250 ORDER BY 1 ") %>% filter(Catcher_Name != "NA")
catcherNames2018 <- dbGetQuery(conn, "SELECT Catcher_Name, count(*) FROM seasonData2018Taken GROUP BY 1 HAVING COUNT(*) > 250 ORDER BY 1 ") %>% filter(Catcher_Name != "NA")
catcherNames2019 <- dbGetQuery(conn, "SELECT Catcher_Name, count(*) FROM seasonData2019Taken GROUP BY 1 HAVING COUNT(*) > 250 ORDER BY 1 ") %>% filter(Catcher_Name != "NA")
catcherNamesAll <- dbGetQuery(conn, "SELECT Catcher_Name, count(*) FROM masterTableTaken GROUP BY 1 HAVING COUNT(*) > 500 ORDER BY 1 ") %>% filter(Catcher_Name != "NA")


#Creates the base strike zone plot to be used
k_zone_plot <- ggplot(NULL, aes(x = `Horizontal Plate Location (ft.)`, y = `Vertical Plate Location (ft.)`)) +
    geom_rect(xmin = -.947,
              xmax = .947,
              ymin = 1.56,
              ymax = 3.39, color = "black", alpha = 0.01) +
    coord_equal() +
    scale_x_continuous("Horizontal location (ft.)",
                       limits = c(-2, 2)) +
    scale_y_continuous("Vertical location (ft.)",
                       limits = c(0, 5))

# Define UI for application
ui <- fluidPage(navbarPage(
    "Catcher Framing Analysis",
    tabPanel(
        "Catcher Framing Heatmaps",
        sidebarPanel(
            helpText("Select which season you would like to view:"),
            radioButtons(
                "seasonData",
                "Season Data",
                c(  "2017 Season" = "seasonData2017Taken",
                    "2018 Season" = "seasonData2018Taken",
                    "2019 Season" = "seasonData2019Taken",
                    "2017-2019 Seasons" = "masterTableTaken"
                 )),
            conditionalPanel(
                condition = "input.seasonData == 'seasonData2017Taken'",
                helpText("Select a catcher that you would like to view the plot for:"),
                selectInput("playerName2017", label = "Catcher Name", choices = catcherNames2017$Catcher_Name)
            ),
            conditionalPanel(
                condition = "input.seasonData == 'seasonData2018Taken'",
                helpText("Select a catcher that you would like to view the plot for:"),
                selectInput("playerName2018", label = "Catcher Name", choices = catcherNames2018$Catcher_Name)
            ),
            conditionalPanel(
                condition = "input.seasonData == 'seasonData2019Taken'",
                helpText("Select a catcher that you would like to view the plot for:"),
                selectInput("playerName2019", label = "Catcher Name", choices = catcherNames2019$Catcher_Name)
            ),
            conditionalPanel(
                condition = "input.seasonData == 'masterTableTaken'",
                helpText("Select a catcher that you would like to view the plot for:"),
                selectInput("playerNameAll", label = "Catcher Name", choices = catcherNamesAll$Catcher_Name)
            ), 
            helpText("Note: All plots are from the catcher's perspective."),
            helpText("Note: Percentiles are of the season selected.")
        ),
        mainPanel(
            plotlyOutput("strikePercentagePlot"), tableOutput("playerData"), tableOutput("playerPercentile")
        )
    ),
    tabPanel("Catcher Framing Value",
             sidebarPanel(
                 helpText("Select which season you would like to view:"),
                 radioButtons(
                     "catcherSeason",
                     "Season Data",
                     c(
                         "2017 Season" = "catcherStrikesStolen2017",
                         "2018 Season" = "catcherStrikesStolen2018",
                         "2019 Season" = "catcherStrikesStolen2019",
                         "2017-2019 Seasons" = "catcherStrikesStolenMaster"
                     )
                 )
             ), 
             mainPanel(DT::dataTableOutput("seasonDataTable"))),
    tabPanel("Catcher Clustering",
             sidebarPanel(
                 helpText("Select which season you would like to view: "),
                 radioButtons(
                     "catcherCluster",
                     "Season Data",
                     c(
                         "2017 Season" = "catcherHitting2017",
                         "2018 Season" = "catcherHitting2018",
                         "2019 Season" = "catcherHitting2019",
                         "2017-2019 Seasons" = "catcherHittingAll"
                     )
                 ), 
                uiOutput("sliderValues"),
             ),
             mainPanel(plotlyOutput("clusterPlot"), tableOutput("clusterMeans"), DT::dataTableOutput("clusterData")))))

# Define server logic required
server <- function(input, output) {
    
    #Creates the slider values to be used
    catcherValues2017 <- dbGetQuery(conn, "SELECT MAX(xwoba), MIN(xwoba), MAX(framingWAR), MIN(framingWAR), MAX(strikesStolenPerPitchCaught), MIN(strikesStolenPerPitchCaught), MAX(salary), MIN(salary), MAX(age), MIN(age), MAX(serviceTime), MIN(serviceTime) FROM catcherStrikesStolen2017 cs LEFT JOIN catcherHitting2017 ch ON cs.Catcher_Name = ch.player_name;")
    catcherValues2018 <- dbGetQuery(conn, "SELECT MAX(xwoba), MIN(xwoba), MAX(framingWAR), MIN(framingWAR), MAX(strikesStolenPerPitchCaught), MIN(strikesStolenPerPitchCaught), MAX(salary), MIN(salary), MAX(age), MIN(age), MAX(serviceTime), MIN(serviceTime) FROM catcherStrikesStolen2018 cs LEFT JOIN catcherHitting2018 ch ON cs.Catcher_Name = ch.player_name;")
    catcherValues2019 <- dbGetQuery(conn, "SELECT MAX(xwoba), MIN(xwoba), MAX(framingWAR), MIN(framingWAR), MAX(strikesStolenPerPitchCaught), MIN(strikesStolenPerPitchCaught), MAX(salary), MIN(salary), MAX(age), MIN(age), MAX(serviceTime), MIN(serviceTime) FROM catcherStrikesStolen2019 cs LEFT JOIN catcherHitting2019 ch ON cs.Catcher_Name = ch.player_name;")
    catcherValuesAll <- dbGetQuery(conn, "SELECT MAX(xwoba), MIN(xwoba), MAX(framingWAR), MIN(framingWAR), MAX(strikesStolenPerPitchCaught), MIN(strikesStolenPerPitchCaught), MAX(salary), MIN(salary), MAX(age), MIN(age), MAX(serviceTime), MIN(serviceTime), MAX((salary2019 + salary2018 + salary2017)/(yearActive2019 + yearActive2018 + yearActive2017)) AS 'MAX(averageSalary)', MIN((salary2019 + salary2018 + salary2017)/(yearActive2019 + yearActive2018 + yearActive2017)) AS 'MIN(averageSalary)' FROM catcherStrikesStolenMaster cs LEFT JOIN catcherHittingAll ch ON cs.Catcher_Name = ch.player_name;")
    
    output$sliderValues <- renderUI({tagList(
        conditionalPanel(
            condition = "input.catcherCluster == 'catcherHitting2017'",
            helpText(
                "This plot shows the clusters of catchers from the 2017 season based on their framing ability and xWoba."
            ),
            helpText("The table below the plot shows the averages for each cluster."),
            sliderInput("xwobaFilter2017", "xWoba Filter", min = catcherValues2017$`MIN(xwoba)`, max = catcherValues2017$`MAX(xwoba)`, value = c(catcherValues2017$`MIN(xwoba)`, catcherValues2017$`MAX(xwoba)`), step = 0.001, dragRange = FALSE),
            sliderInput("framingWARFilter2017", "Framing WAR Filter", min = catcherValues2017$`MIN(framingWAR)`, max = catcherValues2017$`MAX(framingWAR)`, value = c(catcherValues2017$`MIN(framingWAR)`, catcherValues2017$`MAX(framingWAR)`), step = 0.01, dragRange = FALSE),
            sliderInput("strikesStolenFilter2017", "Strikes Stolen Per Pitch Caught Filter", min = catcherValues2017$`MIN(strikesStolenPerPitchCaught)`, max = catcherValues2017$`MAX(strikesStolenPerPitchCaught)`, value = c(catcherValues2017$`MIN(strikesStolenPerPitchCaught)`, catcherValues2017$`MAX(strikesStolenPerPitchCaught)`), step = 0.001, dragRange = FALSE),
            sliderInput("salaryFilter2017", "Salary Filter", min = catcherValues2017$`MIN(salary)`, max = catcherValues2017$`MAX(salary)`, value = c(catcherValues2017$`MIN(salary)`, catcherValues2017$`MAX(salary)`), step = 5000, dragRange = FALSE, pre = "$", sep = ","),
            sliderInput("ageFilter2017", "Age Filter", min = catcherValues2017$`MIN(age)`, max = catcherValues2017$`MAX(age)`, value = c(catcherValues2017$`MIN(age)`,catcherValues2017$`MAX(age)`), step = 1, dragRange = FALSE),
            sliderInput("serviceTimeFilter2017", "Service Time Filter", min = catcherValues2017$`MIN(serviceTime)`, max = catcherValues2017$`MAX(serviceTime)`, value = c(catcherValues2017$`MIN(serviceTime)`,catcherValues2017$`MAX(serviceTime)`), step = 0.001, dragRange = FALSE)
        ),
        conditionalPanel(
            condition = "input.catcherCluster == 'catcherHitting2018'",
            helpText(
                "This plot shows the clusters of catchers from the 2018 season based on their framing ability and xWoba."
            ),
            helpText("The table below the plot shows the averages for each cluster."),
            sliderInput("xwobaFilter2018", "xWoba Filter", min = catcherValues2018$`MIN(xwoba)`, max = catcherValues2018$`MAX(xwoba)`, value = c(catcherValues2018$`MIN(xwoba)`, catcherValues2018$`MAX(xwoba)`), step = 0.001, dragRange = FALSE),
            sliderInput("framingWARFilter2018", "Framing WAR Filter", min = catcherValues2018$`MIN(framingWAR)`, max = catcherValues2018$`MAX(framingWAR)`, value = c(catcherValues2018$`MIN(framingWAR)`, catcherValues2018$`MAX(framingWAR)`), step = 0.01, dragRange = FALSE),
            sliderInput("strikesStolenFilter2018", "Strikes Stolen Per Pitch Caught Filter", min = catcherValues2018$`MIN(strikesStolenPerPitchCaught)`, max = catcherValues2018$`MAX(strikesStolenPerPitchCaught)`, value = c(catcherValues2018$`MIN(strikesStolenPerPitchCaught)`, catcherValues2018$`MAX(strikesStolenPerPitchCaught)`), step = 0.001, dragRange = FALSE),
            sliderInput("salaryFilter2018", "Salary Filter", min = catcherValues2018$`MIN(salary)`, max = catcherValues2018$`MAX(salary)`, value = c(catcherValues2018$`MIN(salary)`, catcherValues2018$`MAX(salary)`), step = 5000, dragRange = FALSE, pre = "$", sep = ","),
            sliderInput("ageFilter2018", "Age Filter", min = catcherValues2018$`MIN(age)`, max = catcherValues2018$`MAX(age)`, value = c(catcherValues2018$`MIN(age)`,catcherValues2018$`MAX(age)`), step = 1, dragRange = FALSE),
            sliderInput("serviceTimeFilter2018", "Service Time Filter", min = catcherValues2018$`MIN(serviceTime)`, max = catcherValues2018$`MAX(serviceTime)`, value = c(catcherValues2018$`MIN(serviceTime)`,catcherValues2018$`MAX(serviceTime)`), step = 0.001, dragRange = FALSE)
        ),
        conditionalPanel(
            condition = "input.catcherCluster == 'catcherHitting2019'",
            helpText(
                "This plot shows the clusters of catchers from the 2019 season based on their framing ability and xWoba."
            ),
            helpText("The table below the plot shows the averages for each cluster."),
            sliderInput("xwobaFilter2019", "xWoba Filter", min = catcherValues2019$`MIN(xwoba)`, max = catcherValues2019$`MAX(xwoba)`, value = c(catcherValues2019$`MIN(xwoba)`, catcherValues2019$`MAX(xwoba)`), step = 0.001, dragRange = FALSE),
            sliderInput("framingWARFilter2019", "Framing WAR Filter", min = catcherValues2019$`MIN(framingWAR)`, max = catcherValues2019$`MAX(framingWAR)`, value = c(catcherValues2019$`MIN(framingWAR)`, catcherValues2019$`MAX(framingWAR)`), step = 0.01, dragRange = FALSE),
            sliderInput("strikesStolenFilter2019", "Strikes Stolen Per Pitch Caught Filter", min = catcherValues2019$`MIN(strikesStolenPerPitchCaught)`, max = catcherValues2019$`MAX(strikesStolenPerPitchCaught)`, value = c(catcherValues2019$`MIN(strikesStolenPerPitchCaught)`, catcherValues2019$`MAX(strikesStolenPerPitchCaught)`), step = 0.001, dragRange = FALSE),
            sliderInput("salaryFilter2019", "Salary Filter", min = catcherValues2019$`MIN(salary)`, max = catcherValues2019$`MAX(salary)`, value = c(catcherValues2019$`MIN(salary)`, catcherValues2019$`MAX(salary)`), step = 5000, dragRange = FALSE, pre = "$", sep = ","),
            sliderInput("ageFilter2019", "Age Filter", min = catcherValues2019$`MIN(age)`, max = catcherValues2019$`MAX(age)`, value = c(catcherValues2019$`MIN(age)`,catcherValues2019$`MAX(age)`), step = 1, dragRange = FALSE),
            sliderInput("serviceTimeFilter2019", "Service Time Filter", min = catcherValues2019$`MIN(serviceTime)`, max = catcherValues2019$`MAX(serviceTime)`, value = c(catcherValues2019$`MIN(serviceTime)`,catcherValues2019$`MAX(serviceTime)`), step = 0.001, dragRange = FALSE)
        ),
        conditionalPanel(
            condition = "input.catcherCluster == 'catcherHittingAll'",
            helpText(
                "This plot shows the clusters of catchers from the 2017-2019 seasons based on their framing ability and xWoba."
            ),
            helpText("The table below the plot shows the averages for each cluster."),
            sliderInput("xwobaFilterAll", "xWoba Filter", min = catcherValuesAll$`MIN(xwoba)`, max = catcherValuesAll$`MAX(xwoba)`, value = c(catcherValuesAll$`MIN(xwoba)`, catcherValuesAll$`MAX(xwoba)`), step = 0.001, dragRange = FALSE),
            sliderInput("framingWARFilterAll", "Framing WAR Filter", min = catcherValuesAll$`MIN(framingWAR)`, max = catcherValuesAll$`MAX(framingWAR)`, value = c(catcherValuesAll$`MIN(framingWAR)`, catcherValuesAll$`MAX(framingWAR)`), step = 0.01, dragRange = FALSE),
            sliderInput("strikesStolenFilterAll", "Strikes Stolen Per Pitch Caught Filter", min = catcherValuesAll$`MIN(strikesStolenPerPitchCaught)`, max = catcherValuesAll$`MAX(strikesStolenPerPitchCaught)`, value = c(catcherValuesAll$`MIN(strikesStolenPerPitchCaught)`, catcherValuesAll$`MAX(strikesStolenPerPitchCaught)`), step = 0.001, dragRange = FALSE),
            sliderInput("salaryFilterAll", "Salary Filter", min = catcherValuesAll$`MIN(averageSalary)`, max = catcherValuesAll$`MAX(averageSalary)`, value = c(catcherValuesAll$`MIN(averageSalary)`, catcherValuesAll$`MAX(averageSalary)`), step = 5000, dragRange = FALSE, pre = "$", sep = ","),
            helpText("Salary data for this filter uses the player's average salary over the course of the 2017-2019 seasons."),
            sliderInput("ageFilterAll", "Age Filter", min = catcherValuesAll$`MIN(age)`, max = catcherValuesAll$`MAX(age)`, value = c(catcherValuesAll$`MIN(age)`,catcherValuesAll$`MAX(age)`), step = 1, dragRange = FALSE),
            helpText("Age data for this filter uses the player's age from the most recent season they played in."),
            sliderInput("serviceTimeFilterAll", "Service Time Filter", min = catcherValuesAll$`MIN(serviceTime)`, max = catcherValuesAll$`MAX(serviceTime)`, value = c(catcherValuesAll$`MIN(serviceTime)`,catcherValuesAll$`MAX(serviceTime)`), step = 0.001, dragRange = FALSE),
            helpText("Service time data for this filter uses the player's last known amount of service time.")
        )
        )
        
    })
    
    clusterData <- reactive({
        if (input$catcherCluster == 'catcherHittingAll')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.*, (salary2019 + salary2018 + salary2017)/(yearActive2019 + yearActive2018 + yearActive2017) AS 'averageSalary', ch.xwoba FROM catcherStrikesStolenMaster cs LEFT JOIN catcherHittingAll ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolenMaster")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba))
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Place the catcher's cluster next to his data row
            clusteringData <- clusteringData %>% mutate(cluster = clusters$cluster)
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT Catcher_Name FROM catcherStrikesStolenMaster")$Catcher_Name
            
            return(
                datatable(clusteringData %>% filter(
                    input$xwobaFilterAll[1] <= clusteringData$xwoba &
                        input$xwobaFilterAll[2] >= clusteringData$xwoba &
                        input$framingWARFilterAll[1] <= clusteringData$framingWAR &
                        input$framingWARFilterAll[2] >= clusteringData$framingWAR &
                        input$strikesStolenFilterAll[1] <= clusteringData$strikesStolenPerPitchCaught &
                        input$strikesStolenFilterAll[2] >= clusteringData$strikesStolenPerPitchCaught &
                        input$salaryFilterAll[1] <= clusteringData$averageSalary &
                        input$salaryFilterAll[2] >= clusteringData$averageSalary &
                        input$ageFilterAll[1] <= clusteringData$age & 
                        input$ageFilterAll[2] >= clusteringData$age &
                        input$serviceTimeFilterAll[1] <= clusteringData$serviceTime & 
                        input$serviceTimeFilterAll[2] >= clusteringData$serviceTime
                ) %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba, averageSalary, age, serviceTime, cluster)) %>% formatCurrency(columns = "averageSalary")
            )
        }
        else if (input$catcherCluster == 'catcherHitting2019')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.*, ch.xwoba FROM catcherStrikesStolen2019 cs LEFT JOIN catcherHitting2019 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2019")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba))
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Place the catcher's cluster next to his data row
            clusteringData <- clusteringData %>% mutate(cluster = clusters$cluster)
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT Catcher_Name FROM catcherStrikesStolen2019")$Catcher_Name  
            
            return(
                datatable(clusteringData %>% filter(
                    input$xwobaFilter2019[1] <= clusteringData$xwoba &
                        input$xwobaFilter2019[2] >= clusteringData$xwoba &
                        input$framingWARFilter2019[1] <= clusteringData$framingWAR &
                        input$framingWARFilter2019[2] >= clusteringData$framingWAR &
                        input$strikesStolenFilter2019[1] <= clusteringData$strikesStolenPerPitchCaught &
                        input$strikesStolenFilter2019[2] >= clusteringData$strikesStolenPerPitchCaught &
                        input$salaryFilter2019[1] <= clusteringData$salary &
                        input$salaryFilter2019[2] >= clusteringData$salary &
                        input$ageFilter2019[1] <= clusteringData$age &
                        input$ageFilter2019[2] >= clusteringData$age &
                        input$serviceTimeFilter2019[1] <= clusteringData$serviceTime &
                        input$serviceTimeFilter2019[2] >= clusteringData$serviceTime
                ) %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba, salary, age, serviceTime, cluster)) %>% formatCurrency(columns = "salary")
            )
        }   
        else if (input$catcherCluster == 'catcherHitting2018')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.*, ch.xwoba FROM catcherStrikesStolen2018 cs LEFT JOIN catcherHitting2018 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2018")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba))
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Place the catcher's cluster next to his data row
            clusteringData <- clusteringData %>% mutate(cluster = clusters$cluster)
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT Catcher_Name FROM catcherStrikesStolen2018")$Catcher_Name 
            
            return(
                datatable(clusteringData %>% filter(
                    input$xwobaFilter2018[1] <= clusteringData$xwoba &
                        input$xwobaFilter2018[2] >= clusteringData$xwoba &
                        input$framingWARFilter2018[1] <= clusteringData$framingWAR &
                        input$framingWARFilter2018[2] >= clusteringData$framingWAR &
                        input$strikesStolenFilter2018[1] <= clusteringData$strikesStolenPerPitchCaught &
                        input$strikesStolenFilter2018[2] >= clusteringData$strikesStolenPerPitchCaught &
                        input$salaryFilter2018[1] <= clusteringData$salary &
                        input$salaryFilter2018[2] >= clusteringData$salary &
                        input$ageFilter2018[1] <= clusteringData$age &
                        input$ageFilter2018[2] >= clusteringData$age &
                        input$serviceTimeFilter2018[1] <= clusteringData$serviceTime &
                        input$serviceTimeFilter2018[2] >= clusteringData$serviceTime
                ) %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba, salary, age, serviceTime, cluster)) %>% formatCurrency(columns = "salary")
            )
        }
        else if (input$catcherCluster == 'catcherHitting2017')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.*, ch.xwoba FROM catcherStrikesStolen2017 cs LEFT JOIN catcherHitting2017 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2017")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba))
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Place the catcher's cluster next to his data row
            clusteringData <- clusteringData %>% mutate(cluster = clusters$cluster)
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT Catcher_Name FROM catcherStrikesStolen2017")$Catcher_Name 
            
            return(
                datatable(clusteringData %>% filter(
                    input$xwobaFilter2017[1] <= clusteringData$xwoba &
                        input$xwobaFilter2017[2] >= clusteringData$xwoba &
                        input$framingWARFilter2017[1] <= clusteringData$framingWAR &
                        input$framingWARFilter2017[2] >= clusteringData$framingWAR &
                        input$strikesStolenFilter2017[1] <= clusteringData$strikesStolenPerPitchCaught &
                        input$strikesStolenFilter2017[2] >= clusteringData$strikesStolenPerPitchCaught &
                        input$salaryFilter2017[1] <= clusteringData$salary &
                        input$salaryFilter2017[2] >= clusteringData$salary & 
                        input$ageFilter2017[1] <= clusteringData$age &
                        input$ageFilter2017[2] >= clusteringData$age &
                        input$serviceTimeFilter2017[1] <= clusteringData$serviceTime &
                        input$serviceTimeFilter2017[2] >= clusteringData$serviceTime
                ) %>% select(framingWAR, strikesStolenPerPitchCaught, xwoba, salary, age, serviceTime, cluster)) %>% formatCurrency(columns = "salary")
            )
        }
    })
    
    #Creates the model used for the strike percentage plot
    gamCreator <- reactive({
        
        # Logic to determine what season's data to use and whether Travis d'Arnuad is the player chosen because he messes everything up 
        if (input$seasonData == "seasonData2017Taken")
        {
            query <-
                paste0("SELECT * FROM ",input$seasonData , " WHERE catcher_name = '", input$playerName2017, "'")   
            title <- paste0(input$playerName2017, "'s Strike Probability Chart")
            if (input$playerName2017 == "Travis d'Arnaud")
            {
                query <- "SELECT * FROM seasonData2017Taken WHERE catcher_name = 'Travis d''Arnaud'"
            }
        }
        else if (input$seasonData == "seasonData2018Taken")
        {
            query <-
                paste0("SELECT * FROM ",input$seasonData , " WHERE catcher_name = '", input$playerName2018, "'")  
            title <- paste0(input$playerName2018, "'s Strike Probability Chart")
            if (input$playerName2018 == "Travis d'Arnaud")
            {
                query <- "SELECT * FROM seasonData2018Taken WHERE catcher_name = 'Travis d''Arnaud'"
            }
        }
        else if (input$seasonData == "seasonData2019Taken")
        {
            query <-
                paste0("SELECT * FROM ",input$seasonData , " WHERE catcher_name = '", input$playerName2019, "'")
            title <- paste0(input$playerName2019, "'s Strike Probability Chart")
            if (input$playerName2019 == "Travis d'Arnaud")
            {
                query <- "SELECT * FROM seasonData2019Taken WHERE catcher_name = 'Travis d''Arnaud'"
            }
        }
        else if (input$seasonData == "masterTableTaken")
        {
            query <-
                paste0("SELECT * FROM ",input$seasonData , " WHERE catcher_name = '", input$playerNameAll, "'")
            title <- paste0(input$playerNameAll, "'s Strike Probability Chart")
            if (input$playerNameAll == "Travis d'Arnaud")
            {
                query <- "SELECT * FROM masterTableTaken WHERE catcher_name = 'Travis d''Arnaud'"
            }
        }

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
        
        return(playerPitchGridMod %>% transmute(`Horizontal Plate Location (ft.)` = round(plate_x, 5), `Vertical Plate Location (ft.)` = round(plate_z, 5), `Strike Probability` = round(.fitted, 4)))
    })
    
    #Creates the estimated strike percentage plot show on the opening page
    output$strikePercentagePlot <- renderPlotly({
        
        if (input$seasonData == "seasonData2017Taken")
        {
            title <- paste0(input$playerName2017, "'s Strike Probability Chart")
        }
        else if (input$seasonData == "seasonData2018Taken")
        {
            title <- paste0(input$playerName2018, "'s Strike Probability Chart")
        }
        else if (input$seasonData == "seasonData2019Taken")
        {
            title <- paste0(input$playerName2019, "'s Strike Probability Chart")
        }
        else if (input$seasonData == "masterTableTaken")
        {
            title <- paste0(input$playerNameAll, "'s Strike Probability Chart")
        }
        
        #Create the plot
        
        ggplotly(k_zone_plot %+% gamCreator() + geom_tile(aes(fill = `Strike Probability`), alpha = 0.7) +
            scale_fill_gradient(low = "gray", high = "red", name = "Estimated Strike Percentage") + ggtitle(title),
            dynamicTicks = TRUE)
        
    })
    
    #Creates the table that displays a player's data for a chosen season
    output$playerData <- renderTable({
        if (input$seasonData == "seasonData2017Taken")
        {
            query <-
                paste0("SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolen2017 WHERE catcher_name = '", input$playerName2017, "'")
            if (input$playerName2017 == "Travis d'Arnaud")
            {
                query <- "SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolen2017 WHERE catcher_name = 'Travis d''Arnaud'"
            }
            dbGetQuery(conn, query)
        }
        else if (input$seasonData == "seasonData2018Taken")
        {
            query <-
                paste0("SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolen2018 WHERE catcher_name = '", input$playerName2018, "'")
            if (input$playerName2018 == "Travis d'Arnaud")
            {
                query <- "SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolen2018 WHERE catcher_name = 'Travis d''Arnaud'"
            }
            dbGetQuery(conn, query)
        }
        else if (input$seasonData == "seasonData2019Taken")
        {
            query <-
                paste0("SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolen2019 WHERE catcher_name = '", input$playerName2019, "'")
            if (input$playerName2019 == "Travis d'Arnaud")
            {
                query <- "SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolen2019 WHERE catcher_name = 'Travis d''Arnaud'"
            }
            dbGetQuery(conn, query)
        }
        else if (input$seasonData == "masterTableTaken")
        {
            query <-
                paste0("SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolenMaster WHERE catcher_name = '", input$playerNameAll, "'")
            if (input$playerNameAll == "Travis d'Arnaud")
            {
                query <- "SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught' FROM catcherStrikesStolenMaster WHERE catcher_name = 'Travis d''Arnaud'"
            }
            dbGetQuery(conn, query)
        }
        
    }, digits = 3)
    
    #Creates the table that display's a player's percentile for a chosen season
    output$playerPercentile <- function(){
        if (input$seasonData == "seasonData2017Taken") 
        {
            query <- "SELECT Catcher_Name, round(PERCENT_RANK() OVER(ORDER BY numberOfStolenStrikes) * 100, 1) AS 'NumberOfStolenStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY numberOfLostStrikes) * 100, 1) AS 'NumberOfLostStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY differenceOfStrikes) * 100, 1) AS 'DifferenceOfStrikesPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalRunDifference) * 100, 1) AS 'TotalRunDifferencePercentile', round(PERCENT_RANK() OVER(ORDER BY framingWAR) * 100, 1) AS 'FramingWARPercentile' , round(PERCENT_RANK() OVER(ORDER BY strikesStolenPerPitchCaught) * 100, 1) AS 'StrikesStolenPerPitchCaughtPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalPitchesCaught) * 100, 1) AS 'TotalPitchesCaughtPercentile' FROM catcherStrikesStolen2017 ORDER BY framingWAR;"
            dbGetQuery(conn, query) %>% mutate(
                `Catcher Name` = Catcher_Name, #The code below is coloring the table data points according to the percentiles they have
                `Number Of Stolen Strikes Percentile` = color_tile("red", "lightblue")(NumberOfStolenStrikesPercentile),
                `Number Of Lost Strikes Percentile` = color_tile("lightblue", "red")(NumberOfLostStrikesPercentile),
                `Difference Of Strikes Percentile` = color_tile("red", "lightblue")(DifferenceOfStrikesPercentile),
                `Total Run Difference Percentile` = color_tile("red", "lightblue")(TotalRunDifferencePercentile),
                `Framing WAR Percentile` = color_tile("red", "lightblue")(FramingWARPercentile),
                `Strikes Stolen Per Pitch Caught Percentile` = color_tile("red", "lightblue")(StrikesStolenPerPitchCaughtPercentile),
                `Total Pitches Caught Percentile` = color_tile("red", "lightblue")(TotalPitchesCaughtPercentile)
            ) %>% filter(Catcher_Name == input$playerName2017) %>% select( 
                `Catcher Name`,
                `Number Of Stolen Strikes Percentile`,
                `Number Of Lost Strikes Percentile`,
                `Difference Of Strikes Percentile`,
                `Total Run Difference Percentile`,
                `Framing WAR Percentile`,
                `Strikes Stolen Per Pitch Caught Percentile`,
                `Total Pitches Caught Percentile`
            ) %>% kable(format = "html", escape = F) %>% kable_styling("striped") #Forces the table to use HTML tags to build itself and has a striped style to it
        }
        else if (input$seasonData == "seasonData2018Taken")
        {
            query <- "SELECT Catcher_Name, round(PERCENT_RANK() OVER(ORDER BY numberOfStolenStrikes) * 100, 1) AS 'NumberOfStolenStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY numberOfLostStrikes) * 100, 1) AS 'NumberOfLostStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY differenceOfStrikes) * 100, 1) AS 'DifferenceOfStrikesPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalRunDifference) * 100, 1) AS 'TotalRunDifferencePercentile', round(PERCENT_RANK() OVER(ORDER BY framingWAR) * 100, 1) AS 'FramingWARPercentile' , round(PERCENT_RANK() OVER(ORDER BY strikesStolenPerPitchCaught) * 100, 1) AS 'StrikesStolenPerPitchCaughtPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalPitchesCaught) * 100, 1) AS 'TotalPitchesCaughtPercentile' FROM catcherStrikesStolen2018 ORDER BY framingWAR;"
            dbGetQuery(conn, query) %>% mutate(
                `Catcher Name` = Catcher_Name, #The code below is coloring the table data points according to the percentiles they have
                `Number Of Stolen Strikes Percentile` = color_tile("red", "lightblue")(NumberOfStolenStrikesPercentile),
                `Number Of Lost Strikes Percentile` = color_tile("lightblue", "red")(NumberOfLostStrikesPercentile),
                `Difference Of Strikes Percentile` = color_tile("red", "lightblue")(DifferenceOfStrikesPercentile),
                `Total Run Difference Percentile` = color_tile("red", "lightblue")(TotalRunDifferencePercentile),
                `Framing WAR Percentile` = color_tile("red", "lightblue")(FramingWARPercentile),
                `Strikes Stolen Per Pitch Caught Percentile` = color_tile("red", "lightblue")(StrikesStolenPerPitchCaughtPercentile),
                `Total Pitches Caught Percentile` = color_tile("red", "lightblue")(TotalPitchesCaughtPercentile)
            ) %>% filter(Catcher_Name == input$playerName2018) %>% select(
                `Catcher Name`,
                `Number Of Stolen Strikes Percentile`,
                `Number Of Lost Strikes Percentile`,
                `Difference Of Strikes Percentile`,
                `Total Run Difference Percentile`,
                `Framing WAR Percentile`,
                `Strikes Stolen Per Pitch Caught Percentile`,
                `Total Pitches Caught Percentile`
            ) %>% kable(format = "html", escape = F) %>% kable_styling("striped") #Forces the table to use HTML tags to build itself and has a striped style to it
            
        }
        else if (input$seasonData == "seasonData2019Taken")
        {
            query <- "SELECT Catcher_Name, round(PERCENT_RANK() OVER(ORDER BY numberOfStolenStrikes) * 100, 1) AS 'NumberOfStolenStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY numberOfLostStrikes) * 100, 1) AS 'NumberOfLostStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY differenceOfStrikes) * 100, 1) AS 'DifferenceOfStrikesPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalRunDifference) * 100, 1) AS 'TotalRunDifferencePercentile', round(PERCENT_RANK() OVER(ORDER BY framingWAR) * 100, 1) AS 'FramingWARPercentile' , round(PERCENT_RANK() OVER(ORDER BY strikesStolenPerPitchCaught) * 100, 1) AS 'StrikesStolenPerPitchCaughtPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalPitchesCaught) * 100, 1) AS 'TotalPitchesCaughtPercentile' FROM catcherStrikesStolen2019 ORDER BY framingWAR;"
            dbGetQuery(conn, query) %>% mutate(
                `Catcher Name` = Catcher_Name, #The code below is coloring the table data points according to the percentiles they have
                `Number Of Stolen Strikes Percentile` = color_tile("red", "lightblue")(NumberOfStolenStrikesPercentile),
                `Number Of Lost Strikes Percentile` = color_tile("lightblue", "red")(NumberOfLostStrikesPercentile),
                `Difference Of Strikes Percentile` = color_tile("red", "lightblue")(DifferenceOfStrikesPercentile),
                `Total Run Difference Percentile` = color_tile("red", "lightblue")(TotalRunDifferencePercentile),
                `Framing WAR Percentile` = color_tile("red", "lightblue")(FramingWARPercentile),
                `Strikes Stolen Per Pitch Caught Percentile` = color_tile("red", "lightblue")(StrikesStolenPerPitchCaughtPercentile),
                `Total Pitches Caught Percentile` = color_tile("red", "lightblue")(TotalPitchesCaughtPercentile)
            ) %>% filter(Catcher_Name == input$playerName2019) %>% select(
                `Catcher Name`,
                `Number Of Stolen Strikes Percentile`,
                `Number Of Lost Strikes Percentile`,
                `Difference Of Strikes Percentile`,
                `Total Run Difference Percentile`,
                `Framing WAR Percentile`,
                `Strikes Stolen Per Pitch Caught Percentile`,
                `Total Pitches Caught Percentile`
            ) %>% kable(format = "html", escape = F) %>% kable_styling("striped") #Forces the table to use HTML tags to build itself and has a striped style to it
            
        }
        else if (input$seasonData == "masterTableTaken")
        {
            query <- "SELECT Catcher_Name, round(PERCENT_RANK() OVER(ORDER BY numberOfStolenStrikes) * 100, 1) AS 'NumberOfStolenStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY numberOfLostStrikes) * 100, 1) AS 'NumberOfLostStrikesPercentile', round(PERCENT_RANK() OVER(ORDER BY differenceOfStrikes) * 100, 1) AS 'DifferenceOfStrikesPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalRunDifference) * 100, 1) AS 'TotalRunDifferencePercentile', round(PERCENT_RANK() OVER(ORDER BY framingWAR) * 100, 1) AS 'FramingWARPercentile' , round(PERCENT_RANK() OVER(ORDER BY strikesStolenPerPitchCaught) * 100, 1) AS 'StrikesStolenPerPitchCaughtPercentile' , round(PERCENT_RANK() OVER(ORDER BY totalPitchesCaught) * 100, 1) AS 'TotalPitchesCaughtPercentile' FROM catcherStrikesStolenMaster ORDER BY framingWAR;"
            dbGetQuery(conn, query) %>% mutate(
                `Catcher Name` = Catcher_Name, #The code below is coloring the table data points according to the percentiles they have
                `Number Of Stolen Strikes Percentile` = color_tile("red", "lightblue")(NumberOfStolenStrikesPercentile),
                `Number Of Lost Strikes Percentile` = color_tile("lightblue", "red")(NumberOfLostStrikesPercentile),
                `Difference Of Strikes Percentile` = color_tile("red", "lightblue")(DifferenceOfStrikesPercentile),
                `Total Run Difference Percentile` = color_tile("red", "lightblue")(TotalRunDifferencePercentile),
                `Framing WAR Percentile` = color_tile("red", "lightblue")(FramingWARPercentile),
                `Strikes Stolen Per Pitch Caught Percentile` = color_tile("red", "lightblue")(StrikesStolenPerPitchCaughtPercentile),
                `Total Pitches Caught Percentile` = color_tile("red", "lightblue")(TotalPitchesCaughtPercentile)
            ) %>% filter(Catcher_Name == input$playerNameAll) %>% select(
                `Catcher Name`,
                `Number Of Stolen Strikes Percentile`,
                `Number Of Lost Strikes Percentile`,
                `Difference Of Strikes Percentile`,
                `Total Run Difference Percentile`,
                `Framing WAR Percentile`,
                `Strikes Stolen Per Pitch Caught Percentile`,
                `Total Pitches Caught Percentile`
            ) %>% kable(format = "html", escape = F) %>% kable_styling("striped") #Forces the table to use HTML tags to build itself and has a striped style to it
            
        }
    }
    
    # Creates the data table used to display catcher value on the second tab
    output$seasonDataTable <- DT::renderDT({
        datatable(
            dbGetQuery(
                conn,
                paste0(
                    "SELECT Catcher_Name AS 'Catcher Name', numberOfStolenStrikes AS 'Number of Stolen Strikes', numberOfLostStrikes AS 'Number of Lost Strikes', differenceOfStrikes AS 'Difference of Strikes', totalRunDifference AS 'Total Run Difference', framingWAR AS 'Framing WAR', strikesStolenPerPitchCaught AS 'Strikes Stolen Per Pitch Caught', totalPitchesCaught AS 'Total Pitches Caught', age AS 'Age', serviceTime AS 'Service Time', salary AS 'Salary' FROM ",
                    input$catcherSeason
                )
            ),
            filter = "top",
            options = list(pageLength = 25)
        ) %>% formatCurrency(columns = "Salary")
    })
    
    #Creates the cluster plot of catchers based off of a chosen season
    output$clusterPlot <- renderPlotly({
        if (input$catcherCluster == 'catcherHittingAll')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolenMaster cs LEFT JOIN catcherHittingAll ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolenMaster")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Determine the proper number of clusters
            #fviz_nbclust(dataScaled, FUNcluster = kmeans, method = "wss")
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Plot the clusters 
            ggplotly(fviz_cluster(clusters, data = dataScaled, repel = FALSE, ggtheme = theme_minimal(), main = "K-Means Clusters Among MLB Catchers", alpha = 0.2, labelsize = 7, pointsize = 1), dynamicTicks = TRUE)
        }
        else if (input$catcherCluster == 'catcherHitting2019')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolen2019 cs LEFT JOIN catcherHitting2019 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2019")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Determine the proper number of clusters
            #fviz_nbclust(dataScaled, FUNcluster = kmeans, method = "wss")
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Plot the clusters 
            ggplotly(fviz_cluster(clusters, data = dataScaled, repel = FALSE, ggtheme = theme_minimal(), main = "K-Means Clusters Among MLB Catchers", alpha = 0.2, labelsize = 7, pointsize = 1), dynamicTicks = TRUE)
        }   
        else if (input$catcherCluster == 'catcherHitting2018')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolen2018 cs LEFT JOIN catcherHitting2018 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2018")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Determine the proper number of clusters
            #fviz_nbclust(dataScaled, FUNcluster = kmeans, method = "wss")
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Plot the clusters 
            ggplotly(fviz_cluster(clusters, data = dataScaled, repel = FALSE, ggtheme = theme_minimal(), main = "K-Means Clusters Among MLB Catchers", alpha = 0.2, labelsize = 7, pointsize = 1), dynamicTicks = TRUE)
        }
        else if (input$catcherCluster == 'catcherHitting2017')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolen2017 cs LEFT JOIN catcherHitting2017 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2017")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Determine the proper number of clusters
            #fviz_nbclust(dataScaled, FUNcluster = kmeans, method = "wss")
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Plot the clusters 
            ggplotly(fviz_cluster(clusters, data = dataScaled, repel = FALSE, ggtheme = theme_minimal(), main = "K-Means Clusters Among MLB Catchers", alpha = 0.2, labelsize = 7, pointsize = 1), dynamicTicks = TRUE)
        }
        
        })
    
    #Creates the table that shows the averages of each cluster
    output$clusterMeans <- renderTable({
        if (input$catcherCluster == 'catcherHittingAll')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolenMaster cs LEFT JOIN catcherHittingAll ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolenMaster")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Return a list with the actual averages for the clusters
            aggregate(clusteringData, by = list(cluster = clusters$cluster), mean)
        }
        else if (input$catcherCluster == 'catcherHitting2019')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolen2019 cs LEFT JOIN catcherHitting2019 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2019")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Return a list with the actual averages for the clusters
            aggregate(clusteringData, by = list(cluster = clusters$cluster), mean)    
        }   
        else if (input$catcherCluster == 'catcherHitting2018')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolen2018 cs LEFT JOIN catcherHitting2018 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2018")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
            
            #Return a list with the actual averages for the clusters
            aggregate(clusteringData, by = list(cluster = clusters$cluster), mean)
        }
        else if (input$catcherCluster == 'catcherHitting2017')
        {
            #Get the data from the database
            clusteringData <- dbGetQuery(conn, "SELECT cs.framingWAR, cs.strikesStolenPerPitchCaught, ch.xwoba FROM catcherStrikesStolen2017 cs LEFT JOIN catcherHitting2017 ch ON cs.Catcher_Name = ch.player_name;")
            
            #Place each catcher's name as the rowname to properly display on graph
            row.names(clusteringData) <- dbGetQuery(conn, "SELECT * FROM catcherStrikesStolen2017")$Catcher_Name
            
            #Scale the data to ensure clean clustering
            dataScaled <- scale(clusteringData)
            
            #Set the randomization seed to ensure consistency among multiple runs
            set.seed(36)
            
            #Create the clusters
            clusters <- kmeans(dataScaled, 4, nstart = 50)
        
            #Return a list with the actual averages for the clusters
            aggregate(clusteringData, by = list(cluster = clusters$cluster), mean)    
        }
    }, digits = 3)
    
    #Creates the table that shows every catcher with the data used to cluster, as well as what cluster they reside in
    output$clusterData <- DT::renderDT({clusterData()
    })
    
    outputOptions(output, "strikePercentagePlot", suspendWhenHidden = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
