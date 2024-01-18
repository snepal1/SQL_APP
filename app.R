# Load required libraries
library(shiny)
library(minpack.lm)
library(nlme)
library(DBI)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Height Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload training database (training.db):"),
      fileInput("file2", "Upload predictions database (Predictions.db):"),
      fileInput("file3", "Upload merch database (Merch.db):"),
      downloadButton("downloadData", "Download Predicted Heights Database")
    ),
    
    mainPanel(
      textOutput("message"),
      tableOutput("predictedData")
    )
  )
)
# Define the server
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10000*1024^2)
  # Reactive function to upload and process the databases
  data_db <- reactiveValues(con4 = NULL)
  processedData <- reactive({
    inFile1 <- input$file1
    inFile2 <- input$file2
    inFile3 <- input$file3
    
    if (is.null(inFile1) || is.null(inFile2) || is.null(inFile3)) {
      return(NULL)
    }
    
    # Connect to the uploaded databases
    con1 <- dbConnect(RSQLite::SQLite(), dbname = inFile1$datapath)
    con2 <- dbConnect(RSQLite::SQLite(), dbname = inFile2$datapath)
    con3 <- dbConnect(RSQLite::SQLite(), dbname = inFile3$datapath)
    
    data_height <- dbGetQuery(con1, "SELECT * FROM training")
    database <- dbGetQuery(con2, "SELECT * FROM Predict")
    data <- dbGetQuery(con3, "SELECT * FROM Merch_train")
    data3a<-subset(data,Merch_Heig>=1 &
                     dbh>=6.5)
    
    data3<-data3a %>%
      group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci)%>%
      summarise(dbh=mean(dbh), height_predicted=mean(height_predicted), ratio=mean(Ratio), 
                elev=mean(elev),  Merch_Heig=mean(Merch_Heig))
    str(data3)
    
    ################################## DO the modelling ###############################
    
    data_height$elev <- round(data_height$elev / 100) * 100
    data_height$elev[data_height$elev > 7900] <- 8000
    data_height$elev[data_height$elev < 1100] <- 1000
    str(data_height)
    
    database$elev <- round(database$elev / 100) * 100
    database$elev[database$elev < 1100] <- 1000
    database$elev[database$elev > 7900] <- 8000
    
    data3$elev<- round(data3$elev/100)*100
    data3$elev[data3$elev>7900] <- 8000
    data3$elev[data3$elev<1100] <- 1000
    summary(data3)
    
    # Define a function to fit the model and make predictions
    fit_and_predict <- function(species, region, data_height, data3, database) {
      filtered_data_height <- data_height %>%
        filter(Tree_Speci == species, Region == region)
      
      filtered_database <- database %>%
        filter(Tree_Speci == species, Region == region)
      
      filtered_merch <- data3 %>%
        filter(Tree_Speci == species, Region == region)
      
      ctrl <- nls.control(maxiter = 150, tol = 1.49e-08, minFactor = 1 / 2048, printEval = T)
      
      nls17_Height <- nlme(
        height ~ 4.5 + exp((b1 + u1) + b2 * (dbh^b3)),
        start = c(b1 = 6.58172, b2 = -7.58311, b3 = -0.41703),
        fixed = b1 + b2 + b3 ~ 1,
        random = u1 ~ 1 | elev,
        data = filtered_data_height,
        control = ctrl
      )
      
      height_predicted <- predict(nls17_Height, newdata = filtered_database)
      filtered_database$height_predicted <- height_predicted
      
      model_df <- lm(Merch_Heig ~ height_predicted, data = filtered_merch)
      merch_pred <- predict(model_df, newdata = filtered_database)
      filtered_database$merch_pred <- merch_pred
      
      return(filtered_database)
    }
    
    # Apply the function for each combination of species and region
    result_SS_PP <- fit_and_predict("PP", "SSierra", data_height, data3, database)
    result_SS_DF <- fit_and_predict("DF", "SSierra", data_height, data3, database)
    result_SS_IC <- fit_and_predict("IC", "SSierra", data_height, data3, database)
    result_SS_WF <- fit_and_predict("WF", "SSierra", data_height, data3, database)
    result_SS_SP <- fit_and_predict("SP", "SSierra", data_height, data3, database)
    result_NS_PP <- fit_and_predict("PP", "NSierra", data_height, data3, database)
    result_NS_DF <- fit_and_predict("DF", "NSierra", data_height, data3, database)
    result_NS_IC <- fit_and_predict("IC", "NSierra", data_height, data3, database)
    result_NS_WF <- fit_and_predict("WF", "NSierra", data_height, data3, database)
    result_NS_SP <- fit_and_predict("SP", "NSierra", data_height, data3, database)
    result_ES_PP <- fit_and_predict("PP", "Eastside", data_height, data3, database)
    result_ES_DF <- fit_and_predict("DF", "Eastside", data_height, data3, database)
    result_ES_IC <- fit_and_predict("IC", "Eastside", data_height, data3, database)
    result_ES_WF <- fit_and_predict("WF", "Eastside", data_height, data3, database)
    result_ES_SP <- fit_and_predict("SP", "Eastside", data_height, data3, database)
    result_CA_PP <- fit_and_predict("PP", "Cascade", data_height, data3, database)
    result_CA_DF <- fit_and_predict("DF", "Cascade", data_height, data3, database)
    result_CA_IC <- fit_and_predict("IC", "Cascade", data_height, data3, database)
    result_CA_WF <- fit_and_predict("WF", "Cascade", data_height, data3, database)
    result_CA_SP <- fit_and_predict("SP", "Cascade", data_height, data3, database)
    result_WS_PP <- fit_and_predict("PP", "Westside", data_height, data3, database)
    result_WS_DF <- fit_and_predict("DF", "Westside", data_height, data3, database)
    result_WS_IC <- fit_and_predict("IC", "Westside", data_height, data3, database)
    result_WS_WF <- fit_and_predict("WF", "Westside", data_height, data3, database)
    result_WS_SP <- fit_and_predict("SP", "Westside", data_height, data3, database)
    
    # List of species to exclude
    excluded_species <- c("PP", "SP", "WF", "IC", "DF")
    
    # Filter data_height to exclude specified species
    included_species_data <- setdiff(unique(data_height$Tree_Speci), excluded_species)
    filtered_data_height <- data_height %>%
      filter(Tree_Speci %in% included_species_data)
    
    # Filter database to exclude specified species
    included_species_db <- setdiff(unique(database$Tree_Speci), excluded_species)
    filtered_database <- database %>%
      filter(Tree_Speci %in% included_species_db)
    
    included_species_merch <- setdiff(unique(data3$Tree_Speci), excluded_species)
    filtered_merch <- data3 %>%
      filter(Tree_Speci %in% included_species_merch)
    
    # Define a function to fit the model and make predictions
    fit_and_predict1 <- function(data_height, data3, database) {
      ctrl <- nls.control(maxiter = 150, tol = 1.49e-08, minFactor = 0.0001, printEval = TRUE)
      
      nls17_Height <- nlme(
        height ~ 4.5 + exp((b1 + u1) + b2 * (dbh^b3)),
        start = c(b1 = 6.58172, b2 = -7.58311, b3 = -0.41703),
        fixed = b1 + b2 + b3 ~ 1,
        random = u1 ~ 1 | elev,
        data = filtered_data_height,
        control = ctrl
      )
      
      height_predicted <- predict(nls17_Height, newdata = database)
      database$height_predicted <- height_predicted
      
      model_df <- lm(Merch_Heig ~ height_predicted, data = filtered_merch)
      merch_pred <- predict(model_df, newdata = database)
      database$merch_pred <- merch_pred
      
      return(database)
    }
    
    # Apply the function for the filtered data and database
    predicted_database <- fit_and_predict1(filtered_data_height, filtered_merch, filtered_database)
    
    dbDisconnect(con1)
    dbDisconnect(con2)
    dbDisconnect(con3)
    
    # Combine the results
    combined_results <- bind_rows(result_SS_PP, result_SS_DF, result_SS_IC, result_SS_WF,
                                  result_SS_SP, result_NS_PP, result_NS_DF, result_NS_IC, result_NS_WF,
                                  result_NS_SP, result_ES_PP, result_ES_DF, result_ES_IC, result_ES_WF,
                                  result_ES_SP, result_CA_PP, result_CA_DF, result_CA_IC, result_CA_WF,
                                  result_CA_SP, result_WS_PP, result_WS_DF, result_WS_IC, result_WS_WF,
                                  result_WS_SP, predicted_database)
    
    # Connect to the Predicted Heights Database
    db_name <- "Predicted_heights.db"
    con4 <- dbConnect(RSQLite::SQLite(), dbname = db_name)
    
    
    # Write the combined results to the database
    dbWriteTable(con4, "Predicted_heights", combined_results, append = TRUE)
    
    # Save the con4 database connection to the reactiveValues
    data_db$con4 <- con4
    
    # Return the processed data (combined_results)
    combined_results
  })
  
  # Display a message
  output$message <- renderText({
    if (!is.null(processedData())) {
      "Modeling and processing completed."
    } else {
      "Upload the databases to start."
    }
  })
  
  # Display the predicted data table
  output$predictedData <- renderTable({
    processedData()
  })
  
  # Download button for the Predicted Heights Database
  output$downloadData <- downloadHandler(
    filename = function() {
      "Predicted_heights.db"
    },
    content = function(file) {
      # Get the stored con4 database connection from reactiveValues
      con4 <- data_db$con4
      
      # Save the con4 database to the download location
      file.copy(from = con4$description, to = file, overwrite = TRUE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
