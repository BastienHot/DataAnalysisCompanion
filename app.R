# app.R
#
# Data Analysis Companion
#
# install.packages(c("shiny","shinythemes","shinyWidgets","DT","dplyr","ggplot2","plotly","caret",
#                    "randomForest","e1071","RColorBrewer","wordcloud2","tidyr","ROCR","corrplot"))

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(caret)
library(randomForest)
library(e1071)
library(RColorBrewer)
library(wordcloud2)
library(tidyr)
library(ROCR)
library(stats)
library(utils)
library(corrplot)

titanic_available <- requireNamespace("titanic", quietly = TRUE)

accentColor <- "#79152d"

appCSS <- "
.navbar, .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a {
    background-color: #79152d !important;
    color: #ffffff !important;
}
.navbar-default .navbar-nav > li > a:hover, .navbar-default .navbar-brand:hover {
    background-color: #5e1023 !important;
    color: #ffffff !important;
}
.btn-primary {
    background-color: #79152d !important;
    border-color: #79152d !important;
}
.btn-primary:hover {
    background-color: #5e1023 !important;
    border-color: #5e1023 !important;
}
.alert {
    border-radius: 0px;
}
"

getTitanicData <- function() {
  if (titanic_available) {
    data <- titanic::titanic_train
  } else {
    data <- data.frame(
      PassengerId = 1:891,
      Survived = c(0,1,1,1,0,0,0,1,1,1, rep(0,881)),
      Pclass = sample(c(1,2,3), 891, replace = TRUE),
      Name = paste("Name", 1:891),
      Sex = sample(c("male","female"), 891, replace = TRUE),
      Age = sample(c(1:80,NA), 891, replace = TRUE),
      SibSp = sample(0:5, 891, replace = TRUE),
      Parch = sample(0:5, 891, replace = TRUE),
      Ticket = sample(1:9999, 891, replace=TRUE),
      Fare = runif(891,1,100),
      Cabin = sample(c("C85","C123","E46",NA),891,replace=TRUE),
      Embarked = sample(c("S","C","Q",NA),891, replace=TRUE)
    )
  }
  data$Survived <- factor(data$Survived, levels = c(0,1), labels = c("No","Yes"))
  return(data)
}

ui <- fluidPage(
  tags$head(tags$style(HTML(appCSS))),
  theme = shinytheme("flatly"),
  uiOutput("mainUI")
)

server <- function(input, output, session) {
  globalData <- reactiveValues(
    raw = NULL,
    cleaned = NULL,
    name = NULL,
    proposedTypes = NULL
  )
  
  stepIndex <- reactiveVal(1)
  
  goNext <- function() {
    stepIndex(stepIndex() + 1)
  }
  
  showError <- function(msg) {
    showNotification(msg, type = "error", duration = 5)
  }
  
  showInfo <- function(msg) {
    showNotification(msg, type = "message", duration = 5)
  }
  
  #######################################
  ### STEP 1: Data Load UI & Logic    ###
  #######################################
  
  output$dataLoadUI <- renderUI({
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 1: Load Data", style="text-align:center;"),
                 p("Upload a CSV file or use the built-in Titanic dataset. Ensure the file format and separators are correct."),
                 fluidRow(
                   column(5,
                          fileInput("datafile", "Upload CSV File", accept = c(".csv")),
                          checkboxInput("header", "Has Header?", TRUE),
                          selectInput("sep", "Separator", choices = c(","=",",";","|"), selected=","),
                          selectInput("quote", "Quote", choices = c("None" = "", '"' = '"', "'" = "'"), selected = '"')
                   ),
                   column(3,
                          br(),
                          actionButton("loadDataBtn", "Load CSV", class = "btn-primary", width="100%")
                   ),
                   column(3,
                          br(),
                          actionButton("loadTitanicBtn", "Use Titanic", class = "btn-primary", width="100%")
                   )
                 )
               )
        )
      )
    )
  })
  
  observeEvent(input$loadDataBtn, {
    req(input$datafile)
    tryCatch({
      df <- read.csv(input$datafile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
      if(nrow(df) == 0 || ncol(df) == 0) {
        showError("The uploaded dataset is empty or invalid.")
        return(NULL)
      }
      globalData$raw <- df
      globalData$cleaned <- df
      globalData$name <- input$datafile$name
      showInfo("Data loaded successfully!")
      goNext()
    }, error = function(e) {
      showError(paste("Error loading data:", e$message))
    })
  })
  
  observeEvent(input$loadTitanicBtn, {
    df <- getTitanicData()
    globalData$raw <- df
    globalData$cleaned <- df
    globalData$name <- "Titanic Dataset"
    showInfo("Titanic dataset loaded successfully!")
    goNext()
  })
  
  ######################################
  ### STEP 2: Data Preview UI & Logic ##
  ######################################
  
  observeEvent(stepIndex(), {
    if (stepIndex() == 2 && !is.null(globalData$cleaned)) {
      df <- globalData$cleaned
      const_cols <- sapply(df, function(x) length(unique(x[!is.na(x)])) < 2)
      if (any(const_cols)) {
        removed <- names(df)[const_cols]
        df <- df[ , !const_cols, drop=FALSE]
        globalData$cleaned <- df
        showModal(modalDialog(
          title = "Columns Removed",
          paste("The following columns were constant and have been removed:", paste(removed, collapse = ", ")),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  })
  
  output$dataPreviewUI <- renderUI({
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 2: Data Preview", style="text-align:center;"),
                 p("Preview of the first 10 rows of:", strong(globalData$name)),
                 tags$div(style = "overflow-x: scroll; width:100%;",
                          DTOutput("dataPreviewTable", width = "100%")
                 ),
                 br(),
                 actionButton("previewNextBtn", "Proceed", class = "btn-primary")
               )
        )
      )
    )
  })
  
  output$dataPreviewTable <- renderDT({
    req(globalData$cleaned)
    
    datatable(
      head(globalData$cleaned, 10),
      options = list(scrollX = TRUE, paging = FALSE),
      style = "bootstrap"
    )
  })
  
  observeEvent(input$previewNextBtn, {
    goNext()
  })
  
  
  ###################################################
  ### STEP 3: Variable Type Recognition UI & Logic ##
  ###################################################
  
  observeEvent(stepIndex(), {
    if (stepIndex() == 3 && !is.null(globalData$cleaned)) {
      df <- globalData$cleaned
      proposedTypes <- sapply(df, function(col) {
        if (is.numeric(col)) {
          "numeric"
        } else {
          uniqueVals <- length(unique(col[!is.na(col)]))
          nRows <- nrow(df)
          if (uniqueVals < 0.2*nRows) "factor" else "character"
        }
      })
      globalData$proposedTypes <- proposedTypes
    }
  })
  
  output$varRecUI <- renderUI({
    req(globalData$proposedTypes)
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 3: Variable Recognition", style="text-align:center;"),
                 p("Review the proposed data types and adjust as necessary."),
                 fluidRow(
                   lapply(seq_along(globalData$proposedTypes), function(i) {
                     colName <- names(globalData$proposedTypes)[i]
                     column(4,
                            selectInput(paste0("typeSelect_", colName),
                                        label = colName,
                                        choices = c("numeric","factor","character"),
                                        selected = globalData$proposedTypes[i])
                     )
                   })
                 ),
                 br(),
                 actionButton("varRecConfirm", "Confirm & Proceed", class = "btn-primary")
               )
        )
      )
    )
  })
  
  observeEvent(input$varRecConfirm, {
    df <- globalData$cleaned
    for (colName in names(df)) {
      chosenType <- input[[paste0("typeSelect_", colName)]]
      if (chosenType == "numeric") {
        suppressWarnings(df[[colName]] <- as.numeric(df[[colName]]))
      } else if (chosenType == "factor") {
        df[[colName]] <- factor(df[[colName]])
      } else {
        df[[colName]] <- as.character(df[[colName]])
      }
    }
    globalData$cleaned <- df
    goNext()
  })
  
  #####################################
  ### STEP 4: Missing Values Handling##
  #####################################
  
  output$missingUI <- renderUI({
    df <- globalData$cleaned
    req(df)
    naCount <- sapply(df, function(x) sum(is.na(x)))
    colsWithNA <- names(naCount)[naCount > 0]
    
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 4: Missing Values", style="text-align:center;"),
                 if (length(colsWithNA) == 0) {
                   tagList(
                     p("No missing values found."),
                     actionButton("missingNextBtn", "Proceed", class = "btn-primary")
                   )
                 } else {
                   tagList(
                     p("The following columns have missing values. Choose a strategy for each:"),
                     tags$ul(
                       tags$li("Mean/Mode Imputation: Replace NA with mean (numeric) or mode (factor/character)."),
                       tags$li("Remove Rows: Remove any rows with missing values in this column."),
                       tags$li("No Action: Keep missing values as is.")
                     ),
                     lapply(colsWithNA, function(colName) {
                       fluidRow(
                         column(6, strong(colName), paste0(" (", naCount[colName], " missing)")),
                         column(6, selectInput(paste0("missingStrategy_", colName), "Strategy",
                                               choices = c("Mean/Mode Imputation"="impute","Remove Rows"="remove","No Action"="none"),
                                               selected = "impute"))
                       )
                     }),
                     br(),
                     actionButton("applyMissingBtn", "Apply & Proceed", class = "btn-primary")
                   )
                 }
               )
        )
      )
    )
  })
  
  observeEvent(input$missingNextBtn, { goNext() })
  
  observeEvent(input$applyMissingBtn, {
    df <- globalData$cleaned
    naCount <- sapply(df, function(x) sum(is.na(x)))
    colsWithNA <- names(naCount)[naCount > 0]
    for (colName in colsWithNA) {
      strat <- input[[paste0("missingStrategy_", colName)]]
      if (strat == "impute") {
        colData <- df[[colName]]
        if (is.numeric(colData)) {
          if (all(is.na(colData))) {
            df[[colName]] <- NULL
            showInfo(paste("Column", colName, "entirely missing. Dropped."))
          } else {
            df[[colName]][is.na(df[[colName]])] <- mean(colData, na.rm = TRUE)
          }
        } else {
          nonNA <- colData[!is.na(colData)]
          if (length(nonNA)==0) {
            df[[colName]] <- NULL
            showInfo(paste("Column", colName, "entirely missing. Dropped."))
          } else {
            modeVal <- names(sort(table(nonNA), decreasing = TRUE))[1]
            df[[colName]][is.na(df[[colName]])] <- modeVal
          }
        }
      } else if (strat == "remove") {
        df <- df[!is.na(df[[colName]]), , drop=FALSE]
      }
    }
    globalData$cleaned <- df
    showInfo("Missing value strategies applied.")
    goNext()
  })
  
  ###################################
  ### STEP 5: Outlier Handling    ###
  ###################################
  
  output$outlierUI <- renderUI({
    df <- globalData$cleaned
    req(df)
    numericCols <- names(df)[sapply(df, is.numeric)]
    
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 5: Outlier Handling", style="text-align:center;"),
                 tags$h4("Strategies:"),
                 tags$ul(
                   tags$li("Winsorize(5%-95%): Cap values at the 5th and 95th percentile."),
                   tags$li("Remove Outliers(1.5*IQR): Remove rows outside the typical IQR range."),
                   tags$li("No Action: Leave values as is.")
                 ),
                 if (length(numericCols) == 0) {
                   tagList(
                     p("No numeric columns. No action needed."),
                     actionButton("outlierNextBtn", "Proceed", class = "btn-primary")
                   )
                 } else {
                   tagList(
                     fluidRow(
                       column(6, selectInput("outlierCol","Numeric Column",choices = numericCols)),
                       column(6, selectInput("outlierStrat","Strategy",choices = c("No Action"="none","Winsorize"="winsor","Remove Outliers"="remove")))
                     ),
                     plotOutput("outlierBoxplot", height="250px"),
                     actionButton("applyOutlierBtn","Apply Strategy", class = "btn-primary"),
                     br(),br(),
                     actionButton("outlierNextBtn", "Proceed", class = "btn-primary")
                   )
                 }
               )
        )
      )
    )
  })
  
  output$outlierBoxplot <- renderPlot({
    req(input$outlierCol)
    df <- globalData$cleaned
    colName <- input$outlierCol
    if (!is.numeric(df[[colName]])) return(NULL)
    ggplot(df, aes_string(y=colName)) +
      geom_boxplot(fill=brewer.pal(3,"Pastel1")[2]) +
      theme_minimal() + labs(title=paste("Boxplot of", colName))
  })
  
  observeEvent(input$applyOutlierBtn, {
    req(input$outlierCol, input$outlierStrat)
    df <- globalData$cleaned
    colName <- input$outlierCol
    colData <- df[[colName]]
    strat <- input$outlierStrat
    if (!is.numeric(colData)) {
      showError("Not numeric.")
      return()
    }
    if (strat=="winsor") {
      q5 <- quantile(colData,0.05,na.rm=TRUE)
      q95 <- quantile(colData,0.95,na.rm=TRUE)
      colData[colData < q5] <- q5
      colData[colData > q95] <- q95
      df[[colName]] <- colData
    } else if (strat=="remove") {
      Q1 <- quantile(colData,0.25,na.rm=TRUE)
      Q3 <- quantile(colData,0.75,na.rm=TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5*IQR
      upper <- Q3 + 1.5*IQR
      df <- df[colData >= lower & colData <= upper,,drop=FALSE]
    }
    globalData$cleaned <- df
    showInfo("Outlier strategy applied.")
  })
  
  observeEvent(input$outlierNextBtn, {
    goNext()
  })
  
  ############################################
  ### STEP 6: High Cardinality Factor Fix   ##
  ############################################
  
  output$highCardUI <- renderUI({
    df <- globalData$cleaned
    req(df)
    factorCols <- names(df)[sapply(df, is.factor)]
    highCardCols <- factorCols[sapply(df[factorCols], function(x) length(levels(x)) > 20)]
    
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 6: High Cardinality Factors", style="text-align:center;"),
                 if (length(highCardCols)==0) {
                   tagList(
                     p("No high-cardinality factors found."),
                     actionButton("highCardNextBtn", "Proceed", class = "btn-primary")
                   )
                 } else {
                   tagList(
                     p("These factor columns have more than 20 levels. Choose a strategy:"),
                     tags$ul(
                       tags$li("Drop Column: Remove the column entirely."),
                       tags$li("Convert to Character: Treat it as a text feature."),
                       tags$li("Do Nothing: Leave as is.")
                     ),
                     lapply(highCardCols, function(colName) {
                       fluidRow(
                         column(6, strong(colName), paste0(" (", length(levels(df[[colName]]))," levels)")),
                         column(6, selectInput(paste0("highCardStrat_", colName),
                                               "Strategy",
                                               choices=c("Drop Column"="drop","Convert to Character"="char","Do Nothing"="none"),
                                               selected="char"))
                       )
                     }),
                     br(),
                     actionButton("applyHighCardBtn","Apply & Proceed", class="btn-primary")
                   )
                 }
               )
        )
      )
    )
  })
  
  observeEvent(input$highCardNextBtn, { goNext() })
  
  observeEvent(input$applyHighCardBtn, {
    df <- globalData$cleaned
    factorCols <- names(df)[sapply(df, is.factor)]
    highCardCols <- factorCols[sapply(df[factorCols], function(x) length(levels(x)) > 20)]
    
    for (colName in highCardCols) {
      strat <- input[[paste0("highCardStrat_", colName)]]
      if (strat == "drop") {
        df[[colName]] <- NULL
      } else if (strat == "char") {
        df[[colName]] <- as.character(df[[colName]])
      }
    }
    globalData$cleaned <- df
    showInfo("High cardinality strategies applied.")
    goNext()
  })
  
  ##################################
  ### STEP 7: Class Imbalance    ###
  ##################################
  
  output$classImbalanceUI <- renderUI({
    df <- globalData$cleaned
    req(df)
    factorCols <- names(df)[sapply(df, is.factor)]
    
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 7: Class Imbalance", style="text-align:center;"),
                 p("If you plan classification, select a factor target. Then view distribution and choose a balancing strategy."),
                 selectInput("targetVar","Target Variable (for Classification)", choices=c("None",factorCols), selected="None"),
                 uiOutput("classDistUI"),
                 uiOutput("classImbStratUI"),
                 actionButton("classImbNextBtn","Apply & Proceed", class="btn-primary")
               )
        )
      )
    )
  })
  
  output$classDistUI <- renderUI({
    req(input$targetVar)
    if (input$targetVar=="None") return(NULL)
    df <- globalData$cleaned
    target <- df[[input$targetVar]]
    if (!is.factor(target)) {
      return(p("Selected target is not factor. No class imbalance step needed."))
    } else {
      tagList(
        p(strong("Class distribution for: "), input$targetVar),
        plotOutput("classDistPlot", height="250px")
      )
    }
  })
  
  output$classDistPlot <- renderPlot({
    req(input$targetVar)
    df <- globalData$cleaned
    target <- df[[input$targetVar]]
    if (!is.factor(target)) return(NULL)
    ggplot(df, aes(x=target)) +
      geom_bar(fill=brewer.pal(3,"Pastel1")[1]) +
      theme_minimal() +
      labs(title="Class Distribution", x=input$targetVar, y="Count")
  })
  
  output$classImbStratUI <- renderUI({
    req(input$targetVar)
    df <- globalData$cleaned
    targetVar <- input$targetVar
    if (targetVar == "None" || !is.factor(df[[targetVar]])) {
      return(NULL)
    }
    tagList(
      p("Balancing strategies:"),
      selectInput("classImbStrat","Strategy",choices=c("No Action"="none","Oversample Minority"="over","Undersample Majority"="under"), selected="none"),
      p("Oversample: duplicate minority classes. Undersample: remove majority samples.")
    )
  })
  
  observeEvent(input$classImbNextBtn, {
    df <- globalData$cleaned
    if (input$targetVar=="None" || !is.factor(df[[input$targetVar]])) {
      goNext()
      return()
    }
    strat <- input$classImbStrat
    target <- input$targetVar
    if (strat == "over" || strat=="under") {
      targetCol <- df[[target]]
      freq <- table(targetCol)
      if (strat=="over") {
        maxCount <- max(freq)
        newDF <- lapply(names(freq), function(cls) {
          subdf <- df[targetCol==cls,,drop=FALSE]
          if (nrow(subdf)<maxCount) {
            subdf <- subdf[sample(nrow(subdf), maxCount, replace=TRUE),,drop=FALSE]
          }
          subdf
        })
        df <- do.call(rbind, newDF)
      } else {
        minCount <- min(freq)
        newDF <- lapply(names(freq), function(cls) {
          subdf <- df[targetCol==cls,,drop=FALSE]
          if (nrow(subdf)>minCount) {
            subdf <- subdf[sample(nrow(subdf), minCount, replace=FALSE),,drop=FALSE]
          }
          subdf
        })
        df <- do.call(rbind, newDF)
      }
      globalData$cleaned <- df
      showInfo("Class imbalance strategy applied.")
    }
    goNext()
  })
  
  ############################################
  ### STEP 8: Feature Engineering (Normalize)#
  ############################################
  
  output$featureEngUI <- renderUI({
    df <- globalData$cleaned
    req(df)
    numericCols <- names(df)[sapply(df, is.numeric)]
    
    fluidPage(
      fluidRow(
        column(8, offset=2,
               wellPanel(
                 h2("Step 8: Feature Engineering (Normalization)", style="text-align:center;"),
                 p("Choose a normalization method for each numeric variable:"),
                 tags$ul(
                   tags$li("Standard (Z-score): (x - mean)/sd"),
                   tags$li("Min-Max [0,1]: (x - min)/(max-min)"),
                   tags$li("None: leave as is.")
                 ),
                 if (length(numericCols)==0) {
                   tagList(
                     p("No numeric columns. No normalization needed."),
                     actionButton("featEngNextBtn","Proceed", class="btn-primary")
                   )
                 } else {
                   tagList(
                     lapply(numericCols, function(colName) {
                       selectInput(paste0("normMethod_", colName), colName,
                                   choices=c("None"="none","Standard (Z-score)"="zscore","Min-Max [0,1]"="minmax"))
                     }),
                     br(),
                     actionButton("applyNormBtn","Apply & Proceed", class="btn-primary")
                   )
                 }
               )
        )
      )
    )
  })
  
  observeEvent(input$featEngNextBtn, {
    goNext()
  })
  
  observeEvent(input$applyNormBtn, {
    df <- globalData$cleaned
    numericCols <- names(df)[sapply(df, is.numeric)]
    for (colName in numericCols) {
      method <- input[[paste0("normMethod_", colName)]]
      if (method=="none") next
      colData <- df[[colName]]
      if (all(is.na(colData))) next
      if (method=="zscore") {
        mu <- mean(colData, na.rm=TRUE)
        sdv <- sd(colData, na.rm=TRUE)
        if (sdv==0) df[[colName]] <- 0 else df[[colName]] <- (colData - mu)/sdv
      } else if (method=="minmax") {
        minVal <- min(colData, na.rm=TRUE)
        maxVal <- max(colData, na.rm=TRUE)
        if (minVal == maxVal) df[[colName]] <- 0 else df[[colName]] <- (colData - minVal)/(maxVal - minVal)
      }
    }
    globalData$cleaned <- df
    showInfo("Normalization applied.")
    goNext()
  })
  
  #############################################
  ### POST-PREPROCESSING: Final Interface    ##
  #############################################
  
  observeEvent(stepIndex(), {
    if (stepIndex() > 8) {
      updateNavbarPage(session, "finalNav", selected = "EDA_Univariate")
    }
  })
  
  output$finalUI <- renderUI({
    req(stepIndex() > 8)
    navbarPage(title="Data Analysis Companion",
               id="finalNav",
               tabPanel("EDA (Univariate)",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("uniVar","Select Variable",choices=names(globalData$cleaned)),
                            uiOutput("uniPlotTypeUI"),
                            actionButton("plotUniBtn","Plot", class="btn-primary")
                          ),
                          mainPanel(
                            uiOutput("uniEDAOutput")
                          )
                        )
               ),
               tabPanel("EDA (Bivariate)",
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("biVarSelectors"),
                            uiOutput("biPlotTypeUI"),
                            actionButton("plotBiBtn","Plot", class="btn-primary")
                          ),
                          mainPanel(
                            uiOutput("biEDAOutput")
                          )
                        )
               ),
               tabPanel("Correlation Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("corrVarSelectors"),
                            actionButton("corrPlotBtn","Compute", class="btn-primary")
                          ),
                          mainPanel(
                            uiOutput("corrOutput")
                          )
                        )
               ),
               tabPanel("Modeling",
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("modelingUI")
                          ),
                          mainPanel(
                            uiOutput("modelResultsUI")
                          )
                        )
               ),
               tabPanel("Case Study PDF",
                         sidebarLayout(
                           sidebarPanel(
                             downloadButton("downloadPdf", "Download PDF")
                           ),
                           mainPanel(
                             p("Click the button to download the PDF file.")
                           )
                         )
               )
    )
  })
  
  output$mainUI <- renderUI({
    if (stepIndex() == 1) {
      uiOutput("dataLoadUI")
    } else if (stepIndex() == 2) {
      uiOutput("dataPreviewUI")
    } else if (stepIndex() == 3) {
      uiOutput("varRecUI")
    } else if (stepIndex() == 4) {
      uiOutput("missingUI")
    } else if (stepIndex() == 5) {
      uiOutput("outlierUI")
    } else if (stepIndex() == 6) {
      uiOutput("highCardUI")
    } else if (stepIndex() == 7) {
      uiOutput("classImbalanceUI")
    } else if (stepIndex() == 8) {
      uiOutput("featureEngUI")
    } else {
      uiOutput("finalUI")
    }
  })
  
  ##########################################
  ### EDA Univariate Logic & Rendering   ###
  ##########################################
  
  # Dynamic plot type selection
  observeEvent(input$uniVar, {
    df <- globalData$cleaned
    req(input$uniVar)
    colData <- df[[input$uniVar]]
    varType <- if (is.numeric(colData)) "numeric" else if (is.factor(colData)) "factor" else "character"
    choices <- if (varType=="numeric") {
      c("Histogram","Boxplot","Summary Statistics")
    } else if (varType=="factor") {
      c("Barplot")
    } else {
      c("Wordcloud")
    }
    updateSelectInput(session,"uniPlotType",choices=choices)
  }, ignoreInit=TRUE)
  
  output$uniPlotTypeUI <- renderUI({
    selectInput("uniPlotType","Plot Type", choices=NULL)
  })
  
  observeEvent(input$plotUniBtn, {
    output$uniEDAOutput <- renderUI({
      req(input$uniVar, input$uniPlotType)
      df <- globalData$cleaned
      var <- input$uniVar
      colData <- df[[var]]
      
      varType <- if (is.numeric(colData)) "numeric" else if (is.factor(colData)) "factor" else "character"
      plt <- NULL
      
      if (varType=="numeric") {
        if (input$uniPlotType=="Histogram") {
          plt <- renderPlot({
            ggplot(df, aes_string(x=var)) +
              geom_histogram(fill=brewer.pal(3,"Pastel1")[3], bins=30) +
              theme_minimal() + labs(title=paste("Histogram of",var))
          })
        } else if (input$uniPlotType=="Boxplot") {
          plt <- renderPlot({
            ggplot(df, aes_string(y=var)) +
              geom_boxplot(fill=brewer.pal(3,"Pastel1")[2]) +
              theme_minimal() + labs(title=paste("Boxplot of",var))
          })
        } else {
          stats <- summary(colData)
          statsDF <- as.data.frame(t(stats))
          plt <- renderTable({
            statsDF
          }, rownames=FALSE)
        }
      } else if (varType=="factor") {
        plt <- renderPlot({
          ggplot(df, aes_string(x=var)) +
            geom_bar(fill=brewer.pal(3,"Pastel1")[1]) +
            theme_minimal() + labs(title=paste("Barplot of",var))
        })
      } else {
        words <- unlist(strsplit(paste(colData, collapse=" "), "\\s+"))
        words <- tolower(words)
        words <- words[words!=""]
        wordCount <- sort(table(words), decreasing=TRUE)
        topWords <- head(wordCount,100)
        plt <- renderWordcloud2({
          wordcloud2(data.frame(word=names(topWords), freq=as.integer(topWords)), size=0.7)
        })
      }
      plt
    })
  })
  
  ##########################################
  ### EDA Bivariate Logic & Rendering     ##
  ##########################################
  
  output$biVarSelectors <- renderUI({
    df <- globalData$cleaned
    cols <- names(df)
    characterCols <- cols[sapply(df, is.character)]
    validCols <- setdiff(cols, characterCols)
    if (length(validCols)<2) {
      return(p("Not enough valid columns for bivariate EDA."))
    } else {
      tagList(
        selectInput("biVar1", "Variable 1", choices=validCols),
        selectInput("biVar2", "Variable 2", choices=validCols, selected=validCols[2])
      )
    }
  })
  
  output$biPlotTypeUI <- renderUI({ NULL })
  
  observeEvent(input$plotBiBtn, {
    output$biEDAOutput <- renderUI({
      req(input$biVar1, input$biVar2)
      df <- globalData$cleaned
      v1 <- input$biVar1
      v2 <- input$biVar2
      col1 <- df[[v1]]
      col2 <- df[[v2]]
      
      type1 <- if (is.numeric(col1)) "numeric" else "factor"
      type2 <- if (is.numeric(col2)) "numeric" else "factor"
      
      if (type1 == "numeric" && type2 == "numeric") {
        renderPlot({
          ggplot(df, aes_string(x = v1, y = v2)) +
            geom_point(color = brewer.pal(3, "Pastel1")[3], alpha = 0.7) +
            theme_minimal() +
            labs(title = paste("Scatter:", v1, "vs", v2))
        })
      } else if (type1 == "numeric" && type2 == "factor") {
        renderPlot({
          ggplot(df, aes_string(x = v1, fill = v2)) +
            geom_density(alpha = 0.6) +
            theme_minimal() +
            labs(title = paste("Density Plot of", v1, "by", v2), x = v1, fill = v2)
        })
      } else if (type1 == "factor" && type2 == "numeric") {
        renderPlot({
          ggplot(df, aes_string(x = v2, fill = v1)) +
            geom_density(alpha = 0.6) +
            theme_minimal() +
            labs(title = paste("Density Plot of", v2, "by", v1), x = v2, fill = v1)
        })
      } else {
        renderPlot({
          ggplot(df, aes_string(x = v1, fill = v2)) +
            geom_bar(position = "dodge") +
            scale_fill_brewer(palette = "Pastel1") +
            theme_minimal() +
            labs(title = paste("Grouped Bar:", v1, "by", v2))
        })
      }
    })
  })
  
  #####################################
  ### Correlation Analysis Logic     ##
  #####################################
  
  output$corrVarSelectors <- renderUI({
    df <- globalData$cleaned
    numericCols <- names(df)[sapply(df,is.numeric)]
    factorCols <- names(df)[sapply(df,is.factor)]
    
    tagList(
      h4("Correlation & Chi-square"),
      p("For numeric variables, a correlation matrix is computed. For categorical variables, a chi-square p-value heatmap is generated."),
      checkboxGroupInput("corrNumericVars","Numeric Variables:",choices=numericCols,selected=numericCols),
      checkboxGroupInput("corrFactorVars","Factor Variables:",choices=factorCols,selected=factorCols)
    )
  })
  
  observeEvent(input$corrPlotBtn, {
    output$corrOutput <- renderUI({
      df <- globalData$cleaned
      numVars <- input$corrNumericVars
      facVars <- input$corrFactorVars
      plots <- list()
      
      # Numeric correlation
      if (length(numVars)>1) {
        subdf_num <- df[numVars]
        subdf_num <- subdf_num[sapply(subdf_num, function(x) length(unique(x[!is.na(x)]))>1)]
        if (length(subdf_num)>1) {
          corrMat <- cor(subdf_num, use="complete.obs")
          corrPlot <- renderPlot({
            corrplot(corrMat, method="color", type="upper", tl.col="black",
                     col=colorRampPalette(c("#f7f7f7","#c2e5d3","#66c2a4","#238b45"))(200))
          })
          plots <- c(plots, list(h3("Numeric Correlation Matrix"), corrPlot))
        } else {
          plots <- c(plots, list(p("Not enough numeric data for correlation matrix.")))
        }
      } else {
        plots <- c(plots, list(p("Not enough numeric variables for correlation.")))
      }
      
      # Factor chi-square p-values
      if (length(facVars)>1) {
        subdf_fac <- df[facVars]
        combos <- combn(facVars,2)
        pvals <- matrix(NA, nrow=length(facVars), ncol=length(facVars), dimnames=list(facVars, facVars))
        diag(pvals) <- NA
        for (i in 1:ncol(combos)) {
          f1 <- combos[1,i]
          f2 <- combos[2,i]
          ttab <- table(subdf_fac[[f1]], subdf_fac[[f2]])
          if (all(ttab==0)) {
            pval <- NA
          } else {
            chi <- suppressWarnings(chisq.test(ttab))
            pval <- chi$p.value
          }
          pvals[f1,f2] <- pval
          pvals[f2,f1] <- pval
        }
        
        df_pvals <- as.data.frame(as.table(pvals))
        names(df_pvals) <- c("Var1","Var2","pvalue")
        df_pvals <- df_pvals[!is.na(df_pvals$pvalue),]
        
        chiPlot <- renderPlot({
          ggplot(df_pvals, aes(Var1, Var2, fill=pvalue)) +
            geom_tile(color="white") +
            scale_fill_gradient(low="#f7f7f7", high="#2b8cbe", na.value="grey50") +
            theme_minimal() +
            theme(axis.text.x=element_text(angle=45,hjust=1)) +
            labs(title="Chi-square p-value Heatmap", x="", y="", fill="p-value")
        })
        plots <- c(plots, list(h3("Categorical Chi-square p-values"), chiPlot))
      } else {
        plots <- c(plots, list(p("Not enough factor variables for chi-square analysis.")))
      }
      
      do.call(tagList, plots)
    })
  })
  
  #####################################
  ### Modeling UI & Server Logic    ###
  #####################################
  
  output$modelingUI <- renderUI({
    df <- globalData$cleaned
    if (is.null(df) || nrow(df)<5) {
      return(p("Insufficient data for modeling."))
    }
    tagList(
      h4("Modeling"),
      selectInput("modelTask","Task",choices=c("Classification","Regression")),
      uiOutput("modelTargetUI"),
      uiOutput("modelPredictorsUI"),
      uiOutput("modelTypeUI"),
      uiOutput("modelParamsUI"),
      actionButton("trainModelBtn","Train Model", class="btn-primary")
    )
  })
  
  output$modelTargetUI <- renderUI({
    req(input$modelTask)
    df <- globalData$cleaned
    if (input$modelTask=="Classification") {
      factorTargets <- names(df)[sapply(df,is.factor)]
      if (length(factorTargets)==0) return(p("No factor targets for classification."))
      selectInput("modelTarget","Target Variable", choices=factorTargets)
    } else {
      numericTargets <- names(df)[sapply(df,is.numeric)]
      if (length(numericTargets)==0) return(p("No numeric targets for regression."))
      selectInput("modelTarget","Target Variable", choices=numericTargets)
    }
  })
  
  output$modelPredictorsUI <- renderUI({
    req(input$modelTarget)
    df <- globalData$cleaned
    target <- input$modelTarget
    
    # For classification with logistic regression, target must be factor with 2 levels.
    if (input$modelTask=="Classification" && length(levels(df[[target]]))!=2 && input$modelAlgo=="logreg") {
      return(p("For logistic regression, the target must have exactly 2 classes."))
    }
    
    preds <- setdiff(names(df), target)
    # Filter out character predictors automatically (modeling often fails with raw chars)
    # We'll allow numeric and factor. If needed, we could disable certain predictors.
    viablePreds <- preds[!sapply(df[,preds,drop=FALSE], is.character)]
    if (length(viablePreds)==0) return(p("No viable predictors available."))
    checkboxGroupInput("modelPredictors","Predictors", choices=viablePreds, selected=viablePreds)
  })
  
  output$modelTypeUI <- renderUI({
    req(input$modelTask)
    if (input$modelTask=="Classification") {
      selectInput("modelAlgo","Model Type",choices=c("Random Forest"="rf","SVM"="svm","Logistic Regression"="logreg"))
    } else {
      selectInput("modelAlgo","Model Type",choices=c("Random Forest"="rf","SVM"="svm","Linear Regression"="linreg"))
    }
  })
  
  output$modelParamsUI <- renderUI({
    req(input$modelAlgo)
    if (input$modelAlgo=="rf") {
      numericInput("mtry","mtry (RF)",value=2,min=1,step=1)
    } else if (input$modelAlgo=="svm") {
      numericInput("cost","Cost (SVM)",value=1,min=0.1,step=0.1)
    } else {
      p("No additional hyperparameters.")
    }
  })
  
  output$downloadPdf <- downloadHandler(
    filename = function() {
      "Case_Study_Titanic_Bastien_Hottelet.pdf"
    },
    content = function(file) {
      file.copy("Case_Study_Titanic_Bastien_Hottelet.pdf", file)
    }
  )
  
  observeEvent(input$trainModelBtn, {
    output$modelResultsUI <- renderUI({
      df <- globalData$cleaned
      req(input$modelTarget, input$modelPredictors, input$modelAlgo)
      
      # Check feasibility
      if (length(input$modelPredictors)==0) {
        return(p("No predictors selected."))
      }
      
      targetVar <- input$modelTarget
      predictors <- input$modelPredictors
      task <- input$modelTask
      algo <- input$modelAlgo
      
      # Prepare data
      subdf <- df[, c(targetVar, predictors), drop=FALSE]
      subdf <- subdf[complete.cases(subdf), , drop=FALSE]
      
      # Modify class levels to valid R variable names
      if (is.factor(subdf[[targetVar]])) {
        levels(subdf[[targetVar]]) <- make.names(levels(subdf[[targetVar]]))
      }
      
      # Check if target variable has at least two unique values
      if (length(unique(subdf[[targetVar]])) < 2) {
        return(p("The target variable has fewer than 2 distinct values after preprocessing. 
                This could be due to oversampling/undersampling, outlier removal, or other steps. 
                Please adjust your preprocessing steps or select another target."))
      }
      
      if (nrow(subdf) < 5) {
        return(p("Not enough data after preprocessing. Minimum of 5 rows is required."))
      }
      
      # Dummify factor predictors
      factPreds <- predictors[sapply(subdf[, predictors, drop=FALSE], is.factor)]
      if (length(factPreds) > 0) {
        dummies <- caret::dummyVars(~ ., data=subdf[, factPreds, drop=FALSE])
        dummyData <- predict(dummies, subdf)
        subdf <- cbind(subdf[, setdiff(names(subdf), factPreds), drop=FALSE], dummyData)
      }
      
      set.seed(123)
      
      # Create Data Partition
      trainIndex <- tryCatch({
        createDataPartition(subdf[[targetVar]], p=0.7, list=FALSE)
      }, error = function(e) {
        cat("Error in createDataPartition:", e$message, "\n")
        return(NULL)
      })
      
      if (is.null(trainIndex)) {
        return(p("createDataPartition failed. The target variable might be degenerate (only one class) or insufficient data remains. Check the console for more details."))
      }
      
      trainDF <- subdf[trainIndex, , drop=FALSE]
      testDF <- subdf[-trainIndex, , drop=FALSE]
      
      ctrl <- trainControl(method="cv", number=3, 
                           classProbs=(task=="Classification"), 
                           summaryFunction= if (task=="Classification") twoClassSummary else defaultSummary)
      
      modelMethod <- if (algo=="rf") "rf" else if (algo=="svm") "svmRadial" else if (algo=="logreg") "glm" else if (algo=="linreg") "lm"
      tuneGrid <- NULL
      if (algo=="rf") {
        tuneGrid <- data.frame(.mtry=input$mtry)
      } else if (algo=="svm") {
        tuneGrid <- data.frame(.sigma=0.1, .C=input$cost)
      }
      
      fit <- NULL
      fit <- tryCatch({
        if (algo=="logreg" && task=="Classification") {
          train(as.formula(paste(targetVar,"~.")), data=trainDF, method="glm", family=binomial, trControl=ctrl, metric="ROC")
        } else if (algo=="linreg" && task=="Regression") {
          train(as.formula(paste(targetVar,"~.")), data=trainDF, method="lm", trControl=ctrl, metric="RMSE")
        } else {
          train(as.formula(paste(targetVar,"~.")), data=trainDF, method=modelMethod, tuneGrid=tuneGrid, trControl=ctrl, 
                metric= if(task=="Classification") "ROC" else "RMSE")
        }
      }, error=function(e) {
        return(paste("Model training failed:", e$message))
      })
      
      if (is.character(fit)) {
        return(p(fit))
      }
      if (is.null(fit) || !inherits(fit,"train")) {
        return(p("Model training failed. Check selections and data."))
      }
      
      preds <- predict(fit, testDF)
      if (task=="Classification") {
        if (length(levels(trainDF[[targetVar]]))!=2) {
          return(p("Only ROC-based metrics implemented for binary classification. Your target does not have 2 classes."))
        }
        cm <- confusionMatrix(preds, testDF[[targetVar]])
        probPreds <- predict(fit, testDF, type="prob")
        posClass <- colnames(probPreds)[2]
        predObj <- ROCR::prediction(probPreds[[posClass]], testDF[[targetVar]]==posClass)
        perf <- ROCR::performance(predObj,"tpr","fpr")
        auc <- ROCR::performance(predObj,"auc")@y.values[[1]]
        f1 <- cm$byClass["F1"]
        recall <- cm$byClass["Sensitivity"]
        precision <- cm$byClass["Pos Pred Value"]
        
        resultUI <- list(
          h3("Classification Results"),
          DTOutput("classMetricsTable"),
          plotOutput("rocPlot", height="300px"),
          h4("Variable Importance"),
          plotOutput("vipPlot", height="300px")
        )
        
        output$classMetricsTable <- renderDT({
          data.frame(
            Metric=c("Accuracy","AUC","F1","Recall","Precision"),
            Value=round(c(cm$overall["Accuracy"], auc, f1, recall, precision),3)
          )
        }, options=list(dom='t', paging=FALSE), rownames=FALSE)
        
        output$rocPlot <- renderPlot({
          plot(perf, colorize=FALSE, col="#79152d", lwd=2, main="ROC Curve")
          abline(a=0,b=1,lty=2,col="gray")
        })
        
        vip <- try(varImp(fit), silent = TRUE)
        
        if (!inherits(vip, "try-error")) {
          output$vipPlot <- renderPlot({
            num_vars <- nrow(vip$importance)
            top_n <- min(10, num_vars)
            plot(vip, top = top_n, main = "Top Variable Importance")
          })
        } else {
          output$vipPlot <- renderPlot({
            plot.new()
            text(0.5, 0.5, "No variable importance available.")
          })
        }
        
        do.call(tagList, resultUI)
        
      } else {
        # Regression
        obs <- testDF[[targetVar]]
        rmse_val <- sqrt(mean((obs - preds)^2))
        mae_val <- mean(abs(obs - preds))
        r2_val <- cor(obs, preds)^2
        
        resultUI <- list(
          h3("Regression Results"),
          DTOutput("regMetricsTable"),
          plotOutput("regPlot", height="300px"),
          h4("Variable Importance"),
          plotOutput("vipPlotReg", height="300px")
        )
        
        output$regMetricsTable <- renderDT({
          data.frame(
            Metric=c("RMSE","MAE","R²"),
            Value=round(c(rmse_val, mae_val, r2_val),3)
          )
        }, options=list(dom='t', paging=FALSE), rownames=FALSE)
        
        output$regPlot <- renderPlot({
          ggplot(data.frame(obs=obs,pred=preds),aes(x=obs,y=pred))+
            geom_point(color="#79152d")+
            geom_abline(slope=1,intercept=0,lty=2,col="gray")+
            theme_minimal()+
            labs(title="Predicted vs Actual", x="Actual", y="Predicted")
        })
        
        vip <- try(varImp(fit), silent = TRUE)
        
        if (!inherits(vip, "try-error")) {
          output$vipPlotReg <- renderPlot({
            num_vars <- nrow(vip$importance)
            top_n <- min(10, num_vars)
            plot(vip, top = top_n, main = "Top Variable Importance")
          })
        } else {
          output$vipPlotReg <- renderPlot({
            plot.new()
            text(0.5, 0.5, "No variable importance available.")
          })
        }
        
        do.call(tagList, resultUI)
      }
    })
  })
}

shinyApp(ui, server)
