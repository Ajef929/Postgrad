shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models
  
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)
  
  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    #d$ChemoTreatments <- as.factor(d$ChemoTreatments)
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  
  
  output$DataSummary2 <- renderPrint({
    data <- getData()
    dfSummary(data)
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
    m <- getMethods()
    m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
    DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  
  
  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = input$glmnet_tuneLength0, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })
  
  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = input$pls_tuneLength0, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })
  
  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  
  
  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = input$rpart_tuneLength0, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  
  
  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  
  
  #selecting candidate models
  output$candidates <- renderPrint({
    
    modelInfo <-  caret::getModelInfo()
    tags <- vector(mode = "list", length = length(modelInfo))
    Classification <- Regression <- ClassProbs <- rep(NA, length = length(modelInfo))
    for (i in seq(along = modelInfo)){
      tags[[i]] <- modelInfo[[i]]$tags
      Classification[i] <- ifelse("Classification" %in% modelInfo[[i]]$type, 1, 0)
      Regression[i] <- ifelse("Regression" %in% modelInfo[[i]]$type, 1, 0)
      ClassProbs[i] <- ifelse(is.null(modelInfo[[i]]$prob), 0, 1)
    }
    tabs <- table(unlist(tags))
    tabs <- tabs[order(tolower(names(tabs)))]
    terms <- names(tabs)
    terms <- terms[terms != ""]
    dat <- matrix(0, ncol = length(terms), nrow = length(tags))
    colnames(dat) <- terms
    hasTag <- lapply(tags, function(x, y) which(y %in% x), y = terms)
    for (i in seq(along = hasTag)) {
      dat[i, hasTag[[i]]] <- 1
    }
    dat <- cbind(Classification, Regression, dat)
    wide <- as.data.frame(dat, row.names = names(modelInfo))
    wide
    ##starting with regression models
    #model_type <- input$candidate_method_type
    #models <- dat[data[,model_type] == 1,]
    
    regModels <- wide[wide[,"Regression"] == 1,]
    
    all <- 1:nrow(regModels)
    ## Seed the analysis with the SVM model
    rnmaes <- rownames(regModels)
    
    start <- grep("svmRadial", rnmaes, fixed = TRUE)
    
    pool <- all[all != start]
    ## Select 4 model models by maximizing the Jaccard
    ## dissimilarity between sets of models
    nextMods <- maxDissim(regModels[start,,drop = FALSE], 
                          regModels[pool, ], 
                          method = "Jaccard",
                          n = input$n_models)
    
    rownames(regModels)[c(nextMods)]
  })
  
  
  
  
  
  ##cubist
  library(Cubist)
  
  ##obtain cubist recipe
  getCubistRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$cubist_Go,
    {
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getCubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength=input$cubist_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$cubist_Load,
    {
      method  <- "cubist"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$cubist_Delete,
    {
      models[["cubist"]] <- NULL
      gc()
    }
  )
  
  
  
  output$cubist_ModelSummary0 <- renderText({
    description("cubist")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$cubist_Metrics <- renderTable({
    req(models$cubist)
    models$cubist$results[which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$cubist_Recipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$cubist_ModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  output$cubist_ModelSummary2 <- renderPrint({
    req(models$cubist)
    summary(models$cubist$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$cubist_Coef <- renderTable({
    req(models$cubist)
    co <- coef(models$cubist$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  #rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  
  
  
  ##brnn section
  
  library(brnn)
  
  ##obtain brnn recipe
  getbrnnRecipe <- reactive({
    form <- formula(Response ~ .)
    rec <- recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$brnn_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
    
    rec
  })
  
  
  
  observeEvent(
    input$brnn_Go,
    {
      method <- "brnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        trcont <- getTrControl()
        trcont$seeds <- NULL
        
        model <- caret::train(getbrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont,tuneLength = input$brnn_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$brnn_Load,
    {
      method  <- "brnn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$brnn_Delete,
    {
      models[["brnn"]] <- NULL
      gc()
    }
  )
  
  
  
  output$brnn_ModelSummary0 <- renderText({
    description("brnn")   # Use the caret method name here
  })
  
  output$brnn_Metrics <- renderTable({
    req(models$brnn)
    models$brnn$results[which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  output$brnn_Recipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })  
  
  output$brnn_ModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })
  
  output$brnn_ModelSummary2 <- renderPrint({
    req(models$brnn)
    summary(models$brnn$finalModel)
  })
  
  output$brnn_Coef <- renderTable({
    req(models$brnn)
    co <- coef(models$brnn$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # 
  # library
  # 
  # library(foba)
  # ##obtain foba recipe
  # getfobaRecipe <- reactive({
  #   form <- formula(Response ~ .)
  #   recipes::recipe(form, data = getTrainData()) %>%
  #     dynamicSteps(input$foba_Preprocess) %>%   # use <method>_Preprocess
  #     step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # 
  # 
  # observeEvent(
  #   input$foba_Go,
  #   {
  #     method <- "foba"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       model <- caret::train(getfobaRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
  #       deleteRds(method)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     }, 
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # observeEvent(
  #   input$foba_Load,
  #   {
  #     method  <- "foba"
  #     model <- loadRds(method, session)
  #     if (!is.null(model)) {
  #       models[[method]] <- model
  #     }
  #   }
  # )
  # 
  # observeEvent(
  #   input$foba_Delete,
  #   {
  #     models[["foba"]] <- NULL
  #     gc()
  #   }
  # )
  # 
  # 
  # 
  # output$foba_ModelSummary0 <- renderText({
  #   description("foba")   # Use the caret method name here
  # })
  # 
  # output$foba_Metrics <- renderTable({
  #   req(models$foba)
  #   models$foba$results[which.min(models$foba$results[, "RMSE"]), ]
  # })
  # 
  # output$foba_Recipe <- renderPrint({
  #   req(models$foba)
  #   models$foba$recipe
  # })  
  # 
  # output$foba_ModelPlots <- renderPlot({
  #   req(models$foba)
  #   plot(models$foba)
  # })
  # 
  # output$foba_ModelSummary2 <- renderPrint({
  #   req(models$foba)
  #   summary(models$foba$finalModel)
  # })
  # 
  # ##coeffs required for regression
  # output$foba_Coef <- renderTable({
  #   req(models$foba)
  #   co <- coef(models$foba$finalModel)
  #   as.data.frame(co, row.names = rownames(co))
  # }, rownames = TRUE, colnames = FALSE)
  # 
  # 
  # 
  
  # library(qrnn)
  # 
  # ##obtain qrnn recipe
  # getqrnnRecipe <- reactive({
  #   form <- formula(Response ~ .)
  #   recipes::recipe(form, data = getTrainData()) %>%
  #     dynamicSteps(input$qrnn_Preprocess) %>%   # use <method>_Preprocess
  #     step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # 
  # 
  # observeEvent(
  #   input$qrnn_Go,
  #   {
  #     method <- "qrnn"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       
  #       qrtc <- getTrControl()
  #       #set trControl seed to null - will make not repeatable
  #       qrtc$seeds <- NULL
  #       
  #       #index <- qrtc$index
  #       #print(index)
  #       ##searching for missing values ~
  #       
  #       tg <- expand.grid(n.hidden = c(1,2,3,4,5,6,7,8,9),penalty=c(0,0.1,0.01,0.001),bag=c(TRUE,FALSE))
  # 
  #       model <- caret::train(getqrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = qrtc,tunegrid = tg,na.action=na.omit)
  #       deleteRds(method)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # observeEvent(
  #   input$qrnn_Load,
  #   {
  #     method  <- "qrnn"
  #     model <- loadRds(method, session)
  #     if (!is.null(model)) {
  #       models[[method]] <- model
  #     }
  #   }
  # )
  # 
  # observeEvent(
  #   input$qrnn_Delete,
  #   {
  #     models[["qrnn"]] <- NULL
  #     gc()
  #   }
  # )
  # 
  # 
  # 
  # output$qrnn_ModelSummary0 <- renderText({
  #   description("qrnn")   # Use the caret method name here
  # })
  # 
  # output$qrnn_Metrics <- renderTable({
  #   req(models$qrnn)
  #   models$qrnn$results[which.min(models$qrnn$results[, "RMSE"]), ]
  # })
  # 
  # output$qrnn_Recipe <- renderPrint({
  #   req(models$qrnn)
  #   models$qrnn$recipe
  # })
  # 
  # output$qrnn_ModelPlots <- renderPlot({
  #   req(models$qrnn)
  #   plot(models$qrnn)
  # })
  # 
  # output$qrnn_ModelSummary2 <- renderPrint({
  #   req(models$qrnn)
  #   summary(models$qrnn$finalModel)
  # })
  # 
  # 
  # ##coeffs required for regression
  # output$qrnn_Coef <- renderTable({
  #   req(models$qrnn)
  #   co <- coef(models$qrnn$finalModel)
  #   as.data.frame(co, row.names = rownames(co))
  # }, rownames = TRUE, colnames = FALSE)
  # 
  
  # library(frbs)
  # 
  # ##obtain ANFIS recipe
  # getANFISRecipe <- reactive({
  #   form <- formula(Response ~ .)
  #   recipes::recipe(form, data = getTrainData()) %>%
  #     dynamicSteps(input$ANFIS_Preprocess) %>%   # use <method>_Preprocess
  #     step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # 
  # 
  # observeEvent(
  #   input$ANFIS_Go,
  #   {
  #     method <- "ANFIS"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       model <- caret::train(getANFISRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
  #       deleteRds(method)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     }, 
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # observeEvent(
  #   input$ANFIS_Load,
  #   {
  #     method  <- "ANFIS"
  #     model <- loadRds(method, session)
  #     if (!is.null(model)) {
  #       models[[method]] <- model
  #     }
  #   }
  # )
  # 
  # observeEvent(
  #   input$ANFIS_Delete,
  #   {
  #     models[["ANFIS"]] <- NULL
  #     gc()
  #   }
  # )
  # 
  # 
  # 
  # output$ANFIS_ModelSummary0 <- renderText({
  #   description("ANFIS")   # Use the caret method name here
  # })
  # 
  # output$ANFIS_Metrics <- renderTable({
  #   req(models$ANFIS)
  #   models$ANFIS$results[which.min(models$ANFIS$results[, "RMSE"]), ]
  # })
  # 
  # output$ANFIS_Recipe <- renderPrint({
  #   req(models$ANFIS)
  #   models$ANFIS$recipe
  # })  
  # 
  # output$ANFIS_ModelPlots <- renderPlot({
  #   req(models$ANFIS)
  #   plot(models$ANFIS)
  # })
  # 
  # output$ANFIS_ModelSummary2 <- renderPrint({
  #   req(models$ANFIS)
  #   summary(models$ANFIS$finalModel)
  # })
  # 
  
  
  library(earth)
  
  ##obtain bagEarth recipe
  getbagEarthRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bagEarth_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$bagEarth_Go,
    {
      method <- "bagEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getbagEarthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength=input$bagEarth_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$bagEarth_Load,
    {
      method  <- "bagEarth"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bagEarth_Delete,
    {
      models[["bagEarth"]] <- NULL
      gc()
    }
  )
  
  
  
  output$bagEarth_ModelSummary0 <- renderText({
    description("bagEarth")   # Use the caret method name here
  })
  
  output$bagEarth_Metrics <- renderTable({
    req(models$bagEarth)
    models$bagEarth$results[which.min(models$bagEarth$results[, "RMSE"]), ]
  })
  
  output$bagEarth_Recipe <- renderPrint({
    req(models$bagEarth)
    models$bagEarth$recipe
  })  
  
  output$bagEarth_ModelPlots <- renderPlot({
    req(models$bagEarth)
    plot(models$bagEarth)
  })
  
  output$bagEarth_ModelSummary2 <- renderPrint({
    req(models$bagEarth)
    summary(models$bagEarth$finalModel)
  })
  
  
  ##coeffs required for regression
  output$bagEarth_Coef <- renderTable({
    req(models$bagEarth)
    co <- coef(models$bagEarth$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  library(KRLS)
  
  ##obtain krlsPoly recipe
  getkrlsPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$krlsPoly_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$krlsPoly_Go,
    {
      method <- "krlsPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getkrlsPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength=input$krlsPoly_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$krlsPoly_Load,
    {
      method  <- "krlsPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$krlsPoly_Delete,
    {
      models[["krlsPoly"]] <- NULL
      gc()
    }
  )
  
  
  
  output$krlsPoly_ModelSummary0 <- renderText({
    description("krlsPoly")   # Use the caret method name here
  })
  
  output$krlsPoly_Metrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  output$krlsPoly_Recipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })  
  
  output$krlsPoly_ModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly,which=1)
  })
  
  output$krlsPoly_ModelPlots2 <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly,which=2)
  })
  
  output$krlsPoly_ModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    
    #summary(models$krlsPoly$finalModel)
    summary(models$krlsPoly$finalModel)
  })
  
  
  
  
  ##pcannet
  library(nnet)
  
  ##obtain pcaNNet recipe
  getpcaNNetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pcaNNet_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$pcaNNet_Go,
    {
      method <- "pcaNNet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        tc <- getTrControl()
        tc$seeds <- NULL
        model <- caret::train(getpcaNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = tc,tuneLength=input$pcaNNet_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pcaNNet_Load,
    {
      method  <- "pcaNNet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pcaNNet_Delete,
    {
      models[["pcaNNet"]] <- NULL
      gc()
    }
  )
  
  
  
  output$pcaNNet_ModelSummary0 <- renderText({
    description("pcaNNet")   # Use the caret method name here
  })
  
  output$pcaNNet_Metrics <- renderTable({
    req(models$pcaNNet)
    models$pcaNNet$results[which.min(models$pcaNNet$results[, "RMSE"]), ]
  })
  
  output$pcaNNet_Recipe <- renderPrint({
    req(models$pcaNNet)
    models$pcaNNet$recipe
  })  
  
  output$pcaNNet_ModelPlots <- renderPlot({
    req(models$pcaNNet)
    plot(models$pcaNNet)
  })
  
  output$pcaNNet_ModelSummary2 <- renderPrint({
    req(models$pcaNNet)
    summary(models$pcaNNet$finalModel)
  })
  
  
  ##coeffs required for regression
  output$pcaNNet_Coef <- renderTable({
    req(models$pcaNNet)
    co <- coef(models$pcaNNet$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  ##glmboost
  library(mboost)
  
  ##obtain glmboost recipe
  getglmboostRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmboost_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$glmboost_Go,
    {
      method <- "glmboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getglmboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength=input$glmboost_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$glmboost_Load,
    {
      method  <- "glmboost"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmboost_Delete,
    {
      models[["glmboost"]] <- NULL
      gc()
    }
  )
  
  
  
  output$glmboost_ModelSummary0 <- renderText({
    description("glmboost")   # Use the caret method name here
  })
  
  output$glmboost_Metrics <- renderTable({
    req(models$glmboost)
    models$glmboost$results[which.min(models$glmboost$results[, "RMSE"]), ]
  })
  
  output$glmboost_Recipe <- renderPrint({
    req(models$glmboost)
    models$glmboost$recipe
  })  
  
  output$glmboost_ModelPlots <- renderPlot({
    req(models$glmboost)
    plot(models$glmboost)
  })
  
  output$glmboost_ModelSummary2 <- renderPrint({
    req(models$glmboost)
    
    summary(models$glmboost$finalModel)
  })
  
  
  ##coeffs required for regression
  output$glmboost_Coef <- renderTable({
    req(models$glmboost)
    co <- coef(models$glmboost$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  library(plsRglm)
  
  ##obtain plsRglm recipe
  getplsRglmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$plsRglm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$plsRglm_Go,
    {
      method <- "plsRglm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        
        tc <- getTrControl()
        tc$seeds <- NULL
        model <- caret::train(getplsRglmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = tc,tuneLength=input$plsRglm_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$plsRglm_Load,
    {
      method  <- "plsRglm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$plsRglm_Delete,
    {
      models[["plsRglm"]] <- NULL
      gc()
    }
  )
  
  
  
  output$plsRglm_ModelSummary0 <- renderText({
    description("plsRglm")   # Use the caret method name here
  })
  
  output$plsRglm_Metrics <- renderTable({
    req(models$plsRglm)
    models$plsRglm$results[which.min(models$plsRglm$results[, "RMSE"]), ]
  })
  
  output$plsRglm_Recipe <- renderPrint({
    req(models$plsRglm)
    models$plsRglm$recipe
  })  
  
  output$plsRglm_ModelPlots <- renderPlot({
    req(models$plsRglm)
    plot(models$plsRglm)
  })
  
  output$plsRglm_ModelSummary2 <- renderPrint({
    req(models$plsRglm)
    summary(models$plsRglm$finalModel)
  })
  
  
  ##coeffs required for regression
  output$plsRglm_Coef <- renderPrint({
    req(models$plsRglm)
    co <- coef(models$plsRglm$finalModel)
    co
    #as.data.frame(co, row.names = rownames(co))
  })
  
  
  
  
  
  # 
  # library(kernlab)
  # 
  # ##obtain rvmLinear recipe
  # getrvmLinearRecipe <- reactive({
  #   form <- formula(Response ~ .)
  #   recipes::recipe(form, data = getTrainData()) %>%
  #     dynamicSteps(input$rvmLinear_Preprocess) %>%   # use <method>_Preprocess
  #     step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # 
  # 
  # observeEvent(
  #   input$rvmLinear_Go,
  #   {
  #     method <- "rvmLinear"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       trct <- getTrControl()
  #   
  #       trct$seeds <- NA
  #       
  #       rec <- getrvmLinearRecipe()
  #       
  #       model <- caret::train(getrvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trct)
  #       deleteRds(method)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     }, 
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # observeEvent(
  #   input$rvmLinear_Load,
  #   {
  #     method  <- "rvmLinear"
  #     model <- loadRds(method, session)
  #     if (!is.null(model)) {
  #       models[[method]] <- model
  #     }
  #   }
  # )
  # 
  # observeEvent(
  #   input$rvmLinear_Delete,
  #   {
  #     models[["rvmLinear"]] <- NULL
  #     gc()
  #   }
  # )
  # 
  # 
  # 
  # output$rvmLinear_ModelSummary0 <- renderText({
  #   description("rvmLinear")   # Use the caret method name here
  # })
  # 
  # output$rvmLinear_Metrics <- renderTable({
  #   req(models$rvmLinear)
  #   models$rvmLinear$results[which.min(models$rvmLinear$results[, "RMSE"]), ]
  # })
  # 
  # output$rvmLinear_Recipe <- renderPrint({
  #   req(models$rvmLinear)
  #   models$rvmLinear$recipe
  # })  
  # 
  # output$rvmLinear_ModelPlots <- renderPlot({
  #   req(models$rvmLinear)
  #   plot(models$rvmLinear)
  # })
  # 
  # output$rvmLinear_ModelSummary2 <- renderPrint({
  #   req(models$rvmLinear)
  #   summary(models$rvmLinear$finalModel)
  # })
  # 
  # 
  # ##coeffs required for regression
  # output$rvmLinear_Coef <- renderTable({
  #   req(models$rvmLinear)
  #   co <- coef(models$rvmLinear$finalModel)
  #   as.data.frame(co, row.names = rownames(co))
  # }, rownames = TRUE, colnames = FALSE)
  # 
  # 
  
  
  library(mgcv)
  
  ##obtain bam recipe
  getbamRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bam_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$bam_Go,
    {
      method <- "bam"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getbamRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength=input$bam_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$bam_Load,
    {
      method  <- "bam"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bam_Delete,
    {
      models[["bam"]] <- NULL
      gc()
    }
  )
  
  
  
  output$bam_ModelSummary0 <- renderText({
    description("bam")   # Use the caret method name here
  })
  
  output$bam_Metrics <- renderTable({
    req(models$bam)
    models$bam$results[which.min(models$bam$results[, "RMSE"]), ]
  })
  
  output$bam_Recipe <- renderPrint({
    req(models$bam)
    models$bam$recipe
  })  
  
  output$bam_ModelPlots <- renderPlot({
    req(models$bam)
    plot(models$bam)
  })
  
  output$bam_ModelSummary2 <- renderPrint({
    req(models$bam)
    summary(models$bam$finalModel)
  })
  
  
  ##coeffs required for regression
  output$bam_Coef <- renderTable({
    req(models$bam)
    co <- coef(models$bam$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  
  library(MASS)
  
  ##obtain rlm recipe
  getrlmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rlm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$rlm_Go,
    {
      method <- "rlm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getrlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength=input$rlm_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$rlm_Load,
    {
      method  <- "rlm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rlm_Delete,
    {
      models[["rlm"]] <- NULL
      gc()
    }
  )
  
  
  
  output$rlm_ModelSummary0 <- renderText({
    description("rlm")   # Use the caret method name here
  })
  
  output$rlm_Metrics <- renderTable({
    req(models$rlm)
    models$rlm$results[which.min(models$rlm$results[, "RMSE"]), ]
  })
  
  output$rlm_Recipe <- renderPrint({
    req(models$rlm)
    models$rlm$recipe
  })  
  
  output$rlm_ModelPlots <- renderPlot({
    req(models$rlm)
    plot(models$rlm)
  })
  
  output$rlm_ModelSummary2 <- renderPrint({
    req(models$rlm)
    summary(models$rlm$finalModel)
  })
  
  
  ##coeffs required for regression
  output$rlm_Coef <- renderTable({
    req(models$rlm)
    co <- coef(models$rlm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # library(h2o)
  # 
  # 
  # 
  # ##obtain glmnet_h2o recipe
  # getglmnet_h2oRecipe <- reactive({
  #   form <- formula(Response ~ .)
  #   recipes::recipe(form, data = getTrainData()) %>%
  #     dynamicSteps(input$glmnet_h2o_Preprocess) %>%   # use <method>_Preprocess
  #     step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # 
  # 
  # observeEvent(
  #   input$glmnet_h2o_Go,
  #   {
  #     method <- "glmnet_h2o"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       
  #       tc <- getTrControl()
  #       tc$seeds <- NULL
  #       
  #       model <- caret::train(getglmnet_h2oRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = tc,tuneLength=input$glmnet_h2o_tuneLength0)
  #       deleteRds(method)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     }, 
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # observeEvent(
  #   input$glmnet_h2o_Load,
  #   {
  #     method  <- "glmnet_h2o"
  #     model <- loadRds(method, session)
  #     if (!is.null(model)) {
  #       models[[method]] <- model
  #     }
  #   }
  # )
  # 
  # observeEvent(
  #   input$glmnet_h2o_Delete,
  #   {
  #     models[["glmnet_h2o"]] <- NULL
  #     gc()
  #   }
  # )
  # 
  # 
  # 
  # output$glmnet_h2o_ModelSummary0 <- renderText({
  #   description("glmnet_h2o")   # Use the caret method name here
  # })
  # 
  # output$glmnet_h2o_Metrics <- renderTable({
  #   req(models$glmnet_h2o)
  #   models$glmnet_h2o$results[which.min(models$glmnet_h2o$results[, "RMSE"]), ]
  # })
  # 
  # output$glmnet_h2o_Recipe <- renderPrint({
  #   req(models$glmnet_h2o)
  #   models$glmnet_h2o$recipe
  # })  
  # 
  # output$glmnet_h2o_ModelPlots <- renderPlot({
  #   req(models$glmnet_h2o)
  #   plot(models$glmnet_h2o)
  # })
  # 
  # output$glmnet_h2o_ModelSummary2 <- renderPrint({
  #   req(models$glmnet_h2o)
  #   summary(models$glmnet_h2o$finalModel)
  # })
  # 
  # 
  # ##coeffs required for regression
  # output$glmnet_h2o_Coef <- renderTable({
  #   req(models$glmnet_h2o)
  #   co <- coef(models$glmnet_h2o$finalModel)
  #   as.data.frame(co, row.names = rownames(co))
  # }, rownames = TRUE, colnames = FALSE)
  # 
  
  library(pre)
  
  ##obtain pre recipe
  getpreRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pre_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$pre_Go,
    {
      method <- "pre"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        tc <- getTrControl()
        tc$seeds <- NULL
        model <- caret::train(getpreRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = tc,tuneLength=input$pre_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pre_Load,
    {
      method  <- "pre"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pre_Delete,
    {
      models[["pre"]] <- NULL
      gc()
    }
  )
  
  
  
  output$pre_ModelSummary0 <- renderText({
    description("pre")   # Use the caret method name here
  })
  
  output$pre_Metrics <- renderTable({
    req(models$pre)
    models$pre$results[which.min(models$pre$results[, "RMSE"]), ]
  })
  
  output$pre_Recipe <- renderPrint({
    req(models$pre)
    models$pre$recipe
  })  
  
  output$pre_ModelPlots <- renderPlot({
    req(models$pre)
    plot(models$pre)
  })
  
  output$pre_ModelSummary2 <- renderPrint({
    req(models$pre)
    summary(models$pre$finalModel)
  })
  
  
  ##coeffs required for regression
  output$pre_Coef <- renderTable({
    req(models$pre)
    co <- coef(models$pre$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  #library(ppr)
  
  ##obtain ppr recipe
  getpprRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ppr_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$ppr_Go,
    {
      method <- "ppr"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getpprRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),tuneLength=input$ppr_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$ppr_Load,
    {
      method  <- "ppr"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$ppr_Delete,
    {
      models[["ppr"]] <- NULL
      gc()
    }
  )
  
  
  
  output$ppr_ModelSummary0 <- renderText({
    description("ppr")   # Use the caret method name here
  })
  
  output$ppr_Metrics <- renderTable({
    req(models$ppr)
    models$ppr$results[which.min(models$ppr$results[, "RMSE"]), ]
  })
  
  output$ppr_Recipe <- renderPrint({
    req(models$ppr)
    models$ppr$recipe
  })  
  
  output$ppr_ModelPlots <- renderPlot({
    req(models$ppr)
    plot(models$ppr)
  })
  
  output$ppr_ModelSummary2 <- renderPrint({
    req(models$ppr)
    summary(models$ppr$finalModel)
  })
  
  
  ##coeffs required for regression
  # output$ppr_Coef <- renderTable({
  #   req(models$ppr)
  #   mod <- models$ppr
  #   co <- coef(models$ppr$finalModel)
  #   as.data.frame(co, row.names = rownames(co))
  # }, rownames = TRUE, colnames = FALSE)
  
  library(deepnet)
  
  ##obtain dnn recipe
  getdnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$dnn_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$dnn_Go,
    {
      method <- "dnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        tc <- getTrControl()
        tc$seeds <- NULL
        model <- caret::train(getdnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = tc)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$dnn_Load,
    {
      method  <- "dnn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$dnn_Delete,
    {
      models[["dnn"]] <- NULL
      gc()
    }
  )
  
  
  
  output$dnn_ModelSummary0 <- renderText({
    description("dnn")   # Use the caret method name here
  })
  
  output$dnn_Metrics <- renderTable({
    req(models$dnn)
    models$dnn$results[which.min(models$dnn$results[, "RMSE"]), ]
  })
  
  output$dnn_Recipe <- renderPrint({
    req(models$dnn)
    models$dnn$recipe
  })  
  
  output$dnn_ModelPlots <- renderPlot({
    req(models$dnn)
    plot(models$dnn)
  })
  
  output$dnn_ModelSummary2 <- renderPrint({
    req(models$dnn)
    summary(models$dnn$finalModel)
  })
  
  
  ##coeffs required for regression
  output$dnn_Coef <- renderTable({
    req(models$dnn)
    co <- coef(models$dnn$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  library(spls)
  
  ##obtain spls recipe
  getsplsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$spls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$spls_Go,
    {
      method <- "spls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        
        tc <- getTrControl()
        tc$seeds <- NULL
        model <- caret::train(getsplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = tc,tuneLength=input$spls_tuneLength0)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$spls_Load,
    {
      method  <- "spls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$spls_Delete,
    {
      models[["spls"]] <- NULL
      gc()
    }
  )
  
  
  
  output$spls_ModelSummary0 <- renderText({
    description("spls")   # Use the caret method name here
  })
  
  output$spls_Metrics <- renderTable({
    req(models$spls)
    models$spls$results[which.min(models$spls$results[, "RMSE"]), ]
  })
  
  output$spls_Recipe <- renderPrint({
    req(models$spls)
    models$spls$recipe
  })  
  
  output$spls_ModelPlots <- renderPlot({
    req(models$spls)
    plot(models$spls)
  })
  
  output$spls_ModelSummary2 <- renderPrint({
    req(models$spls)
    summary(models$spls$finalModel)
  })
  
  
  ##coeffs required for regression
  output$spls_Coef <- renderTable({
    req(models$spls)
    co <- coef(models$spls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  library(kernlab)
  
  ##obtain svmLinear recipe
  getsvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmLinear_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  
  
  observeEvent(
    input$svmLinear_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getsvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmLinear_Load,
    {
      method  <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmLinear_Delete,
    {
      models[["svmLinear"]] <- NULL
      gc()
    }
  )
  
  
  
  output$svmLinear_ModelSummary0 <- renderText({
    description("svmLinear")   # Use the caret method name here
  })
  
  output$svmLinear_Metrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  output$svmLinear_Recipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  # output$svmLinear_ModelPlots <- renderPlot({
  #   req(models$svmLinear)
  #   plot(models$svmLinear)
  # })
  
  output$svmLinear_ModelSummary2 <- renderPrint({
    req(models$svmLinear)
    summary(models$svmLinear$finalModel)
  })
  
  
  ##coeffs required for regression
  output$svmLinear_Coef <- renderTable({
    req(models$svmLinear)
    co <- coef(models$svmLinear$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  ###
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
  
})
