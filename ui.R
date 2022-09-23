shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Alexander Jefferies"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             verbatimTextOutput(outputId = "DataSummary2"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Browse Candidate Models",
             numericInput(inputId = "n_models",label="No. of Candidate Models",min=2,max=50,value=30),
             verbatimTextOutput(outputId = "candidates")
             
             
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           ##for the model add additional functionality 
                                           numericInput(inputId = "glmnet_tuneLength0",label="Tune Length",min = 2,max=50,value=30),
                                           
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "pls_tuneLength0",label="Tune Length",min = 2,max=50,value=15),
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           numericInput(inputId = "rpart_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  )
                                  
                                  
                                  
                                  
                                  ,
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         
                         
                         
                         
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         
                         tabPanel("cubist Model",
                                  verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "cubist_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(cubist_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = cubist_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           numericInput(inputId = "cubist_tuneLength0",label="Tune Length",min = 2,max=50,value=25),
                                           bsTooltip(id = "cubist_Preprocess", title = "These entries will be populÿated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "cubist_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "cubist_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "cubist_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "cubist_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "cubist_ModelPlots"),
                                  verbatimTextOutput(outputId = "cubist_Recipe"),
                                  verbatimTextOutput(outputId = "cubist_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "cubist_Coef")  #  <- typically this is specific to OLS
                                  )),
                         
                         
                         
                         tabPanel("brnn Model",
                                  verbatimTextOutput(outputId = "brnn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "brnn_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(brnn_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = brnn_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           numericInput(inputId = "brnn_tuneLength0",label="Tune Length",min = 2,max=50,value=25),
                                           bsTooltip(id = "brnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "brnn_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "brnn_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "brnn_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "brnn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "brnn_ModelPlots"),
                                  verbatimTextOutput(outputId = "brnn_Recipe"),
                                  verbatimTextOutput(outputId = "brnn_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "brnn_Coef")  #  <- typically this is specific to OLS
                                  )),
                         
                         # tabPanel("foba Model",
                         #    verbatimTextOutput(outputId = "foba_ModelSummary0"),
                         #    fluidRow(
                         #      column(width = 4,
                         #             # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #             selectizeInput(inputId = "foba_Preprocess",
                         #                            label = "Pre-processing",
                         #                            choices = unique(c(common_initial, ppchoices)),
                         #                            multiple = TRUE,
                         #                            selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                         #             
                         #             numericInput(inputId = "foba_tuneLength0",label="Tune Length",min = 2,max=50,value=25),
                         #             bsTooltip(id = "foba_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #      ),
                         #      column(width = 1,
                         #             actionButton(inputId = "foba_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #             bsTooltip(id = "foba_Go", title = "This will train or retrain your model (and save it)")
                         #      ),
                         #      column(width = 1,
                         #             actionButton(inputId = "foba_Load", label = "Load", icon = icon("file-upload")),
                         #             bsTooltip(id = "foba_Load", title = "This will reload your saved model")
                         #      ),
                         #      column(width = 1,
                         #             actionButton(inputId = "foba_Delete", label = "Forget", icon = icon("trash-alt")),
                         #             bsTooltip(id = "foba_Delete", title = "This will remove your model from memory")
                         #      )
                         #    ),
                         #    hr(),
                         #    h3("Resampled performance:"),
                         #    tableOutput(outputId = "foba_Metrics"),
                         #    hr(),
                         #    plotOutput(outputId = "foba_ModelPlots"),
                         #    verbatimTextOutput(outputId = "foba_Recipe"),
                         #    verbatimTextOutput(outputId = "foba_ModelSummary2"),
                         #    wellPanel(
                         #      h3("Coefficients"),
                         #      tableOutput(outputId = "foba_Coef")  #  <- typically this is specific to OLS
                         #              )
                         #  ),
                         # tabPanel("qrnn Model",
                         #          verbatimTextOutput(outputId = "qrnn_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #                   selectizeInput(inputId = "qrnn_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(common_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                         #                   
                         #                   #numericInput(inputId = "qrnn_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                         #                   bsTooltip(id = "qrnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "qrnn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "qrnn_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "qrnn_Load", label = "Load", icon = icon("file-upload")),
                         #                   bsTooltip(id = "qrnn_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "qrnn_Delete", label = "Forget", icon = icon("trash-alt")),
                         #                   bsTooltip(id = "qrnn_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "qrnn_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "qrnn_ModelPlots"),
                         #          verbatimTextOutput(outputId = "qrnn_Recipe"),
                         #          verbatimTextOutput(outputId = "qrnn_ModelSummary2"),
                         #          wellPanel(
                         #            h3("Coefficients"),
                         #            tableOutput(outputId = "qrnn_Coef")  #  <- typically this is specific to OLS
                         #          )
                         # ),
                         
                         # ##ANFIS MODEL
                         # tabPanel("ANFIS Model",
                         #          verbatimTextOutput(outputId = "ANFIS_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #                   selectizeInput(inputId = "ANFIS_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(common_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                         #                   
                         #                   #numericInput(inputId = "ANFIS_tuneLength0",label="Tune Length",min = 2,max=50,value=25),
                         #                   bsTooltip(id = "ANFIS_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "ANFIS_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "ANFIS_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "ANFIS_Load", label = "Load", icon = icon("file-upload")),
                         #                   bsTooltip(id = "ANFIS_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "ANFIS_Delete", label = "Forget", icon = icon("trash-alt")),
                         #                   bsTooltip(id = "ANFIS_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "ANFIS_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "ANFIS_ModelPlots"),
                         #          verbatimTextOutput(outputId = "ANFIS_Recipe"),
                         #          verbatimTextOutput(outputId = "ANFIS_ModelSummary2")
                         #          
                         # ),
                         
                         
                         tabPanel("bagEarth Model",
                                  verbatimTextOutput(outputId = "bagEarth_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "bagEarth_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bagEarth_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bagEarth_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "bagEarth_tuneLength0",label="Tune Length",min = 2,max=50,value=25),
                                           bsTooltip(id = "bagEarth_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarth_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "bagEarth_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarth_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "bagEarth_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bagEarth_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "bagEarth_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bagEarth_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "bagEarth_ModelPlots"),
                                  verbatimTextOutput(outputId = "bagEarth_Recipe"),
                                  verbatimTextOutput(outputId = "bagEarth_ModelSummary2")#,
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "bagEarth_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         tabPanel("krlsPoly Model",
                                  verbatimTextOutput(outputId = "krlsPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "krlsPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(common_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "krlsPoly_tuneLength0",label="Tune Length",min = 2,max=50,value=5),
                                           bsTooltip(id = "krlsPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "krlsPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsPoly_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "krlsPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsPoly_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "krlsPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "krlsPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "krlsPoly_ModelPlots"),
                                  plotOutput(outputId = "krlsPoly_ModelPlots2"),
                                  verbatimTextOutput(outputId = "krlsPoly_Recipe"),
                                  verbatimTextOutput(outputId = "krlsPoly_ModelSummary2")
                                  
                         ),
                         
                         tabPanel("pcaNNet Model",
                                  verbatimTextOutput(outputId = "pcaNNet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pcaNNet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(common_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "pcaNNet_tuneLength0",label="Tune Length",min = 2,max=50,value=5),
                                           bsTooltip(id = "pcaNNet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcaNNet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pcaNNet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcaNNet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pcaNNet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcaNNet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pcaNNet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pcaNNet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pcaNNet_ModelPlots"),
                                  verbatimTextOutput(outputId = "pcaNNet_Recipe"),
                                  #verbatimTextOutput(outputId = "pcaNNet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pcaNNet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         tabPanel("glmboost Model",
                                  verbatimTextOutput(outputId = "glmboost_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmboost_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmboost_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmboost_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "glmboost_tuneLength0",label="Tune Length",min = 2,max=50,value=6),
                                           bsTooltip(id = "glmboost_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmboost_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmboost_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmboost_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmboost_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmboost_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmboost_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmboost_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmboost_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmboost_Recipe"),
                                  verbatimTextOutput(outputId = "glmboost_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmboost_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("plsRglm Model",
                                  verbatimTextOutput(outputId = "plsRglm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "plsRglm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(plsRglm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = plsRglm_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "plsRglm_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                                           bsTooltip(id = "plsRglm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "plsRglm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "plsRglm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "plsRglm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "plsRglm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "plsRglm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "plsRglm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "plsRglm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "plsRglm_ModelPlots"),
                                  verbatimTextOutput(outputId = "plsRglm_Recipe"),
                                  verbatimTextOutput(outputId = "plsRglm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    verbatimTextOutput(outputId = "plsRglm_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         # tabPanel("rvmLinear Model",
                         #          verbatimTextOutput(outputId = "rvmLinear_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #                   selectizeInput(inputId = "rvmLinear_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(rvmLinear_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = rvmLinear_initial), # <-- These are suggested starting values. Set these to your best recommendation
                         #                   
                         #                   #numericInput(inputId = "rvmLinear_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                         #                   bsTooltip(id = "rvmLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rvmLinear_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "rvmLinear_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rvmLinear_Load", label = "Load", icon = icon("file-upload")),
                         #                   bsTooltip(id = "rvmLinear_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rvmLinear_Delete", label = "Forget", icon = icon("trash-alt")),
                         #                   bsTooltip(id = "rvmLinear_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "rvmLinear_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "rvmLinear_ModelPlots"),
                         #          verbatimTextOutput(outputId = "rvmLinear_Recipe"),
                         #          verbatimTextOutput(outputId = "rvmLinear_ModelSummary2"),
                         #          wellPanel(
                         #            h3("Coefficients"),
                         #            tableOutput(outputId = "rvmLinear_Coef")  #  <- typically this is specific to OLS
                         #          )
                         # ),
                         tabPanel("bam Model",
                                  verbatimTextOutput(outputId = "bam_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "bam_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bam_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bam_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           #numericInput(inputId = "bam_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                                           bsTooltip(id = "bam_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bam_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "bam_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bam_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "bam_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bam_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "bam_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bam_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "bam_ModelPlots"),
                                  verbatimTextOutput(outputId = "bam_Recipe"),
                                  verbatimTextOutput(outputId = "bam_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "bam_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("rlm Model",
                                  verbatimTextOutput(outputId = "rlm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rlm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(common_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "rlm_tuneLength0",label="Tune Length",min = 2,max=50,value=5),
                                           bsTooltip(id = "rlm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rlm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rlm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rlm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rlm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rlm_ModelPlots"),
                                  verbatimTextOutput(outputId = "rlm_Recipe"),
                                  verbatimTextOutput(outputId = "rlm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "rlm_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         # tabPanel("glmnet_h2o Model",
                         #          verbatimTextOutput(outputId = "glmnet_h2o_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                         #                   selectizeInput(inputId = "glmnet_h2o_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(common_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                         #                   
                         #                   numericInput(inputId = "glmnet_h2o_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                         #                   bsTooltip(id = "glmnet_h2o_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "glmnet_h2o_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "glmnet_h2o_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "glmnet_h2o_Load", label = "Load", icon = icon("file-upload")),
                         #                   bsTooltip(id = "glmnet_h2o_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "glmnet_h2o_Delete", label = "Forget", icon = icon("trash-alt")),
                         #                   bsTooltip(id = "glmnet_h2o_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "glmnet_h2o_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "glmnet_h2o_ModelPlots"),
                         #          verbatimTextOutput(outputId = "glmnet_h2o_Recipe"),
                         #          verbatimTextOutput(outputId = "glmnet_h2o_ModelSummary2"),
                         #          wellPanel(
                         #            h3("Coefficients"),
                         #            tableOutput(outputId = "glmnet_h2o_Coef")  #  <- typically this is specific to OLS
                         #          )
                         # ),
                         
                         tabPanel("pre Model",
                                  verbatimTextOutput(outputId = "pre_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pre_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(common_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "pre_tuneLength0",label="Tune Length",min = 2,max=50,value=25),
                                           bsTooltip(id = "pre_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pre_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pre_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pre_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pre_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pre_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pre_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pre_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pre_ModelPlots"),
                                  verbatimTextOutput(outputId = "pre_Recipe"),
                                  verbatimTextOutput(outputId = "pre_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pre_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         tabPanel("ppr Model",
                                  verbatimTextOutput(outputId = "ppr_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "ppr_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(common_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = common_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "ppr_tuneLength0",label="Tune Length",min = 2,max=50,value=7),
                                           bsTooltip(id = "ppr_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "ppr_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "ppr_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "ppr_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "ppr_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "ppr_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "ppr_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "ppr_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "ppr_ModelPlots"),
                                  verbatimTextOutput(outputId = "ppr_Recipe"),
                                  verbatimTextOutput(outputId = "ppr_ModelSummary2"),
                                  # wellPanel(
                                  #   h3("Coefficients"),
                                  #   tableOutput(outputId = "ppr_Coef")  #  <- typically this is specific to OLS
                                  # )
                         ),
                         
                         tabPanel("dnn Model",
                                  verbatimTextOutput(outputId = "dnn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "dnn_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(deepn_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = deepn_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           #numericInput(inputId = "dnn_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                                           bsTooltip(id = "dnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "dnn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "dnn_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "dnn_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "dnn_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "dnn_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "dnn_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "dnn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "dnn_ModelPlots"),
                                  verbatimTextOutput(outputId = "dnn_Recipe"),
                                  verbatimTextOutput(outputId = "dnn_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "dnn_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         tabPanel("spls Model",
                                  verbatimTextOutput(outputId = "spls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "spls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(spls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = spls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           numericInput(inputId = "spls_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                                           bsTooltip(id = "spls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "spls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "spls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "spls_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "spls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "spls_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "spls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "spls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "spls_ModelPlots"),
                                  verbatimTextOutput(outputId = "spls_Recipe"),
                                  verbatimTextOutput(outputId = "spls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "spls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("svmLinear Model",
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmLinear_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmLinear_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmLinear_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           
                                           #numericInput(inputId = "svmLinear_tuneLength0",label="Tune Length",min = 2,max=50,value=10),
                                           bsTooltip(id = "svmLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmLinear_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmLinear_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmLinear_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmLinear_Metrics"),
                                  hr(),
                                  #plotOutput(outputId = "svmLinear_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmLinear_Recipe"),
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "svmLinear_Coef")  #  <- typically this is specific to OLS
                                  )
                         )
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = modelChoices, inline = TRUE,selected = "brnn" )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals")
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1)
    )
  )
)
)

