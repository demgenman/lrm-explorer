# Linear Regression Model Explorer

dslist <- c("attitude", "ChickWeight", "mtcars", "uploaded data")

library(shiny)

shinyUI(fluidPage(
    titlePanel("Linear Regression Model Explorer"),
    sidebarLayout(
        sidebarPanel(width=3,
            p("Use this app to quickly explore a data set, apply a linear regression model and obtain various summary statistics."),
            h4("Select data set"),
            p("Upload your own data or select a data set from the list."),
            fileInput("mydatasetfile", "a) Upload data set",
                      accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            selectInput("dataset", "b) Choose data set", dslist, multiple=FALSE, selected="attitude"),
            h4("Model parameters"),
            selectInput("responseVar", "Response variable", names(attitude), multiple=FALSE
                        #,selected=names(attitude)[1]
            ),
            checkboxGroupInput("predictors", "Predictor variables", names(attitude)
                               #,selected=names(attitude)[2]
            ),
            checkboxGroupInput("interactions", "Predictor interactions", names(attitude)
                               #,selected=names(attitude)[2]
            ),
            checkboxInput("intercept", label = "Model with intercept", value = TRUE)
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel(title = "Data set summary", value = "dssummary",
                         h4("Data set summary"),
                         fluidRow(
                             column(2,
                                    htmlOutput("dsmeta1"), p(""),
                                    radioButtons("dsplottype", label = h4("Plot type"),
                                                 choices = list("Histogram" = 1, "Box plot" = 2), 
                                                 selected = 1)
                             ),
                             column(10,
                                    plotOutput("dsplot"),
                                    tableOutput("dssummary")
                             )
                         )
                ),
                tabPanel(title = "Linear regression model", value = "lm",
                     h4("Linear regression model"),
                     textOutput("formula"),
                     fluidRow(
                         column(2,
                                radioButtons("plottype", label = h4("Plot type"),
                                             choices = list("Pairs plot" = 3, "Model diagnostics" = 4,
                                                            "Residuals" = 5, "Hat values" = 6), 
                                             selected = 3)
                         ),
                         column(10,
                                plotOutput("modelplot"))
                         ),
                     fluidRow(
                         column(5,
                                h4("Model summary"),
                                tableOutput("summary")
                         ),
                         column(4,
                                h4("Confidence interval"),
                                tableOutput("confint")
                         ),
                         column(3,
                                h4("Model statistics"),
                                tableOutput("stats")
                         )
                     )
                ),
                tabPanel(title = "About", value="about",
                         h4("About Linear Regression Model Explorer"),
                         tags$b("Data set summary"),
                         p("Use pre-loaded data sets (no upload needed):"),
                         tags$ul(
                             tags$li("attitude"),
                             tags$li("ChickWeight"),
                             tags$li("mtcars")
                         ),
                         p("Provide your own data set:"),
                         tags$ul(
                             tags$li("Upload 'well-formatted' csv file: no transcoding or format changes needed"),
                             tags$li("Data set must have a header row"),
                             tags$li("Variables that are numeric or can be converted to numeric"),
                             tags$li("After uploading select data set 'uploaded data'")
                         ),
                         p("Statistics"),
                         tags$ul(
                             tags$li("Data counts: Nr of rows (observations), columns (variables), NA values"),
                             tags$li("Variable summaries: min, max, median, mean, quantiles, NA"),
                             tags$li("Plots: Histogram of response variable, box plot of response and predictors")
                         ),
                         tags$b("Linear regression model"),
                         p("Formula: Response ~ Predictor(s) + Interaction(s) + Intercept"),
                         p("Create model:"),
                         tags$ul(
                             tags$li("Select response (dependent) variable, predictor (independent) variables, and any interactions."),
                             tags$li("Indicate if regression is to be performed with or without intercept.")
                         ),
                         p("LRME automatically:"),
                         tags$ul(
                             tags$li("Lists all variables found in the selected data set"),
                             tags$li("Hides the selected response variable from the predictor choices"),
                             tags$li("Generates choices for possible predictor interactions")
                         ),
                         p("Notes:"),
                         tags$ul(
                             tags$li("All variable choices are reset when you select a different data set."),
                             tags$li("The default response variable is the first variable found in the data set.")
                         )
                )
            )
        )
    )
))
