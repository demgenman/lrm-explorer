# Linear Regression Model Explorer

library(shiny)

shinyServer(function(input, output, session) {

    mydata <- reactive({
        
        # input$mydatasetfile will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        
        inFile <- input$mydatasetfile
        message("inFile: ", inFile)
        if (is.null(inFile)) return(NULL)
        mydata <- read.csv(inFile$datapath, header=TRUE, sep=",", quote='"', nrows=-1)
    })        
    
    ds <- reactive({
        message("Selected dataset: ", input$dataset)
        validate(need(input$dataset != "", "Please select a data set."))
        if (input$dataset == "uploaded data") {
            validate(need(nrow(mydata()) > 0, "Data set not available. Please upload your data set first or select a different data set from the list."))
            ds <- mydata()
        } else {
            ds <- get(input$dataset)
        }
        if (!"data.frame" %in% class(ds)) return(NULL)
        ds
    })
    
    # Basic data set meta info
    output$dsmeta1 <- renderText({
        ds <- ds()
        s1 <- sprintf("Observations: %d", dim(ds)[1])
        s2 <- sprintf("Variables: %d", dim(ds)[2])
        s3 <- sprintf("Total NA values: %d\n", sum(is.na(ds)))
        HTML(paste(s1, s2, s3, sep="<br/>"))
    })
    
    # NA values per variable
    output$dsmeta2 <- renderTable({
        ds <- ds()
        data.frame(NA.values=as.integer(colSums(is.na(ds))))
    })
    
    # Data set summary statistics
    output$dssummary <- renderTable({
        ds <- ds()
        summary(ds)
    })
    
    # Fitted linear regression model
    fit.lm <- reactive({
        modelVars <- c(input$predictors, input$interactions)
        validate(need(!is.null(ds), "Data set not available. Please select a different data set."))
        validate(need(length(modelVars) > 0, "Please select predictors"))        
        ds <- ds()
        
        # Check if all response and predictor variables are in the model. When switching between data sets
        # it may happen that the model does not yet reflect the names from the UI (transient situation). 
        if (!input$responseVar %in% names(ds)) return(NULL)
        
        message("fit.lm: ", paste(input$responseVar, "~", modelVars))
        if (input$intercept) intercept = "" else intercept="-1"
        if (length(modelVars) > 0) {
            lm(as.formula(paste(input$responseVar, "~", paste(modelVars, collapse="+"), intercept)),
               data = ds)            
        } else return(NULL)
    })

    # Linear regression model formula
    output$formula <- renderText({
        modelVars <- c(input$predictors, input$interactions)
        validate(
            need(length(modelVars) > 0, "Please select predictors")
        ) 
        f <- as.character(formula(fit.lm()))
        if (paste(f, collapse="") == "")  return("Not specified.")
        paste("Formula: ", f[2], f[1], f[3])
    })

    # Watch UI selections and dynamically update selection choices
    observe({
        message("observe:")
        
        # Exclude response variable from choice of predictors
        # Generate interaction combinations
        ds <- ds()
        choicesResponse <- names(ds)
        choicesPredictors <- setdiff(names(ds), input$responseVar)
        interactions <- t(combn(choicesPredictors, 2))
        choicesInteractions <- paste(interactions[,1], interactions[,2], sep=":")
        sel <- input$responseVar
        if (!input$responseVar %in% choicesResponse) sel <- choicesResponse[1]
        updateSelectInput(session, "responseVar", choices=choicesResponse
                          ,selected=sel
                          )
        updateCheckboxGroupInput(session, "predictors", choices=choicesPredictors)
        updateCheckboxGroupInput(session, "interactions", choices=choicesInteractions)
    })

    # Data set exploratory plots
    output$dsplot <- renderPlot({
        ds <- ds()
        if (!input$responseVar %in% names(ds)) return(NULL)
        switch(input$dsplottype,
               "1" = {   
                   x <- ds[, input$responseVar]
                   # histogram of numeric variables, table of non-numeric variables
                   if (!is.numeric(x)) x <- as.integer(ds[, input$responseVar])
                   hist(x, xlab=input$responseVar, main=paste("Histogram of", input$responseVar))
               },
               "2" = {
                   boxplotVars <- c(input$responseVar, input$predictors)
                   # boxplot numeric variables only, convert non-numeric to integer
                   x <- as.data.frame(ds[, boxplotVars])
                   xnum <- sapply(x, function(y) is.numeric(y))
                   x1 <- data.frame(x[,xnum, drop=FALSE])
                   x2 <- sapply(x[,!xnum, drop=FALSE], as.numeric)
                   if (sum(xnum) > 0)
                       if (sum(!xnum) > 0) x <- cbind(x1, x2) else x <- x1
                   else
                       x <- x2
                   boxplot(x, plot=TRUE)
                   if (length(input$predictors) == 0) axis(side = 1, at = 1, labels = input$responseVar)
               }
        )
    })  
    
    # Linear regression model diagnostic plots
    output$modelplot <- renderPlot({
        modelVars <- c(input$predictors, input$interactions)
        validate(
            need(length(modelVars) > 0, "\nPlease select predictors")
        )      

        # Here we use some third party code to extend the functionality of the pairs plot
        # panel extensions for pairs function
        # source: R Documentation, pairs {graphics}
        panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
        {
            usr <- par("usr"); on.exit(par(usr))
            par(usr = c(0, 1, 0, 1))
            r <- abs(cor(x, y))
            txt <- format(c(r, 0.123456789), digits = digits)[1]
            txt <- paste0(prefix, txt)
            if(missing(cex.cor)) cex.cor <- 0.6/strwidth(txt)
            text(0.5, 0.5, txt, cex = cex.cor * r)
        }
        panel.hist <- function(x, ...)
        {
            usr <- par("usr"); on.exit(par(usr))
            par(usr = c(usr[1:2], 0, 1.5) )
            h <- hist(x, plot = FALSE)
            breaks <- h$breaks; nB <- length(breaks)
            y <- h$counts; y <- y/max(y)
            rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
        }
        # source: http://stackoverflow.com/questions/15271103/how-to-modify-this-correlation-matrix-plot
        # Didzis Elferts
        panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                                cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
        {
            points(x, y, pch = pch, col = col, bg = bg, cex = cex)
            ok <- is.finite(x) & is.finite(y)
            if (any(ok)) 
                lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
                      col = col.smooth, ...)
        }
        # End of third party code
        
        ds <- ds()
        modelVars <- c(input$responseVar, input$predictors, 
                       unlist(strsplit(paste0("", input$interactions), split=":")))
        modelVars <- modelVars[!duplicated(modelVars)]
        switch(input$plottype,
               "3" = pairs(ds[, modelVars], main = "Pairs plot",
                           lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist),
               "4" = { 
                   validate(need(is.numeric(ds[,input$responseVar]), 
                                 "\nFor proper linear regression select a numeric variable as response."))
                   par(mfrow=c(2,2)); plot(fit.lm(), which=c(1:3,5), cex=1.2); par(mfrow=c(1,1)) 
               },
               "5" = plot(predict(fit.lm()), residuals (fit.lm()), 
                          xlab="Fitted values", ylab="Residuals", main="Residuals"),
               "6" = {
                   validate(need(is.numeric(ds[, input$responseVar]), 
                                 "\nFor proper linear regression select a numeric variable as response."))
                   plot(hatvalues(fit.lm()), ylab = "Hat values", main="Hat values", 
                        xlab="Observation nr", cex=1.2)
               }
        )
    })
    
    # Linear regression model summary
    output$summary <- renderTable({
        summary(fit.lm())
    })
    
    # Linear regression model confidence interval for coefficients
    output$confint <- renderTable({
        confint(fit.lm())
    })
    
    # Linear regression model statistics
    output$stats <- renderTable({
        data.frame(Statistic=c("Residual Std Err", "R squared", "Adj. R squared", "F-statistic", 
                               "Numerator DF", " Denominator DF"), 
                   Value=c(summary(fit.lm())$sigma, summary(fit.lm())$r.squared, 
                           summary(fit.lm())$adj.r.squared, summary(fit.lm())$fstatistic))
    })
})
