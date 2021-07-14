library(RDS)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

function(input, output, session) {
  data(faux)
  data(fauxmadrona)
  data(fauxsycamore)
  
  trySummary <- function(){
    if (is.null(readfile$data)) {
      output$error <-  renderText("No file uploaded")
      return()
    }
    if (is.null(response$data)) {
      output$error <- renderText("No response variable selected")
      return()
    }
    
    df <- as.rds.data.frame(readfile$data)
    resp <- response$data
    output$rds1 <- DT::renderDataTable({RDS.I.estimates(rds.data=df, 
                                                        outcome.variable=resp)$interval})
    output$rds2 <- DT::renderDataTable({RDS.II.estimates(rds.data=df, 
                                                         outcome.variable=resp)$interval})
    output$ss <- DT::renderDataTable({RDS.SS.estimates(rds.data=df, 
                                                       outcome.variable=resp, N=N$data)$interval})
    
    output$plot1 <- renderPlot({convergence.plot(df, c(resp))})
    output$plot2 <- renderPlot({bottleneck.plot(df, c(resp))})
    
    seed_ids <- df[df$recruiter.id == "seed",]$id
    cols <- sapply(df[[resp]], function(x) ifelse(x==unique(df[[resp]])[1], "blue", "red"))
    df2 <- data.frame(recruit=seq(1,nrow(df)), seed=match(df$seed, seed_ids) - 0.25 + .5 * 
                       (match(df[[resp]], unique(df[[resp]])) - 1))
    output$legend <- renderText({paste("Red: ",resp," = ",unique(df[[resp]])[2],", Blue: ",resp," = ",unique(df[[resp]])[1])})
    output$plot3 <- renderPlot({
      ggplot(df2, aes(x=recruit, y=seed)) +
        geom_point(color=cols, shape="|", size=5) +
        scale_y_continuous(breaks = seq(1,length(seed_ids)))
    })
    
    output$error <- NULL
  }
  
  infile <- reactiveValues(data=NULL)
  readfile <- reactiveValues(data=NULL)
  output$fileread <- renderTable({
    infile$data <- input$file1
    if (is.null(infile$data)) return()
    readfile$data <- read.csv(infile$data$datapath, header=input$header)
    output$fileread <- DT::renderDataTable({
      head(readfile$data)
    })
    if (input$dataset == "cust") trySummary()
  })
  
  response <- reactiveValues(data=NULL)
  observeEvent(input$submit, {
    response$data <- input$resp
    output$text1 <- renderText(paste("Chosen response variable: ", response$data))
    if (input$dataset == "cust") trySummary()
  })
  
  N <- reactiveValues(data=NULL)
  observeEvent(input$submit2, {
    N$data <- input$estN
    output$text2 <- renderText(paste("Chosen population size estimate: ", N$data))
    if (input$dataset == "cust") trySummary()
  }) 
  
  observeEvent(input$dataset, {
    f <- switch(
      input$dataset,
      "f" = function() {
        output$error <- NULL
        output$rds1 <- DT::renderDataTable({RDS.I.estimates(rds.data=faux, outcome.variable="X")$interval})
        output$rds2 <- DT::renderDataTable({RDS.II.estimates(rds.data=faux, outcome.variable="X")$interval})
        output$ss <- DT::renderDataTable({RDS.SS.estimates(rds.data=faux, outcome.variable="X")$interval})
        output$plot1 <- renderPlot({convergence.plot(faux, c("X"))})
        output$plot2 <- renderPlot({bottleneck.plot(faux, c("X"))})
        
        seed_ids <- faux[faux$recruiter.id == "seed",]$id
        cols <- sapply(faux$X, function(x) ifelse(x=="red", "red", "blue"))
        df <- data.frame(recruit=seq(1,nrow(faux)), seed=match(faux$seed, seed_ids) - 0.25 + .5 * 
                           (match(faux$X, c("blue","red")) - 1))
        output$legend <- renderText("Red: X = red, Blue: X = blue")
        output$plot3 <- renderPlot({ 
          ggplot(df, aes(x=recruit, y=seed)) + 
          geom_point(color=cols, shape="|", size=5) + 
          scale_y_continuous(breaks = seq(1,length(seed_ids)))
        })
        
      },
      "fm" = function() {
        output$error <- NULL
        output$rds1 <- DT::renderDataTable({RDS.I.estimates(rds.data=fauxmadrona, 
                                                            outcome.variable="disease")$interval})
        output$rds2 <- DT::renderDataTable({RDS.II.estimates(rds.data=fauxmadrona, 
                                                             outcome.variable="disease")$interval})
        output$ss <- DT::renderDataTable({RDS.SS.estimates(rds.data=fauxmadrona, 
                                                           outcome.variable="disease")$interval})
        output$plot1 <- renderPlot({convergence.plot(fauxmadrona, c("disease"))})
        output$plot2 <- renderPlot({bottleneck.plot(fauxmadrona, c("disease"))})
        
        seed_ids <- fauxmadrona[fauxmadrona$recruiter.id == "seed",]$id
        cols <- sapply(fauxmadrona$disease, function(x) ifelse(x==1, "red", "blue"))
        df <- data.frame(recruit=seq(1,nrow(fauxmadrona)), seed=match(fauxmadrona$seed, seed_ids) - 0.25 + .5 * fauxmadrona$disease)
        output$legend <- renderText("Red: Disease = 1, Blue: Disease = 0")
        output$plot3 <- renderPlot({
          ggplot(df, aes(x=recruit, y=seed)) + 
          geom_point(color=cols, shape="|", size=5) + 
          scale_y_continuous(breaks = seq(1,length(seed_ids)))
        })
        
      },
      "fc" = function() {
        output$error <- NULL
        output$rds1 <- DT::renderDataTable({RDS.I.estimates(rds.data=fauxsycamore, 
                                                            outcome.variable="disease")$interval})
        output$rds2 <- DT::renderDataTable({RDS.II.estimates(rds.data=fauxsycamore, 
                                                             outcome.variable="disease")$interval})
        output$ss <- DT::renderDataTable({RDS.SS.estimates(rds.data=fauxsycamore, 
                                                           outcome.variable="disease")$interval})
        output$plot1 <- renderPlot({convergence.plot(fauxsycamore, c("disease"))})
        output$plot2 <- renderPlot({bottleneck.plot(fauxsycamore, c("disease"))})
        
        seed_ids <- fauxsycamore[fauxsycamore$recruiter.id == "seed",]$id
        cols <- sapply(fauxsycamore$disease, function(x) ifelse(x==1, "red", "blue"))
        df <- data.frame(recruit=seq(1,nrow(fauxsycamore)), seed=match(fauxsycamore$seed, seed_ids) - 0.25 + .5 * fauxsycamore$disease)
        output$legend <- renderText("Red: Disease = 1, Blue: Disease = 0")
        output$plot3 <- renderPlot({
          ggplot(df, aes(x=recruit, y=seed)) + 
            geom_point(color=cols, shape="|", size=5) + 
            scale_y_continuous(breaks = seq(1,length(seed_ids)))
        })
        
      },
      "cust" = function() {
        output$rds1 <- NULL
        output$rds2 <- NULL
        output$ss <- NULL
        output$plot1 <- NULL
        output$plot2 <- NULL
        output$plot3 <- NULL
        
        trySummary()
      }
    )
    f()
  })
}