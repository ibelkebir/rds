library(RDS)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(reshape2)

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
    
    waves <- split(df, df$wave)
    waves <- waves[1:length(waves)-1]
    avgs1 <- rep(0,length(waves))
    avgs2 <- rep(0,length(waves))
    x = 1
    for (wave in waves) {
      dfs <- split(wave, wave[[resp]])
      for (df3 in dfs) {
        avg = nrow(df[df$recruiter.id %in% df3$id,]) / nrow(df3)
        if (df3[[resp]][1] == unique(df[[resp]])[1]) {
          avgs1[x] = avg
        }else{
          avgs2[x] = avg
        }
      }
      x = x+1
    }
    
    df3 <- data.frame(x=seq(1,length(waves)))
    output$plot4 <- renderPlot({
      ggplot(data=df3, aes(x=x)) +
        geom_line(aes(y=avgs1, color=toString(unique(df[[resp]])[1]))) +
        geom_line(aes(y=avgs2, color=toString(unique(df[[resp]])[2]))) +
        scale_colour_manual("",
                            breaks = c(toString(unique(df[[resp]])[1]),toString(unique(df[[resp]])[2])),
                            values = c("red", "blue")) +
        xlab("Wave") +
        ylab("Average number of recruits") +
        scale_x_continuous(breaks = seq(1,length(waves)))
    })
    
    recruits <- rep(0,nrow(df[df$wave != max(df$wave),]))
    responses <- df[df$wave != max(df$wave),][[resp]]
    i = 0
    for (id in df[df$wave != max(df$wave),]$id) {
      recruits[i] = nrow(df[df$recruiter.id == id,])
      i = i+1
    }
    df4 <- data.frame(recs=recruits, response=sapply(responses,toString))
    output$plot5 <- renderPlot ({
      ggplot(df4, aes(x=recs, fill=response)) +
        geom_histogram(position="dodge", binwidth=1) +
        xlab("# of recruits")
    })
    
    waves <- split(df, df$wave)
    c <- sapply(waves,nrow)
    df5 <- data.frame(x=seq(0,length(waves)-1), y=c)
    output$plot6 <- renderPlot({
      ggplot(df5, aes(x=x,y=y)) +
        geom_line(aes(color="red")) +
        scale_x_continuous(breaks = seq(0,length(waves)-1)) +
        xlab("wave") +
        ylab("# of recruits") +
        theme(legend.position="none")
    })
    
    M <- matrix(, nrow(df[df$recruiter.id == "seed",]), length(split(df, df$wave)))
    i = 1
    for (seed in split(df,df$seed)) {
      j = 1
      for (wave in split(seed, seed$wave)) {
        M[i,j] = nrow(wave)
        j = j + 1
      }
      i = i + 1
    }
    M[is.na(M)] <- 0
    M <- data.frame(t(M))
    colnames(M) <- seq(1, nrow(df[df$recruiter.id == "seed",]))
    M$wave <- seq(0,length(split(df, df$wave))-1)
    
    output$plot7 <- renderPlot({
      ggplot(melt(M, id.vars="wave"), aes(x=wave, y=value, color=variable)) +
        geom_line() +
        xlab("wave") +
        ylab("# of recruits") +
        guides(color=guide_legend("seed"))
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
        
        waves <- split(faux, faux$wave)
        waves <- waves[1:length(waves)-1]
        avgs1 <- rep(0,length(waves))
        avgs2 <- rep(0,length(waves))
        x = 1
        for (wave in waves) {
          y = 0
          dfs <- split(wave, wave$X)
          for (df2 in dfs) {
            avg = nrow(faux[faux$recruiter.id %in% df2$id,]) / nrow(df2)
            if (y %% 2 == 0) {
              avgs1[x] = avg
            }else{
              avgs2[x] = avg
            }
            y = y+1
          }
          x = x+1
        }
        df2 <- data.frame(x=seq(1,length(waves)))
        output$plot4 <- renderPlot({
          ggplot(data=df2, aes(x=x)) +
            geom_line(aes(y=avgs1, color="blue")) +
            geom_line(aes(y=avgs2, color="red")) +
            scale_colour_manual("",
                                breaks = c("red","blue"),
                                values = c("red", "blue")) +
            xlab("Wave") +
            ylab("Average number of recruits") +
            scale_x_continuous(breaks = seq(1,length(waves)))
        })
        
        recruits <- rep(0,nrow(faux[faux$wave != max(faux$wave),]))
        responses <- faux[faux$wave != max(faux$wave),]$X
        i = 0
        for (id in faux[faux$wave != max(faux$wave),]$id) {
          recruits[i] = nrow(faux[faux$recruiter.id == id,])
          i = i+1
        }
        df3 <- data.frame(recs=recruits, response=responses)
        output$plot5 <- renderPlot ({
          ggplot(df3, aes(x=recs, fill=response)) +
            geom_histogram(position="dodge", binwidth=1) +
            xlab("# of recruits")
        })
        
        waves <- split(faux, faux$wave)
        c <- sapply(waves,nrow)
        df4 <- data.frame(x=seq(0,length(waves)-1), y=c)
        output$plot6 <- renderPlot({
          ggplot(df4, aes(x=x,y=y)) +
            geom_line(aes(color="red")) +
            scale_x_continuous(breaks = seq(0,length(waves)-1)) +
            xlab("wave") +
            ylab("# of recruits") +
            theme(legend.position="none")
        })
        
        output$plot7 <- NULL
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
        
        waves <- split(fauxmadrona, fauxmadrona$wave)
        waves <- waves[1:length(waves)-1]
        avgs1 <- rep(0,length(waves))
        avgs2 <- rep(0,length(waves))
        x = 1
        for (wave in waves) {
          y = 0
          dfs <- split(wave, wave$disease)
          for (df2 in dfs) {
            avg = nrow(fauxmadrona[fauxmadrona$recruiter.id %in% df2$id,]) / nrow(df2)
            if (y %% 2 == 0) {
              avgs1[x] = avg
            }else{
              avgs2[x] = avg
            }
            y = y+1
          }
          x = x+1
        }
        df2 <- data.frame(x=seq(1,length(waves)))
        output$plot4 <- renderPlot({
          ggplot(data=df2, aes(x=x)) +
          geom_line(aes(y=avgs1, color="positive")) +
          geom_line(aes(y=avgs2, color="negative")) +
          scale_colour_manual("",
                              breaks = c("positive","negative"),
                              values = c("red", "blue")) +
          xlab("Wave") +
          ylab("Average number of recruits") +
          scale_x_continuous(breaks = seq(1,length(waves)))
        })
        
        recruits <- rep(0,nrow(fauxmadrona[fauxmadrona$wave != max(fauxmadrona$wave),]))
        responses <- fauxmadrona[fauxmadrona$wave != max(fauxmadrona$wave),]$disease
        i = 0
        for (id in fauxmadrona[fauxmadrona$wave != max(fauxmadrona$wave),]$id) {
          recruits[i] = nrow(fauxmadrona[fauxmadrona$recruiter.id == id,])
          i = i+1
        }
        df3 <- data.frame(recs=recruits, response=sapply(responses,toString))
        output$plot5 <- renderPlot ({
          ggplot(df3, aes(x=recs, fill=response)) +
            geom_histogram(position="dodge", binwidth=1) +
            xlab("# of recruits")
        })
        
        waves <- split(fauxmadrona, fauxmadrona$wave)
        c <- sapply(waves,nrow)
        df4 <- data.frame(x=seq(0,length(waves)-1), y=c)
        output$plot6 <- renderPlot({
          ggplot(df4, aes(x=x,y=y)) +
            geom_line(aes(color="red")) +
            scale_x_continuous(breaks = seq(0,length(waves)-1)) +
            xlab("wave") +
            ylab("# of recruits") +
            theme(legend.position="none")
        })
        
        M <- matrix(, nrow(fauxmadrona[fauxmadrona$recruiter.id == "seed",]), length(split(fauxmadrona, fauxmadrona$wave)))
        i = 1
        for (seed in split(fauxmadrona,fauxmadrona$seed)) {
          j = 1
          for (wave in split(seed, seed$wave)) {
            M[i,j] = nrow(wave)
            j = j + 1
          }
          i = i + 1
        }
        M[is.na(M)] <- 0
        M <- data.frame(t(M))
        colnames(M) <- seq(1, nrow(fauxmadrona[fauxmadrona$recruiter.id == "seed",]))
        M$wave <- seq(0,length(split(fauxmadrona, fauxmadrona$wave))-1)
        
        output$plot7 <- renderPlot({
          ggplot(melt(M, id.vars="wave"), aes(x=wave, y=value, color=variable)) +
            geom_line() +
            xlab("wave") +
            ylab("# of recruits") +
            guides(color=guide_legend("seed"))
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
        
        waves <- split(fauxsycamore, fauxsycamore$wave)
        waves <- waves[1:length(waves)-1]
        avgs1 <- rep(0,length(waves))
        avgs2 <- rep(0,length(waves))
        x = 1
        for (wave in waves) {
          y = 0
          dfs <- split(wave, wave$disease)
          for (df2 in dfs) {
            avg = nrow(fauxsycamore[fauxsycamore$recruiter.id %in% df2$id,]) / nrow(df2)
            if (y %% 2 == 0) {
              avgs1[x] = avg
            }else{
              avgs2[x] = avg
            }
            y = y+1
          }
          x = x+1
        }
        df2 <- data.frame(x=seq(1,length(waves)))
        output$plot4 <- renderPlot({
          ggplot(data=df2, aes(x=x)) +
            geom_line(aes(y=avgs1, color="positive")) +
            geom_line(aes(y=avgs2, color="negative")) +
            scale_colour_manual("",
                                breaks = c("positive","negative"),
                                values = c("red", "blue")) +
            xlab("Wave") +
            ylab("Average number of recruits") +
            scale_x_continuous(breaks = seq(1,length(waves)))
        })
        
        recruits <- rep(0,nrow(fauxsycamore[fauxsycamore$wave != max(fauxsycamore$wave),]))
        responses <- fauxsycamore[fauxsycamore$wave != max(fauxsycamore$wave),]$disease
        i = 0
        for (id in fauxsycamore[fauxsycamore$wave != max(fauxsycamore$wave),]$id) {
          recruits[i] = nrow(fauxsycamore[fauxsycamore$recruiter.id == id,])
          i = i+1
        }
        df3 <- data.frame(recs=recruits, response=sapply(responses,toString))
        output$plot5 <- renderPlot ({
          ggplot(df3, aes(x=recs, fill=response)) +
            geom_histogram(position="dodge", binwidth=1) +
            xlab("# of recruits")
        })
        
        waves <- split(fauxsycamore, fauxsycamore$wave)
        c <- sapply(waves,nrow)
        df4 <- data.frame(x=seq(0,length(waves)-1), y=c)
        output$plot6 <- renderPlot({
          ggplot(df4, aes(x=x,y=y)) +
            geom_line(aes(color="red")) +
            scale_x_continuous(breaks = seq(0,length(waves)-1)) +
            xlab("wave") +
            ylab("# of recruits") +
            theme(legend.position="none")
        })
        
        M <- matrix(, nrow(fauxsycamore[fauxsycamore$recruiter.id == "seed",]), length(split(fauxsycamore, fauxsycamore$wave)))
        i = 1
        for (seed in split(fauxsycamore,fauxsycamore$seed)) {
          j = 1
          for (wave in split(seed, seed$wave)) {
            M[i,j] = nrow(wave)
            j = j + 1
          }
          i = i + 1
        }
        M[is.na(M)] <- 0
        M <- data.frame(t(M))
        colnames(M) <- seq(1, nrow(fauxsycamore[fauxsycamore$recruiter.id == "seed",]))
        M$wave <- seq(0,length(split(fauxsycamore, fauxsycamore$wave))-1)
        
        output$plot7 <- renderPlot({
          ggplot(melt(M, id.vars="wave"), aes(x=wave, y=value, color=variable)) +
            geom_line() +
            xlab("wave") +
            ylab("# of recruits") +
            guides(color=guide_legend("seed"))
        })
      },
      "cust" = function() {
        output$rds1 <- NULL
        output$rds2 <- NULL
        output$ss <- NULL
        output$plot1 <- NULL
        output$plot2 <- NULL
        output$plot3 <- NULL
        output$plot4 <- NULL
        
        trySummary()
      }
    )
    f()
  })
}