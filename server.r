library(RDS)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(reshape2)
library(visNetwork)
library(RColorBrewer)

function(input, output, session) {
  data(faux)
  data(fauxmadrona)
  data(fauxsycamore)
  
  trySummary <- function(){
    
    resp <- response$data
    
    task1 <- tryCatch({
      df <- as.rds.data.frame(readfile$data)
      output$rds1 <- DT::renderDataTable({RDS.I.estimates(rds.data=as.rds.data.frame(df),
                                                          outcome.variable=resp)$interval})
      output$rds2 <- DT::renderDataTable({RDS.II.estimates(rds.data=as.rds.data.frame(df),
                                                           outcome.variable=resp)$interval})
      output$ss <- DT::renderDataTable({RDS.SS.estimates(rds.data=as.rds.data.frame(df),
                                                         outcome.variable=resp, N=N$data)$interval})

      output$plot1 <- renderPlot({convergence.plot(as.rds.data.frame(df), c(resp))})
      output$plot2 <- renderPlot({bottleneck.plot(as.rds.data.frame(df), c(resp))})
    }, warning = function(war) {
    }, error = function(err) {
      if (is.null(readfile$data)) {
        output$error <- renderText("No dataset uploaded")
        output$error8 <- renderText("No dataset uploaded")
      } else {
        output$error <- renderPrint(err)
      }
    }, finally = {})
    
    task2 <- tryCatch({
      df <- readfile$data
      seed_ids <- df[df$recruiter.id == "seed",]$id
      if (input$ordering == "t") {
        df$date <- lubridate::dmy(df$date)
        df <- dplyr::arrange(df, date)
      }
      cols <- sapply(df[[resp]], function(x) ifelse(x==unique(df[[resp]])[1], "blue", "red"))
      df2 <- data.frame(recruit=seq(1,nrow(df)), seed=match(df$seed, seed_ids) - 0.25 + .5 * 
                          (match(df[[resp]], unique(df[[resp]])) - 1))
      output$legend <- renderText({paste("Red: ",resp," = ",unique(df[[resp]])[2],", Blue: ",resp," = ",unique(df[[resp]])[1])})
      output$plot3 <- renderPlot({
        ggplot(df2, aes(x=recruit, y=seed)) +
          geom_point(color=cols, shape="|", size=5 + 10/length(seed_ids)) +
          scale_y_continuous(breaks = seq(1,length(seed_ids))) +
          theme(axis.title=element_text(size=16))
      })
    }, warning = function(war) {
    }, error = function(err) {
    }, finally = {})
    
    task3 <- tryCatch({
      df <- readfile$data
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
    }, warning = function(war) {
    }, error = function(err) {
    }, finally = {})
    
    task4 <- tryCatch({
      df <- readfile$data
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
    }, warning = function(war) {
    }, error = function(err) {
    }, finally = {})
    
    task5 <- tryCatch({
      df <- readfile$data
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
    }, warning = function(war) {
    }, error = function(err) {
    }, finally = {})
    
    task6 <- tryCatch({
      df <- readfile$data
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
    }, warning = function(war) {
    }, error = function(err) {
    }, finally = {})
    
    task7 <- tryCatch({
      df <- readfile$data
      if (is.null(lab_var$data)) {
        nodes <- data.frame(id=df$id, label=df$id, level=df$wave)
        edges <- data.frame(from=df[df$recruiter.id != "seed",]$recruiter.id, 
                            to=df[df$recruiter.id != "seed",]$id)
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100)
        })
      } else {
        pal <- brewer.pal(length(unique(df[[trait]])), "Set1")
        group <- match(df[[trait]], unique(df[[trait]]))
        nodes <- data.frame(id=df$id, label=df$id, level=df$wave, color=pal[group])
        edges <- data.frame(from=df[df$recruiter.id != "seed",]$recruiter.id, 
                            to=df[df$recruiter.id != "seed",]$id)
        lnodes <- data.frame(label=unique(df[[trait]]), color=pal[1:length(unique(df[[trait]]))])
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main=trait,addNodes=lnodes, useGroups = FALSE)
        })
      }
    }, warning = function(war) {
    }, error = function(err) {
      output$error7 <- renderPrint(err)
    }, finally = {})
  }
  
  infile <- reactiveValues(data=NULL)
  readfile <- reactiveValues(data=NULL)
  observeEvent(input$file1, {
    infile$data <- input$file1
    if (is.null(infile$data)) return()
    readfile$data <- read.csv(infile$data$datapath, header=input$header)
    output$fileread <- DT::renderDataTable({
      readfile$data
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
  
  observeEvent(input$ordering, {
    if (input$dataset == "cust") {
      resp <- response$data
      task <- tryCatch({
        df <- readfile$data
        seed_ids <- df[df$recruiter.id == "seed",]$id
        if (input$ordering == "t") {
          df$date <- lubridate::dmy(df$date)
          df <- dplyr::arrange(df, date)
        }
        cols <- sapply(df[[resp]], function(x) ifelse(x==unique(df[[resp]])[1], "blue", "red"))
        df2 <- data.frame(recruit=seq(1,nrow(df)), seed=match(df$seed, seed_ids) - 0.25 + .5 * 
                            (match(df[[resp]], unique(df[[resp]])) - 1))
        output$legend <- renderText({paste("Red: ",resp," = ",unique(df[[resp]])[2],", Blue: ",resp," = ",unique(df[[resp]])[1])})
        output$plot3 <- renderPlot({
          ggplot(df2, aes(x=recruit, y=seed)) +
            geom_point(color=cols, shape="|", size=5 + 10/length(seed_ids)) +
            scale_y_continuous(breaks = seq(1,length(seed_ids))) +
            theme(axis.title=element_text(size=16))
        })
      }, warning = function(war) {
      }, error = function(err) {
      }, finally = {})
    }
  })
  
  lab_var <- reactiveValues(data=NULL)
  observeEvent(input$submit3, {
    lab_var$data = input$trait
    output$text3 <- renderText(paste("Chosen trait: ", lab_var$data))
    if(input$dataset == "cust") {
      trait <- lab_var$data
      task <- tryCatch({
        df <- readfile$data
        pal <- brewer.pal(length(unique(df[[trait]])), "Set1")
        group <- match(df[[trait]], unique(df[[trait]]))
        nodes <- data.frame(id=df$id, label=df$id, level=df$wave, color=pal[group])
        edges <- data.frame(from=df[df$recruiter.id != "seed",]$recruiter.id, 
                            to=df[df$recruiter.id != "seed",]$id)
        lnodes <- data.frame(label=unique(df[[trait]]), color=pal[1:length(unique(df[[trait]]))])
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main=trait,addNodes=lnodes, useGroups = FALSE)
        })
      }, warning = function(war) {
      }, error = function(err) {
        output$error7 <- renderPrint(err)
      }, finally = {})
    }
  })
  
  observeEvent(input$dataset, {
    f <- switch(
      input$dataset,
      "f" = function() {
        output$error <- NULL
        output$error2 <- NULL
        output$error3 <- NULL
        output$error4 <- NULL
        output$error5 <- NULL
        output$error6 <- NULL
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
            geom_point(color=cols, shape="|", size=5 + 10/length(seed_ids)) + 
            scale_y_continuous(breaks = seq(1,length(seed_ids))) +
            theme(axis.title=element_text(size=16))
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
        
        nodes <- data.frame(id=faux$id, label=faux$id, level=faux$wave, color=faux$Y)
        edges <- data.frame(from=faux[faux$recruiter.id != "seed",]$recruiter.id, 
                            to=faux[faux$recruiter.id != "seed",]$id)
        lnodes <- data.frame(label=unique(faux$Y), color=unique(faux$Y))
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main="Y",addNodes=lnodes, useGroups = FALSE)
        })
      },
      "fm" = function() {
        output$error <- NULL
        output$error2 <- NULL
        output$error3 <- NULL
        output$error4 <- NULL
        output$error5 <- NULL
        output$error6 <- NULL
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
          geom_point(color=cols, shape="|", size=5 + 10/length(seed_ids)) + 
          scale_y_continuous(breaks = seq(1,length(seed_ids))) +
          theme(axis.title=element_text(size=16))
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
        
        pal <- brewer.pal(length(unique(fauxmadrona$disease)), "Set1")
        group <- match(fauxmadrona$disease, unique(fauxmadrona$disease))
        nodes <- data.frame(id=fauxmadrona$id, label=fauxmadrona$id, level=fauxmadrona$wave, color=pal[group])
        edges <- data.frame(from=fauxmadrona[fauxmadrona$recruiter.id != "seed",]$recruiter.id, 
                            to=fauxmadrona[fauxmadrona$recruiter.id != "seed",]$id)
        lnodes <- data.frame(label=unique(fauxmadrona$disease), color=pal[1:2])
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main="disease",addNodes=lnodes, useGroups = FALSE)
        })
      },
      "fc" = function() {
        output$error <- NULL
        output$error2 <- NULL
        output$error3 <- NULL
        output$error4 <- NULL
        output$error5 <- NULL
        output$error6 <- NULL
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
            geom_point(color=cols, shape="|", size=5 + 10/length(seed_ids)) + 
            scale_y_continuous(breaks = seq(1,length(seed_ids))) +
            theme(axis.title=element_text(size=16))
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
        
        pal <- brewer.pal(length(unique(fauxsycamore$disease)), "Set1")
        group <- match(fauxsycamore$disease, unique(fauxsycamore$disease))
        nodes <- data.frame(id=fauxsycamore$id, label=fauxsycamore$id, level=fauxsycamore$wave, color=pal[group])
        edges <- data.frame(from=fauxsycamore[fauxsycamore$recruiter.id != "seed",]$recruiter.id, 
                            to=fauxsycamore[fauxsycamore$recruiter.id != "seed",]$id)
        lnodes <- data.frame(label=unique(fauxsycamore$disease), color=pal[1:2])
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main="disease",addNodes=lnodes, useGroups = FALSE)
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
        output$plot5 <- NULL
        output$plot6 <- NULL
        output$plot7 <- NULL
        output$plot8 <- NULL
        output$error <- NULL
        output$error2 <- NULL
        output$error3 <- NULL
        output$error4 <- NULL
        output$error5 <- NULL
        output$error6 <- NULL
        
        trySummary()
      }
    )
    f()
  })
}