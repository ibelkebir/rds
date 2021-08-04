library(RDS)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(reshape2)
library(visNetwork)
library(RColorBrewer)

function(input, output, session) {
  data(fauxmadrona)
  data(fauxsycamore)
  
  fm_1 <- RDS.I.estimates(rds.data=fauxmadrona, outcome.variable="disease")$interval
  fm_2 <- RDS.II.estimates(rds.data=fauxmadrona, outcome.variable="disease")$interval
  fm_3 <- RDS.SS.estimates(rds.data=fauxmadrona, outcome.variable="disease")$interval
  fm_5 <- bottleneck.plot(fauxmadrona, c("disease"))
  
  seed_ids <- fauxmadrona[fauxmadrona$recruiter.id == "seed",]$id
  cols <- sapply(fauxmadrona$disease, function(x) ifelse(x==1, "red", "blue"))
  df <- data.frame(recruit=seq(1,nrow(fauxmadrona)), seed=match(fauxmadrona$seed, seed_ids) - 0.25 + .5 * fauxmadrona$disease)
  fm_6 <- ggplot(df, aes(x=recruit, y=seed)) + 
          geom_point(color=cols, shape="|", size=5 + 10/length(seed_ids)) + 
          scale_y_continuous(breaks = seq(1,length(seed_ids))) +
          theme(axis.title=element_text(size=16))

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
  fm_7 <- ggplot(data=df2, aes(x=x)) +
          geom_line(aes(y=avgs1, color="positive")) +
          geom_line(aes(y=avgs2, color="negative")) +
          scale_colour_manual("",
                              breaks = c("positive","negative"),
                              values = c("red", "blue")) +
          xlab("Wave") +
          ylab("Average number of recruits") +
          scale_x_continuous(breaks = seq(1,length(waves)))
  
  recruits <- rep(0,nrow(fauxmadrona[fauxmadrona$wave != max(fauxmadrona$wave),]))
  responses <- fauxmadrona[fauxmadrona$wave != max(fauxmadrona$wave),]$disease
  i = 0
  for (id in fauxmadrona[fauxmadrona$wave != max(fauxmadrona$wave),]$id) {
    recruits[i] = nrow(fauxmadrona[fauxmadrona$recruiter.id == id,])
    i = i+1
  }
  df3 <- data.frame(recs=recruits, response=sapply(responses,toString))
  fm_8 <- ggplot(df3, aes(x=recs, fill=response)) +
          geom_histogram(position="dodge", binwidth=1) +
          xlab("# of recruits")
  
  waves <- split(fauxmadrona, fauxmadrona$wave)
  c <- sapply(waves,nrow)
  df4 <- data.frame(x=seq(0,length(waves)-1), y=c)
  fm_9 <- ggplot(df4, aes(x=x,y=y)) +
            geom_line(aes(color="red")) +
            scale_x_continuous(breaks = seq(0,length(waves)-1)) +
            xlab("wave") +
            ylab("# of recruits") +
            theme(legend.position="none")
  
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
  
  fm_10 <- ggplot(melt(M, id.vars="wave"), aes(x=wave, y=value, color=variable)) +
            geom_line() +
            xlab("wave") +
            ylab("# of recruits") +
            guides(color=guide_legend("seed"))
  
  pal <- brewer.pal(length(unique(fauxmadrona$disease)), "Set1")
  group <- match(fauxmadrona$disease, unique(fauxmadrona$disease))
  nodes <- data.frame(id=fauxmadrona$id, label=fauxmadrona$id, level=fauxmadrona$wave, color=pal[group], font.size=rep(30,nrow(fauxmadrona)))
  edges <- data.frame(from=fauxmadrona[fauxmadrona$recruiter.id != "seed",]$recruiter.id, 
                      to=fauxmadrona[fauxmadrona$recruiter.id != "seed",]$id)
  lnodes <- data.frame(label=unique(fauxmadrona$disease), color=pal[1:2], shape="dot")
  fm_11 <- visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main="disease",addNodes=lnodes, useGroups = FALSE)
  
  
  fc_1 <- RDS.I.estimates(rds.data=fauxsycamore, outcome.variable="disease")$interval
  fc_2 <- RDS.II.estimates(rds.data=fauxsycamore, outcome.variable="disease")$interval
  fc_3 <- RDS.SS.estimates(rds.data=fauxsycamore, outcome.variable="disease")$interval
  fc_5 <- bottleneck.plot(fauxsycamore, c("disease"))
  
  seed_ids <- fauxsycamore[fauxsycamore$recruiter.id == "seed",]$id
  cols <- sapply(fauxsycamore$disease, function(x) ifelse(x==1, "red", "blue"))
  df <- data.frame(recruit=seq(1,nrow(fauxsycamore)), seed=match(fauxsycamore$seed, seed_ids) - 0.25 + .5 * fauxsycamore$disease)
  output$legend <- renderText("Red: Disease = 1, Blue: Disease = 0")
  fc_6 <- ggplot(df, aes(x=recruit, y=seed)) + 
            geom_point(color=cols, shape="|", size=5 + 10/length(seed_ids)) + 
            scale_y_continuous(breaks = seq(1,length(seed_ids))) +
            theme(axis.title=element_text(size=16))
  
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
  fc_7 <- ggplot(data=df2, aes(x=x)) +
            geom_line(aes(y=avgs1, color="positive")) +
            geom_line(aes(y=avgs2, color="negative")) +
            scale_colour_manual("",
                                breaks = c("positive","negative"),
                                values = c("red", "blue")) +
            xlab("Wave") +
            ylab("Average number of recruits") +
            scale_x_continuous(breaks = seq(1,length(waves)))
  
  recruits <- rep(0,nrow(fauxsycamore[fauxsycamore$wave != max(fauxsycamore$wave),]))
  responses <- fauxsycamore[fauxsycamore$wave != max(fauxsycamore$wave),]$disease
  i = 0
  for (id in fauxsycamore[fauxsycamore$wave != max(fauxsycamore$wave),]$id) {
    recruits[i] = nrow(fauxsycamore[fauxsycamore$recruiter.id == id,])
    i = i+1
  }
  df3 <- data.frame(recs=recruits, response=sapply(responses,toString))
  fc_8 <- ggplot(df3, aes(x=recs, fill=response)) +
            geom_histogram(position="dodge", binwidth=1) +
            xlab("# of recruits")
  
  waves <- split(fauxsycamore, fauxsycamore$wave)
  c <- sapply(waves,nrow)
  df4 <- data.frame(x=seq(0,length(waves)-1), y=c)
  fc_9 <- ggplot(df4, aes(x=x,y=y)) +
            geom_line(aes(color="red")) +
            scale_x_continuous(breaks = seq(0,length(waves)-1)) +
            xlab("wave") +
            ylab("# of recruits") +
            theme(legend.position="none")
  
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
  
  fc_10 <- ggplot(melt(M, id.vars="wave"), aes(x=wave, y=value, color=variable)) +
            geom_line() +
            xlab("wave") +
            ylab("# of recruits") +
            guides(color=guide_legend("seed"))
  
  pal <- brewer.pal(length(unique(fauxsycamore$disease)), "Set1")
  group <- match(fauxsycamore$disease, unique(fauxsycamore$disease))
  nodes <- data.frame(id=fauxsycamore$id, label=fauxsycamore$id, level=fauxsycamore$wave, color=pal[group], font.size=rep(30,nrow(fauxsycamore)))
  edges <- data.frame(from=fauxsycamore[fauxsycamore$recruiter.id != "seed",]$recruiter.id, 
                      to=fauxsycamore[fauxsycamore$recruiter.id != "seed",]$id)
  lnodes <- data.frame(label=unique(fauxsycamore$disease), color=pal[1:2], shape="dot")
  fc_11 <- visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main="disease",addNodes=lnodes, useGroups = FALSE)
  
  trySummary <- function(){
    
    if (is.null(response$data)) {
      output$error <- renderText("No response variable chosen")
      output$error8 <- renderText("No response variable chosen")
    }
    
    if (is.null(readfile$data)) {
      output$error <- renderText("No dataset uploaded")
      output$error8 <- renderText("No dataset uploaded")
    } else if (is.null(response$data)){
      output$error <- renderText("No response variable chosen")
      output$error8 <- renderText("No response variable chosen")
    } else {
      output$error <- NULL
      output$error8 <- NULL
    }
    
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
    }, finally = {})
    
    task2 <- tryCatch({
      output$error2 <- NULL
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
      if (!is.null(readfile$data) && !is.null(response$data)) {
        output$error2 <- renderPrint(err)
      }
    }, finally = {})
    
    task3 <- tryCatch({
      output$error3 <- NULL
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
      if (!is.null(readfile$data) && !is.null(response$data)) {
        output$error3 <- renderPrint(err)
      }
    }, finally = {})
    
    task4 <- tryCatch({
      output$error4 <- NULL
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
      if (!is.null(readfile$data) && !is.null(response$data)) {
        output$error4 <- renderPrint(err)
      }
    }, finally = {})
    
    task5 <- tryCatch({
      output$error5 <- NULL
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
      if (!is.null(readfile$data) && !is.null(response$data)) {
        output$error5 <- renderPrint(err)
      }
    }, finally = {})
    
    task6 <- tryCatch({
      output$error6 <- NULL
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
      if (!is.null(readfile$data)) {
        output$error6 <- renderPrint(err)
      }
    }, finally = {})
    
    task7 <- tryCatch({
      output$error7 <- NULL
      df <- readfile$data
      if (is.null(lab_var$data)) {
        nodes <- data.frame(id=df$id, label=df$id, level=df$wave,font.size=rep(30,nrow(df)))
        edges <- data.frame(from=df[df$recruiter.id != "seed",]$recruiter.id, 
                            to=df[df$recruiter.id != "seed",]$id)
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100)
        })
      } else {
        trait <- lab_var$data
        pal <- brewer.pal(length(unique(df[[trait]])), "Set1")
        group <- match(df[[trait]], unique(df[[trait]]))
        nodes <- data.frame(id=df$id, label=df$id, level=df$wave, color=pal[group], font.size=rep(30,nrow(df)))
        edges <- data.frame(from=df[df$recruiter.id != "seed",]$recruiter.id, 
                            to=df[df$recruiter.id != "seed",]$id)
        lnodes <- data.frame(label=unique(df[[trait]]), color=pal[1:length(unique(df[[trait]]))], shape="dot")
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main=trait,addNodes=lnodes, useGroups = FALSE)
        })
      }
    }, warning = function(war) {
    }, error = function(err) {
      if (!is.null(readfile$data)) {
        output$error7 <- renderPrint(err)
      }
    }, finally = {})
  }
  
  infile <- reactiveValues(data=NULL)
  readfile <- reactiveValues(data=NULL)
  observeEvent(input$file1, {
    infile$data <- input$file1
    if (is.null(infile$data)) return()
    readfile$data <- read.csv(infile$data$datapath, header=TRUE)
    output$fileread <- DT::renderDataTable({
      readfile$data
    })
    remove <- c("id", "recruiter.id", "network.size.variable", "seed", "wave", "date")
    updateSelectInput(session=session, inputId="response", choices=colnames(readfile$data)[! colnames(readfile$data) %in% remove])
    updateSelectInput(session=session, inputId="trait", choices=colnames(readfile$data)[! colnames(readfile$data) %in% remove])
    if (input$dataset == "cust") trySummary()
  }) 
  
  response <- reactiveValues(data=NULL)
  observeEvent(input$response, {
    response$data <- input$response
    if (input$dataset == "cust") trySummary()
  })
  
  lab_var <- reactiveValues(data=NULL)
  observeEvent(input$trait, {
    lab_var$data <- input$trait
    output$error7 <- NULL
    if(input$dataset == "cust") {
      trait <- lab_var$data
      task <- tryCatch({
        df <- readfile$data
        pal <- brewer.pal(length(unique(df[[trait]])), "Set1")
        group <- match(df[[trait]], unique(df[[trait]]))
        nodes <- data.frame(id=df$id, label=df$id, level=df$wave, color=pal[group], font.size=rep(30,nrow(df)))
        edges <- data.frame(from=df[df$recruiter.id != "seed",]$recruiter.id, 
                            to=df[df$recruiter.id != "seed",]$id)
        lnodes <- data.frame(label=unique(df[[trait]]), color=pal[1:length(unique(df[[trait]]))], shape="dot")
        output$plot8 <- renderVisNetwork({
          visNetwork(nodes, edges) %>%
            visNodes() %>%
            visHierarchicalLayout(direction = "UD", levelSeparation = 100) %>%
            visLegend(main=trait,addNodes=lnodes, useGroups = FALSE)
        })
      }, warning = function(war) {
      }, error = function(err) {
        if (!is.null(readfile$data)) {
          output$error7 <- renderPrint(err)
        }
      }, finally = {})
    }
  })
  
  N <- reactiveValues(data=NULL)
  observeEvent(input$submit2, {
    N$data <- input$estN
    output$text2 <- renderText(paste("Chosen population size estimate: ", N$data))
    if (input$dataset == "cust") trySummary()
  }) 
  
  observeEvent(input$ordering, {
    if (input$dataset == "cust") {
      output$error2 <- NULL
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
        if (!is.null(readfile$data)) {
          output$error2 <- renderPrint(err)
        }
      }, finally = {})
    }
  })
  
  observeEvent(input$dataset, {
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
    output$error7 <- NULL
    output$error8 <- NULL
    f <- switch(
      input$dataset,
      "fm" = function() {
        output$rds1 <- DT::renderDataTable(fm_1)
        output$rds2 <- DT::renderDataTable(fm_2)
        output$ss <- DT::renderDataTable(fm_3)
        output$plot1 <- renderPlot(convergence.plot(fauxmadrona, c("disease")))
        output$plot2 <- renderPlot(fm_5)
        output$plot3 <- renderPlot(fm_6)
        output$plot4 <- renderPlot(fm_7)
        output$plot5 <- renderPlot(fm_8)
        output$plot6 <- renderPlot(fm_9)
        output$plot7 <- renderPlot(fm_10)
        output$plot8 <- renderVisNetwork(fm_11)
      },
      "fc" = function() {
        output$rds1 <- DT::renderDataTable(fc_1)
        output$rds2 <- DT::renderDataTable(fc_2)
        output$ss <- DT::renderDataTable(fc_3)
        output$plot1 <- renderPlot(convergence.plot(fauxsycamore, c("disease")))
        output$plot2 <- renderPlot(fc_5)
        output$plot3 <- renderPlot(fc_6)
        output$plot4 <- renderPlot(fc_7)
        output$plot5 <- renderPlot(fc_8)
        output$plot6 <- renderPlot(fc_9)
        output$plot7 <- renderPlot(fc_10)
        output$plot8 <- renderVisNetwork(fc_11)
      },
      "cust" = function() {
        trySummary()
      }
    )
    f()
  })
}