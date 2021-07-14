library(shiny)
library(DT)

bootstrapPage(
  tags$head(
    tags$title("RDS"),
    tags$meta(charset="utf-8"),
    tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/3/yeti/bootstrap.min.css")
  ),
  
  navbarPage(title=HTML('<a href="./">RDS</a>'), 
    tabPanel("Datasets",
      fluidPage(
        titlePanel(
          fluidRow(
            column(
              width=6,
              offset=5,
              align="center",
              h2("Welcome!")
            )
          )
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons("dataset", "", c("Faux"="f", "Fauxmadrona"="fm", "Fauxsycamore"="fc", "Custom"="cust")),
            p("If you are using a custom dataset, please input a response variable and an estimated population size"),
            textInput("resp", label="Response Variable"),
            actionButton("submit", "Submit"),
            br(),
            textOutput("text1"),
            numericInput("estN", "Estimated Population Size", 10, min=1),
            actionButton("submit2", "Submit"),
            br(),
            textOutput("text2")
          ),
          mainPanel(
            HTML('<p>This application is intended to aid in the analysis and understanding of RDS datasets.
              To begin, please select one of the sample datasets from the left panel, or upload your own custom dataset.
              More information on these datasets is given in the RDS package documentation <a href="https://cran.r-project.org/web/packages/RDS/RDS.pdf">
              which can be found here.</a></p>'),
            p("Please note that any uploaded dataset must include the columns"),
            tags$ul(
              tags$li("id"),
              tags$li('recruiter.id (for seeds, this value must be "seed")'),
              tags$li("network.size.variable"),
              tags$li("seed"),
              tags$li("wave"),
              tags$li("at least one binary response variable")
            ),
            br(),
            fileInput("file1", "Upload a CSV File", accept=".csv"),
            checkboxInput("header", "File contains header", TRUE),
            br(),
            DT::dataTableOutput("fileread")
          )
        )
      )
    ),
    tabPanel("Summary",
      fluidPage(
        p("This tab shows multiple estimators of population proportion for the given response variable.
           It additionally shows the"),
        tags$ul(
          tags$li("Convergence plot: how the estimate changes after each consecutive wave"),
          tags$li("Bottleneck plot: how the estimate changes after each consecutive wave for each seed")
        ),
        p("This information can be used to asses the prevelance of seed dependency and homophily in the RDS sample"),
        tags$b(textOutput("error"), style="color:red"),
        h3("RDS I Estimate"),
        DT::dataTableOutput("rds1"),
        br(),
        h3("RDS II Estimate"),
        DT::dataTableOutput("rds2"),
        br(),
        h3("SS Estimate"),
        DT::dataTableOutput("ss"),
        br(),
        fluidRow(
          column(
            width=6,
            plotOutput("plot1")
          ),
          column(
            width=6,
            plotOutput("plot2")
          )
        )
      )
    ),
    tabPanel("Visualizations",
      fluidPage(
        p("This tab shows various plots useful in assesing characteristics of the RDS sampling process"),
        tags$b(textOutput("error2"), style="color:red"),
        
        h3("All Points Plot"),
        h5("Plot of the value of the response variable for each respondent seperated by seed 
           and ordered by sample order."),
        h5('By default, the order of the data is assumed to be the sampling order. 
           You can change this by including a column "date" and specifying to order by date'),
        radioButtons("ordering", "", c("Default ordering", "Order by date")),
        plotOutput("plot3"),
        
        h3("Recruitment Effectiveness Plots"),
        h5("Plot of the average number of recruits for each value of the response at each wave, as well as a histogram
           of the number of successful recruitings for each value of the response"),
        fluidRow(
          column(
            width=6,
            plotOutput("plot4")
          ),
          column(
            width=6,
            plotOutput("plot5")
          )
        ),
        
        h3("Recruitment Monitering Plots"),
        h5("Plot of the number of recruits at each wave, 
           as well as the number of recruits for each seed at each wave"),
        fluidRow(
          column(
            width=6,
            plotOutput("plot6")
          ),
          column(
            width=6,
            plotOutput("plot7")
          )
        )
      )
    )
  )
)
