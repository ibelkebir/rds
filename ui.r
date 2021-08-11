library(shiny)
library(DT)
library(visNetwork)

bootstrapPage(
  tags$head(
    tags$title("RDS"),
    tags$meta(charset="utf-8"),
    tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/3/yeti/bootstrap.min.css")
  ),
  
  navbarPage(title=HTML('<a href="./">Home</a>'), 
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
            radioButtons("dataset", "", c("Fauxmadrona"="fm", "Fauxsycamore"="fc", "Custom"="cust")),
            p("If you are using a custom dataset, please select a response variable and an estimated population size"),
            selectInput(inputId = "response", label = "Response variable", choices=NULL),
            br(),
            selectInput(inputId = "trait", label = "Covariate to label (recruitment tree)", choices=NULL),
            br(),
            numericInput("estN", "Estimated Population Size", NULL, min=1),
            actionButton("submit2", "Submit"),
            br(),
            tags$b(textOutput("text2"))
          ),
          mainPanel(
            HTML('<p>This application is intended to aid in the analysis of RDS datasets.
              To begin, please select one of the sample datasets from the left panel, or upload your own custom dataset.
              More information on these datasets is given in the RDS package documentation <a href="https://cran.r-project.org/web/packages/RDS/RDS.pdf">
              which can be found here.</a></p>'),
            p("Please note that any uploaded dataset must include the columns"),
            tags$ul(
              tags$li("id"),
              tags$li('recruiter.id (for seeds, this value must be "seed")'),
              tags$li("network.size.variable"),
              tags$li("seed (id of the seed the individual was recruited from)"),
              tags$li("wave"),
              tags$li("at least one binary response variable")
            ),
            br(),
            fileInput("file1", "Upload a CSV File", accept=".csv"),
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
        br(),
        tags$b(textOutput("error9"), style="color:red"),
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
        tags$b(textOutput("error8"), style="color:red"),
        h3("Recruitment Tree"),
        p('An interactive tree showing the recruitment process. You can also select a trait to color the nodes by for custom datasets.'),
        tags$b(textOutput("error7"), style="color:red"),
        visNetworkOutput("plot8"),
        
        h3("All Points Plot"),
        h5("Plot of the value of the response variable for each respondent seperated by seed 
           and ordered by sample order."),
        p('By default, the sample order is assumed to be the ordering of the data. To order by date, make sure
          your dataset has a "date" column in day/month/year format and specify below'),
        radioButtons("ordering", "", c("Default"="d", "By date"="t")),
        tags$b(textOutput("error2"), style="color:red"),
        plotOutput("plot3"),
        
        h3("Recruitment Effectiveness Plots"),
        h5("Plot of the average number of recruits for each value of the response at each wave, as well as a histogram
           of the number of successful recruitings for each value of the response"),
        tags$b(textOutput("error3"), style="color:red"),
        tags$b(textOutput("error4"), style="color:red"),
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
        tags$b(textOutput("error5"), style="color:red"),
        tags$b(textOutput("error6"), style="color:red"),
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
