library(shiny)
library(shinyjs)


shinyUI(fluidPage(
  useShinyjs(),
  shinyjs::inlineCSS(list(body = "padding-top: 70px;")), #due to fixed navbar
  
  HTML('<nav class="navbar navbar-inverse navbar-fixed-top">
       <div class="container-fluid">
       <div class="navbar-header">
       <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#myNavbar">
       <span class="icon-bar"></span>
       <span class="icon-bar"></span>
       <span class="icon-bar"></span>                        
       </button>
       <a class="navbar-brand" href="#">hambue</a>
       </div>
       <div class="collapse navbar-collapse" id="myNavbar">
       <ul class="nav navbar-nav">
       <li><a href="https://www.twitter.com/hambue" target="_blank">Twitter</a></li>
       </ul>
       </div>
       </div>
       </nav>'),
  
  # Application title
  titlePanel("CL Last 16 Draw Probabilities 2016/17"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Draw match after match and watch how the probabilities change."),
      
      selectizeInput(
        'game1', 'Match 1', choices = NULL),
      selectizeInput(
        'game2', 'Match 2', choices = NULL),
      selectizeInput(
        'game3', 'Match 3', choices = NULL),
      selectizeInput(
        'game4', 'Match 4', choices = NULL),
      selectizeInput(
        'game5', 'Match 5', choices = NULL),
      selectizeInput(
        'game6', 'Match 6', choices = NULL),
      selectizeInput(
        'game7', 'Match 7', choices = NULL)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      helpText("To ensure a fast calculation there are some simplifications in my algorithm which result in minor
               deviations (\u00B1 0.5%) from the true probabilities."),
      helpText("(Why deviations? Explanation from ", 
               a("@bimbeshausen", href = "http://twitter.com/bimbeshausen",
                 target = "_blank"),
               "in german: ", 
               a("http://www.neureich-bimbeshausen.de/artikel/nachgerechnet.html", 
                 href = "http://www.neureich-bimbeshausen.de/artikel/nachgerechnet.html",
                 target = "_blank"),")"),
      #helpText("Exact probabilities be found", a("here", 
      #         href = "http://www.zeit.de/sport/2016-12/champions-league-auslosung-achtelfinale-fc-bayern-borussia-dortmund/komplettansicht",
      #         target = "_blank")),
      br(),
      #tableOutput("table")
      downloadButton('downloadPlot', 'Save as PNG'),
      plotOutput('view'),
      br(),
      wellPanel(h4("Your draw:"),
                #textOutput('games'),
                tableOutput("table"))
      
      #downloadLink('downloadData', 'Download table as PNG')
      )
  )
))
