library(shiny)
library(rCharts)
shinyUI(pageWithSidebar(
  headerPanel("World Cup South Africa 2010/Brazil 2014"),
sidebarPanel(radioButtons("zon", h5("Zone"),c('Africa',"Asia",
                   "Europe" ,"North and Central America","South America","World"),selected ='World'),
  tags$hr(),
  checkboxGroupInput("date", h5("Year of World cup"),c("2010","2014"),selected=c('2010','2014')),
  tags$hr(),
  sliderInput('goal', h5('Minimum total goals scored'),0,4,value=0),
  width = 3),
  mainPanel(tabsetPanel(
            tabPanel(h5('Scatterplot'),showOutput("plot1","nvd3")),
            tabPanel(h5('Barchart'),showOutput("plot2","nvd3")),
            tabPanel(h5('Correlation coefficient'),verbatimTextOutput("corr")),
            tabPanel(h5('Documentation'),verbatimTextOutput("Doc"))
    ))))
