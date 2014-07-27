library(shiny)
library(rCharts)
library(xlsx)
library(stringr)


shinyServer(function(input, output) {
  ##Data source: Fifa official website http://www.fifa.com/worldcup/statistics/index.html
  #Loading of the working space for data preparing
  load('Data_preparing.RData')
  
  #reactive expression zone
  ZoneInput <- reactive({switch(input$zon,'Africa'='^Afr','Asia'='^Asi','Europe'='^Eur',
    'North and Central America'='^Nor',"South America"='^Sou',
    'World'='^Afr|^Asi|^Eur|^Nor|^Sou')})
  
  #reactive expression year
  ZoneDate<- reactive({switch(input$date,2010,2014)})

##Interactive scatterplot between average goals by match and passes completed  
#############################################################################
output$plot1<-renderChart({
  #Extracting the teams ID according to zone
  indexZon<-grep(ZoneInput(),dataAll[,6])
  
  #Extracting the teams ID according to minimun scored goals
  indexgoal<-which(dataAll$Total_goals>=input$goal)
  
  #Extracting the teams ID according to year
  indexdate<-which(is.element(dataAll$Year,input$date))
  
  #Extracting the teams ID according to zone and minimum scored goals
  index0<-intersect(indexZon,indexgoal)
  
  #Extracting the teams ID according to zone, minimum scored goals and date
  index1<-intersect(index0,indexdate) 
     
    if (input$zon!='World'){
    #Scatterplot between average goals by match and passes completed
    p1<-nPlot(Average_goal~Passes_completed_percent,group='Team',
              data =dataAll[index1,],type = 'scatterChart')
  
    #Labeled  points by team names
    p1$chart(tooltipContent = "#! function(key, x, y, e)
                               {return '<b>Team</b>: ' + e.point.Team} !#")
  
    #Rename X-axis title  
     p1$xAxis(axisLabel='Passes completed (%)')
  
    #Rename Y-axis title
     p1$yAxis(axisLabel='Average goal by match')
  
    p1$addParams(dom='plot1')
    return(p1)}
  
  else{
    #Adding a new variable team with only three letters of team name
    dataAll$Team2<-substr(dataAll$Team,1,3)
    
    p1<-nPlot(Average_goal~Passes_completed_percent,group='Team2',
      data =dataAll[index1,],type = 'scatterChart')
    
    #Labeled  points by team names
    p1$chart(tooltipContent = "#! function(key, x, y, e)
                               {return '<b>Team</b>: ' + e.point.Team} !#")
    
    #Rename X-axis title  
    p1$xAxis(axisLabel='Passes completed (%)')
    
    #Rename Y-axis title
    p1$yAxis(axisLabel='Average goal by match')
    
    p1$addParams(dom='plot1')
    return(p1)}
  
})

##Interactive barchart the total scored goals by team during all world cup
##########################################################################  
  output$plot2<-renderChart({
    #Extracting the teams ID according to zone
    indexZon<-grep(ZoneInput(),dataAll[,6])
    
    #Extracting the teams ID according to minimun scored goals
    indexgoal<-which(dataAll$Total_goals>=input$goal)
    
    #Extracting the teams ID according to year
    indexdate<-which(is.element(dataAll$Year,input$date))
    
    #Extracting the teams ID according to zone and minimum scored goals
    index0<-intersect(indexZon,indexgoal)
    
    #Extracting the teams ID according to zone, minimum scored goals and date
    index1<-intersect(index0,indexdate) 
    
    #Barchart the total goals scored accordance to names teams
    p2<-nPlot(Total_goals ~ Team, data =dataAll[index1,], group='Year',type = 'multiBarChart')
    
    #Colored barchart according to year
    p2$chart(color=c('blue','pink'))
    
    #rename Y-axis title
    p2$yAxis(axisLabel='Total goals scored',width = 50)
    
    #rename X-axis title
    p2$xAxis(axisLabel='Team name')    
    
    #Force rChart displays all ticks labels X-axis
    p2$chart(reduceXTicks = FALSE)
   
    #Rotate ticks labels X-axis
    p2$xAxis(rotateLabels=-45)  

    p2$addParams(dom='plot2')
    return(p2)
  })

##Performing correlation coefficient
####################################  
  output$corr<-renderPrint({
    #Extracting the teams ID according to zone
    indexZon<-grep(ZoneInput(),dataAll[,6])
    
    #Extracting the teams ID according to minimun scored goals
    indexgoal<-which(dataAll$Total_goals>=input$goal)
    
    #Extracting the teams ID according to year
    indexdate<-which(is.element(dataAll$Year,input$date))
    
    #Extracting the teams ID according to zone and minimum scored goals
    index0<-intersect(indexZon,indexgoal)
    
    #Extracting the teams ID according to zone, minimum scored goals and date
    index1<-intersect(index0,indexdate) 
    
    corr1<-cor(dataAll[index1,3],dataAll[index1,5])
     
    if (sum(is.element(dataAll$Year,input$date))>32){
cat('Correlation coefficient between the average goal and the percentage of passes completed  during the World Cup South \n Africa 2010 and Brasil 2014 is',round(corr1,2), 'in',input$zon,'. \n') }
  
    if (sum(is.element(dataAll$Year,input$date))==32) {
cat('Correlation coefficient between the average goal and the percentage of passes completed percent during the World\n Cup',input$date,'is',round(corr1,2),'in',input$zon,'. \n ')}
    
    if (length(input$date)==0){cat('')}
    
  })
  
##Reading the documentation  
output$Doc<-renderPrint({
cat('This web application enable you a graphical assessement between the average goals by match and passes completed during \n the World cup south Africa 2010 and Brazil 2014. This assessement is done by zone, year and the minimum total goals\n scored.')
    })
})
