library(shiny)
library(plotly)


ui<-shinyUI(fluidPage(
  titlePanel("Predicting calories for a bike ride -- model based on Strava data"),
  titlePanel("Bike calorie predictor"),
  sidebarLayout(
    sidebarPanel(  #sidebarPanel = input
      numericInput("new_dist", "Enter distance in km:", value = 20),
      numericInput("new_elev", "Enter elevation gain in m:", value = 200),
      numericInput("new_weig", "Enter body weight in kg:", value = 50),
      numericInput("new_bw", "Enter bike weight in kg:", value=10),
      submitButton("Submit") # New!
      
    ), #slidebarPanel
    
    
    
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Tab 1: calories prediction", br(), textOutput("out1"),
                           
                           
                           h3("Based on your input parameters, the calories for this bike ride is:"),
                           
                           h3(textOutput("pred1")),
                           h3("calories")
                           
                  ), #tabPanel1
                  
                  
                  tabPanel("Tab 2: underlying model details ", br(), textOutput("out2"),
                           
                           h3(em("calories vs distance, elevation, and weight")),
                           h3("Result of 3-variable linear regression:"),
                           tableOutput("model.bike.coef"),
                           h3("graphic representation (drag to change view angles:"),
                           h5("The recorded data (black solid circles) was modeled by linear regression, 
                            and the prediction spans a plane defined by the faint circles. 
                            The fitted values are plotted in red, all lying on the prediction plane."),
                           plotlyOutput("plot3")
                )#tabPanel2
    )#tabsetPanel
    
   
  ) #mainPanel

 ) #sidbarlayout  
#

)) #fluidPage, shinyUI


server<-shinyServer(function(input, output) {
  
  output$out1 <- renderText(input$box1)
  output$out2 <- renderText(input$box2)
  output$out3 <- renderText(input$box3)
  
  
  bike <- read.csv("bike_data.csv")  #need to load this data outside of shinyServer 
  bike_D <-read.csv("bike_data_D.csv") 
  bike_all<-rbind(bike,bike_D)
  names(bike)<-names(bike_D)<- names(bike_all)<- c("datetime","dist.km","mtime","elev.m","power.w","E.kJ","sp.km.h","HR","calories","bike.type","weight")
  
  # fit model
  m.bike <- lm(calories ~ dist.km + elev.m , data = bike)  # model with 2 variables
  m.bike_all <- lm(calories ~ dist.km + elev.m + weight, data = bike_all) # 3 variables
  
  ##prediction with 3 variable model:
  # d_temp<-setNames(c(input$new_dist,input$new_elev,input$new_weig ), c("dist.km","elev.m","weight"))
  # d_input<-data.frame(t(d_temp))
  
  model1pred <- reactive({
    distInput <- input$new_dist
    elevInput<- input$new_elev
    weigInput<- input$new_weig + input$new_bw
    predict(m.bike_all, newdata = data.frame(dist.km = distInput, elev.m=elevInput, weight=weigInput))
  })
  
  output$pred1 <- renderText({
    model1pred()
  })
  
  outcoef<-data.frame(summary(m.bike_all)[[4]])
  
  output$model.bike.coef <- renderTable({
    
    data.frame(coef=rownames(outcoef),outcoef)
    
  }) #rednerTable
  
  
  # for plotting
  
  dists <- unique(bike$dist.km)
  elevs<-unique(bike$elev.m)
  sps <- unique(bike$sp.km.h)
  grid.bike <- with(bike, expand.grid(dists, elevs))
  d1 <- setNames(data.frame(grid.bike), c("dist.km", "elev.m"))
  vals.bike <- predict(m.bike, newdata = d1)
  
  d2<-data.frame(d1,vals.bike)
  
  # form matrix and give to plotly
  d2mat.bike <- matrix(vals.bike, ncol = length(unique(d2$dist.km)), nrow = length(unique(d2$elev.m)))
  
  colnames(d2mat.bike)<-unique(d2$dist.km)
  rownames(d2mat.bike)<-unique(d2$elev.m)
  
  
  
  output$plot3 <- renderPlotly({
    
    plot_ly() %>%
      add_trace(data=bike,
                x = ~dist.km, 
                y = ~elev.m,
                z = ~calories, 
                type = "scatter3d",
                mode = "markers",
                marker = list(color = "black", symbol = "circle-solid"),
                
                name = "recorded") %>%
      add_trace(data=d2,
                x = ~dist.km, 
                y = ~elev.m,
                z = ~vals.bike, 
                type = "scatter3d", 
                mode = "markers",
                color = ~elev.m,
                colors = c("gray70", '#6d98f3'),
                opacity = 0.15,
                # line = list(color = "black", width = 1, dash = 'dash'),
                name = "predicted plane") %>%
      
      add_trace(data=bike,
                x = ~dist.km,
                y = ~elev.m,
                z = ~m.bike$fitted.values,
                type = "scatter3d",
                mode = "markers",
                marker = list(color = "red", symbol = "circle-open"),
                name ="fitted values") %>%
      layout(title = 'Calories vs distance and elevation -- 
            Bike commute data',
             scene = list(xaxis = list(title = 'Distance (km)', range = c(min(bike$dist.km),max(bike$dist.km)), ticktype = "array"),
                          yaxis = list(title = 'Elevation (m)', range = c(min(bike$elev.m),max(bike$elev.m)), ticktype = "array"),
                          zaxis = list(title = 'Calories', range = c(min(bike$calories),max(bike$calories)), ticktype = "array"),
                          camera = list(zoom = 10),
                          showlegend = FALSE))
    
    
    
    
    
    
  })
  
}
)


shinyApp(ui, server)