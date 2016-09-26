library(shiny)
library(ECharts2Shiny)



# Prepare sample data for plotting ---------------------------------------

data = data.frame(x = c(10,0,30,40,25,100),
                  y = c(20,0,22,100,30,500),
                  z = c(1000,3000,2000,1000,1500,100000),
                  group = c('TypeA','TypeA','TypeB','TypeB','TypeC','TypeC'),
                  time = c('Week 1', 'Week 2', 'Week 1', 'Week 2','Week 1', 'Week 2'),
                  size = c(20,0,7.3,25,50,40))



shinyServer(function(input, output) {
  
  # Call functions from ECharts2Shiny to render charts
  
  # Scatter plot
  
  renderBubbleTL("test_bubbleTL", data = data,
                 size.option = list(
                   name = 'Our Size',
                   unit = '%',
                   unit.show = 'after',
                   max = 40
                 ))
  
}
)
