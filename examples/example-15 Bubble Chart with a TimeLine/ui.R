library(shiny)
library(ECharts2Shiny)


shinyUI(fluidPage(
  
  # We HAVE TO to load the ECharts javascript library in advance
  loadEChartsLibrary(),
  
  h2("Bubble Chart With A Time Line'"),
  wellPanel(
    p("More documentation will be finished later")
  ),
  # Compare normal line chart and stack line chart
  
  tags$div(id="test_bubbleTL", style="width:90%;height:600px;"),  # Specify the div for the chart. Can also be considered as a space holder
  deliverChart(div_id = "test_bubbleTL")  # Deliver the plotting
  
)
)
