###########################################################################
# Bar Chart ---------------------------------------------------------------
###########################################################################


renderBarChart <- function(div_id,
                           data, theme = "default",
                           stack_plot = FALSE,
                           direction = "horizontal",
                           grid_left = "3%", grid_right = "4%", grid_top = "16%", grid_bottom = "3%",
                           show.legend = TRUE, show.tools = TRUE,
                           font.size.legend = 12,
                           font.size.axis.x = 12, font.size.axis.y = 12,
                           rotate.axis.x = 0, rotate.axis.y = 0,
                           bar.max.width = NULL,
                           animation = TRUE,
                           color = "default",
                           y.name = list(
                             show.yname = FALSE,
                             name = 'Y',
                             axisLabel = list(
                               formatter = '{value}'
                             ),
                             nameLocation = 'middle',
                             nameGap = 40,
                             nameTextStyle = list(
                               color = 'Black',
                               fontStyle = 'normal',
                               fontWeight = 'bold',
                               fontSize = 20
                             )
                           ),
                           running_in_shiny = TRUE){

  data <- isolate(data)

  data <- .process_NA(data)

  # Check the value for theme
  theme_placeholder <- .theme_placeholder(theme)

  # Check if the "direction" value is valid
  if(direction == "horizontal"){
    direction_vector = c("xAxis", "yAxis")
  }else{
    if(direction == "vertical"){
      direction_vector = c("yAxis", "xAxis")
    }else{
      stop("The 'direction' argument can be either 'horizontal' or 'vertical'")
    }
  }

  xaxis_name <- paste(sapply(row.names(data), function(x){paste("'", x, "'", sep="")}), collapse=", ")
  xaxis_name <- paste("[", xaxis_name, "]", sep="")
  legend_name <- paste(sapply(names(data), function(x){paste("'", x, "'", sep="")}), collapse=", ")
  legend_name <- paste("[", legend_name, "]", sep="")

  # The label for yAxis
  if (y.name$show.yname == TRUE) {
    yAxis.js = paste(direction_vector[1],
                     ":{type:'value',",
                     "name:'",y.name$name,"',",
                     "nameLocation:'",y.name$nameLocation,"',",
                     "nameGap:",y.name$nameGap,",",
                     "nameTextStyle:{fontStyle:'",y.name$nameTextStyle$fontStyle,"',",
                     "fontSize:",y.name$nameTextStyle$fontSize,",",
                     "fontWeight:'",y.name$nameTextStyle$fontWeight,"',",
                     "color:'",y.name$nameTextStyle$color,"'},",
                     "axisLabel:{formatter:'",y.name$axisLabel$formatter,"'}},",
                     sep = "")
  } else {
    yAxis.js = paste(direction_vector[1],": {type: 'value'},",sep = "")
  }
  # Convert raw data into JSON format (Prepare the data in "series" part)
  series_data <- rep("", dim(data)[2])
  for(i in 1:length(series_data)){
    temp <- paste("{name:'", names(data)[i], "', type:'bar', ",

                  ifelse(stack_plot,
                         " stack:' ', ",
                         " "),

                  ifelse(is.null(bar.max.width),
                         "barMaxWidth: null,",
                         paste("barMaxWidth:'", bar.max.width, "',", sep="")),

                  "data:[",
                  paste(data[, i], collapse = ", "),
                  "]}",
                  sep=""
    )
    series_data[i] <- temp
  }
  series_data <- paste(series_data, collapse = ", ")
  series_data <- paste("[", series_data, "]", sep="")
  
  # Implement customized color set
  # color = c('balck','white')
  # color -> "color: ['black'],"
  if (color == "default") {
    color.js = ""
  } else {
    color.js = "color: ["
    if (length(color) > 1) {
      for (i in 1:(length(color)-1)) {
        color.js = paste0(color.js, "'", color[i], "',")
      }
    }
    color.js = paste0(color.js, "'", color[length(color)], "'],")
  }

  js_statement <- paste("var " ,
                        div_id,
                        " = echarts.init(document.getElementById('",
                        div_id,
                        "')",
                        theme_placeholder,
                        ");",

                        "option_", div_id,
                        " = {tooltip : {trigger:'axis', axisPointer:{type:'shadow'}}, ",

                        ifelse(show.tools,
                               "toolbox:{feature:{magicType:{type: ['stack', 'tiled']}, saveAsImage:{}}}, ",
                               ""),

                        ifelse(animation,
                               "animation:true,",
                               "animation:false,"),

                        ifelse(show.legend,
                               paste("legend:{data:",
                                     legend_name,
                                     ", textStyle:{fontSize:", font.size.legend, "}",
                                     "},",
                                     sep=""),
                               ""),
                        color.js,
                        "grid: {left:'", grid_left, "', right:'", grid_right, "', top:'", grid_top, "', bottom:'", grid_bottom, "', containLabel: true},",
                        # Labels for yAxis
                        yAxis.js,
                        direction_vector[2],
                        ":[{type:'category', axisTick:{show:false}, axisLabel:{rotate:", rotate.axis.x, ",textStyle:{fontSize:", font.size.axis.x, "}}, data:",
                        xaxis_name,
                        "}],series :",
                        series_data,
                        "};",

                        div_id,
                        ".setOption(option_",
                        div_id,
                        ");",

                        "window.addEventListener('resize', function(){",
                        div_id, ".resize()",
                        "});",

                        sep="")

  to_eval <- paste("output$", div_id ," <- renderUI({fluidPage(tags$script(\"",
                   js_statement,
                   "\"))})",
                   sep="")

  if(running_in_shiny == TRUE){
    eval(parse(text = to_eval), envir = parent.frame())
  } else {
    cat(to_eval)
  }
}
