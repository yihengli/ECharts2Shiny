###########################################################################
# Line Chart --------------------------------------------------------------
###########################################################################
renderLineChart <- function(div_id,
                            data, theme = "default",
                            line.width = 2, line.type = "solid",
                            point.size = 5, point.type = "emptyCircle",
                            stack_plot = FALSE, step = "null",
                            show.legend = TRUE, show.tools = TRUE,
                            running_in_shiny = TRUE, show.slider = FALSE,
                            smooth = TRUE,
                            legend.option = list(
                              orient = 'horizontal',
                              
                              left = 'center',
                              top = 'auto',
                              borderColor = '#ccc',
                              borderWidth = 0),
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
                            markline.option = list(
                              show.markline = FALSE,
                              yAxis = sapply(data,mean),
                              name = 'Mean'
                            ),
                            selected = list(
                              show = FALSE,
                              jscode = ''
                            ),
                            show.maxmin = FALSE){

  data <- isolate(data)

  data <- .process_NA(data)

  # check the type of line.width
  if((class(line.width) %in% c("numeric", "integer")) == FALSE){
    stop("The line.width should either be numeric or integer.")
  }

  # check the type of point.size
  if((class(point.size) %in% c("numeric", "integer")) == FALSE){
    stop("The point.size should either be numeric or integer.")
  }

  # check the value of point.type
  unique_point.types <- unique(point.type)
  if(sum(sapply(unique_point.types, function(x){x %in% c('emptyCircle', 'circle', 'rect', 'roundRect', 'triangle', 'diamond', 'pin', 'arrow')})) != length(unique_point.types)){
    stop("The point.type can only be 'emptyCircle', 'circle', 'rect', 'roundRect', 'triangle', 'diamond', 'pin', 'arrow'.")
  }

  # check the value of line.type
  unique_line.types <- unique(line.type)
  if(sum(sapply(unique_line.types, function(x){x %in% c("solid", "dashed", "dotted")})) != length(unique_line.types)){
    stop("The line.type can only be 'solid', 'dashed', or 'dotted'.")
  }

  # check the value of 'step'
  if((step %in% c('null', 'start', 'middle', 'end')) == FALSE){
    stop("The 'step' can only be 'null', 'start', 'middle' or 'end'.")
  }


  n_of_category <- dim(data)[2]
  # Check the length of line.width
  if(length(line.width) != 1){
    if(length(line.width) != n_of_category){
      stop("The length of line.width should either be one or the same as the number of categories.")
    }
  } else {
    line.width <- rep(line.width, n_of_category)
  }

  # check the length of line.type
  if(length(line.type) != 1){
    if(length(line.type) != n_of_category){
      stop("The length of line.type should either be one or the same as the number of categories.")
    }
  } else {
    line.type <- rep(line.type, n_of_category)
  }

  # Check the length of point.size
  if(length(point.size) != 1){
    if(length(point.size) != n_of_category){
      stop("The length of point.size should either be one or the same as the number of categories.")
    }
  } else {
    point.size <- rep(point.size, n_of_category)
  }

  # Check the length of point.type
  if(length(point.type) != 1){
    if(length(point.type) != n_of_category){
      stop("The length of point.type should either be one or the same as the number of categories.")
    }
  } else {
    point.type <- rep(point.type, n_of_category)
  }

  # Check the value for theme
  theme_placeholder <- .theme_placeholder(theme)

  xaxis_name <- paste(sapply(row.names(data), function(x){paste("'", x, "'", sep="")}), collapse=", ")
  xaxis_name <- paste("[", xaxis_name, "]", sep="")
  legend_name <- paste(sapply(names(data), function(x){paste("'", x, "'", sep="")}), collapse=", ")
  legend_name <- paste("[", legend_name, "]", sep="")

  step_statement <- ifelse(step == 'null',
                           "step:false,",
                           paste("step:'", step, "',", sep = ""))

  # Convert raw data into JSON format
  series_data <- rep("", dim(data)[2])
  for(i in 1:length(series_data)){
    temp <- paste("{name:'", names(data)[i], "', type:'line', ",

                  step_statement,

                  paste("symbolSize:", point.size[i], ",", sep=""),
                  paste("symbol:'", point.type[i], "',", sep=""),

                  "itemStyle:{normal:{lineStyle: {width:", line.width[i],", type:'", line.type[i], "'}}},",

                  ifelse(stack_plot,
                         "stack: ' ', areaStyle: {normal: {}},",
                         " "),
                  "data:[",
                  paste(data[, i], collapse = ", "),
                  "]",
                  sep=""
    )
    # Make the line smooth or not
    if (smooth == TRUE) {
      temp <- paste(temp,
                    ",smooth: true",sep="")
    }
    # Show the max and min points
    if (show.maxmin == TRUE) {
      temp <- paste(temp, 
                    ",markPoint:{data:[{type:'max',name:'Max'},{type:'min',name:'Min'}]}",sep="")
    }
    # Show the customised markline
    if (markline.option$show.markline == TRUE) {
      temp <- paste(temp,
                    ",markLine:{data:[{yAxis:",
                    markline.option$yAxis[i],
                    ",name:'",
                    markline.option$name,
                    "'}]}")
    }
    temp <- paste(temp, "}", sep="")
    series_data[i] <- temp
  }
  series_data <- paste(series_data, collapse = ", ")

  
  # The label for yAxis
  if (y.name$show.yname == TRUE) {
    yAxis.js = paste("yAxis:{type:'value',",
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
    yAxis.js = "yAxis: {type: 'value'},"
  }
  
  
  js_statement <- paste("var " ,
                  div_id,
                  " = echarts.init(document.getElementById('",
                  div_id,
                  "')",
                  theme_placeholder,
                  ");",
                  "option_", div_id, " = {tooltip : {trigger: 'axis'}, ",

                  ifelse(show.tools,
                         "toolbox:{feature:{saveAsImage:{}}}, ",
                         ""),
                  # Slider for both xAxis and yAxis
                  ifelse(show.slider,
                        "dataZoom: [{type:'slider',xAxisIndex:0},{type:'slider',left:'3%',yAxisIndex: 0}],",
                          ''),
                  
                  # Legend Box: more options to change legend's layout
                  # Legend Box: change default selected items when initial plot (good for too many lines)
                  # selected$jscode should follow {'Item1':true,'Item2':false...}
                  # Only work when show.legend = TRUE
                  ifelse((show.legend == TRUE & selected$show == TRUE),
                         paste("legend:{data:",
                               legend_name,",",
                               "orient:'",legend.option$orient,"',",
                               "left:'",legend.option$left,"',",
                               "top:'",legend.option$top,"',",
                               "borderColor:'",legend.option$borderColor,"',",
                               "borderWidth:",legend.option$borderWidth,",",
                               "selected:",selected$jscode,
                               "},",
                               sep=""),
                         ""),
                  ifelse((show.legend == TRUE & selected$show == FALSE),
                         paste("legend:{data:",
                               legend_name,",",
                               "orient:'",legend.option$orient,"',",
                               "left:'",legend.option$left,"',",
                               "top:'",legend.option$top,"',",
                               "borderColor:'",legend.option$borderColor,"',",
                               "borderWidth:",legend.option$borderWidth,
                               "},",
                               sep=""),
                         ""),
                  ifelse((show.legend == FALSE & selected$show == TRUE),
                         paste(
                           "legend:{selected:",
                           selected$jscode,
                           "},",sep=""),
                         ""),
                  # Labels for yAxis
                  yAxis.js,"xAxis:{type:'category', boundaryGap: false, data:",
                  xaxis_name,
                  "}, series:[",
                  series_data,
                  "]};",

                  div_id,
                  ".setOption(option_",
                  div_id,
                  ");",
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


