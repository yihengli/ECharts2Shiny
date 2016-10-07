###########################################################################
# BubbleTL ----------------------------------------------------------------
###########################################################################

renderBubbleTL <- function(div_id, data,
                          theme = "default", auto.scale = TRUE,
                          sorted.TL = 'default',
                          x.option = list(
                            type = 'value',
                            min = 0,
                            max = 'default',
                            name = 'null',
                            unit = '',
                            unit.show = 'after'
                          ),
                          y.option = list(
                            type = 'value',
                            min = 0,
                            max = 'default',
                            name = 'null',
                            unit = '',
                            unit.show = 'after'
                          ),
                          size.option = list(
                            name = 'Size Dimension',
                            unit = '',
                            unit.show = 'after',
                            max = 'default'
                          ),
                          colorset = "['#3D3935','#B0232A','#A4B1CD','#FDC87D']",
                          running_in_shiny = TRUE) {
  
  data <- isolate(data)
  data <- .process_NA(data)
  
  # For scatter plots, the data must be prepared as a data.frame of 5 columns.
  # "x", "y", "group", "time", "z", "size"
  if(dim(data)[2] !=6 )
    stop("The data must be made up of three columns, 'x', 'y', 'group', 'time' 'z' and 'size'")
  
  if(sum(sapply(c("x", "y", "group", "time", "size"), function(x){x %in% names(data)})) != 5)
    stop("The data must be made up of three columns, 'x', 'y', 'group', 'time' 'z' and 'size'")
  
  # Check the value of theme
  theme_placeholder <- .theme_placeholder(theme)
  
  # Convert data to JS vars
  # time -> TimeLine
  var_tl <- sort(unique(data$time))
  if (sorted.TL != 'default') {
    if (length(setdiff(var_tl, sorted.TL)) != 0) {
      stop("The sorted time line must match your input column - time")
    } else {
      var_tl <- sorted.TL
    }
  }
  tl_name <- paste(sapply(var_tl, function(x){paste("'", x, "'", sep="")}), collapse=", ")
  tl_name <- paste("[", tl_name, "]", sep="")
  
  # group -> Legend
  group_names <- sort(unique(data$group))
  legend_name <- paste(sapply(group_names, function(x){paste("'", x, "'", sep="")}), collapse=", ")
  legend_name <- paste("[", legend_name, "]", sep="")
  
  # data frame -> values (a nested list)
  values = c()
  for (i in 1:length(var_tl)) {
    bubbles = c()
    for (s in 1:length(group_names)) {
      bubble <- data[data$time == var_tl[[i]] & data$group == group_names[[s]],]
      bubble.js <- paste(bubble$x, bubble$y, bubble$z, paste0("'", bubble$group, "'"), bubble$size, sep = ",")
      bubble.js <- paste("[", bubble.js, "]", sep="")
      bubbles[s] <- bubble.js
    }
    bubbles <- paste(sapply(bubbles, function(x){paste("", x, "", sep="")}), collapse=", ")
    values[i] <- paste("[", bubbles, "]", sep="")
  }
  values <- paste(sapply(values, function(x){paste("", x, "", sep="")}), collapse=", ")
  values <- paste("[", values, "]", sep="")
  
  # x,y option -> varsX and varsY
  if (x.option$max == 'default') {
    x.option$max = ceiling(max(data$x)/100)*100
  } else if (x.option$use & x.option$max < max(data$x)) {
    stop("The max of X axis must be greater than your input")
  } 
  
  if (y.option$max == 'default') {
    y.option$max = ceiling(max(data$y)/100)*100
  } else if (y.option$use & y.option$max < max(data$y)) {
    stop("The max of Y axis must be greater than your input")
  }
  
  if (size.option$max == 'default') {
    size.option$max = max(data$size)
  }
  
  varx.js = paste("['",x.option$type,"',",x.option$min,",",x.option$max,",'",
                  x.option$name,"','",x.option$unit,"','",x.option$unit.show,"']",sep="")

  vary.js = paste("['",y.option$type,"',",y.option$min,",",y.option$max,",'",
                  y.option$name,"','",y.option$unit,"','",y.option$unit.show,"']",sep="")
  
  varsize.js = paste("['",size.option$name,"','",size.option$unit,"','",
                     size.option$unit.show,"',",size.option$max,"]",sep="")
  
  js_statement <- paste("var " ,
                        div_id,
                        " = echarts.init(document.getElementById('",
                        div_id,
                        "')",
                        theme_placeholder,
                        ");",
                        "var itemStyle = {normal: {opacity: 0.8,
                              shadowBlur: 10,
                              shadowOffsetX: 0,
                              shadowOffsetY: 0,
                              shadowColor: 'rgba(0, 0, 0, 0.5)'
                            }
                        };",
                        "var data = {};
                        data.timeline = ",
                        tl_name,";",
                        "data.legend = ",
                        legend_name,";",
                        "data.values = ",
                        values,";",
                        "var varsX = ",
                        varx.js,";",
                        "var varsY =",
                        vary.js,";",
                        "var varsSize = ",
                        varsize.js,";",
                        "var UnitFormatter = function(value, unit, method) { 
                          if (method == 'before') {
                            formatter = unit + ' ' + value
                          } else {
                            formatter = value + ' ' + unit
                          }    
                          return(formatter)};option_", div_id, " = {",
                            "baseOption: {
                                timeline: {
                                  axisType: 'category',
                                  orient: 'vertical',
                                  autoPlay: true,
                                  inverse: true,
                                  playInterval: 1000,
                                  left: null,
                                  right: 0,
                                  top: 20,
                                  bottom: 20,
                                  width: 65,
                                  height: null,
                                  label: {
                                    normal: {
                                      textStyle: {
                                        color: '#999'
                                      }
                                    },
                                    emphasis: {
                                      textStyle: {
                                        color: '#fff'
                                      }
                                    }
                                  },
                                  symbol: 'none',
                                  lineStyle: {
                                    color: '#555'
                                  },
                                  checkpointStyle: {
                                    color: '#bbb',
                                    borderColor: '#777',
                                    borderWidth: 2
                                  },
                                  controlStyle: {
                                    showNextBtn: false,
                                    showPrevBtn: false,
                                    normal: {
                                      color: '#666',
                                      borderColor: '#666'
                                    },
                                    emphasis: {
                                      color: '#aaa',
                                      borderColor: '#aaa'
                                    }
                                  },
                                  data: data.timeline
                                },
                                title: [{
                                  'text': 'Wk1',
                                  textAlign: 'center',
                                  left: '63%',
                                  top: '65%',
                                  textStyle: {
                                    fontSize: 80,
                                    color: 'grey'
                                  }
                                }],
                                tooltip: {
                                  padding: 5,
                                  backgroundColor: '#222',
                                  borderColor: '#777',
                                  borderWidth: 1,
                                  formatter: function (obj) {
                                    var value = obj.value;
                                    return 'Name' + ':' + value[3] + '<br>'
                                    + varsX[3] + ':' + UnitFormatter(value[0], varsX[4], varsX[5]) + '<br>'
                                    + varsY[3] + ':' + UnitFormatter(value[1], varsY[4], varsY[5])  + '<br>'
                                    + varsSize[0] + ': ' + UnitFormatter(value[2], varsSize[1], varsSize[2]) + '<br>';
                                  }
                                },
                                grid: {
                                    left: '12%',
                                    right: '110',
                                    top: '12%',
                                },
                                legend: {
                                  data: data.legend
                                },
                                xAxis: {
                                  type: varsX[0],
                                  name: varsX[3],
                                  max: varsX[2],
                                  min: varsX[1],
                                  nameGap: 25,
                                  nameLocation: 'middle',
                                  nameTextStyle: {
                                    fontSize: 18
                                  },
                                  splitLine: {
                                    show: false
                                  },
                                  axisLabel: {
                                    formatter: UnitFormatter('{value}', varsX[4], varsX[5])
                                  }
                                },
                                yAxis: {
                                  type: varsY[0],
                                  name: varsY[3],
                                  max: varsY[2],
                                  min: varsY[1],
                                  nameTextStyle: {
                                    fontSize: 18
                                  },
                                  splitLine: {
                                    show: false
                                  },
                                  axisLabel: {
                                    formatter: UnitFormatter('{value}', varsY[4], varsY[5])
                                  }
                                },
                                series: [],
                                animationDurationUpdate: 1500,
                                animationEasingUpdate: 'quinticInOut'
                              },
                              options: []
                        };",
                        "for (var n = 0; n < data.legend.length; n++) {
                          option_",div_id,".baseOption.series.push({
                            name: data.legend[n],
                            type: 'scatter'
                          });
                        }",
                        "for (var n = 0; n < data.timeline.length; n++) {
                          series_append = [];for (var s = 0; s < data.legend.length; s++) {
                            series_append.push({
                              itemStyle: itemStyle,
                              data: [data.values[n][s]],
                              symbolSize: function(val) {
                                if (val[4] <= varsSize[3]) {return (val[4])}
                                else {return (varsSize[3])}
                              }
                            })
                          };option_",div_id,".options.push({
                            title: {
                              show: true,
                              'text': data.timeline[n] + ''
                            },
                            series: series_append
                          });
                        }",
                        div_id,
                        ".setOption(option_",
                        div_id,
                        ");",
                        
                        "window.addEventListener('resize', function(){",
                        div_id, ".resize()",
                        "});",
                        sep="")
  js_statement = gsub('\n', '', js_statement)
  js_statement = gsub('\n', ' ', js_statement)
  
  to_eval <- paste("output$", div_id ," <- renderUI({fluidPage(tags$script(HTML(\"",
                   js_statement,
                   "\")))})",
                   sep="")
  
  

  if(running_in_shiny == TRUE){
    eval(parse(text = to_eval), envir = parent.frame())
  } else {
    cat(to_eval)
  }
}
