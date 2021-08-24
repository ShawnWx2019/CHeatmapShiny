#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @import dashboardthemes
#' @import shinydashboard
#' @import DT
#' @import stringr
#' @import tidyverse
#' @import ComplexHeatmap
#' @import InteractiveComplexHeatmap
#' @import circlize
#' @import shinythemes
#' @import colourpicker
#' @import shinyjqui
#' @noRd
app_ui <- function(request) {
  navbarPage(
    theme = shinytheme("spacelab"),
    ## Logo
    
    shinyDashboardLogoDIY(
      boldText = "Zhang Lab"
      ,mainText = "CHeatmap Shiny"
      ,textSize = 14
      ,badgeText = "v1.0"
      ,badgeTextColor = "white"
      ,badgeTextSize = 2
      ,badgeBackColor = "#40E0D0"
      ,badgeBorderRadius = 3
    ),
    tabPanel(
      useShinyjs(),
      ## title
      title = "Data cleaning",
      icon =  icon("Table"),
      ## sidebar
      sidebarLayout(
        div(id = "Sidebar",
            sidebarPanel(width = 2,
                         ## gene list input 
                         fileInput(
                           inputId = "input_df",
                           label = "Upload your data",
                           accept = c(".txt",".tsv",".xls",".tab")
                         ),
                         p("only accecpt Tab-delimited .txt, .tsv and .xls .tab file",
                           style = "color: #7a8788;font-size: 12px; font-style:Italic"),
                         radioButtons(
                           inputId = "na_treat",
                           label = "how to treat NA values",
                           choices = c("fill","remove entire row"),
                           selected = "fill"
                         ),
                         ## how to treat na values
                         textInput(
                           inputId = "na_tag",
                           label = "NA value tag",
                           value = "NA"
                         ),
                         textInput(
                           inputId = "na_replace",
                           label = "NA replace value",
                           value = 0
                         ),
                         ## how to treat noise
                         radioButtons(
                           inputId = "noise_remove",
                           label = "remove noise or not",
                           choices = c("TRUE","FALSE"),
                           selected = "FALSE"
                         ),
                         sliderInput(
                           inputId = "sample_percentage",
                           label = "Sample percentage",
                           min = 0,
                           max = 1,
                           step = 0.01,
                           value = 0.3
                         ),
                         textInput(
                           inputId = "noise_cutoff",
                           label = "the threshold of noise",
                           value = 0
                         ),
                         actionButton("start_clean","Start data cleaning")
            )),
        mainPanel(
          fluidPage(
            actionButton("toggleSidebar", 
                         "Toggle sidebar"),
            tabsetPanel(
              tabPanel(title = "Original table preview",height = "500px",width = "100%",collapsible = T,
                       icon = icon("table"),
                       DT::dataTableOutput(outputId = "Original_tbl")
              ),
              tabPanel(title = "Clean table preview",height = "500px",width = "100%",collapsible = T,
                       icon = icon("table"),
                       DT::dataTableOutput(outputId = "Clean_tbl")
              )
            )
          )
        )
      )
    ),
    tabPanel(
      useShinyjs(),
      ## title
      title = "Simple Heatmap",
      icon =  icon("chart-line"),
      ## sidebar
      sidebarLayout(
        div(id = "Sidebar2",
            sidebarPanel(width = 2,
                         ## 1.2. Specific single gene information 
                         fileInput(
                           inputId = "row_annotation",
                           label = "Upload row annotation file",
                           accept = c(".txt",".tsv",".xls",".tab")
                         ),
                         p("If you don't need annotation rows, keep it blank. do not upload any file hear!. only accecpt Tab-delimited .txt, .tsv and .xls .tab file, row names in 1st column, category in 2--n column, Header of table is required!",
                           style = "color: #7a8788;font-size: 12px; font-style:Italic"),
                         fileInput(
                           inputId = "col_annotation",
                           label = "Upload column annotation file",
                           accept = c(".txt",".tsv",".xls",".tab")
                         ),
                         p("If you don't need annotation columns, keep it blank. do not upload any file hear!. only accecpt Tab-delimited .txt, .tsv and .xls .tab file, column names in 1st column, category in 2--n column, Header of table is required!",
                           style = "color: #7a8788;font-size: 12px; font-style:Italic"),
                         selectInput(
                           inputId = "norm_method",
                           label = "normlization method",
                           choices = c("none","log2","log10","log2(x+1)","log10(x+1)"),
                           selected = "none"
                         ),
                         selectInput(
                           inputId = "scale_method",
                           label = "scale method",
                           choices = c(none = "none",
                                       row = "row",
                                       column = "col")
                         ),
                         actionButton("start_norm","start normalization")
            )),
        mainPanel(
          fluidPage(
            ###==========part 2 Gene expression===============
            actionButton("toggleSidebar2", 
                         "Toggle sidebar"),
            tabsetPanel(
              # title = "Expression profile",
              # id = "tabs2",height = "500px",width = "70%",
              tabPanel(title = "Table after normlized or scaled",height = "500px",width = "100%",
                       icon = icon("table"),
                       DT::dataTableOutput(outputId = "norm_tbl")
              ),
              ## table panal =========
              tabPanel(title = "basic setting",
                       icon = icon("chess-board"),
                       actionButton("basicSetting","Basic Setting"),
                       column(3,
                              h4("Basic setting"),
                              radioButtons(
                                inputId = "show_rownames",
                                label = "Show row names.",
                                choices = c(yes = "TRUE",no = "FALSE"),
                                selected = "FALSE"
                              ),
                              radioButtons(
                                inputId = "show_colnames",
                                label = "Show column names.",
                                choices = c(yes = "TRUE",no = "FALSE"),
                                selected = "FALSE"
                              ),
                       ),
                       column(4,
                              h4("Setting breaks and colors"),
                              textInput(
                                inputId = "break_min",
                                label = "minimal value",
                                value = "-4"
                              ),
                              textInput(
                                inputId = "break_mid",
                                label = "mid value",
                                value = "0"
                              ),
                              textInput(
                                inputId = "break_max",
                                label = "max value",
                                value = "4"
                              ),
                              colourpicker::colourInput(inputId = "colormin",
                                                        label = "Minimum",
                                                        value = "blue"),
                              colourpicker::colourInput(inputId = "colormid",
                                                        label = "Middle",
                                                        value = "white"),
                              colourpicker::colourInput(inputId = "colormax",
                                                        label = "Maxmum",
                                                        value = "red")
                       ),
                       column(3,
                              h4("Annotation location"),
                              radioButtons(
                                inputId = "row_anno_side",
                                label = "row annotation location",
                                choices = c("left","right"),
                                selected = "left"
                              ),
                              radioButtons(
                                inputId = "col_anno_side",
                                label = "col annotation location",
                                choices = c("top","bottom"),
                                selected = "top"
                              )
                       )
                       
              ),
              tabPanel(title = "hclust Setting",
                       icon = icon("brush"),
                       column(5, 
                              h4("cluster setting for row"),
                              offset = 1,
                              radioButtons(
                                inputId = "cluster_row",
                                label = "Cluster row",
                                choices = c(false = "FALSE",true = "TRUE"),
                                selected = "FALSE"
                              ),
                              selectInput(
                                inputId = "cluster_row_method",
                                label = "cluster row method",
                                choices = c("ward.D","ward.D2","single",
                                            "complete","average","mcquitty",
                                            "median","centroid"),
                                selected = "complete",
                                multiple = F),
                              selectInput(
                                inputId = "cluster_row_distance",
                                label = "cluster row distance",
                                choices = c("euclidean","maximum","manhattan",
                                            "canberra","binary","minkowski"),
                                selected = "euclidean",
                                multiple = F),),
                       column(5, offset = 1,
                              h4("cluster setting for column"),
                              radioButtons(
                                inputId = "cluster_col",
                                label = "Cluster column",
                                choices = c(false = "FALSE",true = "TRUE"),
                                selected = "FALSE"
                              ),
                              selectInput(
                                inputId = "cluster_col_method",
                                label = "cluster column method",
                                choices = c("ward.D","ward.D2","single",
                                            "complete","average","mcquitty",
                                            "median","centroid"),
                                selected = "complete",
                                multiple = F),
                              selectInput(
                                inputId = "cluster_col_distance",
                                label = "cluster column distance",
                                choices = c("euclidean","maximum","manhattan",
                                            "canberra","binary","minkowski"),
                                selected = "euclidean",
                                multiple = F)),
                       actionButton("start_hclust","Hclust setting"),
              ),
              tabPanel(title = "kmeans heatmap",
                       icon = icon("star"),
                       column(8, offset = 1,
                              h4("kmeans setting"),
                              textInput(
                                inputId = "km_row",
                                label = "k-means row number",
                                value = 0
                              ),
                              textInput(
                                inputId = "km_col",
                                label = "k-means column number",
                                value = 0
                              ),
                              textInput(
                                inputId = "km_repeat_time",
                                label = "k-means repeat times",
                                value = 100)
                       ),
                       actionButton("start_kmeans","Kmeans setting"),
                       
              ),
              tabPanel(title = "Generate Heatmap",
                       icon = icon("start"),
                       actionButton("get_hclust","Generate hclust heatmap"),
                       actionButton("get_kmeans","Generate kmeans heatmap"),
              )
            )
          ))))
  )
}



