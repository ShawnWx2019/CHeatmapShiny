#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
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
app_server <- function( input, output, session ) {
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  observeEvent(input$toggleSidebar2, {
    shinyjs::toggle(id = "Sidebar2")
  })
  
  ## input dataframe
  raw_tbl <- reactive({
    file1 <- input$input_df
    if(is.null(file1)){return()}
    read.table(file = file1$datapath,
               sep="\t",
               header = T,
               stringsAsFactors = F)
  })
  ## raw df play
  output$Original_tbl = DT::renderDataTable({
    if(is.null(raw_tbl())){return()}
    tbl1 <- as.data.frame(raw_tbl())
    tbl1
  })
  
  clean<-reactiveValues(data=NULL)
  
  observeEvent(
    input$start_clean,
    {
      clean$na_tag = as.character(input$na_tag)
      clean$na_treat = as.character(input$na_treat)
      clean$na_replace = as.numeric(input$na_replace)
      clean$noise_remove = as.logical(input$noise_remove)
      clean$sample_percentage = as.numeric(input$sample_percentage)
      clean$noise_cutoff = as.numeric(input$noise_cutoff)
      clean$tbl <- Datacleaning(x = raw_tbl(),
                                na_value_treat = clean$na_treat,
                                na_tag = clean$na_tag,
                                na_replace_value = clean$na_replace,
                                remove_noise = clean$noise_remove,
                                sample_percetage = clean$sample_percentage,
                                noise_cutoff = clean$noise_cutoff)
    }
  )
  
  output$Clean_tbl = DT::renderDataTable({
    if(is.null(raw_tbl())){return()}
    if(is.null(clean$tbl)) {return()}
    clean$tbl
  })
  
  norm<-reactiveValues(data=NULL)
  
  observeEvent(
    input$start_norm,
    {
      norm$norm_method = as.character(input$norm_method)
      norm$scale_method = as.character(input$scale_method)
      norm$p_data = getNormTable(
        x = clean$tbl,
        normlize = norm$norm_method,
        scale = norm$scale_method
      )
    }
  )
  output$norm_tbl = DT::renderDataTable({
    if(is.null(norm$p_data)){return()}
    norm$p_data
  })
  ## 03.basicHeatmap
  row_anno_tbl <- reactive({
    file2 <- input$row_annotation
    if(is.null(file2)){return()}
    read.table(file = file2$datapath,
               sep="\t",
               header = T,
               stringsAsFactors = F)
  })
  col_anno_tbl <- reactive({
    file3 <- input$col_annotation
    if(is.null(file3)){return()}
    read.table(file = file3$datapath,
               sep="\t",
               header = T,
               stringsAsFactors = F)
  })
  ## annotation
  basic<-reactiveValues(data=NULL)
  observeEvent(
    input$basicSetting,
    {
      ## annotation 
      if (is.null(row_anno_tbl())) {
        basic$row_anno = NULL
      } else {
        basic$row_anno = row_anno_tbl() %>% 
          column_to_rownames(colnames(row_anno_tbl())[1])
      }
      if (is.null(col_anno_tbl())) {
        basic$col_anno = NULL
      } else {
        basic$col_anno = col_anno_tbl() %>% 
          column_to_rownames(colnames(col_anno_tbl())[1])
      }
      ## basic all
      basic$show_rownames = as.logical(input$show_rownames)
      basic$show_colnames = as.logical(input$show_colnames)
      ## color
      basic$break_min = as.numeric(input$break_min)
      basic$break_mid = as.numeric(input$break_mid)
      basic$break_max = as.numeric(input$break_max)
      basic$colormin= as.character(input$colormin)
      basic$colormid= as.character(input$colormid)
      basic$colormax= as.character(input$colormax)
      basic$row_anno_side = as.character(input$row_anno_side)
      basic$col_anno_side = as.character(input$col_anno_side)
    }
  )
  hclust_list <- reactiveValues(data = NULL)
  observeEvent(
    input$start_hclust,
    {
      ## hclust
      hclust_list$cluster_row = as.logical(input$cluster_row)
      hclust_list$cluster_row_method = as.character(input$cluster_row_method)
      hclust_list$cluster_row_distance = as.character(input$cluster_row_distance)
      hclust_list$cluster_col= as.logical(input$cluster_col)
      hclust_list$cluster_col_method = as.character(input$cluster_col_method)
      hclust_list$cluster_col_distance = as.character(input$cluster_col_distance)
    }
  )
  
  
  ht_list = reactive({
    p = ComplexHeatmap::Heatmap(
      matrix = as.matrix(norm$p_data),
      show_row_names = basic$show_rownames,
      show_column_names = basic$show_colnames,
      cluster_rows =    hclust_list$cluster_row,
      cluster_columns =   hclust_list$cluster_col,
      clustering_distance_columns =   hclust_list$cluster_col_distance,
      clustering_distance_rows =   hclust_list$cluster_row_distance,
      clustering_method_columns =   hclust_list$cluster_col_method,
      clustering_method_rows =   hclust_list$cluster_row_method,
      col = colorRamp2(c(basic$break_min, basic$break_mid, basic$break_max),c(basic$colormin, basic$colormid, basic$colormax))
    )
    p.final = getFinalHeatmap(
      basicPlot = p,
      row_anno_side = basic$row_anno_side,
      col_anno_side = basic$col_anno_side,
      anno_row = basic$row_anno,
      anno_col = basic$col_anno
    )
    draw(p.final)
  })
  
  observeEvent(input$get_hclust,{
    if(is.null(norm$p_data)){return()}
    InteractiveComplexHeatmapModal(input,output,session,ht_list())
  })
  
  kmeans_list <- reactiveValues(data = NULL)
  observeEvent(
    input$start_kmeans,
    {
      ## kmeans
      kmeans_list$km_row = as.numeric(input$km_row)
      kmeans_list$km_col = as.numeric(input$km_col)
      kmeans_list$km_repeat_time = as.numeric(input$km_repeat_time)
    }
  )
  
  ht_list1 = reactive({
    p2 = ComplexHeatmap::Heatmap(
      matrix = as.matrix(norm$p_data),
      show_row_names = basic$show_rownames,
      show_column_names = basic$show_colnames,
      column_km = kmeans_list$km_col,
      row_km = kmeans_list$km_row,
      row_km_repeats = kmeans_list$km_repeat_time,
      column_km_repeats = kmeans_list$km_repeat_time,
      col = colorRamp2(c(basic$break_min, basic$break_mid, basic$break_max), c(basic$colormin, basic$colormid, basic$colormax))
    )
    p.final2 = getFinalHeatmap(
      basicPlot = p2,
      row_anno_side = basic$row_anno_side,
      col_anno_side = basic$col_anno_side,
      anno_row = basic$row_anno,
      anno_col = basic$col_anno
    )
    draw(p.final2)
  })
  
  observeEvent(input$get_kmeans,{
    if(is.null(norm$p_data)){return()}
    InteractiveComplexHeatmapModal(input,output,session,ht_list1())
  })
}
