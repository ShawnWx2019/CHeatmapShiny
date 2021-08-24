## Functions of WGCNA shiny App
suppressMessages(library(tidyverse))
suppressMessages(library(ComplexHeatmap))
suppressMessages(library(InteractiveComplexHeatmap))
suppressMessages(library(circlize))


# Datacleaning ------------------------------------------------------------

# Datacleaning ------------------------------------------------------------

#' Functions for data cleaning, aiming at remove na values or replace it.
#' @param x Data for heatmap.
#' @param na_value_treat How to deal with NA values, default is "fill" which means you have to provide a value to replace NA value eg:0.
#' @param na_tag If NA in the original data is not in the form of blank, but in the form of other characters such as "N/F", it needs to be marked.
#' @param na_replace_value The value will replace the NA value.
#' @param remove_noise For example, there may be noise in transcriptome data and metabolome data, and the noise needs to be removed before visualization
#' @param sample_percetage How to remove noise: where the value is greater than the threshold "noise_cutoff" in "sample_percetage" percent of the samples.
#' @param noise_cutoff How to remove noise: where the value is greater than the threshold "noise_cutoff" in "sample_percetage" percent of the samples.
#' @return dx a matrix like datExpr
#' @references https://jokergoo.github.io/ComplexHeatmap-reference/book/
#' @export
#' @import tidyverse
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>

Datacleaning <- function(
  x,
  na_value_treat = "fill",
  na_tag = "NA",
  na_replace_value,
  remove_noise = FALSE,
  sample_percetage,
  noise_cutoff
){
  ## 01. import datExpr
  row_tag <- colnames(x)[1]
  ## convert na_tag to NA
  x %>%
    mutate_if(is.numeric,as.character) %>% 
    replace(is.na(.),na_tag) %>% 
    ## convert to long table to remove na tags
    pivot_longer(
      cols = !row_tag,
      names_to = "sample",
      values_to = "value"
    ) %>%
    ## replace the na_tag with NA value
    mutate(
      value = gsub(pattern = na_tag,replacement = NA,x = value)
    ) %>%
    pivot_wider(
      names_from = "sample",
      values_from = "value"
    ) %>%
    column_to_rownames(row_tag)-> x.na
  ## remove or fill
  if (na_value_treat == "fill") {
    x.na %>%
      replace(is.na(.),na_replace_value) %>%
      mutate_if(is.character,as.numeric) -> x.clean
  } else if (na_value_treat == "remove entire row") {
    x.na %>%
      ## remove rows with na
      drop_na() %>% 
      mutate_if(is.character,as.numeric) -> x.clean
  }
  
  ## 02. remove noise
  if(remove_noise == TRUE) {
    x.final <- x.clean[apply(x.clean, 1, function(x) sum(x > noise_cutoff) > sample_percetage*ncol(x.clean)),]
  } else if (remove_noise == FALSE) {
    x.final <- x.clean
  }
  return(x.final)
}


# Normdata ----------------------------------------------------------------

#' Draw Basic heatmaps with not annotation
#' @param x Data for heatmap after cleaning.
#' @param normlize Normlization method for log2,log10,log2 x+1 ,log10 x+1
#' @param scale Z-score scaled data input, rowscale, colscale and none, Default = "row".
#' @return a Normlized heatmap data 
#' @references https://jokergoo.github.io/ComplexHeatmap-reference/book/
#' @export
#' @import tidyverse
#' @import ComplexHeatmap
#' @import InteractiveComplexHeatmap
#' @import circlize
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>

getNormTable <- function(x,normlize,scale) {
  ## step01 scale table
  
  if(normlize == "log2") {
    x <- log2(x)
  } else if (normlize == "log10") {
    x <- log10(x)
  } else if (normlize == "log2(x+1)"){
    x <- log2(x+1)
  } else if (normlize == "log10(x+1)"){
    x <- log10(x+1)
  } else if(normlize == "none"){
    x <- x
  }
  
  ## remove all stable rows
  tmp <- t(scale(t(x))) %>%
    as.data.frame() %>%
    na.omit()
  x_diff <- x[rownames(tmp),]
  
  ## scale
  if(scale == "row") {
    p.data <- tmp
  } else if (scale == "col") {
    p.data <- scale(x_diff)
  } else if (scale == "none") {
    p.data <- x_diff
  }
  return(p.data)
}


# BasicHeatmap ------------------------------------------------------------

#' Draw Basic heatmaps with not annotation
#' @param p.data normlized matrix
#' @param color_min Color for minmal values.
#' @param color_mid Color for middle values.
#' @param color_max Color for max values.
#' @param break_min min values.
#' @param break_mid mid values.
#' @param break_max max values.
#' @param cluster_type Hierarchical clustering or k-means.
#' @param cluster_row For hclust, cluster row or not, default = FALSE.
#' @param cluster_col For hclust, cluster col or not, default = FALSE.
#' @param km_col For k-means, number of expected group of column.
#' @param km_row For k-means, number of expected group of column.
#' @param km_repeat_time Avoid different results of k-means each time, Default = 100.
#' @param cluster_row_method see hclust.
#' @param cluster_row_distance see hclust.
#' @param cluster_col_distance see hclust.
#' @param cluster_col_method see hclust.
#' @param show_rowname Show rownames or not.
#' @param show_colname Show colnames or not.
#' @return a CompexHeatmap result
#' @references https://jokergoo.github.io/ComplexHeatmap-reference/book/
#' @export
#' @import tidyverse
#' @import ComplexHeatmap
#' @import InteractiveComplexHeatmap
#' @import circlize
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>

BasicHeatmap = function(
  p.data,
  color_min = "cyan",
  color_mid = "black",
  color_max = "gold",
  break_min = -4,
  break_mid = 0,
  break_max = 4,
  cluster_type = "hclust",
  cluster_row = FALSE,
  cluster_col = FALSE,
  km_col = 3,
  km_row = 3,
  km_repeat_time = 100,
  cluster_row_method = "complete",
  cluster_row_distance = "euclidean",
  cluster_col_method = "complete",
  cluster_col_distance = "euclidean",
  show_rowname = T,
  show_colname = T
) {
  
  ## set color
  if (break_min == "minimal number of input data") {
    break_min = min(p.data)
  }
  if (break_mid == "middle number of input data") {
    break_mid = (min(p.data)+max(p.data))/2
  }
  if (break_max == "max number of input data") {
    break_min = max(p.data)
  }
  
  col_fun = colorRamp2(c(break_min,break_mid,break_max), c(color_min, color_mid, color_max))
  if (cluster_type == "hclust") {
    a = Heatmap(
      ## datmat
      matrix = p.data,
      ## color
      col = col_fun,
      ## col and row names
      show_row_names = show_rowname,
      show_column_names = show_rowname,
      ## hclust for col and row
      cluster_rows = cluster_row,
      cluster_columns = cluster_col,
      clustering_distance_columns = cluster_col_distance,
      clustering_distance_rows = cluster_row_distance,
      clustering_method_columns = cluster_col_method,
      clustering_method_rows = cluster_row_method
    )
  }
  if (cluster_type == "kmeans") {
    a = Heatmap(
      ## datmat
      matrix = p.data,
      ## color
      col = col_fun,
      ## col and row names
      show_row_names = show_rowname,
      show_column_names = show_rowname,
      ## kmeans
      row_km = km_row,
      row_km_repeats = km_repeat_time,
      column_km = km_col,
      column_km_repeats = km_repeat_time,
    )
  }
  if (cluster_type == "none"){
    a = Heatmap(
      ## datmat
      matrix = p.data,
      ## color
      col = col_fun,
      ## col and row names
      show_row_names = show_rowname,
      show_column_names = show_rowname
    )
  }
  
  return(a)
}


# HeatAnno ----------------------------------------------------------------

#' generate annotations for row and column
#' @param anno_row annotations for row
#' @param anno_col annotations for column.
#' @references https://jokergoo.github.io/ComplexHeatmap-reference/book/
#' @return A list contains row_annotation and col_annotatin df will generated.
#' @export
#' @import tidyverse
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>

HeatAnno <- function(
  anno_row = NULL,
  anno_col = NULL
){
  ## anno_row
  if (is.null(anno_row)) {
    x.mat_row <- NULL
  } else {
    anno_row.tmp = data.frame(
      row.names = anno_row[,1],
      anno_row[,-1]
    )
    x.name = colnames(anno_row.tmp)
    if (length(x.name > 1)) {
      tmp1 <- list()
      for(i in 1:length(x.name)) {
        tmp1[[i]] = data.frame(
          row.names = rownames(anno_row.tmp),
          x = factor(anno_row.tmp[,i],levels = unique(anno_row.tmp[,i]))
        )
      }
      tmp1[[2]]
      x.mat_row <- bind_cols(tmp1)
      colnames(x.mat_row) = colnames(anno_row.tmp)
    } else {
      x.mat_row <- data.frame(
        row.names = rownames(anno_row.tmp),
        x = factor(anno_row.tmp[,1],levels = unique(anno_row.tmp[,1]))
      )
      colnames(x.mat_row) = colnames(anno_row)[-1]
    }
  }
  ## anno_col
  if (is.null(anno_col)) {
    x.mat_col <- NULL
  } else {
    anno_col.tmp = data.frame(
      row.names = anno_col[,1],
      anno_col[,-1]
    )
    x.name = colnames(anno_col.tmp)
    if (length(x.name > 1)) {
      tmp1 <- list()
      for(i in 1:length(x.name)) {
        tmp1[[i]] = data.frame(
          row.names = rownames(anno_col.tmp),
          x = factor(anno_col.tmp[,i],levels = unique(anno_col.tmp[,i]))
        )
      }
      tmp1[[2]]
      x.mat_col <- bind_cols(tmp1)
      colnames(x.mat_col) = colnames(anno_col.tmp)
    } else {
      x.mat_col <- data.frame(
        row.names = rownames(anno_col.tmp),
        x = factor(anno_col.tmp[,1],levels = unique(anno_col.tmp[,1]))
      )
      colnames(x.mat_col) = colnames(anno_col)[-1]
    }
  }
  anno <- list(
    x.mat_row = x.mat_row,
    x.mat_col = x.mat_col
  )
  return(anno)
}


# getFinalHeatmap ---------------------------------------------------------

#' Add annotations
#' @param basicPlot Heatmap generate by 1st step.
#' @param row_anno_side Location of row annotation
#' @param col_anno_side Location of column annotation
#' @param anno_row Need row annotation or not?
#' @param anno_col Need col annotation or not?
#' @references https://jokergoo.github.io/ComplexHeatmap-reference/book/
#' @return will generate final heatmap plot
#' @export
#' @import tidyverse
#' @import ComplexHeatmap
#' @import InteractiveComplexHeatmap
#' @import circlize
#' @author Shawn Wang <http://www.shawnlearnbioinfo.top>



getFinalHeatmap = function(
  basicPlot,
  row_anno_side = "left",
  col_anno_side = "top",
  anno_row,
  anno_col
){
  if (is.null(anno_row) & is.null(anno_col)) {
    final_plot <- basicPlot
  } else if (is.null(anno_row) & !is.null(anno_col)) {
    anno_col <- HeatmapAnnotation(Col_anno = as.matrix(anno_col))
    final_plot = attach_annotation(basicPlot,anno_col,side = col_anno_side)
  } else if (!is.null(anno_row) & is.null(anno_col)) {
    anno_row <- rowAnnotation(Row_anno = as.matrix(anno_row))
    final_plot = attach_annotation(basicPlot,anno_row,side = row_anno_side)
  } else if (!is.null(anno_row) & !is.null(anno_col)) {
    anno_col <- HeatmapAnnotation(Col_anno = as.matrix(anno_col))
    anno_row <- rowAnnotation(Row_anno = as.matrix(anno_row))
    final_plot = attach_annotation(basicPlot,anno_col,side = col_anno_side)
    final_plot = attach_annotation(final_plot,anno_row,side = row_anno_side)
  }
  return(final_plot)
}
