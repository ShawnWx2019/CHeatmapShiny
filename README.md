# CHeatmapShiny App

Based on ComplexHeatmap. A simple shiny app for upload files to generate heatmap by CoomplexHeatmap.

## Content
- Data cleaning
	1. Missing value treatment
		a. remove entire row: `dplyr::drop_na`
		b. replace NA value with a customized value.
	2. Noise remove
		a. `sample_percetage`: eg: remove genes which has fpkm value less than `noise_cutoff` of `sample_percetage` samples. 
		b. `noise_cutoff`: 
		c. If do not need, choose "none"

- Data normlization
	1. log transfer:
		a. log2(x) or  log10(x) 
		b. log2(x+1) or log10(x+1)
	2. Z-score:
		a.  row-scale
		b.  column-scale

- Cluster
	1. Hclust: 
		a. Distance: follow Rbase dist description: [dist: Distance Matrix Computation](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist)
		b. Method: follow Rbase hclust description: [hclust: Hierarchical Clustering]https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust()
	2. Kmeans:
		a. kmeans center number. how many groups you want.
		b. kmeans repeat time. Since the selection of the center point of k-means is random each time, it is recommended to increase the number of repetitions to ensure the stability of the result.

- Annotation
	1. Location:
		a. For column annotation top or bottom
		b. For row annotation left or right
		c. bugs: legend will mix all groupname because I haven't thought of a better way to separate them now.

## Install

```R
## install devtools
if (!require('devtools')) install.packages("devtools");
if (!require("CHeatmapShiny")) devtools::install_github("ShawnWx2019/CHeatmapShiny");
```

## Usage:

```R
library(CHeatmapShiny)
run_CHeatmapShiny()
```
