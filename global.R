library("limma")
#library("tcltk")
library("ggplot2");  theme_set(theme_bw())
library(plotly)
library("tidyverse")
library(Biobase)
library(MSnbase)
library(reshape)
library(dplyr)
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

library(OptimalCutpoints)
library(magrittr)
library(tibble)
library(foreach) 

library(shinydashboard)

library(EnhancedVolcano)
#library(shinyjs)


source('shiny_functions.R')

#### options #####

options(DT.options = list(pageLength = 25),
				datatable.options = list(rownames= FALSE))



#### Version #####
app_version = 'pro'
(node = Sys.info()["nodename"])
if(grepl('metaomics',node)){
	app_version = 'basic'
}else{
	if(!grepl('Pro',getwd())){
		app_version = 'basic'
	}
}

#app_version = 'basic'


#### Defaults #####

single_plot_height = 300
max_heatmap_rows = 1000
multi_plot_height = 600

sep_categories = T
plot_lim = 'None'
collapse_boxplots = F
heatmap_order = 'Cluster'
min_corr = F
volcano_type = 'EnhancedVolcano'

##### Files #####


data_dir = 'www/'
if(node == 'WoW.local'){
	data_dir = '/Users/sgarnett/University of Cape Town/Antibody Arrays - Documents/Hazel/Data/Raw Data/'
	data_dir = '/Users/sgarnett/University of Cape Town/Antibody Arrays - Documents/SCaMP/Data/Prostate cancer EUR/'
	data_dir = 'www/'
	data_dir = '/Users/sgarnett/University of Cape Town/Antibody Arrays - Documents/Hazel/Data/HCW data files_06072021_HRM/'
}

pro_data_list = list(
	
	'TR cohort' = 'www/TR cohort/',
	'GSH cohort' = 'www/GSH Cohort/',
	'JHB cohort' = 'www/JHB cohort/',
	'PC cohort' = 'www/PC cohort/',
	#'TR cohort' = 'www/TR cohort/',
	'COVID HWC' = 'www/HWC',
	'EUR cohort' = 'www/EUR cohort/',
	'EUR cohort shors' = 'www/EUR cohort shorts/'
)


basic_data_list = pro_data_list[c('TR cohort','GSH cohort','JHB cohort')]


pro_dataset = 'www/TR Cohort/'
basic_dataset = 'www/TR Cohort/'



# 
# if(grepl('srvubushi',node)){
# 	data_dir = '/mnt/MetaOmics/SharePoint/Antibody\ Arrays\ -\ Documents/Hazel/Data/Raw\ Data/'
# }

