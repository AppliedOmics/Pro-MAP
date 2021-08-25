library(shiny)
library(shinydashboard)
library(shinydisconnect)

library(tidyverse)
library(dplyr)
library(tibble)

library(Biobase)
library(MSnbase)
#library(reshape)

library(ComplexHeatmap)
library(circlize) #colorRamp
library(RColorBrewer)


library(limma)
library(OptimalCutpoints)
library(magrittr)
library(foreach) 


library(ggplot2);  theme_set(theme_bw())
library(plotly)

library(EnhancedVolcano)

#library(knitr)
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
max_heatmap_rows = 1500
multi_plot_height = 600

sep_categories = FALSE
plot_lim = 'None'
collapse_boxplots = FALSE
heatmap_order = 'Cluster'
min_corr = F
volcano_type = 'EnhancedVolcano'
drop_by_weight = FALSE
log_rb_default = TRUE
array_weight_threshold = 0.5
pvalue_select = 0.05
mtc = 'BH'
fc_cutoff = 1.5
probe_collapse_digits = FALSE
cont_matrix_comp = 'All'

##### Files #####
devel = F

data_dir = '/mnt/MetaOmics/SharePoint/Antibody Arrays - Documents/SCaMP/Data'
if(node == 'WoW.local'){
	data_dir = '/Users/sgarnett/University of Cape Town/Antibody Arrays - Documents/SCaMP/Data/'
	devel = T
}

if(grepl('devel',getwd())){
	devel = T
}

pro_data_list = list(
	
	'TR cohort' = 'www/TR cohort/',
	'GSH cohort' = 'www/GSH Cohort/',
	'JHB cohort' = 'www/JHB cohort/',
	'PC cohort' = file.path(data_dir,'PC cohort/'),
	'COVID HWC' = file.path(data_dir,'HWC'),
	'EUR cohort' = 'www/EUR cohort/'
)

if(!node == 'WoW.local'){
	pro_data_list = pro_data_list[c('TR cohort','GSH cohort','JHB cohort','EUR cohort','COVID HWC')]
}

basic_data_list = pro_data_list[c('TR cohort','GSH cohort','JHB cohort','EUR cohort')]


pro_dataset = 'www/TR cohort/'
basic_dataset = 'www/TR cohort/'



