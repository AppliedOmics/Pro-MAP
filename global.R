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
#library(naniar)

options(DT.options = list(pageLength = 25))
node = Sys.info()["nodename"]
data_dir = 'www/'

single_plot_height = 300

multi_plot_height = 600
if(node == 'WoW.local'){
	data_dir = '/Users/sgarnett/University of Cape Town/Antibody Arrays - Documents/Hazel/Data/Raw Data/'
	data_dir = '/Users/sgarnett/University of Cape Town/Antibody Arrays - Documents/SCaMP/Data/Prostate cancer EUR/'
	data_dir = 'www/'
	data_dir = '/Users/sgarnett/University of Cape Town/Antibody Arrays - Documents/Hazel/Data/HCW data files_06072021_HRM/'
}

paper_data_list = list(
	#'EUR cohort' = 'www/EUR cohort/',
	'GSH cohort' = 'www/GSH Cohort/',
	'JHB cohort' = 'www/JHB cohort/',
	'PC cohort' = 'www/PC cohort/',
	'TC cohort' = 'www/TR cohort/',
	'COVID HWC' = 'www/HWC'
)



# 
# if(grepl('srvubushi',node)){
# 	data_dir = '/mnt/MetaOmics/SharePoint/Antibody\ Arrays\ -\ Documents/Hazel/Data/Raw\ Data/'
# }

