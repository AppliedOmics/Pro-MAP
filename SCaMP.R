library("limma")
library("tcltk")
library("ggplot2")
library("tidyverse")

#---------------------------Read in data and filter spots----------------------------------------------------

#first make a targets file (.txt) with the following colunms
#Name, filename, and Disease state (include any other extra information if necessary)

targets <- readTargets((file.choose())) #Read in target file

myfun <- function(x, threshold = 2*sd){
	okred <- abs(x[,"F635 Median"]) < threshold*abs(x[,"B635Median"])
	+  as.numeric(okred) #Spot filtering 
}

# E – column name for median intensity for the foreground spots in .txt or .gpr file
# Eb – column namefor median intensity for the background spots .txt or .gpr file

E <- read.maimages(tk_choose.files(),
									 columns=list(E="F635 Median", Eb="B635 Median"),
									 annotation=c("Block", "Column", "Row", "ID", "Name", 
									 						 wt.fun=myfun))

#----------------------------Visualization of Raw data---------------------------------------------
boxplot(data.frame(log2(E$E)), 
				main = "Rawdata",
				ylab="Expression Intensity",
				col = "white",
				font =12,
				frame = FALSE)

#---------------------------Background correction----------------------------------

E_corr <- backgroundCorrect(E, method = "normexp")


#---------------------------------Normalization------------------------------------
E_norm <- log2(normalizeCyclicLoess(E, method = "fast"))
rownames(E_norm) <- paste(E$genes$ID) #ID represents column name of protein IDs/names 
colnames(E_norm) <- c(targets$Name) #Name represents column name of array identifiers

#----------------------------Visualization of Normalized data---------------------------------------------

boxplot(data.frame(log2(E_norm)), 
				main = "Rawdata",
				ylab="Expression Intensity",
				col = "white",
				font =12,
				frame = FALSE)

#---------------------------------Array weights----------------------------------

arrayw <- arrayWeights(E_norm)
barplot(arrayw, xlab="Array", ylab="Weight", col="cornflowerblue", las=2)
abline(h=0.5, lwd=1, lty=2)

#---------------------Condense and clean up data--------------------------------

data <- tibble::rownames_to_column(as.data.frame(E_norm), var = "Proteins")
data <- data %>% mutate_at(vars(2:ncol(data)), as.numeric)
data$Proteins <- gsub("\\.[[:digit:]]*$", "", data$Proteins)
data <- as.data.frame(data) %>% group_by(Proteins) %>%
	summarise_all(funs(mean))

drops <- c("control 1", "control2", "control3") 
#Name all your controls and empty spots to be dropped from the condensed dataset

data <- tibble::column_to_rownames(data, var = "Proteins")
data <- data[!(row.names(data) %in% drops), ]

#----------------------------Visualization of Condensed dataset-----------------------------------------

boxplot(data.frame(data), 
				main = "Normalized",
				ylab="Expression Intensity",
				col = "white",
				font =12,
				frame = FALSE)


