The Single Channel Microarray Preprocessing pipeline (SCaMP) enables the easy upload of antibody array data from a variety of file formats. It then enables users to view the raw data, perform spot filterning, background correction, normalisation and significance testing

# SidePanel

Settings to adjust the file inputs and the display properties of the plots generated

### Data Upload

This section allows user to upload new data. Array files can be dragged into the browse window or selected from the file system.

- **Dataset** : switched between example datasets already uploaded. These can be used to explore the app. 
- **Upload array files** upload new array data. The app will accept .gpr and .txt files. 

### Array Properties

Here the foreground and background columns extracted from the array data can be assigned. These fields will be populated by default values, extracted from the uploaded array data. 

- **Array Chanel** : the fluorophore wavelenght used the read the probes
- **Array Column** : The measurement column used for foreground and background (usually median)
- **Foreground** and **Background** :  columns to use in the analysis. 
- **Spot Filtering** : 'TRUE" removes all probes that have an intensity value less than two standard deviations of background
- **Annotation Columns** : Additional annotation columns used in the Probes Tab. 

### Plot Properties

Alter the colour palletes and themes of the plots generated in the app. 

- **Plot Themes** : a selection of ggplot themes that can be used
- **Grey Scale** : ignors the colour pallete and creates greyscale plots
- **Colour Palettes** : a selection of RColourBrewer palletes. 

# MainPanel

The MainPanel displays the array processing outputs. The mainpanel is arranged as a  series or tabs. The tabs appear sequentially.

## File Details

When array files are uploaded into the app, the files are first checked for consistency. Information on the number of array files uploaded, the number of probes and annotation colums is displayed here. If there are any errors or inconsistencies in the uploaded file, warnings or errors are displayed. 
ERORRS with prevent furthern processing. Warning should be made not of, as some data may be missing from the final analysis, due to inconsistency in the data upload. 

# Samples
A sample template is extracted from the uploaded array files. This sample template can be downloaded (**Download**) as a tab delimited file (targets.txt) and edited in Excel. The file can then be Uploaded again (**Upload Sample File**). Additional annotation columns can be added. One of these can be used to group the samples into conditions (**Select Condition Column**). The condition column is used to colour subsequent plots in the app. This is not essential to the app functioning. but it does enhance the visualisation of data. The dataset can be subsetted only selecting some of the assigned conditions (**Select Conditions**). This can be useful in assessing the reproducibility of replicates.  

# Probes

A probes template is extracted from the uploaded array files. Array files use different annotation columns, a default probe identification column will be assigned, but can be altered using **Probe Column**.In addition a 'Category' column is added to the probe template. This is used to group probes and annotate the subsequent plots. Drop down menu's allow easy annotation of the probes the **Control Probes** and **Probes to Remove** menu will assign "control" and "remove" to the Category column. Probes annotated as remove will be removed before the normalisation step, so that they do not interfere with normalisation. Probes annotated as 'control' will be coloured in plots, and can be selected to be removed in the "Proteins Tab".

This probe template can be downloaded (**Download**) as a tab delimited file (spots.txt) and edited in Excel. The file can then be Uploaded again (**Upload Probe File**). Additional annotation can be added to the Category column, these will colour the subsequence plots and can also be seletect for removal. 


## Data Tables
The uploaded array file are processed using the limma function 'read.maimages'. This creates a EListRaw object, which is used for all subsequenct data processing. The Data Tables tab shows the raw data within the EListRaw object. 

- **Foreground** and **Background** : data can be visualised as 
	- **Table** : the raw intensity expression values as selected in the SidePanel
	- **CV** : Tables and plots of CV's for duplicate measurements for each probe
	- **Clustering** : A heatmap or dendrogram of the raw intensity values.
	
- **Spot Filtering** : within the EListRaw object there is a weights table. Filtered spots are given a zero, while other spots are given a 1. The results are visualised here as a table or a heatmap. 

## Proteins

A protein template is extracted from the condensed data table. A column from the spots table can be used as the **Protein Column**. **Control**'s can be selected using the drop down menu. These proteins will then be assigned as control in the Category column. 
The proteins template can be downloaded, edited in excel and uploaded again. Multiple protein annotation columns can be added. Using the **Drop by** menu in the SidePanel a column in the protein column can be selected. The **Category to Drop** menu will select an annotation, proteins with this annotation, will not be used in the final data output. 

###

#### Plots

Shows boxplots of the Expression intensity for the 
- Raw Data
		- Foreground Data 
- Background Corrected and Normalised
	- Data after log2 transformation, background corrected and normalisised
		- the data can be log2 transformed before normalisation using the **log2 transformation** radio button.
		- **Background Correction Method** and **Normalisation Method** can be selected using the top menus
- Array Weights
- Condensed Data
	- spots are condensed into proteins using the mean value for the replicae spots
	
#### Data

Array Weights and the final data table can be downloaded for futher use. 





# Recent additions 

- Spot filter Tab under PreProcessing Pipeline
- CV plots 
	- add categories separate CV's
- MA plots
	- using plotly
- Significance Testing
	- generetate contigency matrix for all conditions
- plots are now downloadable as png
- CV of triplicates for foreground and background 

	
	
#	Bugs to fix
	
- check annotation column assignment for difference file types. 
- Expression Set Downloads
- plots and heatmaps after arrays weight filtering
- check the requirements on the significance testing table


# Additions

- Error messages for all uploaded files
	
- Contingency Matrix
	- all vs control
	- manual contingency matrix ??
	
- All Methods
	- add tabs for plots. 
	
- Batch Normalise
	
	# Questions

	