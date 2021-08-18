# Header

The **Single Channel Microarray Preprocessing pipeline (SCaMP)** enables the easy upload of antibody array data in various file formats. Users are able to view the raw data, perform spot filtering, background correction, normalization, data conosolidation and differential expression analysis. The webtool needs to be used in the sequential order by which the following instructions below are ordered.

# Side Panel

This section refers to the grey panel to the left, which contains file input settings and plot display properties. 


### Data Upload

This section allows users to upload new data. Array files can be dragged into the browse window or selected from the file system.

- **Example Datasets**: switched between example datasets already uploaded. The example datasets can be used to explore the app functionality.
- **Upload array files** upload new array data. The app will accept .gpr and .txt files. 

### Array Properties

Once array files are upload the foreground and background columns information is extracted from the array data. The app will attempt to determine the default values, these can be adjusted using the dropdown menus. 

- **Array Chanel**: the fluorophore wavelength used to the read the probes.
- **Array Column**: The measurement column used for foreground and background (usually median), determined by the array processing software.
- **Foreground** and **Background** :  columns to use in the pre-processing pipeline.  
- **Spot Filtering**: 'TRUE" removes all probes that have an intensity value less than two standard deviations of the background
- **Annotation Columns** : Additional annotation columns used in the Probes Tab. 

### Plot Properties

Alter the colour palletes and themes of the plots generated in the app. 

- **Plot Themes**: a selection of ggplot themes that can be used
- **Grey Scale**: ignores the colour pallete and creates greyscale plots
- **Colour Palettes** : a selection of RColourBrewer palletes. 

# MainPanel



The MainPanel to the right of the SidePanel, displays the array processing outputs. The Main Panel is arranged as a  series of tabs. The tabs will appear sequentially as sections of the analysis are completed. Click on the **Next >>** tab to continue with the processing steps. 




## File Details

When array files are uploaded into the app, the files are first checked for consistency. Information on the number of array files uploaded, the number of probes and annotation columns are dispayed. If there are any errors or inconsistencies in the uploaded file, warnings (orange) or errors (red) are displayed. 
Errors will prevent further processing, while warnings should be taken note of, as some data may be missing from the final analysis due to the inconsistency in the data upload. 

# Samples
A sample template is extracted from the uploaded array files. This sample template can be downloaded (**Download**) as a tab-delimited file (targets.txt) and edited in Excel. The file can then be uploaded again (**Upload Sample File**). Keep this file safe if you are intending to use the app again. 

An uploaded sample file needs to be a tab-delimted file with extension .txt. It needs a FileName column that corresponds to the array file names. It needs a Name column that will be used to label samples throughtout the app. Entries in the Name column need to be unique.

Additional annotation columns can be added. One of these annotation columns can be selected to group the samples into conditions (**Select Condition Column**). The condition column is used to colour subsequent plots in the app. This is not essential to the app functioning, but it does enhance the visualisation of data. The dataset can be subsetted by selecting some of the assigned conditions (**Select Conditions**). This can be useful in assessing the reproducibility of replicates. When sample annotaion is complete click **Next >>** to view the probes tab.

# Probes

A probes template is extracted from the uploaded array files. Array files assign annotation columns to probes, a default probe identification column will be assigned, but can be altered using the **Probe Column** dropdown menu. The app adds a 'Category' column to the probe template. This is used to group the probes and annotate the subsequent plots. 

Dropdown menu's allow easy annotation of the probes using the **Control Probes** and **Probes to Remove** dropdown menus, which will will assign "control" and "remove" to the Category column respectively. Probes annotated as remove will be removed before the normalisation step, so that they do not interfere with normalisation. Probes annotated as 'control' will be labelled in plots, and can be selected to be removed in the "Proteins Tab".

This probe template can be downloaded (**Download**) as a tab-delimited file (spots.txt) and edited in Excel. The file can then be Uploaded again (**Upload Probe File**). Additional annotations can be added to the Category column; these will be labelled in the plots and can also be selected for removal. 


## Data Tables
The uploaded array files are processed using the limma function 'read.maimages'. This creates a EListRaw object, which is used for all subsequent data processing. The Data Tables tab shows the raw data within the EListRaw object. 

- **Foreground** and **Background** : data can be visualised as 
	- **Table** : the raw intensity expression values as selected in the SidePanel
	- **CV** : Tables and plots of CV's for duplicate measurements for each probe
	- **Clustering** : A heatmap or dendrogram of the raw intensity values.
	
- **Spot Filtering**: within the EListRaw object there is a weights table. Filtered spots are given a zero, while other spots are given a 1. The results are visualised here as a table or a heatmap. 

## Proteins

Probes expression intensity values are condensed by using the mean expression intensity for duplicate probes. This results in a more condensed data table. 
A protein template is extracted from the condensed data table. An annotation column from the spots table can be selected (**Protein Column**) as the protein id to use when condensing the data. Like the probe table a Category column is used to annotate protein. Controls are automatically assigned from the probe table annotation. An additional "other' annotation can be selected using the **Other** drop-down menu. 

The protein template can be downloaded (**Download**) as a tab-delimited file (proteins.txt) and edited in Excel. The file can then be Uploaded again (**Upload Probe File**). Additional annotations can be added to the Category column. Additional annotation columns can also be added and used to label plots. This additional protein annotation information is retained when saving expression sets at the end of the analysis. 

#### Proteins removed from final data
Probes labelled 'remove' are removed before normalisation. Additional proteins can be removed from the Final Data table, this occurs post normalisation. These proteins are selected using the protein template annotation columns. The column used is selected by **Drop by**. The annotations to be removed are then selected by **Category to drop**. 'Control' is usually selected by default. 



## Pre-Processing Pipeline

This the sequential pre-processing of array data in the EListRaw object. 

### Raw Data 

Data is displayed as boxplot of heatmap of the expression intensity values, before any processing. 

### Background Correction. 

Expression intensity values after spot filtering and background correction are diplayed as boxplots or a heatmap. The background correction method used is selected using **Background Correction Method** on the top of the main panel. 

### Normalisation

Expression intensity values are normalised using the method selected by **Normalisation Method**. The values are then log2 transformed and displayed as boxplots or a heatmap. 

### Array Weight

Samples are weighted using the limma function 'arrayWeights'. The result are displayes as a downloadable table or barplot. These weight are applied when performing eBayes differential expression significance testing. 

### Final Data

Post normalisation the the selected proteins are removed. 

The results are diplayed as

- **Table**
	- the data table can be downloaded as tab-delimted file (data.txt) 
	- the data table can also be saved as an R expressionSet object, which incorporates sample and protein annotation information. 
		- this would allow subsequence custom analysis to be performed in R. 
- **boxplot**: of expression intensities per sample
- **CV plot**:  by condition
- **MA plot**
- **Heatmap**



## Differential Expression Significance Testing

### eBayes





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

	
