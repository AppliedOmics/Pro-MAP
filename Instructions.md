The **Single Channel Microarray Preprocessing pipeline (SCaMP)** enables the easy upload of antibody array data from various file formats. It then allows users to view the raw data, perform spot filtering, background correction, normalisation and significance testing. The app follows a series of steps, these instructions are arranged in the same manner. Each section in the instructions corresponds to a tab in the app.

# SidePanel

Settings to adjust the file inputs and the display properties of the plots generated.
### Data Upload

This section allows users to upload new data. Array files can be dragged into the browse window or selected from the file system.

- **Dataset**: switched between example datasets already uploaded. The example datasets can be used to explore the app functionality
- **Upload array files** upload new array data. The app will accept .gpr and .txt files. 

### Array Properties

Here the foreground and background columns extracted from the array data can be assigned. These fields will are populated by default values extracted from the uploaded array data. 

- **Array Chanel**: the fluorophore wavelength used the read the probes
- **Array Column**: The measurement column used for foreground and background (usually median)
- **Foreground** and **Background** :  columns to use in the analysis. 
- **Spot Filtering**: 'TRUE" removes all probes that have an intensity value less than two standard deviations of the background
- **Annotation Columns** : Additional annotation columns used in the Probes Tab. 

### Plot Properties

Alter the colour palletes and themes of the plots generated in the app. 

- **Plot Themes**: a selection of ggplot themes that can be used
- **Grey Scale**: ignores the colour pallete and creates greyscale plots
- **Colour Palettes** : a selection of RColourBrewer palletes. 

# MainPanel

The Main Panel displays the array processing outputs. The Main Panel is arranged as a  series of tabs. The tabs will appear sequentially as sections of the analysis are completed. Click on the **Next >>** tab to continue with the processing steps. 


## File Details

When array files are uploaded into the app, the files are first checked for consistency. Information on the number of array files uploaded, the number of probes and annotation columns are dispayed. If there are any errors or inconsistencies in the uploaded file, warnings (orange) or errors (red) are displayed. 
Errors will prevent further processing, while warnings should be taken note of, as some data may be missing from the final analysis due to the inconsistency in the data upload. 

# Samples
A sample template is extracted from the uploaded array files. This sample template can be downloaded (**Download**) as a tab-delimited file (targets.txt) and edited in Excel. The file can then be uploaded again (**Upload Sample File**). Additional annotation columns can be added. One of these annotation columns can be selected to group the samples into conditions (**Select Condition Column**). The condition column is used to colour subsequent plots in the app. This is not essential to the app functioning, but it does enhance the visualisation of data. The dataset can be subsetted by only selecting some of the assigned conditions (**Select Conditions**). This can be useful in assessing the reproducibility of replicates.

# Probes

A probes template is extracted from the uploaded array files. Array files use different annotation columns, a default probe identification column will be assigned, but can be altered using **Probe Column**. In addition, a 'Category' column is added to the probe template. This is used to group probes and annotate the subsequent plots. Dropdown menu's allow easy annotation of the probes using the **Control Probes** and **Probes to Remove** menus, which will will assign "control" and "remove" to the Category column respectively. Probes annotated as remove will be removed before the normalisation step, so that they do not interfere with normalisation. Probes annotated as 'control' will be labelled in plots, and can be selected to be removed in the "Proteins Tab".

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

	