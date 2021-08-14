The SCaMP shiny enables the easy upload of antibody array data from a variety of formats. It then enables users to view the raw data, perform spot filterning,background correction, normalisation and significance testing


### SidePanel

#### Upload Data

**Dataset** allows the upload of different example datasets, to get an idea of what tha app does without requiring any data.

In the SideBar use **"Upload array files"** to upload your own data. The app will accept .gpr and .txt files. 

Once user data is uploaded options to select the **Foreground** and **Background** columns to use in the analysis. 
**Annotation Columns** can also be customised

### MainPanel

#### Targets
A target template is extracted from the uploaded array files. **Download** the target.txt file, edit in excell and **Upload Targets File** to update it. Separating the targets into  **Condition** will colour subsequent plots in the app.

#### Spots

A spots template is extracted from the uploaded array files. Spots to be removed from the Normalisation can be selected using the drop down menu. They will then be assigned as remove in the Category column. Alternatively this file can be downloaded, edited in excell and uploaded. Only spots assigned as remove will be removed from the analysis prior to normalisation. 

#### Proteins

A protein template is extracted from the condensed data table. A column from the spots table can be used as the **Protein Column**. **Control**'s can be selected using the drop down menu. These proteins will then be assigned as control in the Category column. 
The proteins template can be downloaded, edited in excel and uploaded again. Multiple protein annotation columns can be added. Using the **Drop by** menu in the SidePanel a column in the protein column can be selected. The **Category to Drop** menu will select an annotation, proteins with this annotation, will not be used in the final data output. 

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

	