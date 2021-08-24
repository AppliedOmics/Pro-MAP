# Header

The **Single Channel Microarray Preprocessing pipeline (SCaMP)** enables the easy upload of antibody array data in various file formats. Users are able to view the raw data, perform spot filtering, background correction, normalization, data conosolidation and differential expression analysis. The webtool needs to be used in the sequential order by which the following instructions below are ordered.


# Side Panel

This section refers to the grey panel to the left, which contains file input settings and plot display properties. 


### Data Upload

Array files, in .txt or .gpr format, and of any wavelength, can be selected from a file system or dragged into the browser by users.

- **Example Datasets**: Four example datasets, showcased in the linked paper (Mowoe et al., 2021), are available here to explore app functionality.

### Array Properties

Array information, such as foreground, background and ID columns, is extracted from uploaded data and is available in a dropdown menu. The default colunms used are set to median foreground and background and is editable by the user.

- **Array Chanel**: This refers to the fluorophore wavelengths used to the visualise the probes.
- **Array Column**: The measurement column used for foreground and background (default is median), determined by the image extraction software used.
- **Foreground** and **Background** :  Expression intensity columns used for microarray analysis
- **Spot Filtering**: If set to 'TRUE", this removes all probes that have a foreground intensity that is less than 2 standard deviations of the associated background
- **Annotation Columns** : Additional identifier columns for each array e.g., ID, Flags, Uniprot ID etc. 

### Plot Properties

Colour palettes and/or themes for plots generated in the app. 

- **Plot Themes**: a selection of ggplot themes that can be used
- **Grey Scale**: Creates greyscale plots regardless of themes chosen (default is set at FALSE)
- **Colour Palettes** : a selection of RColourBrewer palletes. 


# MainPanel

The Main Panel to the right of the aforementionedd Side Panel, displays the array processing outputs. The Main Panel is arranged as a  series of tabs, which appear sequentially as sections of the analysis are completed. Click on the **Next >>** tab to navigate through processing steps. 


## File Details

Following upload to the app, array files are checked for consistency. Information on the number of array files uploaded and probes, as well as annotation columns is dispayed. If there are any errors or inconsistencies in the uploaded file, warnings (orange) or errors (red) are displayed. 
Errors prevent further processing unless addressed. 
However, users should take note of warnings as they may affect analyses downstream.

# Metadata

An editable metadata template is extracted from the uploaded array files and can be downloaded by the user(**Download**) as a tab-delimited file (targets.txt) and edited in Excel to be re-uploaded by (**Upload Sample File**) ONLY as a tab-delimited file.

Each targets file requires a FileName column that corresponds to the array file names. It also requires a Name column, which should include unique values that will be used to identify each array downstream. Additional metadata columns, such as Disease state (most crucial), Age, Gender, and Race, can be added and modified at the users discretion. This file should be retained for future analysis of the same dataset.

Any of the metadata columns can be selected to subset data into conditions (**Select Condition Column**). This is not an essential step, however if applied it is used to group resulting preprocessing plots, which enhances data visualisation. It is also useful for assessing the reproducibility of replicates. 
Click **Next >>** to view the next tab.

# Probes

An editable Probes template is produced based on the uploaded array files.  A default probe identification column will be assigned, but can be altered using the **Spot Column** dropdown menu the contents of which are linked to the annotation column in the Side Panel. 
The 'Category' column in the probe template is used to group the probes and annotate resulting preprocessing plots. This colunm can be edited by the user using the: **Control Probes** dropdown menu, which enable users to identify control probes downstream (Note: These can be selected for removal following normalization in the Proteins Tab),
**Probes to Remove** dropdown menu, which enables the user to select probes, such as EMPTY probes, that should be removed prior to normalization.

ALternatively, the probe template can be downloaded (**Download**) as a tab-delimited file (spots.txt) and edited in Excel to be re-uploaded by the user (**Upload Probe File**). Additional annotations can be added to the Category column at the users' discretion to be labelled in resulting plots and or selected for removal. 

## Data Tables
The expression intensity and annotation data from uploaded array files are read into R, using the limma function 'read.maimages'. This creates an EListRaw object within which the foreground spots are weighted according to the spot filtering threshold described in the Array Properties section. Raw data can be inspected and visualized in this tab via three subtabs: 

- Within the **Foreground** and **Background** subtabs, users can visualize:
	- A **Table** which contains raw intensity expression values based on Array properties selected in the Side Panel,
	- **CV** data including tables, boxplots and desnity plots of triplicated or more measurements for each probe,
	- A **Heatmap** of the raw intensity values.
	
- Within the **Spot Filtering** subtab, users can visualise:
	- A weights **Table** which contains probe weights. Filtered spots are weighted as zero, while "good" spots are weighted as 1. 
	- A **Heatmap** of the weighted spots. However, if no filtering is needed for the arrays a message is shown to show this. 

## Proteins

Here an editable Proteins template based on the Probes tab is produced and includes the 'Category' column used to group probes.
Users can select probes to remove at the consolidation stage, following normalization using the:
**Drob by** dropdown menu, which enables users to select a grouping variable/s by which probes to be removed can be selected. 
	If **Category** is selected, users can choose from the grouping variables created previously in the Probes tab (e.g., control),
	If **Proteins** is selected, users can manually select probes to be removed after normalization.
Probes annotated as 'control' or any other grouping name set by the user can be selected to be removed in the **Category to drop** dropdown.

Alternatively, the protein template can be downloaded (**Download**) as a tab-delimited file (proteins.txt) and edited in Excel to be re-uploaded by the user (**Upload Probe File**). Additional annotations can be added to the Category column as well as additional columns used to label plots or select proteins for removal. Additional protein annotation are retained when saving expression sets at the end of the pre-processing analyses. 

## Pre-Processing Pipeline

This includes the pre-processing analysis of the raw data as described in Mowoe et al., (2021). Here data is spot filtered, background corrected, normalized, array weighted, and consolidated into a downloadable tab-delimited Datatable and an Expressionset which can be directly opened in R for further analysis outside of the app if desired. Data can be inspected and visualised in five subtabs:

-Within the **Raw Data**, **Background corrected**, and **Normalised** subtabs, users can visualise:
	- A boxplot of raw, background corrected, and background corrected, and normalized data, respectively. Data is grouped based on **Conditions** selected 			previously 	in the **Metadata** tab 
	- A heatmap showing heirarcichal clustering of raw, background corrected, and normalized data, respectively.
Thus, users are able to easily visualise changes to the dataset after each preprocessing step.

-Within the **Array weight** subtab, users can vsualise:
	-Calculated array weights in table form and as a boxplot with data grouped based on **Conditions** selected

-Within the -**Final data** subtab, data has been preprocessed, previously selected proteins are removed and data is consolidated using the means of triplicated or more probes; users are thus able to visualise this as:
	- A **Table** which contains pre-processed and consolidated expression intensity data
	- **Boxplot** of pre-processed and consolidated expression intensity data grouped based on the previously selected **Conditions**
	-A boxplot showing the **CV** of the probe groups based on previously selected **Conditions**
	-A barplot which conatiains the pecentage of **Missing values** in each array
	-An **MA plot** of the first array versus all the other arrays to give the user an idea of fold change within 	the dataset
	-Euclidean heirarcichal **Clustering** of the data based on the proteins and the arrays to determine relationships.

All tables are downloadable as tab-delimited datasets or Esets for use directly in R and all plots are downloadable as vectorised png files in every tab.

Users may download data to be analysed elsewhere or move on to the next tab for differential expression analysis within the app.

## Differential Expression Analysis

Here, A linear model, using calculated array weights, is fit to the normalized data to fully model the systematic part of the data and determine variability between the groups. Contrasts matrices are then pulled from the metadata to determine the variability in the data based on comparisons of interest. This is selected based on the previously selected **Conditions**. Subsequently, an empirical Bayes method is used to moderate the standard errors of the estimated log-fold changes. 

The **Controls** dropdown allows users to select the condition to which all the other conditions should be compared. If no condition is selected here, all conditions will be compared to each other.
The **p-value** dropdown allows the user to manually select or fill in a probability value below which probes will be considered differentially expressed between conditions. Commonly used p-values include 0.001, 0.01, and 0.5
The **Multiple Testing Correction** dropdown allows the user to select the procedure used to adjust the p-values for multiple testing
The **Fold change** dropdown allows the user to manually select or fill in the fold change at and above which probes will be considered differentially expressed between conditions.

In this tab users are able to visualise the data in two subtabs:

-The **Table** contains a dataframe with a row for the proteins and several colunms including log fold change(logFC), average expression (AveExpr), moderated t statistic(t), p-value, adjusted p-value, log odds that the probe is differentially expressed (B), and a threshold based on the p-value selected (TRUE - adj p-value is below set p-value, FALSE - adj p-value is above set p-value)
-The **Plots** contains three plots:
	-An interactive **Volcanoplot**, which shows probes with large fold changes that may be of biological significance 
	-An interactive **MA plot** which potrays differential expression
	- A **Heatmap**, which shows relationships between arrays and probes based on a euclidean method of clustering


All tables are downloadable as tab-delimited datasets or Esets for use directly in R and all plots are downloadable as vectorised png files in every tab.


For further assistance contact
Dr. S. Garnett - provide email 
M. Mowoe - m_mowoe@yahoo.co.uk

#	Bugs to fix
	
- check annotation column assignment for difference file types. 
- Expression Set Downloads
- plots and heatmaps after arrays weight filtering
- check the requirements on the significance testing table


# Additions

- Error messages for all uploaded files
	
- Contingency Matrix (not necessary for now)
	- all vs control
	- manual contingency matrix ??
	
- All Methods
	- add tabs for plots. 
	
- Batch Normalise (necessary)
	
	# Questions

	
