#names(outputOptions(output))

counterButton <- function(id, label = "Counter") {
	ns <- NS(id)
	tagList(
		actionButton(ns("button"), label = label),
		verbatimTextOutput(ns("out"))
	)
}

counterServer <- function(id) {
	moduleServer(
		id,
		function(input, output, session) {
			count <- reactiveVal(0)
			observeEvent(input$button, {
				count(count() + 1)
			})
			output$out <- renderText({
				count()
			})
			count
		}
	)
}

#### Functions #####


header_UI = function(app_version){
	#print(app_version)
	#browser()
	if(app_version == 'pro'){
		lst = list(titlePanel("SCaMP Pro"))
	}else{
		lst = list(
			titlePanel("SCaMP: Single Channel Microarray Preprocessing pipeline"),
			tags$h5('A Robust Pipeline for the Pre-processing of Single Channel Microarrays'),
			tags$h6('Mowoe MO., Garnett S., Lennard K., Talbot J., Jonas E., Blackburn J, (',
							tagList(a("doi link - not available yet", href="doi link")),')'),
			tags$h6('R script for pipeline available on Github (',
							tagList(a("github link - not available yet", href="github link")),')')
			)
	}
	#titlePanel("SPOT-Pro Full")
	lst
}

spots_tab_UI = function(){
	lst = tagList(tabPanel(title = uiOutput('probe_label'),value = 'probes',
					 column(8,uiOutput('spot_file_upload_ui'),),
					 
					 column(2,downloadButton('download_spots',"Download")),
					 column(2,actionButton('reset_spots','Reset')), 
					 #column(12,uiOutput('gene_control_ui')),
					 #uiOutput('R_spot_control_ui'),
					 #uiOutput('G_spot_control_ui'),
					 uiOutput('spot_columns_ui'),
					 uiOutput('both_spot_control_ui'),
					 radioButtons('remove_spot_duplicates','Remove Spot Duplicated',c(T,F),F,inline = T),
					 column(12,DT::dataTableOutput('spot_table'))
					 
	))
	do.call(tagList,lst)
}

#### PLOT and SERVER UI ####


plot_UI = function(id,name,title = NULL,plot_height = 400){
	ns <- NS(id)
	list(
		column(11,tags$h4(title)),
		column(1,downloadButton(ns(paste0(name,'_downloadPlot')), 'png')),
		column(12,plotOutput(ns(name),height = plot_height))
	)
}

plot_Server <- function(id,name,p,plot_height = 400) { 
	moduleServer(
		id,
		function(input, output, session) {
			
			output[[name]] = renderPlot({
				p
			},height = plot_height)
			
			output[[paste0(name,'_downloadPlot')]] <- downloadHandler(
				filename = function() { paste0(id,'_',name,'.png') },
				content = function(file) {
					ggsave(file,p, width = 9, height = 6)
				}
			)
		}
	)
}

ht_plot_Server <- function(id,name,p,plot_height,plot_width) { 
	moduleServer(
		id,
		function(input, output, session) {
			
			output[[name]] = renderPlot({
				p
			},height = plot_height)
			
			output[[paste0(name,'_downloadPlot')]] <- downloadHandler(
				filename = function() { paste0(id,'_',name,'.png') },
				content = function(file) {
					png(file, height = plot_height, width = plot_width)
					print(p)
					dev.off()
				}
			)
		}
	)
}

volcano_plot_Server <- function(id,name,p,type,plot_height = 400) { 
	moduleServer(
		id,
		function(input, output, session) {
			if(type != 'gg plotly'){
				output[[name]] = renderPlot({
					p
				},height = plot_height)
			}else{
				output[[name]] = renderPlotly({
					p
				})
			}
			
			output[[paste0(name,'_downloadPlot')]] <- downloadHandler(
				filename = function() { paste0(id,'_',name,'.png') },
				content = function(file) {
					ggsave(file,p, width = 9, height = 6)
				}
			)
		}
	)
}

plotly_UI = function(id,name,title = NULL){
	ns <- NS(id)
	list(
		column(11,tags$h4(title)),
		column(1,downloadButton(ns(paste0(name,'_downloadPlot')), 'png')),
		column(12,plotlyOutput(ns(name)))
	)
}

plotly_Server <- function(id,name,p) { 
	moduleServer(
		id,
		function(input, output, session) {
			
			output[[name]] = renderPlotly({
				p
			})
			
			output[[paste0(name,'_downloadPlot')]] <- downloadHandler(
				filename = function() { paste0(id,'_',name,'.png') },
				content = function(file) {
					ggsave(file,p, width = 9, height = 6)
				}
			)
		}
	)
}


table_UI = function(id,name,title = NULL){
	ns <- NS(id)
	list(
		column(11,tags$h4(title)),
		column(1,downloadButton(ns(paste0(name,'_downloadData')), 'csv')),
		column(12,DT::dataTableOutput(ns(paste0(name,'_table'))))
	)
}

table_Server <- function(id,name,df) { 
	moduleServer(
		id,
		function(input, output, session) {
			
			output[[paste0(name,'_table')]] = DT::renderDataTable({
				df
			},rownames = FALSE)
			
			output[[paste0(name,'_downloadData')]] <- downloadHandler(
				filename = function() {
					paste0(id,'_',name,'.csv')
				},
				content = function(file) {
					write.csv(df, file, row.names = FALSE)
				}
			)
		}
	)
}


### Plot Tabs ####

PlotTabs_UI <- function(id,values) {
	ns <- NS(id)
	if(values$app_version == 'pro'){
			lst = list(
				tabsetPanel(
					tabPanel('Data Table',
									 DT::dataTableOutput(ns('table'))
									 ),
					tabPanel('Plots',
									 uiOutput(ns('boxplot_ui')),
									 uiOutput(ns('CV_plot_ui')),
									 uiOutput(ns('missing_plot_ui'))
					),
					tabPanel('CV',
									 uiOutput(ns('triplicate_cv_plot_ui'))),
					tabPanel("MA Plots",
									 uiOutput(ns('MA_plot_ui')),
					),
					tabPanel('Heatmap',
									 column(12,uiOutput(ns('Heatmap_ui')))
					))
			)
	}else{
		lst = list(
			tabsetPanel(
				tabPanel('Boxplot',
								 uiOutput(ns('boxplot_ui'))
								 #uiOutput(ns('CV_plot_ui')),
								 #uiOutput(ns('missing_plot_ui'))
				),
				tabPanel('Heatmap',
								 column(12,uiOutput(ns('Heatmap_ui')))
				))
		)
	}
}

### Pro UI ###

CV_Server = function(id,name,plot_list){
	table_Server(id,name,plot_list$df_list$df_cv)
	table_Server(id,'triplicate_CV_mean',plot_list$df_list$df_cv_mean)
	plot_Server(id,'triplicate_CV',plot_list$p)
	plot_Server(id,'triplicate_CV_density',plot_list$d)
	plot_Server(id,'triplicate_diff',plot_list$b)
}

CV_UI = function(id,name,values){
	if(values$app_version == 'pro'){
		lst = list(tabsetPanel(
			tabPanel('Data Tables',
							 table_UI(id,paste0(name,'_mean'),"Summary of triplicate CV's"),
							 table_UI(id,name,'Triplicate spot statistics'),
							 ),
			tabPanel('Plots',
							 plot_UI(id,name,"Boxplots of CV's for probe replicates"),
							 plot_UI(id,paste0(name,'_density'),"Density plot of CV's for probe replicates"),
							 plot_UI(id,'triplicate_diff','Boxplot of the absolute difference between probe replicate means and medians') 
			)
		
		))
	}else{
		lst = list(tabsetPanel(
			tabPanel('Data Tables',
							 table_UI(id,paste0(name,'_mean'),"Summary of triplicate CV's"),
							 table_UI(id,name,'Triplicate spot statistics'),
			),
			tabPanel('Plots',
							 plot_UI(id,name,"Boxplots of CV's for probe replicates"),
							 plot_UI(id,paste0(name,'_density'),"Density plot of CV's for probe replicates") 
			)
			
		))
	}
				 
	lst
}

Pipeline_UI = function(values){
	if(values$app_version == 'pro'){
		
		lst = list(tabsetPanel(
				tabPanel('Raw Data',
								 uiOutput('Raw_tabs_ui')
				),
				
				tabPanel('Spot Filtering',
								 uiOutput('Raw_filter_tabs_ui')),
				
				tabPanel('Background Correction',
								 uiOutput('Raw_corr_tabs_ui'),
				),
				
				tabPanel('Normalisation',
								 uiOutput('Raw_norm_tabs_ui')
				),
				tabPanel('Array Weights',
								 
								 column(4,numericInput('array_weight_threshold','Array Weight Threshold','0.5')),
								 column(1),
								 column(4,radioButtons('drop_by_weight','Drop arrays below array weight threshold',c(FALSE,TRUE),inline = T)),
								 column(1),
								 column(2,downloadButton('download_arrayw',"Download")),
								 column(12,DT::dataTableOutput('arrayw_table')),
								 
								 column(12,uiOutput('arrayw_barplot_ui'))
								 
				),
				tabPanel('Final Data',
								 uiOutput('Data_tabs_ui')
								 
								 
				),
				tabPanel('Optimal Cutpoints',
								 tags$h5('Calculates optimal cutpoints, as a minimum value set for specificity'),
								 uiOutput('threshold_control_col_ui'),
								 
								 uiOutput('threshold_ui'),
								 #tags$h6('3'),
								 uiOutput('threshold_output_ui'),
								 #tags$h6('4'),
								 uiOutput('threshold_Heatmap_ui')
				)
		))
		
		
	}else{
		lst = list(tabsetPanel(
			tabPanel('Raw Data',
							 uiOutput('Raw_tabs_ui')
			),
		
			
			tabPanel('Background Correction',
							 uiOutput('Raw_corr_tabs_ui'),
			),
			
			tabPanel('Normalisation',
							 uiOutput('Raw_norm_tabs_ui')
			),
			tabPanel('Array Weights',
							 
							 column(4,numericInput('array_weight_threshold','Array Weight Threshold','0.5')),
							 column(1),
							 column(4,radioButtons('drop_by_weight','Drop arrays below array weight threshold',c(FALSE,TRUE),inline = T)),
							 column(1),
							 column(2,downloadButton('download_arrayw',"Download")),
							 column(12,DT::dataTableOutput('arrayw_table')),
							 
							 column(12,uiOutput('arrayw_barplot_ui'))
							 
			),
			tabPanel('Final Data',
							 uiOutput('Data_tabs_ui')
							 
							 
			)
		))
		
	}
}




