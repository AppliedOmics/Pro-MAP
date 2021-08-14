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
	if(app_version == 'full'){
		titlePanel("SCaMP Pro")
	}else{
		tagList(
			titlePanel("SCaMP: Single Channel Microarray Preprocessing pipeline"),
			tags$h5('A Robust Pipeline for the Pre-processing of Single Channel Microarrays'),
			tags$h6('Mowoe MO., Lennard K., Garnett S., Talbot J., Jonas E., Blackburn J, (',
							tagList(a("doi link - not available yet", href="doi link")),')'),
			tags$h6('R script for pipeline available on Github (',
							tagList(a("github link - not available yet", href="github link")),')')
			)
	}
	#titlePanel("SPOT-Pro Full")
	
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


table_UI = function(id,name){
	ns <- NS(id)
	list(
		column(11),
		column(1,downloadButton(ns(paste0(name,'_downloadData')), 'csv')),
		column(12,DT::dataTableOutput(ns(name)))
	)
}

table_Server <- function(id,name,df) { 
	moduleServer(
		id,
		function(input, output, session) {
			
			output[[name]] = DT::renderDataTable({
				df
			})
			
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

PlotTabs_UI <- function(id) {
	ns <- NS(id)
	list(
		tabsetPanel(
			tabPanel('Data Table',
							 DT::dataTableOutput(ns('table'))
							 ),
			tabPanel('Plots',
							 uiOutput(ns('boxplot_ui')),
							 uiOutput(ns('CV_plot_ui')),
							 uiOutput(ns('missing_plot_ui'))
			),
			tabPanel("MA Plots",
							 uiOutput(ns('MA_plot_ui')),
			),
			tabPanel('Clustering',
							 column(12,uiOutput(ns('Heatmap_ui')))
			))
	)
}

### Pro UI ###








