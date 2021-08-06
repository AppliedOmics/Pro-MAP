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


#### PLOT and SERVER UI ####

plot_UI = function(id,name,title = NULL){
	ns <- NS(id)
	list(
		column(11,tags$h4(title)),
		column(1,downloadButton(ns(paste0(name,'_downloadPlot')), 'png')),
		column(12,plotOutput(ns(name)))
	)
}

plot_Server <- function(id,name,p) { 
	moduleServer(
		id,
		function(input, output, session) {
			
			output[[name]] = renderPlot({
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







