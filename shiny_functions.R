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


data_heatmap_UI = function(id,plot_height){
	ns <- NS(id)
	list(
				column(11),
				column(1,downloadButton(ns('downloadData'), 'png')),
				column(12,plotOutput(ns('heatmap'),height = plot_height))
	)
}

data_heatmap_Server <- function(id,m,target_names,selected_targets,spot_names,pallete,cluster) {
	moduleServer(
		id,
		function(input, output, session) {
				#ns <- NS(id)
				#m = E()$E  
				print(id)
				print(dim(m))

				colnames(m)
				colnames(m) = target_names$Name
				m = m[,selected_targets$Name]

				plot_height = 300+(dim(m)[1]*10)
				if(cluster == 'dend'){
					plot_height = 300
					ht = dend_function(m,target_names)
				}else{
					plot_height = 300+(dim(m)[1]*10)
					ht = Heatmap_function(m,selected_targets,spot_names,pallete,cluster)
				}
				#print(ht)
				
				output$heatmap = renderPlot({
					print(ht)
				},height = plot_height)
				
				output$downloadData <- downloadHandler(
					filename = function() {paste0(id,'_Hcluster.png')},
					content = function(file) {
						png(file, height = plot_height)
						print(ht)
						dev.off()
					}
				)
			return(plot_height)
		}
	)
}

Heatmap_function = function(m, targets,spots,pallete,cluster = 'Cluster'){ 
	
	plot_min = min(as.numeric(m),na.rm = T)
	if(plot_min < 0){
		col_fun = colorRamp2(c(plot_min,0,mean(as.numeric(m),na.rm = T),max(as.numeric(m),na.rm = T)), c('blue',"white",'orange',"red"))
		
	}else{
		col_fun = colorRamp2(c(plot_min,mean(as.numeric(m),na.rm = T),max(as.numeric(m),na.rm = T)), c("white",'orange',"red"))
	}
	
	targets = targets %>% 
		filter(Name %in% colnames(m))
	
	ha_annotation = targets$Condition
	names(ha_annotation) = targets$Name
	(condition_col = brewer.pal(n = length(unique(targets$Condition)), name = pallete)[1:length(unique(targets$Condition))])
	(names(condition_col) = unique(targets$Condition))
	column_ha = HeatmapAnnotation(Condition = ha_annotation, col = list(Condition = condition_col))
	
	sample_order = targets %>% 
		arrange(Condition)
	
	
	if(cluster == 'Cluster'){
		ht = Heatmap(m,
								 col = col_fun,
								 row_labels = spots,
								 column_dend_height = unit(4, "cm"), 
								 row_dend_width = unit(4, "cm"),
								 column_names_side = "top",
								 top_annotation = column_ha
		)
	}
	if(cluster == 'Order'){
		ht = Heatmap(m,
								 col = col_fun,
								 row_labels = spots,
								 row_names_side = "left",
								 column_names_side = "top",
								 cluster_columns = FALSE,
								 cluster_rows = FALSE,
								 top_annotation = column_ha,
								 column_order = sample_order$Name)
	}
	if(cluster == 'None'){
		ht = Heatmap(m,
								 col = col_fun,
								 row_labels = spots,
								 row_names_side = "left",
								 column_names_side = "top",
								 cluster_columns = FALSE,
								 cluster_rows = FALSE,
								 top_annotation = column_ha)
	}
	ht
	
}

dend_function = function(m,targets){
	m
	
	
	dend <- m %>% scale %>% dist %>% 
		hclust %>% as.dendrogram
	
	library(dendextend)
	library(ggdendro)
	ggd1 <- as.ggdend(dend)
	ggplot(ggd1) 
	
	x <- as.matrix(scale(m))
	dd.row <- as.dendrogram(hclust(dist(t(x))))
	ddata_x <- dendro_data(dd.row)
	
	
	ddata_x$labels = ddata_x$labels %>%
		mutate(Name = label) %>% 
		left_join(targets)
	
	ddata_x$leaf_labels = ddata_x$labels$label
	
	p2 <- ggplot(segment(ddata_x)) +
		geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
	p2 = p2 + geom_text(data=label(ddata_x),
											aes(label=label, x=x, y=y-2, colour=ddata_x$labels$Condition, angle = 90))
	p2
}

