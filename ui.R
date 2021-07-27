
library(shiny)


shinyUI(fluidPage(


    titlePanel("SPOT-Pro Full"),
    #fluidRow(
        sidebarPanel(
          fluidRow(
            column(12,
                   
                   
                   column(4,uiOutput('debug_ui')),
                   column(4,uiOutput('pull_ui')),
                   column(4,uiOutput('update_ui')),
                   column(12,
                          uiOutput('select_datasets_ui'),   

        fileInput(
            inputId = "gpr_files", 
            label = "Choose gpr Files", 
            multiple = TRUE,
            accept = c(".gpr")
        ),
        
        #uiOutput('array_type_ui'),
        #uiOutput('array_colours_ui'),
        #uiOutput('array_column_ui'),
        #uiOutput('col_num_ui'),
        column(12,uiOutput('array_type_ui')),
        column(6,uiOutput('array_colours_ui')),
        column(6,uiOutput('array_column_ui')),
        #uiOutput('col_num_ui'),
        
        column(6,uiOutput('foreground_column_ui')),
        column(6,uiOutput('background_column_ui')),
        column(12,radioButtons('spot_filtering','Spot Filtering',c(F,'wtflags(0.1)','wtarea(150)','sd_filter'),F,inline = T)),
        uiOutput('annotation_columns_ui'),
        #uiOutput('sample_file_upload_ui'),
        selectInput('gg_theme','Plot Themes',c('theme_grey','theme_gray','theme_bw','theme_linedraw',
                                               'theme_light','theme_dark','theme_minimal','theme_classic','theme_void'),'theme_bw'),
        radioButtons('gg_grey','Grey Scale',c(FALSE,TRUE),inline = T),
        selectInput('r_col','Colour Palettes',c('scale_color_grey',rownames(brewer.pal.info)),'Dark2')
  
       
        )))),
    mainPanel(
        #uiOutput('example_data_text_ui'),
        column(3,uiOutput('select_conditions_column_ui')),
        column(9,uiOutput('condition_select_ui')),
        column(2,radioButtons('log_rb','log2 transform',c(FALSE,TRUE),TRUE)),
        column(2,radioButtons('min_corr','Correct Negatives',c(FALSE,TRUE))),
        
        column(4,selectInput('backgroundCorrect_method','Background Correct Method',c("none", "subtract", "movingmin","normexp"),"normexp")),
        column(4,selectInput('normalisation_method','Method',c("none", "scale", "quantile" , "cyclicloess"),'cyclicloess')),
        column(12,
          column(2,uiOutput('drop_cols_ui')),
          column(8,uiOutput('drop_rows_ui')),
          column(2,radioButtons('drop_by_weight','Drop arrays below array weight threshold',c(FALSE,TRUE),inline = T))
        ),
        column(9,uiOutput('protein_columns_ui')),
        column(3,radioButtons('heatmap_order','Heatmap',c('Cluster','Order'))),
        #column(3,selectInput('spot_collapse','Collapse Spots by',c('mean','median','sum','CV'))),
        column(12,tabsetPanel(
          tabPanel("Instructions",
                   uiOutput("readme_markdown_ui")
          ),
          tabPanel('File Details',
                   uiOutput('name_column_ui'),
                   DT::dataTableOutput('test_files_table')
          ),
          tabPanel('Targets',
                   column(8,uiOutput('target_file_upload_ui')),
                   column(2,downloadButton('download_targets',"Download")),
                   column(2,actionButton('reset_targets','Reset')),  
                   
                   column(12,DT::dataTableOutput('target_table'))
          ),
          tabPanel('Spots',
                   column(8,uiOutput('spot_file_upload_ui'),),
                   
                   column(2,downloadButton('download_spots',"Download")),
                   column(2,actionButton('reset_spots','Reset')), 
                   #column(12,uiOutput('gene_control_ui')),
                   #uiOutput('R_spot_control_ui'),
                   #uiOutput('G_spot_control_ui'),
                   uiOutput('both_spot_control_ui'),
                   column(12,DT::dataTableOutput('spot_table'))
                   
          ),
          
          tabPanel('Proteins',
                   column(8,uiOutput('protein_file_upload_ui'),),
                   
                   column(2,downloadButton('download_proteins',"Download")),
                   column(2,actionButton('reset_proteins','Reset')),
                   column(12,uiOutput('protein_control_ui')),
                   column(12,DT::dataTableOutput('proteins_table'))
          ),
        tabPanel('Data Tables',
                 tabsetPanel(
                   tabPanel('ForeGround',
                            tabsetPanel(selected = 'Plot',
                              tabPanel('Table',
                                  DT::dataTableOutput('foreground_table')
                              ),
                              tabPanel('Plot',
                                       uiOutput('foreground_heatmap_ui')
                              )
                            )
                      
                      ),
                   tabPanel('BackGround',
                            tabsetPanel(selected = 'Plot',
                              tabPanel('Table',
                                       DT::dataTableOutput('background_table')
                              ),
                              tabPanel('Plot',
                                       uiOutput('background_heatmap_ui')
                              )
                            )
                      ),
                   tabPanel('Spot Filtering 2sd',
                            textOutput('spot_filtering_threshold_text'),
                            tabsetPanel(selected = 'Plot',
                              tabPanel('Table',
                                       DT::dataTableOutput('spot_filtering_table')
                              ),
                              tabPanel('Plot',
                                       uiOutput('spot_filtering_heatmap_ui')
                              )
                            )
                   ),
                   tabPanel('Spot Filtering Elist',
                            #textOutput('spot_filtering_threshold_text'),
                            tabsetPanel(selected = 'Plot',
                                        tabPanel('Table',
                                                 DT::dataTableOutput('spot_filtering_E_table')
                                        ),
                                        tabPanel('Plot',
                                                 uiOutput('spot_filtering_E_heatmap_ui')
                                        )
                            )
                   ),
                   tabPanel('Signal to Noise',
                            tabsetPanel(selected = 'Plot',
                                        tabPanel('Table'),
                                        tabPanel('Plot'))
                            
                            )
                 )),
  
        tabPanel('Pre Processing Pipeline',
            radioButtons('collapse_boxplots','Collapse Boxplots',c(F,T),inline = T),
            tabsetPanel(
              tabPanel('Raw Data',
                column(12,plotOutput('E_boxplot')),
                column(12,plotOutput('E_CV_plot')),
                column(12,uiOutput('E_Heatmap_ui'))
                #column(12,uiOutput('E_corr_Heatmap_ui'))
              ),
              tabPanel('Background Correction',
                       column(12,plotOutput('E_corr_boxplot')),
                       column(12,plotOutput('E_corr_CV_plot')),
                       column(12,uiOutput('E_corr_Heatmap_ui'))
              ),
              
              tabPanel('Spot Filtering',
                       column(12,plotOutput('E_filter_boxplot')),
                       column(12,plotOutput('E_filter_CV_plot')),
                       column(12,uiOutput('E_filter_Heatmap_ui'))  
                       
              ),
              
              tabPanel('Normalisation',
                column(12,plotOutput('E_norm_boxplot')),
                column(12,plotOutput('E_norm_CV_plot')),
                column(12,uiOutput('norm_Heatmap_ui'))
              ),
              
          
              
             
              tabPanel('Array Weights',
                       #column(4,tags$h2('Array Weights')),
                       column(2,numericInput('array_weight_threshold','Array Weight Threshold','0.5')),
                       column(8),
                       column(2,downloadButton('download_arrayw',"Download")),
                       column(12,DT::dataTableOutput('arrayw_table')),
                       column(12,plotOutput('arrayw_barplot'))
                       ),
              tabPanel('Final Data',
                       
                       #column(6,tags$h2('Data')),
                       column(2,downloadButton('download_data',"Data Table")),
                       column(2,downloadButton('download_ExpSet',"ExpSet")),
                       column(2,downloadButton('download_MSnSet',"MSnSet")),
                       column(12,tags$h4(htmlOutput('data_dim_text'))),
                       
                       
                       column(12,DT::dataTableOutput('data_table')),
                        column(12,plotOutput('data_boxplot')),
                       column(12,plotOutput('data_CV_plot')),
                        column(12,uiOutput('data_Heatmap_ui'))
              ),
              tabPanel('Threshold',
                       uiOutput('threshold_control_col_ui'),
                       uiOutput('threshold_ui'),
                       uiOutput('threshold_output_ui'),
                       uiOutput('threshold_Heatmap_ui')
                       )
            )
        ),
        tabPanel('All Methods',
                 tags$h4('MA plot'),
                 plotOutput('MA_plot',height = multi_plot_height),
                 tags$h4('A mean Plot'),
                 plotOutput('Amean_plot',height = multi_plot_height),
                 tags$h4('M plot'),
                 plotOutput('M_plot',height = multi_plot_height),
                 tags$h4('Precision Plots'),
                 plotOutput('precision_plot_1',height = multi_plot_height),
                 plotOutput('precision_plot_2',height = multi_plot_height),
                 plotOutput('multi_line_plot',height = multi_plot_height)
                 
                 
        ),
        tabPanel('Significance Testing',
                 selectInput('mtc','Multiple testing Correction',c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"),'BH'),
                 tabsetPanel(
                   tabPanel('eBayes',
                            tabsetPanel(
                              tabPanel('Table',
                                       DT::dataTableOutput('eBays_table')
                                       ),
                              tabPanel('Plot',
                                       uiOutput('eBayes_Heatmap_ui'))
                            )
                            )
                 )
                 
                 )
        # tabPanel("Data",
        #          #tags$h2('E norm'),
        #          #column(12,DT::dataTableOutput('E_norm_table')),
        #          
        #          #column(10,tags$h2('Array Weights')),
        #          #column(2,downloadButton('download_arrayw',"Download")),
        #          
        #         
        #          
        #          column(6,tags$h2('Data')),
        #          column(2,downloadButton('download_data',"Data Table")),
        #          column(2,downloadButton('download_ExpSet',"ExpSet")),
        #          column(2,downloadButton('download_MSnSet',"MSnSet")),
        #          
        #          
        #          
        #          column(12,DT::dataTableOutput('data_table'))
        #          
        #          )
    ))

)

))
