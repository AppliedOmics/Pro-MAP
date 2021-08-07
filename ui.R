
library(shiny)


shinyUI(fluidPage(


    titlePanel("SPOT-Pro Full"),
    #fluidRow(
        #### sidebarPanel ####
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
        #column(12,uiOutput('array_type_ui')),
        column(6,uiOutput('array_colours_ui')),
        column(6,uiOutput('array_column_ui')),
        #uiOutput('col_num_ui'),
        
        column(6,uiOutput('foreground_column_ui')),
        column(6,uiOutput('background_column_ui')),
        column(12,radioButtons('spot_filtering','Spot Filtering',c(T,F),inline = T)),
        uiOutput('annotation_columns_ui'),
        #uiOutput('sample_file_upload_ui'),
        selectInput('gg_theme','Plot Themes',c('theme_grey','theme_gray','theme_bw','theme_linedraw',
                                               'theme_light','theme_dark','theme_minimal','theme_classic','theme_void'),'theme_bw'),
        radioButtons('gg_grey','Grey Scale',c(FALSE,TRUE),inline = T),
        selectInput('r_col','Colour Palettes',c('scale_color_grey',rownames(brewer.pal.info)),'Dark2'),
        radioButtons('plot_lim','Limit Plot Axis',c('None','Quantile','2x Quantile'),inline = T),
        radioButtons('collapse_boxplots','Collapse plot by condition',c(F,T),inline = T),
        radioButtons('heatmap_order','Heatmap',c('Cluster','Order','None','dend'),inline = T),
        radioButtons('min_corr','Correct Negatives',c(FALSE,TRUE),inline = T),
        radioButtons('apply_spot_filtering','Apply Spot Filtering',c(T,F),inline = T)
        
        )))),
    #### mainPanel ####
    mainPanel(
        #uiOutput('example_data_text_ui'),
        column(3,uiOutput('select_conditions_column_ui')),
        column(9,uiOutput('condition_select_ui')),
        column(8,
        column(4,radioButtons('log_rb','log2 transform',c(FALSE,TRUE),TRUE,inline = T)
               ),
        
        column(8,selectInput('backgroundCorrect_method','Background Correction Method',c("none", "subtract", "movingmin","normexp"),"normexp")
               
               ),
        #column(4,selectInput('normalisation_method','Method',c("none", "scale", "quantile" , "cyclicloess"),'cyclicloess')),
        
          
          #column(2,radioButtons('drop_by_weight','Drop arrays below array weight threshold',c(FALSE,TRUE),inline = T))
        ),
        #column(9,uiOutput('protein_columns_ui')),
        column(4,selectInput('normalisation_method','Normalisation Method',c("none", "scale", "quantile" , "cyclicloess"),'cyclicloess')
               #radioButtons('drop_by_weight','Drop arrays below array weight threshold',c(FALSE,TRUE),inline = T),
               ),
        #column(3,selectInput('spot_collapse','Collapse Spots by',c('mean','median','sum','CV'))),
        column(12,tabsetPanel(id = 'main',
          #### _Instructions ####
          tabPanel('Instructions',
                   uiOutput("readme_markdown_ui")
          ),
          #### _File Details #####
          tabPanel(title = tags$h5('File Details'), value = 'files',
                   tags$h4(htmlOutput('test_files_text')),
                   tags$hr(),
                   uiOutput('name_column_ui'),
                   tags$h4('Data Columns'),
                   tags$h5(htmlOutput('data_columns_text')),
                   tags$hr(),
                   DT::dataTableOutput('test_files_table')
          ),
          #### _targets ####
          tabPanel(title = uiOutput('target_label'),value = 'targets',
                   column(8,uiOutput('target_file_upload_ui')),
                   column(2,downloadButton('download_targets',"Download")),
                   column(2,actionButton('reset_targets','Reset')),  
                   column(12,uiOutput('target_upload_error_ui')),
                   column(12,DT::dataTableOutput('target_table'))
          ),
          #### _spots ####
          tabPanel(title = uiOutput('probe_label'),value = 'probes',
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
                   
          ),
          
          
          #### _Data Tables ####
          tabPanel(title = tags$h5('Data Tables'),value = 'data',
                   tabsetPanel(
                     tabPanel('ForeGround',
                              tabsetPanel(selected = 'Plot',
                                          tabPanel('Table',
                                                   uiOutput('foreground_table_ui'),
                                                   #DT::dataTableOutput('foreground_table')
                                          ),
                                          tabPanel('Plot',
                                                   tabsetPanel(
                                                     tabPanel('Heatmap',
                                                              uiOutput('foreground_heatmap_ui')
                                                     ),
                                                     tabPanel('CV',
                                                              uiOutput('foreground_triplicate_cv_plot_ui')
                                                     )
                                                   )
                                                   
                                          )
                              )
                              
                              
                     ),
                     tabPanel('BackGround',
                              tabsetPanel(selected = 'Plot',
                                          tabPanel('Table',
                                                   uiOutput('background_table_ui')
                                          ),
                                          tabPanel('Plot',
                                                   tabsetPanel(
                                                     tabPanel("HeatMap",
                                                              uiOutput('background_heatmap_ui')
                                                     ),
                                                     tabPanel('CV',
                                                              uiOutput('background_triplicate_cv_plot_ui')
                                                     )
                                                   )
                                                   
                                          )
                              )
                     ),
                     tabPanel('Spot Filtering Elist',
                              tabsetPanel(selected = 'Plot',
                                          tabPanel('Table',
                                                   uiOutput('spot_filtering_table_ui')
                                          ),
                                          tabPanel('Plot',
                                                   uiOutput('spot_filtering_E_heatmap_ui')
                                          )
                              )
                     )
                   )),
          
          
          #### _proteins ####
          tabPanel(title = uiOutput('protein_label'),value = 'proteins',
                   column(8,uiOutput('protein_file_upload_ui'),),
                   
                   column(2,downloadButton('download_proteins',"Download")),
                   column(2,actionButton('reset_proteins','Reset')),
                   column(12,uiOutput('protein_control_ui')),
                   column(3,uiOutput('drop_cols_ui')),
                   column(9,uiOutput('drop_rows_ui')),
                   column(12,uiOutput('protein_columns_ui')),
                   column(12,uiOutput('proteins_upload_error_ui')),
                   column(12,DT::dataTableOutput('proteins_table'))
          ),
          

        #### Pipeline ####
        tabPanel(title = tags$h5('Pre Processing Pipeline'),value = 'pipeline',
            tabsetPanel(
        
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
            )
        ),
        tabPanel(title = tags$h5('All Methods'),value = 'all',
                 column(3,uiOutput('MA_correction_ui')),
                 column(3,uiOutput('MA_normalisation_ui')),
                 column(3,radioButtons('log_rb_M','log(M)',c(T,F),inline = T)),
                 column(3,radioButtons('MA_quantile','Limit axes plots to Quantitles',c(T,F))),
                column(12,
                 tags$h4('MA plot'),
                 uiOutput('MA_plot_ui'),
                 #plotOutput('MA_plot',height = multi_plot_height),
                 tags$h4('A mean Plot'),
                 #plotOutput('Amean_plot',height = multi_plot_height),
                 uiOutput('Amean_plot_ui'),
                 tags$h4('M plot'),
                 uiOutput('M_plot_ui'),
                 #plotOutput('M_plot',height = multi_plot_height),
                 tags$h4('Precision Plots'),
                 uiOutput('precision_plot_ui')
                 #plotOutput('precision_plot_1',height = multi_plot_height),
                 #plotOutput('precision_plot_2',height = multi_plot_height)
                 )
                 #plotOutput('multi_line_plot',height = multi_plot_height)
                 
                 
        ),
        tabPanel(title = tags$h5('Significance Testing'),value = 'sig',
                 selectInput('mtc','Multiple testing Correction',c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"),'BH'),
                 tabsetPanel(
                   tabPanel('eBayes',
                            textOutput('cont_matrix_text'),
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
