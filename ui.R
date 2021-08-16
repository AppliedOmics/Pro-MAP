
library(shiny)


shinyUI(fluidPage(
 
  
    uiOutput('header_version'),
    
 
        #### sidebarPanel ####
        sidebarPanel(
          fluidRow(
            
         
        span(tags$h3('Data Upload'),style="color:#6b8eb7"),
                   column(4,uiOutput('debug_ui')),
                   column(4,uiOutput('pull_ui')),
                   column(4,uiOutput('update_ui')),
                   column(12,
                          uiOutput('select_datasets_ui')),   

        column(12,fileInput(
            inputId = "gpr_files", 
            label = "Upload array files", 
            multiple = TRUE
        )),
        tags$hr(),
        span(tags$h3('Array Properties'),style="color:#6b8eb7"),
        column(6,uiOutput('array_colours_ui')),
        column(6,uiOutput('array_column_ui')),
    
        
        column(6,uiOutput('foreground_column_ui')),
        column(6,uiOutput('background_column_ui')),
        column(12,radioButtons('spot_filtering','Spot Filtering',c(T,F),inline = T)),
        column(12,
        uiOutput('annotation_columns_ui')),
        
        tags$hr(),
        span(tags$h3('Plot Properties'),style="color:#6b8eb7"),
        column(12,
          selectInput('gg_theme','Plot Themes',c('theme_grey','theme_gray','theme_bw','theme_linedraw',
                                                 'theme_light','theme_dark','theme_minimal','theme_classic','theme_void'),'theme_bw'),
          radioButtons('gg_grey','Grey Scale',c(FALSE,TRUE),inline = T),
          selectInput('r_col','Colour Palettes',c('scale_color_grey',rownames(brewer.pal.info)),'Dark2')
          ),
        tags$hr(),
        uiOutput('pro_heading_ui'),
        #span(tags$h3('Pro Settings'),style="color:#6b8eb7"),
        column(12,
          uiOutput('app_version_ui'),
          uiOutput('sep_categories_ui'),
          uiOutput('collapse_boxplots_ui'),
          uiOutput('plot_lim_ui'),
          #radioButtons('sep_categories','Separate plots by category',c(F,T),T,inline = T),
          #radioButtons('plot_lim','Limit Plot Axis',c('None','Quantile','2x Quantile'),inline = T),
          
          uiOutput('heatmap_order_ui')#,
          
          #uiOutput('min_corr'),
          #radioButtons('apply_spot_filtering','Apply Spot Filtering',c(T,F),inline = T)
          )
          )
        
        ),
    #### mainPanel ####
    mainPanel(
      uiOutput('main_header_options_ui'),
        #uiOutput('example_data_text_ui'),
      # column(12,
      #   column(4,uiOutput('select_conditions_column_ui')),
      #   column(4,uiOutput('condition_select_ui')),
      #   column(4,uiOutput('cont_matrix_comp_ui'))
      #   ),
      # column(12,
      #   column(4,radioButtons('log_rb','log2 transform',c(FALSE,TRUE),TRUE,inline = T)),
      #   column(4,selectInput('backgroundCorrect_method','Background Correction Method',c("none", "subtract", "movingmin","normexp"),"normexp")),
      #   column(4,selectInput('normalisation_method','Normalisation Method',c("none", "scale", "quantile" , "cyclicloess"),'cyclicloess'))
      # ),
        
      
        column(12,tabsetPanel(id = 'main',
          #### _Instructions ####
          tabPanel(title = tags$h5("Instructions"), value = 'instructions',
                   uiOutput("instructions_markdown_ui")
          ),
          #### _File Details #####
          tabPanel(title = tags$h5('File Details'), value = 'files',
                   tags$h4(htmlOutput('file_num_text')),
                   uiOutput('test_files_text_cols_ui'),
                   uiOutput('test_files_text_rows_ui'),
                   
                   tags$hr(),
                   uiOutput('name_column_ui'),
                   tags$h4('Data Columns'),
                   tags$h5(htmlOutput('data_columns_text')),
                   uiOutput('data_columns_list_error_text'),
                   verbatimTextOutput('data_columns_list_text'),
                   tags$hr(),
                   DT::dataTableOutput('test_files_table')
          ),
          #### _targets ####
          tabPanel(title = uiOutput('target_label'),value = 'targets',
                   column(4,uiOutput('target_file_upload_ui')),
                   column(5,uiOutput('select_conditions_column_ui')),
                   column(2,downloadButton('download_targets',"Download")),
                   column(1,actionButton('reset_targets','Reset')), 
                   
                   column(12,uiOutput('condition_select_ui')),
                   column(12,uiOutput('target_upload_error_ui')),
                   #column(12,DT::dataTableOutput('target_table'))
                   column(12,uiOutput('target_table_ui'))
          ),
          #### _spots ####
          tabPanel(title = uiOutput('probe_label'),value = 'probes',
                   column(4,uiOutput('spot_file_upload_ui'),),
                   column(5,uiOutput('spot_columns_ui')),
                   column(2,downloadButton('download_spots',"Download")),
                   column(1,actionButton('reset_spots','Reset')), 
                   #column(12,uiOutput('gene_control_ui')),
                   #uiOutput('R_spot_control_ui'),
                   #uiOutput('G_spot_control_ui'),
                   #uiOutput('spot_columns_ui'),
                   column(12,
                     uiOutput('spot_control_ui'),
                     uiOutput('spot_remove_ui'),
                     #radioButtons('remove_spot_duplicates','Remove Spot Duplicated',c(T,F),F,inline = T),
                     #DT::dataTableOutput('spot_table'))
                     uiOutput('spot_table_ui')
                   )
                   
          ),
          
          
          #### _Data Tables ####
          tabPanel(title = tags$h5('Data Tables'),value = 'data',
                   tabsetPanel(
                     tabPanel('ForeGround',
                              tabsetPanel(
                                          tabPanel('Data Table',
                                                   uiOutput('foreground_table_ui'),
                                                   #DT::dataTableOutput('foreground_table')
                                          ),
                                         tabPanel('CV',
                                                  uiOutput('foreground_triplicate_cv_plot_ui')
                                         ),
                                         tabPanel('Heatmap',
                                                  uiOutput('foreground_heatmap_ui')
                                         )
                              )
                              
                              
                     ),
                     tabPanel('BackGround',
                              tabsetPanel(
                                          tabPanel('Table',
                                                   uiOutput('background_table_ui')
                                          ),
                                          tabPanel('CV',
                                                   uiOutput('background_triplicate_cv_plot_ui')
                                          ),
                                       
                                          tabPanel("HeatMap",
                                                   uiOutput('background_heatmap_ui')
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
                   column(4,uiOutput('protein_file_upload_ui'),),
                   column(5,uiOutput('protein_columns_ui')),
                   
                   column(2,downloadButton('download_proteins',"Download")),
                   column(1,actionButton('reset_proteins','Reset')),
                   column(12,uiOutput('protein_control_ui')),
                   column(3,uiOutput('drop_cols_ui')),
                   column(9,uiOutput('drop_rows_ui')),
                   #column(12,uiOutput('protein_columns_ui')),
                   column(12,uiOutput('proteins_upload_error_ui')),
                   #column(12,DT::dataTableOutput('proteins_table'))
                   column(12,uiOutput('protein_table_ui'))
          ),
          

        #### Pipeline ####
        tabPanel(title = tags$h5('Pre-Processing Pipeline'),value = 'pipeline',
            uiOutput('pipeline_ui')
            # tabsetPanel(
            # 
            #   tabPanel('Raw Data',
            #            uiOutput('Raw_tabs_ui')
            #            ),
            #   
            #   tabPanel('Spot Filtering',
            #            uiOutput('Raw_filter_tabs_ui')),
            #   
            #   tabPanel('Background Correction',
            #            uiOutput('Raw_corr_tabs_ui'),
            #   ),
            # 
            #   tabPanel('Normalisation',
            #            uiOutput('Raw_norm_tabs_ui')
            #   ),
            #   tabPanel('Array Weights',
            #           
            #            column(4,numericInput('array_weight_threshold','Array Weight Threshold','0.5')),
            #            column(1),
            #            column(4,radioButtons('drop_by_weight','Drop arrays below array weight threshold',c(FALSE,TRUE),inline = T)),
            #            column(1),
            #            column(2,downloadButton('download_arrayw',"Download")),
            #            column(12,DT::dataTableOutput('arrayw_table')),
            #            
            #            column(12,uiOutput('arrayw_barplot_ui'))
            #          
            #            ),
            #   tabPanel('Final Data',
            #            uiOutput('Data_tabs_ui')
            #            
            # 
            #   ),
            #   tabPanel('Optimal Cutpoints',
            #            tags$h5('Calculates optimal cutpoints, as a minimum value set for specificity'),
            #            uiOutput('threshold_control_col_ui'),
            #          
            #            uiOutput('threshold_ui'),
            #            #tags$h6('3'),
            #            uiOutput('threshold_output_ui'),
            #            #tags$h6('4'),
            #            uiOutput('threshold_Heatmap_ui')
            #            )
            # )
        ),
        
        ### ALL METHODS PLOTS ####
        tabPanel(title = tags$h5('All Methods'),value = 'all',
                 column(3,uiOutput('MA_correction_ui')),
                 column(3,uiOutput('MA_normalisation_ui')),
                 column(3,radioButtons('log_rb_M','log(M)',c(T,F),inline = T)),
                 #column(3,radioButtons('MA_quantile','Limit axes plots to Quantitles',c(T,F))),
                 column(3,radioButtons('multi_DE','Differential Expression',c(F,T),inline = T)),
                 #column(12,radioButtons('multi_top','topTable',c(F,T),inline = T)),
                 #tabsetPanel(
                #   tabPanel('Single',
                column(12),
                  tabsetPanel(
                       tabPanel('MA plot',
                          uiOutput('MA_plot_ui')
                       ),
                       tabPanel('A Plot',
                          uiOutput('A_plot_ui')
                       ),
                       tabPanel('M plot',
                          uiOutput('M_plot_ui')
                       ),
                       tabPanel('Precision Plots',
                          uiOutput('precision_plot_ui')
                       )
                    )
                 #  ),
                #   tabPanel('Differential Expression')
                # )
                 
                 
        ),
        
        ### Significance Testing ####
        tabPanel(title = tags$h5('Significance Testing'),value = 'sig',
                 column(3,uiOutput('cont_matrix_comp_ui')),
                 #column(4,selectInput('cont_matrix_comp','Contingency Matric Comparison',c('All','Control'),'All')),
                 column(3,numericInput('pvalue_select','p-value',0.05)),
                 column(3,selectInput('mtc','Multiple testing Correction',c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"),'BH')),
                 column(3,numericInput('fc_cutoff','Fold Change',1.5,step = 0.5)),
                 
                 column(12,tabsetPanel(
                   tabPanel('eBayes',
                            uiOutput('cont_matrix_text_ui'),
                            tabsetPanel(id = 'sig_panel',
                              tabPanel('Table',
                                       DT::dataTableOutput('eBays_table')
                                       ),
                              tabPanel('Plots',
                                       tabsetPanel(
                                         tabPanel('Volcano Plots',
                                                  uiOutput('volcano_type_ui'),
                                                  #radioButtons('volcano_type','Volcano plot type',c('ggplot','gg plotly','EnhancedVolcano'),inline = T),
                                                  uiOutput('volcano_plot_ui')
                                         ),
                                         
                                         tabPanel('MA plot',
                                                  plotlyOutput('stat_MA_plot', height = 500, inline = T)
                                                  #htmltools::div(style = "display:inline-block", plotlyOutput("stat_MA_plot",height = 600, width = '100%'))
                                                  ),
                                                  #plotlyOutput('volcano_plots')),
                                         tabPanel('Heatmap',
                                                  uiOutput('eBayes_Heatmap_ui')
                                                  )
                                       )
                              )
                                       
                            )
                            )
                 )
                 
                 ))
        )

    )

)

))
