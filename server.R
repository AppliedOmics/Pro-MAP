#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(session, input, output) {
  
#### Functions ####
  
  observeEvent(input$disconnect, {
    session$close()
  })
  
  values = reactiveValues(
    metadata_file = NULL,
    probe_file = NULL,
    protein_file = NULL,
    metadata = NULL,
    probes = NULL,
    proteins = NULL
    
  )
  
  gg_theme_function = function(p){
    cmd = paste0('p = p + ',input$gg_theme,'()')
    cmd
    eval(parse(text = cmd))
    p + theme(axis.text.x = element_text(angle = 90),
              axis.text = element_text(size = 12),
              axis.title.x = element_blank()) 
  }
  
  gg_col_function = function(p){
    
    p = gg_theme_function(p)
    if(input$gg_grey){
      p = p + scale_colour_grey()
    }else{
      p = p + 
        #theme_classic() + 
        scale_colour_brewer(palette = input$r_col)
    }
    p
  }
  
  gg_fill_function = function(p){
    
    p = gg_theme_function(p)
    if(input$gg_grey){
      p = p + scale_fill_grey()
    }else{
      p = p + 
        #theme_classic() + 
        scale_fill_brewer(palette = input$r_col)
    }
    p
  }

  
  
  CV <- function(x){
    100*(sd(x)/mean(x))
  }
  
  sd_filter <- function(x){
    threshold = 2*sd(abs(x[,background_column()]))
    okred <- abs(x[,foreground_column()]) < threshold*abs(x[,background_column()])
    +  as.numeric(okred) #Spot filtering 
  } 
  
  
  #### Debugging and updating #####

  output$debug_ui = renderUI({
    if(!grepl('public',getwd())){
      actionButton('debug','Debug')
    }
  })
  
  observeEvent(input$debug,{
    browser()
  })
  
  
  output$update_ui = renderUI({

      if(node == 'WoW.local'){
        actionButton('update','Update (ssh)')
      }
    
  })
  observeEvent(input$update,{
    cmd = 'bash update.sh'
    print(cmd)
    system(cmd)
  })
  
  observeEvent(input$push,{
    cmd = 'bash push.sh'
    print(cmd)
    system(cmd)
  })
  
  output$pull_ui = renderUI({
    if(!grepl('public',getwd())){
      actionButton('pull','git pull')
    }
  })
  observeEvent(input$update,{
    cmd = 'bash pull.sh'
    print(cmd)
    system(cmd)
  })
  

  
  ##### Instructions #####
  output$instructions_markdown_ui = renderUI({
    #hideTab('main','Plots')
    #hideTab('main','Data')
    values$app_version = 'basic'
    values$sep_categories = sep_categories
    values$plot_lim = plot_lim
    values$collapse_boxplots = collapse_boxplots
    values$heatmap_order = heatmap_order
    values$min_corr = min_corr
    values$volcano_type = volcano_type
    values$log_rb = log_rb_default
    values$drop_by_weight = drop_by_weight
    values$array_weight_threshold = array_weight_threshold
    values$pvalue_select = pvalue_select
    values$mtc = mtc
    values$fc_cutoff = fc_cutoff
    values$probe_collapse_digits = probe_collapse_digits
    values$cont_matrix_comp = cont_matrix_comp 
    values$protein_column = NULL
    values$drop_col = 'Category'
    values$drop_row = 'control'
    print('readme_markdown_ui')
    includeMarkdown('Instructions.md')

  })
  
  
  output$sessioninfo = renderPrint({
    sessionInfo() 
  })
  
  ##### Default Values ######
  
  observeEvent(values$app_version,{
    if(values$app_version == 'pro'){
      showTab('main','all')
    }else{
      hideTab('main','all')
    }
  })
  
  output$header_version = renderUI({
    do.call(tagList,header_UI(values$app_version))
  })
  
  
  output$pro_heading_ui = renderUI({
    if(app_version == 'pro'){
      span(tags$h3('Pro Settings'),style="color:#6b8eb7")
    }
  })
  
  
  output$app_version_ui = renderUI({
    if(app_version == 'pro'){
      radioButtons('app_version','App Version',c('basic','pro'),'basic',inline = T)
    }
  })
  
  observeEvent(input$app_version,{
    values$app_version = input$app_version
  })
  
  
  output$sep_categories_ui = renderUI({ 
    if(values$app_version == 'pro'){
      radioButtons('sep_categories','Separate plots by category',c(F,T),sep_categories,inline = T)
    }
  })
  
  observeEvent(input$sep_categories,{
    values$sep_categories = input$sep_categories
  }) 
  
  output$plot_lim_ui = renderUI({ 
    if(values$app_version == 'pro'){
      radioButtons('plot_lim','Limit Plot Axis',c('None','Quantile','2x Quantile'),plot_lim,inline = T)
    }
  })
  
  observeEvent(input$plot_lim,{
    values$plot_lim = input$plot_lim
  })
  
  output$collapse_boxplots_ui = renderUI({ 
    if(values$app_version == 'pro'){
      radioButtons('collapse_boxplots','Collapse plot by condition',c(F,T),collapse_boxplots,inline = T)
    }
  })
  
  observeEvent(input$collapse_boxplots,{
    values$collapse_boxplots = input$collapse_boxplots
  })
  
  output$heatmap_order_ui = renderUI({ 
    if(values$app_version == 'pro'){
      radioButtons('heatmap_order','Heatmap',c('Cluster','Order','None','dend'),heatmap_order,inline = T)
    }
  })
  
  observeEvent(input$heatmap_order,{
    values$heatmap_order = input$heatmap_order
  })

  output$min_corr_ui = renderUI({ 
    if(values$app_version == 'pro'){
      radioButtons('min_corr','Correct Negatives',c(FALSE,TRUE),min_corr,inline = T)
    }
  })
  
  observeEvent(input$min_corr,{
    values$min_corr = input$min_corr
  })

  
  output$volcano_type_ui = renderUI({
    if(values$app_version == 'pro'){
      radioButtons('volcano_type','Volcano plot type',c('ggplot','gg plotly','EnhancedVolcano'),volcano_type,inline = T)
    }
  })

  observeEvent(input$volcano_type,{
    values$volcano_type = input$volcano_type
  })
  

  
  observeEvent(input$log_rb,{
    values$log_rb = input$log_rb
  })
  
  observeEvent(input$drop_by_weight,{
    values$drop_by_weight = input$drop_by_weight
  })
  
  observeEvent(input$array_weight_threshold,{
    values$array_weight_threshold = input$array_weight_threshold
  })
  

  
  observeEvent(input$fc_cutoff,{
    values$fc_cutoff = input$fc_cutoff
  })
  
  observeEvent(input$mtc,{
    values$mtc = input$mtc
  })
  
  observeEvent(input$pvalue_select,{
    values$pvalue_select = input$pvalue_select
  })
  
  observeEvent(input$cont_matrix_comp,{ 
    values$cont_matrix_comp = input$cont_matrix_comp
  })
  
  observeEvent(input$protein_column,{ 
    values$protein_column = input$protein_column
  })
  
  observeEvent(input$probe_column,{ 
    values$protein_column = input$probe_column
  })

  
  
  #### UI objects #####
  
  output$main_header_options_ui = renderUI({
    do.call(tagList,Main_header_UI(values))
  })
    
  #### Inputs Options #####
    ##### _Select Datasets #####
    
    output$select_datasets_ui = renderUI({ 
      if(is.null(input$gpr_files$datapath)){
        if(values$app_version == 'pro'){
          selectInput('dataset','Example Datasets',c(pro_data_list),pro_dataset)
        }else{
          selectInput('dataset','Example Datasets',c(basic_data_list),basic_dataset)
        }
      }else{
        if(values$app_version == 'pro'){
          selectInput('dataset','Example Datasets',c('Upload',pro_data_list),'Upload')
        }else{
          selectInput('dataset','Example Datasets',c('Upload',basic_data_list),'Upload')
        }
      }
      
    })
  
  observeEvent(input$dataset,{
    hideTab('main','probes')
    hideTab('main','proteins')
    hideTab('main','data')
    hideTab('main','pipeline')
    hideTab('main','all')
    hideTab('main','sig')
  })
  
  observeEvent(input$gpr_files$datapath,{
    values$probe_file = NULL 
    values$proteins_file = NULL
    values$metadata_file = NULL
    hideTab('main','probes')
    hideTab('main','proteins')
    hideTab('main','data')
    hideTab('main','pipeline')
    hideTab('main','all')
    hideTab('main','sig')
    
  })
    
    
    ##### _Test Files #####
    
    output$protein_tab = renderUI({
      tags$h5("Proteins")
    })
  
    array_file_list = reactive({
      req(input$dataset) 
      if(input$dataset == 'Upload'){
        if(!is.null(input$gpr_files$datapath)){
          file_list = input$gpr_files$name
          file_path_list = input$gpr_files$datapath
        }
      }else{
        values$metadata_file = 'dataset'
        values$probe_file = 'dataset'
        values$protein_file = 'dataset'
        getwd()
        (path = input$dataset)
        file_list = list.files(path)
        file_list = file_list[!file_list %in% c('metadata.txt','probes.txt','proteins.txt')]
        file_list
        file_path_list = file.path(path,file_list)
     
      }
      file_path_list
      list(name = file_list,path = file_path_list)
    })
   
    test_files = reactive({#withProgress(message = 'testing array files',{ 
      req(array_file_list())  
   
    #Tab('main','metadata')
      values$metadata = NULL
      values$probes = NULL
      values$proteins = NULL
      
      updateTabsetPanel(session, "main",
                        selected = 'instructions')
      
      hideTab('main','probes')
      hideTab('main','proteins')
      hideTab('main','data')
      hideTab('main','pipeline')
      hideTab('main','all')
      hideTab('main','sig')
      showTab('main','metadata')
      # Create a Progress object
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Testing file", value = 0)
      
      (file_name_list = array_file_list()$name)
      file_path_list = array_file_list()$path
      row_list = c()
      col_list = c()
      colnames_list = list()
      rownames_list = c()
      i = 1
      n = length(file_name_list)
      #colnames_list = c()
      for(i in c(1:length(file_name_list))){
        progress$inc(1/n, detail = paste(i,'of',n))
        print(i)
        file_path = file_path_list[i]
        file_name = file_name_list[i]
        print(file_name)
        df = tryCatch(read.table(file_path,sep ='\t',stringsAsFactors = FALSE),
                      error = function(e) {NULL})
        df
        if(!is.null(df)){
          col_names = df[1,]
          colnames(df) = df[1,]
          df = df[-1,]
          rownames_list = c(rownames_list,paste(df[,2],collapse =', '))
          type = 'flat'
        }else{
          df = read_table(file_path)
          as.tbl(df)
          (row_index = max(grep("Block",df[[1]])))
          library(readr)
          df = read_tsv(file_path,skip = row_index)
          col_names = colnames(df)
          rownames_list = c(rownames_list,dim(df)[1])
          type = 'table'
          
        }
        row_list = c(row_list,dim(df)[1])
        col_list = c(col_list,dim(df)[2])
        colnames_list[[file_name]] = colnames(df)
      }
      
      file_df = data.frame(File_name = file_name_list,
                           `Number of Spots` = row_list,
                           `Number of Metric Columns` = col_list)
      file_df
      
      
      file_error_list = list()
      if(length(unique(row_list)) != 1){
        file_error = 'There are differing number of probes on the arrays, only the intersection probes will be used'
        file_error_list[['rows']] = file_error
        
      } 

      
      if(!TRUE %in% grepl('635|535',col_names)){
        file_error = 'No array channels found in the uploaded files'
        file_error_list[['cols']] = file_error
      }

      colnames_list = unique(colnames_list)
      col_names = colnames_list[1]
      if(length(colnames_list) > 1){
      
        for(i in 2:length(colnames_list)){
          col_names = intersect(unlist(col_names),unlist(colnames_list[i]))
        }
      }
      col_names = unlist(col_names)
      list(file_df = file_df,file_error = file_error_list, col_names = col_names,type = type,colnames_list = colnames_list)
    })#})
    

    output$file_num_text = renderText({
  
        updateTabItems(session,"main", "Targets")
        df = test_files()$file_df 
        as.tbl(df)
        
        cmd = paste(dim(df)[1],'Files with',
                    paste(unique(df$Number.of.Spots),collapse=', '),
                    'probes and ',
                    paste(unique(df$Number.of.Metric.Columns),collapse =', '),'columns')
   
      
      print(cmd)
    })
    
    output$test_files_text_rows_ui = renderUI({ 
   
      if(length(test_files()$file_error) > 0){
        if('rows' %in% names(test_files()$file_error)){
          span(tags$h4(test_files()$file_error$rows), style="color:red")
        }
      }
        

    }) 
    
    output$test_files_text_cols_ui = renderUI({ 
      
      if(length(test_files()$file_error) > 0){
        if('cols' %in% names(test_files()$file_error)){
          hideTab('main','metadata')
          
          span(tags$h4(test_files()$file_error$cols), style="color:red")
        }
      }
      
      
    }) 
    
    
    
    data_col_names = reactive({  
      paste(test_files()$col_names)
    })
    
    output$data_columns_text = renderPrint({
      cat(paste(data_col_names(),collapse =', '))
    })
    
    output$data_columns_list_error_text = renderUI({
      if(length(test_files()$colnames_list) > 1){
        span(tags$h5('WARNING : There are different metric columns amongst the files, only the overlapping columns will be used'), style="color:orange")
      }
    })
    
    output$data_columns_list_text = renderPrint({
      if(length(test_files()$colnames_list) > 1 ){
        print(test_files()$colnames_list)
      }
    })
    
    
    
    output$test_files_table = DT::renderDataTable({
      df = test_files()$file_df  
      df_g = df %>% group_by(Number.of.Spots,Number.of.Metric.Columns) %>% 
        summarise(FileNames = paste(File_name,collapse = ' ,')) %>% 
        ungroup()
      df_g
    },rownames = FALSE)
    
    output$test_files_text_2 = renderPrint({
      df = test_files()$file_df 
      as.tbl(df)
      
      cat(dim(df)[1],'Files with',
      paste(unique(df$Number.of.Spots),collapse=', '),
      'probes and ',
      paste(unique(df$Number.of.Metric.Columns),collapse =', '),'columns')
      
    })
    
  

  
    #### _Columns Selection ####
    
    output$array_colours_ui = renderUI({
        col_names = data_col_names() 
        colour_list = c()
        colour_list_test = c('532','635')
        for(col_test in colour_list_test){
          if(TRUE %in% grepl(col_test,col_names)){
            colour_list = c(colour_list,col_test)
          }
        }
        colour_list
        selectInput('array_colours', 'Array Chanel',colour_list,colour_list[1],multiple = F)
    })
    
    output$array_column_ui = renderUI({ 
      if(test_files()$type != 'flat'){
          colour_list = c('Median','Mean')
          selected = 'Median'
        }else{
          colour_list = c('(mean) [mean]','(med) [med]','(mean) [med]','(med) [mean]')
          selected = '(med) [med]'
        }
        selectInput('array_column', 'Array Column',colour_list,selected)

    })
    
    output$foreground_column_ui = renderUI({
 
            col_names = data_col_names()
            col_names
            (selected = paste0('F',input$array_colours,' ',input$array_column))

            if(test_files()$type == 'flat'){
                f_col = paste0('Raw intensity ',input$array_column,' {',input$array_colours,'}')
                if(f_col %in% col_names){
                  selected = f_col
                }else{
                  selected = "Raw intensity (med) [med] {635} t"
                }
              }else{
                f_col = paste0('F',input$array_colours,' ',input$array_column)
                if(f_col %in% col_names){
                  selected = f_col
                }else{
                  (selected = grep('F',grep('Median',col_names,value = T),value = T))
                }
              }

              selectInput('foreground_column', 'Foreground',col_names,selected)
     
    })

    output$background_column_ui = renderUI({
            col_names = data_col_names()
            col_names
            selected = paste0('B',input$array_colours,' ',input$array_column)

            if(test_files()$type == 'flat'){

                (f_col = paste0('Background ',input$array_column,' {',input$array_colours,'}'))
                if(f_col %in% col_names){
                  selected = f_col
                }else{
                  selected = "Background (med) [med] {635}"
                }
              }else{
                f_col = paste0('B',input$array_colours,' ',input$array_column)
                if(f_col %in% col_names){
                  selected = f_col
                }else{
                  (selected = grep('B',grep('Median',col_names,value = T),value = T))
                }
              }


            selectInput('background_column', 'Background',col_names,selected)

    })

    output$annotation_columns_ui = renderUI({
            if(!is.null(input$background_column)){
                if(!is.null(input$foreground_column)){

                    (col_names = data_col_names())
                    (col_names = col_names[!col_names %in% c(input$background_column,input$foreground_column)])
                    annotation=c("Block","Column","Row","ID","Name",'Gene_symbol')

                    if(test_files()$type == 'flat'){
                          annotation = c("Grid","Column","Row","Annotation", "Name")
                      }else{
                          annotation=c("Block","Column","Row","ID","Name",'Gene_symbol')
                      }
                    selectInput('select_annotation','Annotation Columns',col_names,annotation,multiple = T)
                }
            }

    })
    
    foreground_column = reactive(input$foreground_column)
    background_column = reactive(input$background_column)
    
    
    
  #---------------------------Read in data and filter probes----------------------------------------------------
    
    
    #### _EListRaw ####
     

    

    

      

    
    E = reactive({withProgress(message = 'Generating EListRaw object',{
      print('E')
      file_path_list = array_file_list()$path  
      file_path_list
 
          if(input$probe_filtering == TRUE){
            E <- read.maimages(file_path_list,
                               columns=list(E=input$foreground_column, Eb=input$background_column),
                               annotation=c(input$select_annotation), 
                               wt.fun=sd_filter)
          }
          if(input$probe_filtering == FALSE){
            E <- read.maimages(file_path_list,
                               columns=list(E=input$foreground_column, Eb=input$background_column),
                               annotation=c(input$select_annotation))
          }

        E
    
    })})
    


    #### _metadata #####
    
    #values$metadata_file is a reactive value, that serves as a global variabe, which can be affected by different reactive elements
    
    # inputs that reset values$metadata file
    observeEvent(c(input$reset_metadata,
                   input$gpr_files),{
                   values$metadata_file = NULL
                   })
    
    
    # assigns input$metadata file to values$metadata file
    observeEvent(input$metadata_file,{
      values$metadata_file = input$metadata_file
    })
    
    output$metadata_file_upload_ui = renderUI({
      input$dataset
      input$reset_metadata
      input$gpr_files
      
      fileInput(
        inputId = "metadata_file", 
        label = "Upload Sample File", 
        accept = c(".txt")
      )
    })
    
    metadata_names = reactive({ 
      metadata_names = metadata()$Name
      metadata_names
    })
    
    
    # reactive object that decided which metadata file to upload. 
    metadata_upload = reactive({ withProgress(message = 'uploding metadata',{  
      input$reset_metadata     
      input$dataset
      input$gpr_files
      metadata_file_path = NULL
      
      file_names = array_file_list()$name
      (n = length(file_names))
      df_files = data.frame(FileName = file_names,
                            Name = tools::file_path_sans_ext(file_names)
      )
      dim(df_files)
      as.tbl(df_files)
      error = NULL
      warning = NULL
      df_upload = NULL
      if(is.null(values$metadata_file)){
        df = df_files
        
      }else{
        if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'metadata.txt')) & values$metadata_file == 'dataset'){
          metadata_file_path = file.path(input$dataset,'metadata.txt')
        }else{
          metadata_file_path = values$metadata_file$datapath
        }
          df_upload = read.csv(metadata_file_path,sep ='\t',stringsAsFactors = F)
          if('FileName' %in% colnames(df_upload)){
            if(TRUE %in% duplicated(df_upload$Name)){
              warning = 'There were duplicates in the Name column of the uploaded sample file'
              df_upload = df_upload %>% 
                filter(!duplicated(df_upload$Name))
            }
              df = df_files %>% 
                dplyr::select(-Name) %>% 
                left_join(df_upload %>% 
                            filter(!duplicated(FileName))) %>%
                filter(FileName %in% file_names)
              if(TRUE %in% is.na(df$Name)){
                warning = 'Some arrays files were missing in the uploaded sample file'
                df$Name[is.na(df$Name)] = df$FileName[is.na(df$Name)]
              }
              
              
              if(length(intersect(df_files$FileName,df_upload$FileName)) == 0){
                error = "There is no intersect between the uploaded metadata file and the array files"
                df = df_files
              }
              
          
          }else{
            error = 'There is no FileName column in the uploaded metadata file.'
            df = df_files
          }
      }
        
        if(!'Condition' %in% colnames(df)){
          df$Condition = 'Condition'
        }
        if(!'Name' %in% colnames(df)){
          df$Name = df$File
        }
      
      df$Condition[is.na(df$Condition)] = 'Unknown'

   
      df$Name = as.character(df$Name)
      list(df = df, upload_df = df_upload, error = error, warning = warning)
    }) })
    
    output$metadata_upload_error_ui = renderUI({
      if(!is.null(metadata_upload()$error)){
        output$metadata_upload_error_text = renderPrint(cat(metadata_upload()$error))
        output$metadata_upload_df = DT::renderDataTable(metadata_upload()$df_upload,rownames = FALSE)
        
        lst = list(span(tags$h4(htmlOutput('metadata_upload_error_text')), style="color:red"),
                   tags$h2('Uploaded Targets Table'),
                   DT::dataTableOutput('metadata_upload_df'),
                   tags$h2('Targets Table'))
        do.call(tagList,lst)
      }
    })
    
    metadata = reactive(metadata_upload()$df)
    
    output$select_conditions_column_ui = renderUI({
      req(metadata())
      selectInput('select_conditions_column','Select Condition Column',colnames(metadata()),colnames(metadata()))
    })
    
    metadata_conditions = reactive({
      df = metadata()
      df$Condition = df[,input$select_conditions_column]
      df
    })
    
    
    
    selected_metadata = reactive({
      metadata_conditions() %>% filter(Condition %in% input$condition_select)
    })
    
    selected_metadata_names = reactive({
      metadata_names = selected_metadata()$Name
      metadata_names
    })
    
    output$metadata_table = DT::renderDataTable({
      showTab('main','probes')
      values$metadata = 'hit'
      metadata()
    },rownames = FALSE)
    
    output$metadata_table_ui = renderUI({
      result_list = metadata_upload()  
      
      if(!is.null(result_list$error) | !is.null(result_list$warning)){
        output$metadata_upload_df = DT::renderDataTable({
          result_list$upload_df
        })
        
        lst = list(
          span(tags$h4(result_list$error), style="color:red"),
          span(tags$h4(result_list$warning), style="color:orange"),
          tags$h3('Uploaded Sample Table'),
          DT::dataTableOutput('metadata_upload_df'),
          tags$h3('Array Sample Table'),
          DT::dataTableOutput('metadata_table')
        )
      }else{
        DT::dataTableOutput('metadata_table')
      }
    }) 
    
    output$download_metadata <- downloadHandler(
      filename = function(){"metadata.txt"}, 
      content = function(fname){
        write.table(metadata_conditions(), fname,sep = '\t', row.names = FALSE)
      }
    ) 
    
    output$condition_select_ui = renderUI({
      req(metadata_conditions())
      df = metadata_conditions()
      selectInput('condition_select','Select Conditions',unique(df$Condition),unique(df$Condition),multiple = T,width = 1200)
    })
    #### _probes ####
    
    observeEvent(input$reset_probes,{
      values$probe_file = NULL
    })
    
    observeEvent(input$probe_file,{
      values$probe_file = input$probe_file
    })
    
    output$probe_file_upload_ui = renderUI({
      input$gpr_files
      input$dataset
      input$reset_probes
        fileInput(
          inputId = "probe_file", 
          label = "Upload Spots File", 
          accept = c(".txt")
        )
    })
    
    output$probe_columns_ui = renderUI({
      req(input$select_annotation) 
      input$dataset 
      input$gpr_files
      input$reset_proteins
      selected = 'Annotation'
      
      columns = input$select_annotation
      
      if("Name" %in% columns){
        selected = 'Name'
      }
      if("ID" %in% columns){
        selected = 'ID'
      }
      if("Gene_symbol" %in% columns){
        selected = 'Gene_symbol'
      }
      selected
      selectInput('probe_column','Spot Column',columns,selected)
    })
    

    
    probe_names = reactive({
      input$reset_probes
      req(input$probe_column)
      req(E())
      probe_names = E()$genes[[input$probe_column]]

      probe_names
    })
    
    
    probe_upload = reactive({  withProgress(message = 'Upload Spots',{
      print('probe_upload')
      req(probe_names())  
      input$reset_probes
      input$dataset
      input$gpr_files
      probe_file_path = NULL
      
      df = E()$genes
      df$probe = probe_names()
      
      probe_names = probe_names()
   
      error = NULL
      warning = NULL
      upload_df = NULL
      if(!is.null(values$probe_file)){
        if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'probes.txt'))){
          probe_file_path = file.path(input$dataset,'probes.txt')
        }
        if(!is.null(input$probe_file$datapath)){
            probe_file_path = input$probe_file$datapath
        }
        if(!is.null(probe_file_path)){
          
          upload_df = read.csv(probe_file_path,sep ='\t',stringsAsFactors = F)
          if('probe' %in% colnames(upload_df)){
            if('Category' %in% colnames(upload_df)){
              print('col hit')
              
              #upload_df_trim = upload_df %>% 
              #  dplyr::select(probe,Category) %>% 
              #  distinct()
            }else{
              error = 'No Category column in uploaded probe file'
            }
          }else{
            error = 'No probe column in uploaded probe file'
            
          }
          
          if(length(intersect(df$probe,upload_df$probe)) == 0){
            error = 'There are no overlapping probes between the uploded probe file and the uploaded array files'
          }else{
            if(length(setdiff(df$probe,upload_df$probe)) > 0){
              warning = 'Not all the probes in the array files are in the uploaded probe file.'
            }
          }
          
          # if(TRUE %in% duplicated(upload_df$probe)){
          #   warning = "There are duplicates in the probe column, the duplicates have been removed" 
          #   upload_df_trim = upload_df_trim %>% 
          #     filter(!duplicated(probe))
          # }
          
          if(is.null(error)){
            df = df %>% 
              left_join(upload_df)
          }
        }
      }
      
      if(!'Category' %in% colnames(df)){
        df$Category = character(dim(df)[1])
      }
      list(df = df, upload_df = upload_df,error = error, warning = warning)
    })})
    
    output$probe_remove_ui = renderUI({withProgress(message = 'remove probes',{
      print('probe_remove') 
      #req(probe_upload()) 
      #req(input$reset_probes)
      #req(input$dataset)
      #req(input$gpr_files)
      df = probe_upload()$df
      #as.tbl(df)
      proteins = unique(df$probe)
      (empty_probes = grep('empty|EMPTY',df$probe,value = T))
      
      control = unique(c(empty_probes,df$probe[df$Category == 'remove']))
      control
      selectInput('select_remove','Probes to Remove before Normalisation',proteins,control,multiple = T, width = 1200)
    })})
    
    output$probe_control_ui = renderUI({withProgress(message = 'control probes',{
      print('probe control') 
      #req(probe_upload())
      #req(input$reset_probes)
      #req(input$dataset)
      #input$gpr_files
      df = probe_upload()$df
      as.tbl(df)
      proteins = unique(df$probe)
      control = unique(df$probe[df$Category == 'control'])
      selectInput('select_control_probe','Control Probes (labelled but not removed)',proteins,control,multiple = T, width = 1200)
    })})
    
    probes = reactive({withProgress (message = 'assigning labels to probes',{
      print('probes')
      req(probe_upload())
      df = probe_upload()$df
      df$Category = 'analyte'
      df$Category[df$probe == ''] = 'remove'
      df$Category[df$probe %in% input$select_remove] = 'remove'
      df$Category[df$probe %in% input$select_control_probe] = 'control'
      #df$Category[df$Category == 'remove' & !df$probe %in% input$select_remove] = ''
      df
    })})
    
    removed_probes = reactive({
      print('removed_probes')
      req(probes())
      probes() %>% 
        filter(!Category %in% c('remove')) %>% 
        pull(probe)
    })
    
    
    output$probe_table = DT::renderDataTable({
      print('probe_table')
      req(probe_upload())
      req(probes())
      if(values$app_version == 'pro'){
        showTab('main','proteins')
        showTab('main','data')
        showTab('main','all')
      }else{
        showTab('main','data')
        showTab('main','pipeline')
        showTab('main','sig')
      }
      if(length(selected_metadata()$Condition) >1){
        showTab('main','sig')
      }
  
      values$probes = 'hit'
      probes()
    },rownames = FALSE)
    
    output$probe_table_ui = renderUI({
      result_list = probe_upload()
      
      if(!is.null(result_list$error) | !is.null(result_list$warning)){
        output$probe_upload_df = DT::renderDataTable({
          result_list$upload_df
        })
        
        lst = list(
          span(tags$h4(result_list$error), style="color:red"),
          span(tags$h4(result_list$warning), style="color:orange"),
          tags$h3('Uploaded Spot Table'),
          DT::dataTableOutput('probe_upload_df'),
          tags$h3('Array Spot Table'),
          DT::dataTableOutput('probe_table')
        )
      }else{
        DT::dataTableOutput('probe_table')
      }
    })
    
 
    
    output$download_probes <- downloadHandler(
      filename = function(){"probes.txt"}, 
      content = function(fname){
        write.table(probes(), fname,sep = '\t',row.names = F)
      }
    )
    
    #### _proteins ####
    
    output$protein_file_upload_ui = renderUI({
      input$reset_proteins
      input$gpr_files
      input$dataset
      fileInput(
        inputId = "protein_file", 
        label = "Upload Protein File", 
        
        accept = c(".txt")
      )
    })
    
    output$protein_columns_ui = renderUI({
      input$dataset
      input$gpr_files
      input$reset_proteins
      selected = 'Annotation'
      if("Name" %in% colnames(probes())){ 
        selected = 'Name'
      }
      if("ID" %in% colnames(probes())){
        selected = 'ID'
      }
      selectInput('protein_column','Protein Column',colnames(probes()),input$probe_column)
    })
    
    observeEvent(input$reset_proteins,{
      values$protein_file = NULL
    })
    
    
    
    observeEvent(input$protein_file,{
      values$protein_file = input$protein_file
    })
    
     
    protein_upload = reactive({ withProgress(message = 'Uploading Proteins',{
      req(data_full()) 
      input$reset_metadata
      input$dataset
      input$gpr_files

      df = data_full() %>% 
        dplyr::select('protein','Category')
      
      #df = data.frame(protein = data_full()$protein)
      error = NULL
      warning = NULL
      upload_df = NULL
      protein_file_path = NULL
      info = NULL
      
      if(!is.null(values$protein_file)){
        if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'proteins.txt'))){
          protein_file_path = file.path(input$dataset,'proteins.txt')
        }
        if(!is.null(input$protein_file$datapath)){
            protein_file_path = input$protein_file$datapath
        }
        if(!is.null(protein_file_path)){
          
          
          upload_df = read.csv(protein_file_path,sep ='\t',stringsAsFactors = F)
          if('protein' %in% colnames(upload_df)){
            if('Category' %in% colnames(upload_df)){
              #upload_df_trim = upload_df %>% 
              #  dplyr::select(probe,Category)
              print('col_hit')
              df = df %>% 
                dplyr::select(-'Category')
            }else{
              warning = 'No Category column in uploaded protein file'
            }
          }else{
            error = 'No protein column in uploaded protein file'
            
          }
          
          if(length(intersect(df$protein,upload_df$protein)) == 0){
            error = 'There are no overlapping proteins between the uploded protein file and the uploaded array file proteins'
          }else{
            if(length(setdiff(df$protein,upload_df$protein)) > 0){
              warning = 'Not all the proteins in the array files are in the uploaded protein file.'
              diff_list = c(paste(setdiff(df$protein,upload_df$protein),collapse = ', '),paste(setdiff(upload_df$protein,df$protein),collapse = ', '))
              diff_list
              info = paste(diff_list,collapse = ' and ')
              info
            }
          }
          
          if(TRUE %in% duplicated(upload_df$protein)){
            warning = 'There are duplicates in the protein column, the duplicates have been removed'
            upload_df = upload_df %>% 
              filter(!duplicated(protein))
          }
          
          if(is.null(error)){
              df = df %>% 
                left_join(upload_df,by = 'protein')
          }
          
        }
      }
      
      if(!'Category' %in% colnames(df)){
     
        df$Category = character(dim(df)[1])

      }
      
      list(df = df, upload_df = upload_df,error = error,warning = warning, info = info)
    })})
    
    output$protein_control_ui = renderUI({
      #if(!is.null(values$data)){
      req(protein_upload())
      input$reset_proteins
      df = protein_upload()$df
      as.tbl(df)
      proteins = unique(df$protein)
      control = unique(df$protein[df$Category == 'other'])
      selectInput('select_controls','Other',proteins,control,multiple = T, width = 1200)
      #}
    })
    
    proteins = reactive({
      #if(!is.null(protein_upload())){
      #req(input$select_controls)
      df = protein_upload()$df
      #df$Category = 'analyte'
      df$Category[df$protein == ''] = 'EMPTY'
      df$Category[df$protein == ''] = 'EMPTY'
      df$Category[df$protein == 'EMPTY'] = 'EMPTY'
      df$Category[df$protein == 'empty'] = 'EMPTY'
      if(!is.null(input$select_controls)){
        df$Category[df$protein %in% input$select_controls] = 'other'
        #df$Category[df$Category == 'control' & !df$protein %in% input$select_controls] = 'analyte'
      }
      df
      #}
    })
    
    observeEvent(input$col, {
      js$pageCol(input$col)
    })


    
    output$proteins_table = DT::renderDataTable({
      showTab('main','pipeline')
      showTab('main','sig')
      if(values$app_version == 'pro'){
        showTab('main','all')
      }else{
        hideTab('main','all')
      }
      
      if(length(selected_metadata()$Condition) >1){
        showTab('main','sig')
      }
      values$proteins = 'hit'
      proteins() 
    },rownames = FALSE)
    
    output$protein_table_ui = renderUI({
      result_list = protein_upload()
      
      if(!is.null(result_list$error) | !is.null(result_list$warning)){
        output$protein_upload_df = DT::renderDataTable({
          result_list$upload_df
        },rownames = FALSE)
        
        lst = list(
          span(tags$h4(result_list$error), style="color:red"),
          span(tags$h4(result_list$warning), style="color:orange"),
          tags$h6(result_list$info),
          
          tags$h3('Uploaded Protein Table'),
          DT::dataTableOutput('protein_upload_df'),
          tags$h3('Array Protein Table'),
          DT::dataTableOutput('proteins_table')
        )
      }else{
        DT::dataTableOutput('proteins_table')
      }
    }) 
    
    output$download_proteins <- downloadHandler(
      filename = function(){"proteins.txt"}, 
      content = function(fname){
        write.table(proteins(), fname,sep = '\t',row.names = F)
      }
    )
    
    
    #### Data Table Heatmaps #####
    
    array_HeatMap_function <- function(m,metadata_names,selected_metadata,probe_names,pallete,cluster) {
          
          colnames(m) = metadata_names$Name
          m = m[,selected_metadata$Name]
          
          remove = c()
          if(FALSE %in% is.finite(m)){
            m[!is.finite(m)] = 0
            remove = c(remove,'infinite')
            
          }
          if(TRUE %in% is.infinite(m)){
            m[is.na(m)] = 0
            remove = c(remove,'na')
          }
          
          title = NULL
          if(length(remove) > 0){
            title = paste('Replaced ',paste(remove,collapse = ' and '), "values with zeros values.")
          }
          plot_height = 300+(dim(m)[1]*10)
          plot_width = 600 + (dim(m)[2]*5) 
          if(values$app_version == 'basic'){
            if(dim(m)[1] > max_heatmap_rows){
              cluster = 'dend'
              title = paste('There are too many rows to generate a heatmap so a dendrogram was produced instead.',
                            title)
            }
          }
          if(is.null(title)){
            title = ''
          }
          if(cluster == 'dend'){
            plot_height = 300
            ht = dend_function(m,metadata_names,pallete)
          }else{
            plot_height = 300+(dim(m)[1]*10)
            ht = Heatmap_function(m,selected_metadata,probe_names,pallete,cluster)
          }
     list(p = ht,plot_height = plot_height, plot_width = plot_width, warning = title,type = cluster)  
    }
    
    Heatmap_function = function(m, metadata,probes,pallete,cluster = 'Cluster'){ 
      
      plot_min = min(as.numeric(m),na.rm = T)
      if(plot_min < 0){
        col_fun = colorRamp2(c(plot_min,0,mean(as.numeric(m),na.rm = T),max(as.numeric(m),na.rm = T)), c('blue',"white",'orange',"red"))
        
      }else{
        col_fun = colorRamp2(c(plot_min,mean(as.numeric(m),na.rm = T),max(as.numeric(m),na.rm = T)), c("white",'orange',"red"))
      }
      
      metadata = metadata %>% 
        filter(Name %in% colnames(m))
      
      ha_annotation = metadata$Condition
      names(ha_annotation) = metadata$Name
      (condition_col = brewer.pal(n = length(unique(metadata$Condition)), name = pallete)[1:length(unique(metadata$Condition))])
      (names(condition_col) = unique(metadata$Condition))
      column_ha = HeatmapAnnotation(Condition = ha_annotation, col = list(Condition = condition_col))
      
      sample_order = metadata %>% 
        arrange(Condition)
      
      
      if(cluster == 'Cluster'){
        ht = Heatmap(m,
                     col = col_fun,
                     row_labels = probes,
                     column_dend_height = unit(4, "cm"), 
                     row_dend_width = unit(4, "cm"),
                     column_names_side = "top",
                     top_annotation = column_ha
        )
      }
      if(cluster == 'Order'){
        ht = Heatmap(m,
                     col = col_fun,
                     row_labels = probes,
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
                     row_labels = probes,
                     row_names_side = "left",
                     column_names_side = "top",
                     cluster_columns = FALSE,
                     cluster_rows = FALSE,
                     top_annotation = column_ha)
      }
      ht
      
    }
    
    

    
    dend_function = function(m,metadata,pallete){
      ## stupid toy example
      groupCodes = metadata$Condition
      groupCodes
      palette
      conditions = unique(metadata$Condition)
      colorCodes = brewer.pal(n = length(conditions), name = pallete)[1:length(conditions)]
      #colorCodes = rainbow(length(conditions))
      names(colorCodes) = conditions
      colorCodes
      
      tm = t(m)
      rownames(tm) = groupCodes
      #as.tbl(tm)
      
      distSamples <- dist(tm)
      hc <- hclust(distSamples)
      
      
      labelCol <- function(x) {
        if (is.leaf(x)) {
          ## fetch label
          label <- attr(x, "label")
          attr(x, "nodePar") <- list(lab.col=colorCodes[label])
        }
        return(x)
      }
      
      d <- dendrapply(as.dendrogram(hc), labelCol)
      
      d
      
    }
    
    dend_function_old = function(m,metadata){
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
        left_join(metadata)
      
      ddata_x$leaf_labels = ddata_x$labels$label
      
      p2 <- ggplot(segment(ddata_x)) +
        geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
      p2 = p2 + geom_text(data=label(ddata_x),
                          aes(label=label, x=x, y=y-2, colour=ddata_x$labels$Condition, angle = 90))
      p2
    }
    
    
    foreground_table = reactive({
      df = as.data.frame(E()$E)  
      dim(df)
      colnames(df) = metadata_names()
      df$probe = probe_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(probe,everything())
      df
    }) 
    
    output$foreground_table_ui = renderUI({
      df = foreground_table()
      id = 'foreground'
      name = 'table'
      table_Server(id,name,df)
      table_UI(id,name)
    })
    
    output$foreground_heatmap_ui = renderUI({ 
      m = E()$E   
      id = 'foreground'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,metadata(),selected_metadata(),probe_names(),input$r_col,values$heatmap_order)
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
    })
    
    triplicate_CV_function = function(df){

      
      df_l = df %>% 
        gather(Name,value,-probe)
      as.tbl(df_l)

      df_cv = df_l %>% 
        group_by(probe,Name) %>%
          summarise(count = n(),
                    mean = mean(value,na.rm = T),
                    median = median(value,na.rm = T),
                    sd = sd(value,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(CV = 100*(sd/mean),
               diff_mm = abs(mean-median)) 
        
      as.tbl(df_cv)
      
      df_cv_mean = df_cv %>% 
        group_by(probe) %>% 
          summarise(mean = mean(CV,na.rm = T),
                    median = median(CV,na.rm = T),
                    max = max(CV,na.rm = T),
                    min = min(CV,na.rm = T)) %>% 
        ungroup()
      
      list(df_cv = df_cv, df_cv_mean = df_cv_mean)
   
    }
    
    triplicate_cv_plot_function = function(df,probes,metadata){withProgress(message = 'Calculating CV',{
      #df = foreground_table() 
      df_list = triplicate_CV_function(df)
      df_cv = df_list$df_cv
      if('Name' %in% colnames(probes)){
        probes = probes %>% 
          dplyr::select(-Name)
      }
      
      df_cv = df_cv %>% 
        left_join(probes) %>% 
        left_join(metadata)
      
      as.tbl(df_cv)
      q = quantile(df_cv$CV,na.rm=T)
      q
      if(values$collapse_boxplots == F){
        p = ggplot(df_cv,aes(x = Name,y = CV, col = Condition)) 
      }else{
        p = ggplot(df_cv,aes(x = Condition,y = CV, col = Condition))
      }
      
      p = p + 
        geom_boxplot() + 
        facet_grid(Category ~ ., scales = 'free_y')
      if(values$plot_lim == 'Quantile'){
       p = p +  ylim(q[2],q[4])
      }
      if(values$plot_lim == '2x Quantile'){
        p = p +  ylim(q[2]/2,q[4]*2)
      }
      p = gg_col_function(p)
      p
      if(values$collapse_boxplots == F){
        
        d = ggplot(df_cv,aes(x = CV, group = Name,col = Condition)) +
          geom_density() + 
          facet_grid(Category ~ .)
      }else{
        d = ggplot(df_cv,aes(x = CV, col = Condition)) +
          geom_density() + 
          facet_grid(Category ~ .)
      }
      d = gg_col_function(d)
      
      bq = quantile(df_cv$diff_mm,na.rm=T)
      if(values$collapse_boxplots == F){
        b = ggplot(df_cv, aes(x = Name, y = diff_mm, fill = Category, col = Condition)) + 
          geom_boxplot() + 
          ylab('Difference between Mean and Median')
      }else{
        b = ggplot(df_cv, aes(x = Condition, y = diff_mm, fill = Category, col = Condition)) + 
          geom_boxplot() + 
          ylab('Difference between Mean and Median')
      }
      if(values$plot_lim == 'Quantile'){
        b = b +  ylim(bq[2],bq[4])
      }
      if(values$plot_lim == '2x Quantile'){
        b = b +  ylim(bq[2]/2,bq[4]*2)
      }
      b = gg_col_function(b)
      b
    })
      list(df_list = df_list,p = p, d = d, b = b)
    } 
    
    output$foreground_triplicate_cv_plot_ui = renderUI({withProgress(message = 'Generating Plots',{
      df = foreground_table()  
      probes = probes()
      metadata = metadata()
      plot_list = triplicate_cv_plot_function(df,probes(),metadata())
    
      id = 'foreground'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)

      do.call(tagList,CV_UI(id,name,values))
    })})
  
    background_table = reactive({
      df = as.data.frame(E()$Eb)
      colnames(df) = metadata_names()
      df$probe = probe_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(probe,everything())
      df
    })
    
    output$background_table_ui = renderUI({
      df = background_table()
      id = 'background'
      name = 'table'
      table_Server(id,name,df)
      table_UI(id,name)
    })
    
    output$background_heatmap_ui = renderUI({ 
      
      m = E()$Eb 
      id = 'backhground'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,metadata(),selected_metadata(),probe_names(),input$r_col,values$heatmap_order)
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))

    })
    
    output$background_triplicate_cv_plot_ui = renderUI({withProgress(message = 'Generating Plots',{
      df = background_table()  
      probes = probes()
      metadata = metadata()
      plot_list = triplicate_cv_plot_function(df,probes(),metadata())
      
      id = 'background'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)
      
      do.call(tagList,CV_UI(id,name,values))
    })})
    


    probe_filtering_E = reactive({
      df = E()$weights %>%  
        as.data.frame
      colnames(df) = metadata_names()
      df$probe = probe_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(probe,everything())
      df
    })

    
    output$probe_filtering_table_ui = renderUI({
      df = probe_filtering_E()
      id = 'probe_filtering'
      name = 'table'
      table_Server(id,name,df)
      table_UI(id,name)
    })
    
    output$probe_filtering_E_heatmap_ui = renderUI({ 
      
    
    
      if('weights' %in% names(E())){
        m = probe_filtering_E() %>% 
          dplyr::select(-probe) %>% 
          as.matrix()
        
        if(length(unique(as.numeric(m))) >1){
        
          id = 'probe_filtering'
          name = 'Hcluster'
          ht_list = array_HeatMap_function(m,metadata(),selected_metadata(),probe_names(),input$r_col,values$heatmap_order)
          ht_plot_Server(id,name,ht_list)
          do.call(tagList,plot_UI(id,name,ht_list$warning))

        }else{
          tags$h4('No probes were filtered')
        }
      }else{
        tags$h4('No probe filtering was applied to the array data')
      }
  
    })
    
    ### Pipeline #####
    output$pipeline_ui = renderUI({
      do.call(tagList,Pipeline_UI(values))
    })
    
    #----------------------------Visualization of Raw data---------------------------------------------
    


    
    array_boxplot_function = function(data,metadata_names,probe_names,metadata,selected_metadata,
                                      log_rb,input){
      df = data.frame(data)

      colnames(df) <- metadata_names
      df$Proteins = probe_names
      as.tbl(df)   
      
      
      df_l = df %>% 
        gather(Name,`Expression Intensity`,c(metadata_names)) %>% 
        left_join(metadata) %>% 
        filter(Name %in% selected_metadata$Name)
      as.tbl(df_l)
      p = ggplot(df_l)
      if(values$collapse_boxplots == F){
        
        if(length(unique(df_l$Condition)) > 1 ){
          p = p + geom_boxplot(aes(x = Name,y = `Expression Intensity`, col = Condition))
          
        }else{
          p = p + geom_boxplot(aes(x = Name,y = `Expression Intensity`))
          
        }
      }else{
        p = p + geom_boxplot(aes(x = Condition,y = `Expression Intensity`, col = Condition))
        
      }
      if(log_rb == FALSE){
        p = p + scale_y_continuous(trans='log2')
      }
      p = gg_col_function(p)
      if(length(unique(df_l$Group)) > 1){
        p = p + facet_grid(Group ~ .)
      }
      
      p 
    }
     
    array_boxplot_function_2 = function(data,metadata_names,probe_names,
                                        metadata,selected_metadata,
                                        probes,
                                        log_rb,input,values){
      df = data.frame(data)

      colnames(df) <- metadata_names
      df$probe = probe_names
      as.tbl(df)   
      
      
      if("Name" %in% colnames(probes)){
        probes = probes %>% 
          dplyr::select(-Name)
      }
      
      df_l = df %>% 
        gather(Name,`Expression Intensity`,c(metadata_names)) %>% 
        left_join(metadata) %>% 
        left_join(probes) %>% 
        filter(Name %in% selected_metadata$Name) 
      as.tbl(df_l)
      p = ggplot(df_l)
      if(values$collapse_boxplots == F){

        if(length(unique(df_l$Condition)) > 1 ){
          p = p + geom_boxplot(aes(x = Name,y = `Expression Intensity`, col = Condition))
          
        }else{
          p = p + geom_boxplot(aes(x = Name,y = `Expression Intensity`))
          
        }
      }else{
        p = p + geom_boxplot(aes(x = Condition,y = `Expression Intensity`, col = Condition))
        
      }
      if(log_rb == FALSE){
        p = p + scale_y_continuous(trans='log2')
      }
      p = gg_col_function(p)
 
      plot_height = 400
      if(values$sep_categories == TRUE){
        if(length(unique(df_l$Category)) > 1){
          p = p + facet_grid(Category ~ ., scales = 'free')
          plot_height = 300 * length(unique(df_l$Category))
        }
      }
      list(p = p, plot_height = plot_height)
    }
    
    CV_df_function  = function(df,metadata){
      
      df_l = df %>% 
        gather(Name,value,-probe) %>% 
        left_join(metadata)
      
      as.tbl(df_l)
      
      df_cv = df_l %>% 
        filter(!is.na(value)) %>% 
        group_by(probe,Condition) %>% 
        summarise(mean = mean(value,na.rm = T),
                  sd = sd(value,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(CV = sd/mean*100)
      
      df_cv
    }
    
    CV_sample_plot_function = function(data,metadata,probes){
      CV_df = data %>%  
        left_join(probes)
      q = quantile(CV_df$CV,na.rm = T)
      
      if(length(unique(CV_df$Condition)) == 1){
        p = ggplot(CV_df) + 
          geom_boxplot(aes(y = CV,x = Condition))
        d = ggplot(CV_df,aes(x = CV)) +
          geom_density()
        
      }else{
        p = ggplot(CV_df) + 
          geom_boxplot(aes(y = CV,x = Condition, fill = Condition))
        
        d = ggplot(CV_df,aes(x = CV,col = Condition)) +
          geom_density()
      }
      
      if(values$plot_lim == 'Quantile'){
        p = p +  ylim(q[2],q[4])
      }
      if(values$plot_lim == '2x Quantile'){
        p = p +  ylim(q[2]/2,q[4]*2)
      }
      
      if(length(unique(CV_df$Category)) > 1){
        p = p +
          facet_grid(Category ~ .)
        
        d = d + 
          facet_grid(Category ~ .)
      }
      list(p = p, d = d)
    }
    
    CV_sample_density_function = function(data,metadata,probes){
      CV_df = data %>%  
        left_join(probes)
      q = quantile(CV_df$CV,na.rm = T)
      
      if(values$collapse_boxplots == F){
       
      }else{
        d = ggplot(df_cv,aes(x = CV, col = Condition)) +
          geom_density() + 
          facet_grid(Category ~ .)
      }
   
      
    }

    
    
    CV_plot_function = function(data,metadata,probes){
      CV_df = data %>%  
        left_join(probes)
      q = quantile(CV_df$CV,na.rm = T)
      
      if(length(unique(CV_df$Category)) == 1){
        p = ggplot(CV_df) + 
          geom_boxplot(aes(y = CV,x = Condition)) 
        
        
      }else{
        p = ggplot(CV_df) + 
          geom_boxplot(aes(y = CV,x = Condition, fill = Category))
         
      }
      
      if(values$plot_lim == 'Quantile'){
        p = p +  ylim(q[2],q[4])
      }
      if(values$plot_lim == '2x Quantile'){
        p = p +  ylim(q[2]/2,q[4]*2)
      }
      
      p
    }
    
    missingness_function = function(df,metadata){
      df[df <= 0] = NA
      as.tbl(df)
      
      df_l = df %>% 

        gather(Name,value,-probe) %>% 
        filter(!is.na(value))
      as.tbl(df_l)
      
      df_count = df_l %>% 
        group_by(Name) %>% 
        summarise(`Percentage missing` = (dim(df)[1]-n())/dim(df)[1]*100) %>% 
        ungroup() %>% 
        left_join(metadata)
      df_count  
      
      p = ggplot(df_count) +
        geom_col(aes(x = Name,y = `Percentage missing`, fill = Condition))
      list(p = p, df = df_count)
    }
    
    MA_plot_function = function(df,probes){
      if('probe' %in% colnames(df)){
        probe_col = df$probe
        df = df %>% dplyr::select(-probe)
      }else{
        probe_col = rownames(df)
      }
      
      Rfit = lmFit(df)
      Rfit2 = eBayes(Rfit)
      Rfit2_df = as.data.frame(Rfit2)
      as.tbl(Rfit2_df)
      dim(Rfit2_df)
      Rfit2_df$probe = probe_col
      
      as.tbl(Rfit2_df)
      
      plot_df = Rfit2_df %>% 
        
        left_join(probes)
      
      as.tbl(plot_df)
      
      
      if(length(unique(plot_df$Category))>1){
        MA_plot = ggplot(plot_df) + 
          geom_point(aes(y = log(F), x = Amean,col = Category,group = probe))
      }else{
        MA_plot = ggplot(plot_df) + 
          geom_point(aes(y = log(F), x = Amean,group = probe))
      }
      
      
      MA_plot
    }
    
    log_min_function = function(m,input){
      if(values$min_corr == TRUE){
        m[m < 1] = 1
      }
      if(values$log_rb == TRUE){
        m = log2(m)
      }
      m
    }
    
    neg_corr_function = function(m,input){
      if(values$min_corr == TRUE){
        m[is.na(m)] = 0
        m[is.infinite(m)] = 0 
        m[m < 0] = 0
      }
      m
    }
    
    cont_matrix_function = function(df,metadata,input){
      time = factor(paste(metadata$Condition), levels = unique(metadata$Condition))
      time
      design = model.matrix(~0+time)
      colnames(design) = levels(time)
      
      conditions = unique(metadata$Condition)
      comparison_list = c()
      if(values$cont_matrix_comp == "All"){
        for(i in c(1:length(conditions))){
          for(j in c(1:length(conditions))){
            
            if(j > i){
              #print(paste(i,j))
              comparisons = paste0(conditions[i],'_vs_',conditions[j],' = ',conditions[i],'-',conditions[j])
              #print(comparisons)
              comparison_list = c(comparison_list,comparisons)
            }
          }
        }
      }else{
        control = values$cont_matrix_comp
        for(j in c(1:length(conditions))){
          comparison = conditions[j]
          if(control != comparison){
            #print(paste(i,j))
            comparisons = paste0(comparison,'_vs_',control,' = ',comparison,'-',control)
            #print(comparisons)
            comparison_list = c(comparison_list,comparisons)
          }
        }
      }
      comparison_list
      design
      cmd = paste0('cont.matrix = makeContrasts(',paste(comparison_list,collapse=', '),', levels=design)')
      #print(cmd)
      eval(parse(text = cmd))
      cont.matrix
      result = list(design = design, cont.matrix = cont.matrix,cmd = cmd, comparison_list = comparison_list)
      return(result)
    }
    
    #### __RAW DATA ####
     
    output[['RAW-boxplot_ui']] = renderUI({   
      
      data = E()$E  
      log_rb = FALSE
      result_list = array_boxplot_function_2(data,
                                   metadata_names(),probe_names(),
                                   metadata_conditions(),selected_metadata(),
                                   probes(),
                                   log_rb,input,values)
      p
      id = 'RAW'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Raw Data Expression Intensity Boxplot',result_list$plot_height))
    })
    
    RAW_df = reactive({
      df = E()$E  
      colnames(df) = metadata_names()
      df = df %>% 
        as.data.frame()
      df$probe = probe_names()
      df
      #CV_df = CV_df_function(df,metadata())
      #CV_df
    })
    
    
    output[['RAW-table']] = DT::renderDataTable({
      RAW_df()
    },rownames = FALSE)
    
    output[['RAW-CV_plot_ui']] = renderUI({  
      df = RAW_df()
      data = CV_df_function(df,metadata()) 
      
      p = CV_plot_function(data,metadata(),probes())
      p = gg_fill_function(p)
      
      id = 'RAW'
      name = 'CV'
      title = "RAW data CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    
    output[['RAW-MA_plot_ui']] = renderUI({
   
      
      df = RAW_df()
      p = MA_plot_function(df,probes())
      #p = gg_col_function(p)
      
      id = 'RAW'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW-missing_plot_ui']] = renderUI({  
 
      df = RAW_df()
      p = missingness_function(df,metadata())$p
      p = gg_fill_function(p)
      
      plot_Server('RAW','missingness',p)
      do.call(tagList,plot_UI('RAW','missingness','Percentage of missing values per array'))
     
    })
    
    output[['RAW-Heatmap_ui']] = renderUI({ 
      df = E()$E      
      colnames(df) = metadata_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input) 
      
      id = 'RAW'
      name = 'Hcluster'
      

      ht_list = array_HeatMap_function(m,metadata(),selected_metadata(),probe_names(),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list) 
      #title = paste('removed ',paste(ht_list$removed,collapse = ' and '), 'values')
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = RAW_df()  
      probes = probes()
      metadata = metadata()
      plot_list = triplicate_cv_plot_function(df,probes(),metadata())
      
      id = 'RAW'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)
      
      do.call(tagList,CV_UI(id,name,values))
    })})
    
    output$Raw_tabs_ui = renderUI({ 
      do.call(tagList,PlotTabs_UI(id = "RAW",values))
    })
    
    
    #----------------------------------SPOT FILTERING----------------------------------------
    
    E_filter_before = reactive({withProgress(message = 'Spot Filtering',{
      E = E()
      if(!is.null(E()$weights)){
        E_fg = E$E
        E_fg
        E_weights = E()$weights
        E_filter = E_fg * E_weights
        E_filter[E_filter == 0] = NA
        E$E = E_filter
      }
      E$E
      E
    })})
    
    E_filter = reactive({
      
      E_filter_before()$E
      
    })
    
    output[['RAW_filter-boxplot_ui']] = renderUI({   
      
      data = E_filter()
      log_rb = FALSE
      result_list = array_boxplot_function_2(data,
                                             metadata_names(),probe_names(),metadata_conditions(),selected_metadata(),probes(),
                                             log_rb,input,values)
      
      id = 'RAW_filter'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Raw Data Expression Intensity Boxplot',result_list$plot_height))
    })
    
    E_filter_df = reactive({
      df = E_filter() 
      colnames(df) = metadata_names()
      df = df %>% 
        as.data.frame
      df$probe = probe_names()
      df
    })
    
    output[['RAW_filter-table']] = DT::renderDataTable({
      E_filter_df()
    },rownames = FALSE)
    
    output[['RAW_filter-CV_plot_ui']] = renderUI({     
      df = E_filter_df() 
      data = CV_df_function(df,metadata())
      p = CV_plot_function(data,metadata(),probes())
      p = gg_fill_function(p)
      
      id = 'RAW_filter'
      name = 'CV'
      title = "RAW data CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    output[['RAW_filter-MA_plot_ui']] = renderUI({
      
      df = E_filter_df()  
      p = MA_plot_function(df,probes())

      id = 'RAW_filter'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW_filter-missing_plot_ui']] = renderUI({   
      df = E_filter_df()
      p = missingness_function(df,metadata())$p
      p = gg_fill_function(p)
      
      id = 'RAW_filter'
      name = 'missingness' 
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Percentage of missing values per array'))
    })
    
    output[['RAW_filter-Heatmap_ui']] = renderUI({ 
      df = E_filter()     
      colnames(df) = metadata_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      
      id = 'RAW_filter'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,metadata(),selected_metadata(),probe_names(),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW_filter-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = E_filter_df()  
      probes = probes()
      metadata = metadata()
      plot_list = triplicate_cv_plot_function(df,probes(),metadata())
      
      id = 'RAW_filter'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)
      
      do.call(tagList,CV_UI(id,name,values))
    })})
    
    output$Raw_filter_tabs_ui = renderUI({ 
      do.call(tagList,PlotTabs_UI(id = "RAW_filter",values))
    })
    
    
    #---------------------------------Background Correction----------------------------
    
    

    
    E_corr = reactive({withProgress(message = 'Background Correction',{
        #backgroundCorrect(E_filter_before(), method = input$backgroundCorrect_method, offset = 0)
      backgroundCorrect(E_filter_before(), method = input$backgroundCorrect_method)
      
    })})
    
    output[['RAW_corr-boxplot_ui']] = renderUI({   
      
      data = E_corr()$E   
      log_rb = FALSE
      result_list = array_boxplot_function_2(data,
                                             metadata_names(),probe_names(),metadata_conditions(),selected_metadata(),probes(),
                                             log_rb,input,values)
      
      id = 'RAW_corr'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Background Corrected Expression Intensity Boxplot',result_list$plot_height))
    })
    
    E_corr_df = reactive({
      df = E_corr()$E
      colnames(df) = metadata_names()
      df = df %>% 
        as.data.frame
      df$probe = probe_names()
      df
    })
    
    output[['RAW_corr-table']] = DT::renderDataTable({
      E_corr_df()
    },rownames = FALSE)
    
    output[['RAW_corr-CV_plot_ui']] = renderUI({     
      df = E_corr_df() 
      data = CV_df_function(df,metadata())
      p = CV_plot_function(data,metadata(),probes())
      p = gg_fill_function(p)
      
      id = 'RAW_corr'
      name = 'CV'
      title = "Background Corrected CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    output[['RAW_corr-MA_plot_ui']] = renderUI({
      
      df = E_corr_df()  
      p = MA_plot_function(df,probes())
      
      id = 'RAW_corr'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW_corr-missing_plot_ui']] = renderUI({   
      df = E_corr_df()
      p = missingness_function(df,metadata())$p
      p = gg_fill_function(p)
      
      id = 'RAW_corr'
      name = 'missingness' 
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Percentage of missing values per array'))
    })
    
    output[['RAW_corr-Heatmap_ui']] = renderUI({ 
      df = E_corr()$E       
      colnames(df) = metadata_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      
      id = 'RAW_corr'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,metadata(),selected_metadata(),probe_names(),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW_corr-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = E_corr_df()  
      probes = probes()
      metadata = metadata()
      plot_list = triplicate_cv_plot_function(df,probes(),metadata())
      
      id = 'RAW_corr'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)
      
      do.call(tagList,CV_UI(id,name,values))
    })})
    
    output$Raw_corr_tabs_ui = renderUI({ 
      do.call(tagList,PlotTabs_UI(id = "RAW_corr",values))
    })
    

    #---------------------------------Normalization------------------------------------
    

    pre_norm_function = function(data,probe_names,metadata_names,selected_metadata_names,removed_probes,log_rb){
      df = as.data.frame(data)
      dim(df)
      colnames(df) <- metadata_names
      df$probe = probe_names
      
      df_f = df %>% 
        dplyr::filter(probe %in% removed_probes)
      
      df_m = df_f %>% 
        dplyr::select(-probe)
      
      df_m = df_m %>% 
        dplyr::select(one_of(selected_metadata_names))
      m = as.matrix(df_m)
      if(log_rb == T){
        m = log2(m)
      }
      m
      list(m = m, probes = df_f$probe)
    }
    
    norm_function = function(m,method,probes){
      E_norm = normalizeBetweenArrays(m,method = method)
      E_norm = as.data.frame(E_norm)
      E_norm$probe <- probes
      E_norm
    }
    
    E_norm = reactive({   withProgress(message = 'Normalisation',{
      
        data = E_corr()$E  
        probe_names = probe_names()
        metadata_names = metadata_names()
        removed_probes = removed_probes()
        log_rb = values$log_rb
      
        norm_list = pre_norm_function(E_corr()$E,probe_names(),metadata_names(),selected_metadata_names(),removed_probes(),values$log_rb)
        E_norm = norm_function(norm_list$m,input$normalisation_method,norm_list$probes)
        E_norm

    })})
    
    output[['RAW_norm-table']] = DT::renderDataTable({
        E_norm()
    },rownames = FALSE)
    
    #----------------------------Visualization of Normalized data---------------------------------------------
    
    
    output[['RAW_norm-boxplot_ui']] = renderUI({   
       
      data = E_norm() %>% 
        dplyr::select(-probe)
      log_rb = values$log_rb
      #p = array_boxplot_function(data,metadata_names(),probe_names(),metadata_conditions(),selected_metadata(),log_rb,input)
      result_list = array_boxplot_function_2(data,
                                             selected_metadata_names(),E_norm()$probe,metadata_conditions(),selected_metadata(),probes(),
                                             log_rb,input,values)
      
      
      id = 'RAW_norm'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Background Corrected & Normalised Expression Intensity Boxplot',result_list$plot_height))
    })
    
    output[['RAW_norm-CV_plot_ui']] = renderUI({     
      df = E_norm() 
      data = CV_df_function(df,metadata())
      #proteins = proteins() %>% 
      #  dplyr::rename('probe' = protein)
      
      
      p = CV_plot_function(data,metadata(),probes())
      p = gg_fill_function(p)
      
      id = 'RAW_norm'
      name = 'CV'
      title = "Background Corrected & Normalised CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    output[['RAW_norm-MA_plot_ui']] = renderUI({
      
      df = E_norm()  
      p = MA_plot_function(df,probes())
      
      id = 'RAW_norm'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW_norm-missing_plot_ui']] = renderUI({   
      df = E_norm()
      p = missingness_function(df,metadata())$p
      p = gg_fill_function(p)
      
      id = 'RAW_norm'
      name = 'missingness' 
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Percentage of missing values per array'))
    })
    
    output[['RAW_norm-Heatmap_ui']] = renderUI({ 
      df = E_norm() %>%  
        dplyr::select(-probe)
      colnames(df) = selected_metadata_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      
      id = 'RAW_norm'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,selected_metadata(),selected_metadata(),E_norm()$probe,input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW_norm-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = E_norm()  
      probes = probes()
      metadata = metadata()
      plot_list = triplicate_cv_plot_function(df,probes(),metadata())
      
      id = 'RAW_norm'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)
      
      do.call(tagList,CV_UI(id,name,values))
    })})
    
    output$Raw_norm_tabs_ui = renderUI({ 
      do.call(tagList,PlotTabs_UI(id = "RAW_norm",values))
    })
    
    
    #---------------------------------Array weights----------------------------------
    arrayw = reactive({withProgress(message = 'Array weights',{
        data = E_norm() %>% dplyr::select(-probe)
        aw = arrayWeights(data)
        names(aw) = colnames(data)
        aw
        

    })})
    
    arrayw_df = reactive({
        df = as.data.frame(t(arrayw()))
        rownames(df) = 'Array Weights'
        str(df)
        df
    })
    
    output$arrayw_table = DT::renderDataTable({
       arrayw_df()
    },rownames = FALSE)
    
    output$download_arrayw <- downloadHandler(
        filename = function(){"arrayw.txt"}, 
        content = function(fname){
            write.table(arrayw_df(), fname,sep = '\t', col.names=NA)
        }
    )
    
    arrayw_barplot = reactive({
        
        df = arrayw_df()
        as.tbl(df)
        df_l = gather(df,Name,Weight) %>% 
            left_join(metadata_conditions())
        as.tbl(df_l)
        p = ggplot(df_l)
          if(length(unique(df_l$Condition)) > 1){
            p = p + geom_col(aes(x = Name,y = Weight,fill = Condition))
          }else{
            p = p + geom_col(aes(x = Name,y = Weight))
          }
          if(length(unique(df_l$Group)) > 1){
            #theme(axis.text.x = element_text(angle = 90)) + 
            p = p + facet_grid(Group ~ .)
          }
           
            p = p + geom_hline(yintercept = values$array_weight_threshold, linetype = 'dashed')# +
            #ggtitle('Weights')
        p = gg_fill_function(p)
        p
        
    
    }) 
    
    output$arrayw_barplot_ui = renderUI({ 
      p = arrayw_barplot()
      id = 'arrayw'
      name = 'barplot'
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name))
      
    })
    
    #---------------------Condense and clean up data--------------------------------
    
    CV <- function(x) ( 100*(sd(x)/mean(x)))
    
    protein_collapse_function = function(df,probes,values){
      
      data = df %>% 
        dplyr::select(-probe)
      colnames(data)
      as.tbl(data)
      colnames(data)

      
      df_probes = df  %>% 
        left_join(probes)
      as.tbl(df_probes)
      colnames(df_probes)
      if(values$probe_collapse_digits == TRUE){
        df_probes$protein <- gsub("\\.[[:digit:]]*$", "", df_probes[,values$protein_column])
      }else{
        df_probes$protein = df_probes[,values$protein_column]
      }
      df_collapse = df_probes %>% 
        dplyr::select(one_of(c('protein','Category',colnames(data))))
      colnames(df_collapse)
      
      data <- as.data.frame(df_collapse) %>% group_by(protein,Category) %>%
        summarise_all(funs(mean))
      data
    }
    
    data_full = reactive({withProgress(message = 'Generating Final Data',{ 
      data = E_norm()
      req(values$protein_column) 
      req(E_norm())
      req(probes())
      df = protein_collapse_function(E_norm(),probes(),values)
      df
    })})
    
    
    output$drop_cols_ui = renderUI({
        df = proteins() 
        selectInput('drop_col','Drop by',colnames(df),'Category')
    })
    
    observeEvent(input$drop_col,{ 
      values$drop_col = input$drop_col
    })
    
    output$drop_rows_ui = renderUI({
        df = proteins()
        selection = unique(df[,values$drop_col])
        selectInput('drop_row',paste(values$drop_col,' to drop'),selection,'control',multiple = T,width = 1200)
    })
    
    observeEvent(input$drop_row,{ 
      values$drop_row = input$drop_row
    })
    
    protein_filter_function = function(data,proteins_df,values){ 
      (var <- rlang::parse_quosures(paste(values$drop_col))[[1]])

      drops = proteins_df %>% 
        filter(!!var %in% values$drop_row) %>% 
        pull(protein)

      drops
      data = data %>%  
        filter(!protein %in% drops)
      rownames(data)
      data
    }
    
    data = reactive({ withProgress(message = 'Collate probes',{
      proteins_df = proteins()  
      data = data_full() %>% 
        dplyr::select(-Category)
      df = protein_filter_function(data,proteins(),values)
      df$protein
      dim(df)
      keep_weight = arrayw_df() %>% 
        gather() %>% 
        filter(value <= values$array_weight_threshold) %>% 
        pull(key)
      keep_weight
      
      if(values$drop_by_weight == TRUE){
        df = df %>% 
          dplyr::select(one_of(c('protein',keep_weight)))
      }
      dim(df)
      as.data.frame(df)

    })})
    
    #----------------------------Visualization of Condensed dataset-----------------------------------------
    
    
    output[['Data-boxplot_ui']] = renderUI({    
      df = data() %>% 
        dplyr::select(-protein)
      values$collapse_boxplots
      p = array_boxplot_function(df,colnames(df),rownames(df),metadata_conditions(),selected_metadata(),values$log_rb,input)
      
      id = 'Data'
      name = 'boxplot'
      
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Data Expression Intensity Boxplot'))
    })
    
    data_df = reactive({
      df = data() %>%  
        dplyr::rename(probe = protein)
    })
    
    output[['Data-CV_plot_ui']] = renderUI({     
      df = data_df()     
      data = CV_df_function(df,metadata())
      proteins = proteins() %>% 
        dplyr::rename('probe' = protein)
      probes = proteins
      result_list = CV_sample_plot_function(data,metadata(),proteins)
      
      p = result_list$p
      p = gg_fill_function(p)
      p
      
      d = result_list$d
      d = gg_col_function(d)
      
      p
      id = 'Data'
      name = 'CV'
      title = "Boxplot of CV's for protein expression intensities across all samples"
      plot_Server(id,name,p)
      p_list = plot_UI(id,name,title)
      
      id = 'Data'
      name = 'CV_density'
      title = "Density plot of CV's for protein expression intensities across all samples"
      plot_Server(id,name,d)
      
      
      d_list = plot_UI(id,name,title)
      
      list(p_list,d_list)
      
      do.call(tagList,list(p_list,d_list))
      
    })
    
    output[['Data-MA_plot_ui']] = renderUI({
      
      df = data_df()  
      proteins = proteins() %>% 
        dplyr::rename('probe' = protein)
      p = MA_plot_function(df,proteins)
      
      id = 'Data'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['Data-missing_plot_ui']] = renderUI({   
      df = data_df()
      p = missingness_function(df,metadata())$p
      p = gg_fill_function(p)
      
      id = 'Data'
      name = 'missingness' 
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Percentage of missing values per array'))
    })
    
    output[['Data-Heatmap_ui']] = renderUI({ 
      df = data() %>%  
        column_to_rownames('protein')
        #dplyr::select(-protein)
      colnames(df) = selected_metadata_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      m
      id = 'Data'
      name = 'Hcluster'

      ht_list = array_HeatMap_function(m,selected_metadata(),selected_metadata(),data()$protein,input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    
    
    
    
    output$Data_tabs_ui = renderUI({
      #do.call(tagList,PlotTabs_UI(id = "Data"))
      
      ns <- NS("Data")
      if(values$app_version == 'pro'){
        lst = list(
          tabsetPanel(
            tabPanel('Table',
                     column(6,tags$h4(htmlOutput('data_dim_text'))),
                     column(2,downloadButton('download_data',"Data Table")),
                     column(2,downloadButton('download_ExpSet',"ExpSet")),
                     column(2,downloadButton('download_MSnSet',"MSnSet")),
                     
                     column(12,DT::dataTableOutput('data_table'))
            ),
            tabPanel('Expression Intensity Boxplot',
                     uiOutput(ns('boxplot_ui'))
                     ),
            tabPanel('CV',
                     uiOutput(ns('CV_plot_ui'))
                     ),
            tabPanel('Missing Values',
                     uiOutput(ns('missing_plot_ui'))
            ),
            tabPanel("MA Plot",
                     uiOutput(ns('MA_plot_ui')),
            ),
            tabPanel('Clustering',
                     column(12,uiOutput(ns('Heatmap_ui')))
            ))
        )
      }else{
        lst = list(
          tabsetPanel(
            tabPanel('Table',
                     column(8,tags$h4(htmlOutput('data_dim_text'))),
                     column(2,downloadButton('download_data',"Data Table")),
                     column(2,downloadButton('download_ExpSet',"ExpSet")),
                     
                     column(12,DT::dataTableOutput('data_table'))
            ),
            tabPanel('Expression Intensity Boxplot',
                     uiOutput(ns('boxplot_ui'))
            ),
            tabPanel('CV',
                     uiOutput(ns('CV_plot_ui'))
            ),
            tabPanel('Missing Values',
                     uiOutput(ns('missing_plot_ui'))
            ),
            tabPanel("MA Plot",
                     uiOutput(ns('MA_plot_ui')),
            ),
            tabPanel('Clustering',
                     column(12,uiOutput(ns('Heatmap_ui')))
            ))
        )
      }
      do.call(tagList,lst)
    })
    
    output$sig_Data_table_ui = renderUI({
      if(values$app_version == 'pro'){
        lst = list(
          column(6),
          column(2,downloadButton('download_sig_data',"Data Table")),
          column(2,downloadButton('download_sig_ExpSet',"ExpSet")),
          column(2,downloadButton('download_sig_MSnSet',"MSnSet")),
          
          column(12,DT::dataTableOutput('eBays_table'))
        )
      }else{
        lst = list(
          column(8),
          column(2,downloadButton('download_sig_data',"Data Table")),
          column(2,downloadButton('download_sig_ExpSet',"ExpSet")),
          column(12,DT::dataTableOutput('eBays_table'))
        )
      }
      do.call(tagList,lst)
      
    })

    
    output$data_table = DT::renderDataTable({
        data()
    },rownames = FALSE)
    
    output$download_data <- downloadHandler(
        filename = function(){"data.txt"},  
        content = function(fname){
            write.table(data(), fname,sep = '\t', row.names = F)
        }
    )
    
    output$download_sig_data <- downloadHandler(
      filename = function(){"data_DE.txt"},  
      content = function(fname){
        write.table(eBayes_test()$df, fname,sep = '\t', row.names = F)
      }
    )
    
    output$data_dim_text = renderPrint({
      cat(paste(dim(data())[1],'proteins and ',dim(data())[2],'samples'))
    })
    
    

    
    data_exp_set = reactive({
      df = data() %>% column_to_rownames('protein')
      m = as.matrix(df)
      
      samples = selected_metadata() %>% as.data.frame
      rownames(samples) = samples$Name
      
      arrayw_df = t(arrayw_df()) %>% 
        as.data.frame() %>% 
        rownames_to_column('Name')
      as.tbl(arrayw_df)
      
      samples = samples %>% 
        left_join(arrayw_df)
      as.tbl(samples)
      
      
      features = proteins() %>% as.data.frame()
      rownames(features) = proteins()$protein
      features = features[rownames(m),]
      
      list(m = m, samples = samples, features = features)
    })
    
    sig_data_exp_set = reactive({
      
      data_exp_set = data_exp_set()
      
      data_exp_set$features
      sig_df = eBayes_test()$df
  
      data_exp_set$features = data_exp_set$features %>% 
        left_join(sig_df)
      
      
      data_exp_set
      
    })
    
    MSnSet_function = function(m,samples,features){
      samples$sample_name = rownames(samples)
      phenoData = new('AnnotatedDataFrame',data = samples)
      phenoData
      
      features$feature = rownames(features)
      featureData =  new('AnnotatedDataFrame',data = features)
      featureData
      
      exp_set  = MSnSet(m) 
      phenoData(exp_set) = phenoData
      featureData(exp_set) = featureData
      exp_set
      #MSnSet(m,phenoData = phenoData,featureData = featureData)
    }
    
    ExpressionSet_function = function(m,samples,features){
      phenoData = new('AnnotatedDataFrame',data = samples)
      phenoData
      
      featureData =  new('AnnotatedDataFrame',data = features)
      featureData
      
      exp_set  = ExpressionSet(m) 
      phenoData(exp_set) = phenoData
      featureData(exp_set) = featureData
      exp_set
                    
    }
    
    data_ExpressionSet = reactive({ 
      
      data_exp_set = data_exp_set()
      
      ExpressionSet_function(data_exp_set$m,data_exp_set$samples,data_exp_set$features)
    })
    
    data_MSnSet = reactive({  

      data_exp_set = data_exp_set()
      
      MSnSet_function(data_exp_set$m,data_exp_set$samples,data_exp_set$features)
       
    })
    
    sig_data_ExpressionSet = reactive({ 
      
      data_exp_set = sig_data_exp_set() 
      as.tbl(data_exp_set$features)
      
      ExpressionSet_function(data_exp_set$m,data_exp_set$samples,data_exp_set$features)
    })
    
    sig_data_MSnSet = reactive({  
      
      data_exp_set = sig_data_exp_set()
      
      MSnSet_function(data_exp_set$m,data_exp_set$samples,data_exp_set$features)
      
    })
    
    output$download_ExpSet <- downloadHandler(
      filename = function(){"ExpressionSet.rds"}, 
      content = function(fname){
        saveRDS(data_ExpressionSet(),fname)
      }
    )
    
    output$download_MSnSet <- downloadHandler(
      filename = function(){"MSnSet.rds"}, 
      content = function(fname){
        saveRDS(data_MSnSet(),fname)
      }
    )
    
    output$download_sig_ExpSet <- downloadHandler(
      filename = function(){"ExpressionSet_DE.rds"}, 
      content = function(fname){
        saveRDS(sig_data_ExpressionSet(),fname)
      }
    )
    
    output$download_sig_MSnSet <- downloadHandler(
      filename = function(){"MSnSet_DE.rds"}, 
      content = function(fname){
        saveRDS(sig_data_MSnSet(),fname)
      }
    )
    

    
    
  ##### Optimal Cutpoints ######
    output$threshold_control_col_ui = renderUI({
      (selection = unique(metadata_conditions()$Condition))
      selected = selection[1]
      if('Control' %in% selection){
        selected = 'Control'
      }
      selectInput('threshold_control_column','Control Condition',selection,selected)
    })
    
    threshold_function = function(data,metadata,input){  
      thres.data = as.data.frame(t(data)) %>% 
        rownames_to_column('Name') %>% 
        left_join(metadata %>% dplyr::select(Name,Condition)) %>% 
        #rename(Name = 'Sample_ID') %>% 
        #rename(Condition = 'Label') %>% 
        dplyr::select(Name,Condition,everything())
      thres.data
      rownames(thres.data)
      as.tbl(thres.data)
      print(length(3:ncol(thres.data)))
      threshold_df = NULL
      threshold_df <- foreach(i = 3:ncol(thres.data), .combine = rbind) %do%{
        print(i)
        protein <- colnames(thres.data)[i]
        print(protein)
        ROC <- optimal.cutpoints( X = protein,
                                  status = "Condition",       ## Class Label column name
                                  data = thres.data, 
                                  tag.healthy = input$threshold_control_column,
                                  methods = "MinValueSp",  ## Specify methodology to determine threshold
                                  control = control.cutpoints(valueSp = 0.95), ## Additional settings to modify (only works for "Value" based methodologies)
                                  ci.fit = T)
        ROC
        data.frame(Name <- protein, ROC[[1]]$Global$optimal.cutoff)
        
      }
      threshold_df
    }
    
    threshold = reactive({  withProgress(message = 'Calculating optimal cuttoffs',{
      input$threshold_control_column 
      data = data() %>% 
        column_to_rownames('protein')
      metadata = metadata_conditions()
      
  
      threshold_df = tryCatch({threshold_function(data,metadata,input)}, error = function(e) {NULL})
      threshold_df
    })})
    
    output$threshold_ui = renderUI({ 
      threshold_df = threshold()
      threshold_df
      if(is.null(threshold_df)){
        lst = list(tags$h3('No cutoffs fullfill the conditions for threshold estimation'))
      }else{
        output$threshold_G_df = DT::renderDataTable({
          threshold_df
        },rownames = FALSE)
        lst = list(DT::dataTableOutput('threshold_G_df'))
      }
      do.call(tagList,lst)
    })
    
    threshold_data = reactive({
      df_t = NULL 
      if(!is.null(threshold())){
        data = data() %>% 
          column_to_rownames('protein')
        metadata = metadata_conditions()
        threshold_df = threshold()
        as.tbl(data)
        rownames(data)
        df = data %>% 
          rownames_to_column('protein') %>% 
          left_join(threshold_df %>% 
                      dplyr::select(Name....protein,cutoff) %>% 
                      dplyr::rename(protein = Name....protein)
          )
        as.tbl(df)
        df_t = df[,colnames(data)] - df$cutoff
        df_t[df_t <= 0] = NA
        df_t = df_t + df$cutoff
        rownames(df_t) = rownames(data)  
        df_t = df_t %>% rownames_to_column('protein')
        colnames(df_t)
        df_t$protein
        colnames(df_t)
      }
      df_t
    })
    
    output$threshold_output_ui = renderUI({
      if(!is.null(threshold())){
        output$threshold_output_df = DT::renderDataTable({
          threshold_data() 
        },rownames = FALSE)
        lst = list(column(12,
                          column(10,tags$h2('Data above threshold')),
                          column(2,downloadButton('download_threshold_MSnSet',"MSnSet")),
                          column(12,DT::dataTableOutput('threshold_output_df'))
        )
        )
        do.call(tagList,lst)
      }
    })
    
    
    threshold_MSnSet = reactive({  
      if(!is.null(threshold_data())){
        data = threshold_data() %>%   
          column_to_rownames('protein')
        colnames(data)
        rownames(data)
        metadata = metadata_conditions()
        rownames(metadata) = metadata$Name
        proteins = proteins()
        rownames(proteins) = proteins$protein
        features = proteins
        duplicated(features$protein)
        features = features %>% filter(protein %in% threshold_data()$protein) %>% 
          left_join(threshold() %>% 
                      dplyr::rename('protein' = Name....protein) %>% 
                      filter(protein %in% threshold_data()$protein))
        
        features
        #features_n$protein
        
        duplicated(features$protein)
        rownames(features) = features$protein
          
        dim(features)
        samples = metadata
        samples = samples[colnames(data),]
        
        arrayw_df = arrayw_df()
        w = t(arrayw_df) %>% 
          as.data.frame %>% 
          rownames_to_column('Name')
        
        samples =samples %>% 
          left_join(w)
        rownames(samples) = samples$Name
        
        dim(samples)
        dim(data)
        expression_set_function(data,samples,features)
      }
      
    })
    
    output$download_threshold_MSnSet <- downloadHandler(
      filename = function(){"threshold_MSnSet.rds"}, 
      content = function(fname){
        saveRDS(threshold_MSnSet(),fname)
      }
    )
    

    
    output$threshold_Heatmap_ui = renderUI({ 
      if(!is.null(threshold_data())){
        msn_set = threshold_MSnSet() 
        m = exprs(msn_set)
        m
        samples  = msn_set@phenoData@data
        features = msn_set@featureData@data
        m[is.na(m)] = 0
        (select_cols = intersect(selected_metadata()$Name,colnames(m)))
        m = m[,select_cols]
        
        id = 'Cutoff'
        name = 'Hcluster'
        ht_list = array_HeatMap_function(m,selected_metadata(),samples,features$protein,input$r_col,values$heatmap_order)
        ht_list$p
        ht_plot_Server(id,name,ht_list)
        do.call(tagList,plot_UI(id,name,ht_list$warning))
      }

      
    })



  #### Comparing Methods ####
  
    
  corr = reactive({withProgress(message = 'multi background correction',{
    E = E_filter_before()
    
    E_corr <-  backgroundCorrect(E, method = "none")
    S_corr <-  backgroundCorrect(E, method = "subtract")
    M_corr <-  backgroundCorrect(E, method = "movingmin")
    N_corr <-  backgroundCorrect(E, method = "normexp")
    
    list(E = E_corr,
         S = S_corr,
         M = M_corr,
         N = N_corr)
  })})
    

    
  multi_norm_function = function(corr_data,probe_names,metadata_names,selected_metadata_names,removed_probes,log_rb,method,proteins,input){
    
    E_norm_list = pre_norm_function(corr_data$E$E,probe_names,metadata_names,selected_metadata_names,removed_probes,log_rb)
    E_norm = norm_function(E_norm_list$m,method,E_norm_list$probes)
    E_proteins = protein_collapse_function(E_norm,probes(),values)
    E_data = protein_filter_function(E_proteins,proteins,values)
    
    E_data = as.data.frame(E_data) %>% dplyr::select(-one_of('protein','Category'))
    E = list(norm = E_norm,data = E_data)
    
    S_norm_list = pre_norm_function(corr_data$S$E,probe_names,metadata_names,selected_metadata_names,removed_probes,log_rb)
    S_norm = norm_function(S_norm_list$m,method,S_norm_list$probes)
    S_proteins = protein_collapse_function(S_norm,probes(),values)
    S_data = protein_filter_function(S_proteins,proteins,values)

    S_data = as.data.frame(S_data) %>% dplyr::select(-one_of('protein','Category'))
    S = list(norm = S_norm,data = S_data)
    
    N_norm_list = pre_norm_function(corr_data$N$E,probe_names,metadata_names,selected_metadata_names,removed_probes,log_rb)
    N_norm = norm_function(N_norm_list$m,method,N_norm_list$probes)
    N_proteins = protein_collapse_function(N_norm,probes(),values)
    N_data = protein_filter_function(N_proteins,proteins,values)

    N_data = as.data.frame(N_data) %>% dplyr::select(-one_of('protein','Category'))
    N = list(norm = N_norm,data = N_data)
    
    
    M_norm_list = pre_norm_function(corr_data$M$E,probe_names,metadata_names,selected_metadata_names,removed_probes,log_rb)
    M_norm = norm_function(M_norm_list$m,method,M_norm_list$probes)
    M_proteins = protein_collapse_function(M_norm,probes(),values)
    M_data = protein_filter_function(M_proteins,proteins,values)

    M_data = as.data.frame(M_data) %>% dplyr::select(-one_of('protein','Category'))
    M = list(norm = M_norm,data = M_data)
    
    
    list(E = E,
         S = S,
         M = M,
         N = N)
  }
  norm = reactive({withProgress(message = 'multi normalisation',{
    corr_data = corr()    

    probe_names = probe_names()
    metadata_names = metadata_names()
    removed_probes = removed_probes()
    log_rb = values$log_rb
    proteins = proteins()
    
    
    method = "none"
    E = multi_norm_function(corr_data,probe_names(),metadata_names(),selected_metadata_names(),removed_probes(),values$log_rb,method,proteins(),input)
    method = "quantile"
    Q = multi_norm_function(corr_data,probe_names(),metadata_names(),selected_metadata_names(),removed_probes(),values$log_rb,method,proteins(),input)
    method = "cyclicloess"
    C = multi_norm_function(corr_data,probe_names(),metadata_names(),selected_metadata_names(),removed_probes(),values$log_rb,method,proteins(),input)
    method = "scale"
    S = multi_norm_function(corr_data,probe_names(),metadata_names(),selected_metadata_names(),removed_probes(),values$log_rb,method,proteins(),input)
    
    
    
    
    list(E = E, 
         Q = Q,
         C = C,
         S = S)
    
  })})


  
  multi_missing_function = function(data,metadata,method){
    miss_E = missingness_function(data$E$norm,metadata)
    miss_E$df$Correction = 'None'
    miss_S = missingness_function(data$S$norm,metadata)
    miss_S$df$Correction = 'Subtraction'
    miss_M = missingness_function(data$M$norm,metadata)
    miss_M$df$Correction = 'Movingminimum'
    miss_N = missingness_function(data$N$norm,metadata)
    miss_N$df$Correction = 'Normexp'
    
    df = miss_E$df %>% 
      rbind(miss_S$df) %>% 
      rbind(miss_M$df) %>% 
      rbind(miss_N$df)
    
    df$Normalisation = method
    df
  }
  
  multi_missing = reactive({ 
    corr_data = corr()
    probe_names = probe_names()
    metadata_names = selected_metadata_names()
    metadata = selected_metadata()
    names(norm())
    E_df = multi_missing_function(norm()$E,selected_metadata(),'None')
    S_df = multi_missing_function(norm()$S,selected_metadata(),'Scale')
    Q_df = multi_missing_function(norm()$Q,selected_metadata(),'Quantile')
    C_df = multi_missing_function(norm()$C,selected_metadata(),'Cyclicloess')
    
    
    df = E_df %>% 
      rbind(S_df) %>% 
      rbind(Q_df) %>% 
      rbind(C_df)
    
    df$Correction = factor(df$Correction, levels = unique(df$Correction))
    
    df$Normalisation = factor(df$Normalisation, levels = unique(df$Normalisation))
    
    

    df
    # p = ggplot(df) +
    #   geom_boxplot(aes(x = Correction,y = `Percentage missing`, fill = Condition)) + 
    #   facet_grid(Normalisation ~ .)
    # 
    # p

  })
  
  output$multi_missing_plot_ui = renderUI({
    
    #plot_height = single_plot_height * length(input$MA_normalisation)
   
      df = multi_missing() %>% 
        filter(Correction %in% input$MA_correction, 
               Normalisation %in% input$MA_normalisation)
      as.tbl(df)
      
      
      
      
      if(input$flip_facets == F){
        plot_height = single_plot_height * length(input$MA_normalisation)
        p = ggplot(df) +
          geom_boxplot(aes(x = Correction,y = `Percentage missing`, fill = Category)) + 
          facet_grid(Normalisation ~ .)
      }else{
        plot_height = single_plot_height * length(input$MA_correction)
        p = ggplot(df) +
          geom_boxplot(aes(x = Normalisation,y = `Percentage missing`, fill = Category)) + 
          facet_grid(Correction ~ .)
      }

    output$multi_missing_plot = renderPlot({
      p
    },height = plot_height)
    plotOutput('multi_missing_plot',height = plot_height)
  
  })
   
  multi_CV_function = function(data,metadata,method){
    miss_E = CV_df_function(data$E$norm,metadata)
    miss_E$Correction = 'None'
    miss_S = CV_df_function(data$S$norm,metadata)
    miss_S$Correction = 'Subtraction'
    miss_M = CV_df_function(data$M$norm,metadata)
    miss_M$Correction = 'Movingminimum'
    miss_N = CV_df_function(data$N$norm,metadata)
    miss_N$Correction = 'Normexp'
    
    df = miss_E %>% 
      rbind(miss_S) %>% 
      rbind(miss_M) %>% 
      rbind(miss_N)
    
    df$Normalisation = method
    df
  }
  
  multi_CV = reactive({ 
    corr_data = corr()
    probe_names = probe_names()
    metadata_names = selected_metadata_names()
    metadata = selected_metadata()
    names(norm())
    data = norm()$E
    E_df = multi_CV_function(norm()$E,selected_metadata(),'None')
    S_df = multi_CV_function(norm()$S,selected_metadata(),'Scale')
    Q_df = multi_CV_function(norm()$Q,selected_metadata(),'Quantile')
    C_df = multi_CV_function(norm()$C,selected_metadata(),'Cyclicloess')
    
    
    df = E_df %>% 
      rbind(S_df) %>% 
      rbind(Q_df) %>% 
      rbind(C_df)
    as.tbl(df)
    
    df$Correction = factor(df$Correction, levels = unique(df$Correction))
    
    df$Normalisation = factor(df$Normalisation, levels = unique(df$Normalisation))
    
    
    
    df = df %>% 
      left_join(probes())
    as.tbl(df)
    
  df
    
  })
  
  output$probe_categories_select_ui = renderUI({
    (selection = unique(multi_CV()$Category))
    selectInput("CV_category",'Category',selection,selection,multiple = T)
  })
  
  
  output$multi_CV_plot_ui = renderUI({  
    #plot_height = single_plot_height * length(input$MA_normalisation)
    df = multi_CV() %>% 
      filter(Correction %in% input$MA_correction, 
             Normalisation %in% input$MA_normalisation,
             Category %in% input$CV_category)
    if(input$flip_facets == F){
      plot_height = single_plot_height * length(input$MA_normalisation)
      p = ggplot(df) +
        geom_boxplot(aes(x = Correction,y = CV, fill = Category)) + 
        facet_grid(Normalisation ~ .)
    }else{
      plot_height = single_plot_height * length(input$MA_correction)
      p = ggplot(df) +
        geom_boxplot(aes(x = Normalisation,y = CV, fill = Category)) + 
        facet_grid(Correction ~ .)
    }

    output$multi_CV_plot = renderPlot({
   

      p
    },height = plot_height)
    plotOutput('multi_CV_plot',height = plot_height)
    
  })
  
  eBayes_single_function = function(df){
    fit <- lmFit(df)
    fit2 <- eBayes(fit)
    #fit2 <- as.data.frame(fit2)
    Sig_Proteins <- topTable(fit2, adjust.method = input$mtc,number = dim(df)[1])
    threshold <- Sig_Proteins$adj.P.Val < as.numeric(input$pvalue_select)
    length(which(threshold))
    Sig_Proteins <- cbind(Sig_Proteins, threshold)
    Sig_Proteins
    list(df = as.data.frame(fit2), top_df = Sig_Proteins)
  }
  
  multi_fit_function = function(norm){
    
    # Rfit <- lmFit(norm$E)
    # Rfit2 <- eBayes(Rfit)
    # Rfit2 <- as.data.frame(Rfit2)
    # 
    # Sfit <- lmFit(norm$S)
    # Sfit2 <- eBayes(Sfit)
    # Sfit2 <- as.data.frame(Sfit2)
    # 
    # Mfit <- lmFit(norm$M)
    # Mfit2 <- eBayes(Mfit)
    # Mfit2 <- as.data.frame(Mfit2)
    # 
    # Nfit <- lmFit(norm$N)
    # Nfit2 <- eBayes(Nfit)
    # Nfit2 <- as.data.frame(Nfit2)
    Rfit2 = eBayes_single_function(norm$E$data)
    Sfit2 = eBayes_single_function(norm$S$data)
    Mfit2 = eBayes_single_function(norm$M$data)
    Nfit2 = eBayes_single_function(norm$N$data)
    
    list(E = Rfit2,
         S = Rfit2,
         M = Mfit2,
         N = Nfit2)
  }
  
  eBayes_function = function(data){
    df = data %>% 
      as.data.frame# %>% 
      #column_to_rownames('protein')  
    
    (selected_cols = intersect(selected_metadata()$Name,colnames(df)))
    
    df = df[,selected_cols]
    
    cont_matrix_list = cont_matrix_function(df,selected_metadata(),input)
    design = cont_matrix_list$design
    cont.matrix = cont_matrix_list$cont.matrix
    
    fit = lmFit(df,design, weights = as.numeric(arrayw_df()[,selected_cols]))
    fit
    
    
    fit2 = contrasts.fit(fit,cont.matrix)
    fit2 = eBayes(fit2)
    fit2
    
    Sig_Proteins <- topTable(fit2, adjust.method = input$mtc,number = dim(df)[1])
    threshold <- Sig_Proteins$adj.P.Val < as.numeric(input$pvalue_select)
    length(which(threshold))
    Sig_Proteins <- cbind(Sig_Proteins, threshold)
    Sig_Proteins
    df =  as.data.frame(fit2)
    list(top_df = Sig_Proteins, df = df,cont_matrix = cont_matrix_list$cmd, comparison_list = cont_matrix_list$comparison_list)
  }
  
  multi_DE_function = function(norm){
   
    df = norm$E$data
    df$protein = data()$protein
    Rfit2 <- eBayes_function(df)
    
    df = norm$S$data
    df$protein = data()$protein
    Sfit2 <- eBayes_function(df)
    
    df = norm$M$data
    df$protein = data()$protein
    Mfit2 <- eBayes_function(df)
    
    df = norm$N$data
    df$protein = data()$protein
    Nfit2 <- eBayes_function(df)
    
    list(E = Rfit2,
         S = Rfit2,
         M = Mfit2,
         N = Nfit2)
  }
  
  cont_matix = reactive({
    df = norm()$E$E$data
    as.tbl(df)
    result_list = cont_matrix_function(df,metadata(),input)
    result_list
    
    
  })
   
  Rfit = reactive({withProgress(message = 'multi fit',{
    list(E = multi_fit_function(norm()$E), 
         Q = multi_fit_function(norm()$Q),
         C = multi_fit_function(norm()$C),
         S = multi_fit_function(norm()$S)
    )
    
  })})
  
  DE_Rfit = reactive({withProgress(message = 'multi DE fit',{
    list(E = multi_DE_function(norm()$E),
         Q = multi_DE_function(norm()$Q),
         C = multi_DE_function(norm()$C),
         S = multi_DE_function(norm()$S)
    )
    
  })})
  

  
  
  
  output$MA_correction_ui = renderUI({
    df = multi_fit_data() 
    selectInput('MA_correction','Background Correction',unique(df$Correction),unique(df$Correction),multiple = T)
  })
  
  output$MA_normalisation_ui = renderUI({
    df = multi_fit_data()
    selectInput('MA_normalisation','Normalisation',unique(df$Normalisation),unique(df$Normalisation),multiple = T)
  })
  

  
  multi_collate_fit_function = function(data,norm,table){
    print('hit') 
    Rfit2 = data$E[[table]]
    Rfit2$Correction = 'None'
    Sfit2 = data$S[[table]]
    Sfit2$Correction = 'Subtraction'
    Mfit2 = data$M[[table]]
    Mfit2$Correction = 'Movingminimum'
    Nfit2 = data$N[[table]]
    Nfit2$Correction = 'Normexp'
    df <- Rfit2 %>% 
      rbind(Sfit2) %>% 
      rbind(Mfit2) %>% 
      rbind(Nfit2)
    df$Normalisation = norm
    df
  }
  
  multi_fit_data = reactive({

    
    if(input$multi_DE == FALSE){
      E_M_l = multi_collate_fit_function(Rfit()$E,"None",'df')
      S_M_l = multi_collate_fit_function(Rfit()$S,"Scale",'df')
      Q_M_l = multi_collate_fit_function(Rfit()$Q,"Quantile",'df')
      C_M_l = multi_collate_fit_function(Rfit()$C,"Cyclicloess",'df')
    }else{
      E_M_l = multi_collate_fit_function(DE_Rfit()$E,"None",'top_df')
      S_M_l = multi_collate_fit_function(DE_Rfit()$S,"Scale",'top_df')
      Q_M_l = multi_collate_fit_function(DE_Rfit()$Q,"Quantile",'top_df')
      C_M_l = multi_collate_fit_function(DE_Rfit()$C,"Cyclicloess",'top_df')
    }
    
    M_l = E_M_l %>% 
      rbind(S_M_l) %>% 
      rbind(Q_M_l) %>% 
      rbind(C_M_l)
    
    as.tbl(M_l)
    
    M_l$Correction = factor(M_l$Correction, levels = unique(M_l$Correction))
    M_l$Normalisation = factor(M_l$Normalisation, levels = unique(M_l$Normalisation))
    M_l
    
  })
  

  
  output$M_plot_ui = renderUI({   
    plot_height = single_plot_height * length(input$MA_normalisation)
  
    output$M_plot = renderPlot({
      

      
      M_l = multi_fit_data()  %>% 
        filter(Correction %in% input$MA_correction, 
               Normalisation %in% input$MA_normalisation)

      if(input$multi_DE == FALSE){
        if(input$log_rb_M == TRUE){
          p = ggplot(M_l, aes(x= Correction, y=log(F), fill=Correction))
          q = quantile(log(M_l$F),na.rm = T)
        }else{
          p = ggplot(M_l, aes(x= Correction, y=F, fill=Correction)) 
            p = p + scale_y_continuous(trans='log2')
          q = quantile(M_l$F,na.rm = T)
        }
      }else{
        p = ggplot(M_l, aes(x= Correction, y=logFC, fill=Correction))
        q = quantile(M_l$logFC,na.rm = T)
      }
          p = p + geom_boxplot() +
  
          theme(axis.text = element_text(size = 12), axis.title.x = element_blank(), legend.position = "none") +
          facet_grid(Normalisation ~ .)

          if(values$plot_lim == 'Quantile'){
            p = p +  ylim(q[2],q[4])
          }
          if(values$plot_lim == '2x Quantile'){
            p = p +  ylim(q[2]/2,q[4]*2)
          }

      p
    },height = plot_height)
  
    plotOutput('M_plot',height = plot_height)
  })
  
  output$A_plot_ui = renderUI({   
    plot_height = single_plot_height * length(input$MA_normalisation)
    
    output$A_plot = renderPlot({
      

      
      M_l = multi_fit_data()  %>% 
        filter(Correction %in% input$MA_correction, 
               Normalisation %in% input$MA_normalisation)
      
      if(input$multi_DE == FALSE){
    
        p = ggplot(M_l, aes(x= Correction, y=log(Amean), fill=Correction))
        q = quantile(log(M_l$Amean),na.rm = T)
       
      }else{
        p = ggplot(M_l, aes(x= Correction, y=AveExpr, fill=Correction))
        q = quantile(M_l$AveExpr,na.rm = T)
      }
      p = p + geom_boxplot() +
        
        theme(axis.text = element_text(size = 12), axis.title.x = element_blank(), legend.position = "none") +
        facet_grid(Normalisation ~ .)
      
      if(values$plot_lim == 'Quantile'){
        p = p +  ylim(q[2],q[4])
      }
      if(values$plot_lim == '2x Quantile'){
        p = p +  ylim(q[2]/2,q[4]*2)
      }
      
      p
    },height = plot_height)
    
    plotOutput('A_plot',height = plot_height)
  })
  



  output$MA_plot_ui = renderUI({ 
    plot_height = single_plot_height * length(input$MA_normalisation)
    output$MA_plot = renderPlot({
      MA = multi_fit_data() %>% 
        filter(Correction %in% input$MA_correction, 
               Normalisation %in% input$MA_normalisation)
      as.tbl(MA)
      
      
      if(input$multi_DE == FALSE){
        if(input$log_rb_M == TRUE){
          p = ggplot(MA, aes(x= Amean, y=log(F)))
          #q = quantile(log(M_l$F),na.rm = T)
        }else{
          p = ggplot(MA, aes(x= Amean, y=F))
          p = p + scale_y_continuous(trans='log2')
          #q = quantile(M_l$F,na.rm = T)
        }
      }else{
        p = ggplot(MA, aes(x= AveExpr, y=logFC, col=threshold)) + 
          geom_hline(aes(yintercept = 0))
        #q = quantile(M_l$logFC,na.rm = T)
      }
      
      p = p + 
        geom_point() +
        facet_grid(Normalisation ~ Correction)
      p
   
    },height = plot_height)
  
    plotOutput('MA_plot',height = plot_height)
  })
    
   
  multi_precision_function = function(data,norm,table){
    #data = E_fit() 
    Rfit2 = data$E[[table]]
    Sfit2 = data$S[[table]]
    Mfit2 = data$M[[table]]
    Nfit2 = data$N[[table]]
    

    Raw_P <- cbind(log2(Rfit2$sigma^2), Rfit2$Amean)
    colnames(Raw_P) <- c("Raw_variance", "Raw_A")
    
    Sub_P <- cbind(log2(Sfit2$sigma^2), Sfit2$Amean)
    colnames(Sub_P) <- c("Sub_variance", "Sub_A")
    
    Mov_P <- cbind(log2(Mfit2$sigma^2), Mfit2$Amean)
    colnames(Mov_P) <- c("Mov_variance", "Mov_A")
    
    Nor_P <- cbind(log2(Nfit2$sigma^2), Nfit2$Amean)
    colnames(Nor_P) <- c("Nor_variance", "Nor_A")
    
    Precision <- cbind(Raw_P, Sub_P, Mov_P, Nor_P)
    Precision <- as.data.frame(Precision)
    
    #--------------------------Scale data for comparison-------------
    Raw_control <- Precision$Raw_A/Precision$Sub_A
    Precision$Sub_A <- Precision$Sub_A*Raw_control
    Precision$Sub_variance <- Precision$Sub_variance*Raw_control
    
    Mov_control <- Precision$Raw_A/Precision$Mov_A
    Precision$Mov_A <- Precision$Mov_A*Mov_control
    Precision$Mov_variance <- Precision$Mov_variance*Mov_control
    
    Nor_control <- Precision$Raw_A/Precision$Nor_A
    Precision$Nor_A <- Precision$Nor_A*Nor_control
    Precision$Nor_variance <- Precision$Nor_variance*Nor_control
    
    Precision_all <- cbind(Precision$Raw_A, Precision$Raw_variance,Precision$Sub_variance,Precision$Mov_variance,Precision$Nor_variance)
    
    colnames(Precision_all) <- c("Amean","Rawdata","Subtraction","Movingminimum","Normexp")
    
    Precision_all <- as.data.frame(Precision_all)
    Precision_melt <- reshape2::melt(Precision_all, id.vars="Amean")
    Precision_melt <- cbind(Precision$Raw_A, Precision_melt)
    colnames(Precision_melt) <- c("N", "Amean", "Background_correction","Variance")
    
    as.tbl(Precision_melt)
    Precision_melt$Normalisation = norm 
    Precision_melt
  }
  
  precision_data = reactive({withProgress(message = 'precision',{
    if(input$multi_DE == FALSE){
      E_Precision_melt = multi_precision_function(Rfit()$E,"None",'df')
      S_Precision_melt = multi_precision_function(Rfit()$S,"Scale",'df')
      Q_Precision_melt = multi_precision_function(Rfit()$Q,"Quantile",'df')
      C_Precision_melt = multi_precision_function(Rfit()$C,"Cyclicloess",'df')
    }else{
      E_Precision_melt = multi_precision_function(DE_Rfit()$E,"None",'df')
      S_Precision_melt = multi_precision_function(DE_Rfit()$S,"Scale",'df')
      Q_Precision_melt = multi_precision_function(DE_Rfit()$Q,"Quantile",'df')
      C_Precision_melt = multi_precision_function(DE_Rfit()$C,"Cyclicloess",'df')
    }
    
    Precision_melt = rbind(E_Precision_melt) %>% 
      rbind(S_Precision_melt) %>% 
      rbind(Q_Precision_melt) %>% 
      rbind(C_Precision_melt)
    Precision_melt
    
    Precision_melt$Background_correction = factor(Precision_melt$Background_correction, levels = unique(Precision_melt$Background_correction))
    Precision_melt$Normalisation = factor(Precision_melt$Normalisation, levels = unique(Precision_melt$Normalisation))
    
    Precision_melt
  })})
  
  precision_plots = reactive({

    Precision_melt = precision_data() %>% 
      filter(Background_correction %in% input$MA_correction, 
             Normalisation %in% input$MA_normalisation)
    #Precision plots
    p = ggplot(Precision_melt, aes(x=Amean, y=log2(Variance), color=Background_correction)) +
      geom_smooth(method = "loess", se=FALSE)+
      #theme_classic()+
      theme(axis.text = element_text(size = 12), axis.title = element_blank(), legend.position = "top") + 
      facet_grid(Normalisation ~ .)
    
    
    p2 = ggplot(Precision_melt, aes(x=Background_correction, y=log2(Variance), fill=Background_correction)) +
      geom_boxplot()+
      #theme_classic()+
      theme(axis.text = element_text(size = 12), axis.title = element_blank(), legend.position = "none") +
      
      #theme(axis.text = element_text(size = 12), axis.text.x = element_text(angle=30), axis.title = element_blank()) + 
      facet_grid(Normalisation ~ .)
    list(p = p, p2 = p2)
    
  })
  
  output$precision_plot_ui = renderUI({
    plot_height = single_plot_height * length(input$MA_normalisation)
  
      output$precision_plot_1 = renderPlot({
        precision_plots()$p
      },height = plot_height)
      
      output$precision_plot_2 = renderPlot({
        precision_plots()$p2
      },height = plot_height)
      
    lst = list(plotOutput('precision_plot_1',height = plot_height),
               plotOutput('precision_plot_2'),height = plot_height)
    do.call(tagList,lst)
  })
  
  
  ### _ Line Graphs ####
  multi_norm_full_function = function(data,norm){
    E_df = as.data.frame(data$E$data)
    E_df_l = E_df %>% 
      rownames_to_column('Proteins') %>% 
      gather('Target','Intensity',colnames(E_df))
    E_df_l$Correction = 'None'
    
    S_df = as.data.frame(data$S$data)
    S_df_l = S_df %>% 
      rownames_to_column('Proteins') %>% 
      gather('Target','Intensity',colnames(S_df))
    S_df_l$Correction = 'Substraction'
    
    M_df = as.data.frame(data$M$data)
    M_df_l = M_df %>% 
      rownames_to_column('Proteins') %>% 
      gather('Target','Intensity',colnames(M_df))
    M_df_l$Correction = 'Movingminimum'
    
    N_df = as.data.frame(data$N$data)
    N_df_l = N_df %>% 
      rownames_to_column('Proteins') %>% 
      gather('Target','Intensity',colnames(N_df))
    N_df_l$Correction = 'Normexp'
    
    df_l = E_df_l %>% 
      rbind(S_df_l) %>% 
      rbind(M_df_l) %>% 
      rbind(N_df_l)
    
    df_l$Normalisation = norm
    df_l
  }
  
  output$multi_line_plot = renderPlot({
     
    names(norm()) 
    
    E_df_l = multi_norm_full_function(norm()$E,'None')
    Q_df_l = multi_norm_full_function(norm()$Q,'Quantile')
    C_df_l = multi_norm_full_function(norm()$C,'Cyclicloess')
    S_df_l = multi_norm_full_function(norm()$S,'Scale')
    
    df_l = rbind(E_df_l) %>% 
      rbind(Q_df_l) %>% 
      rbind(C_df_l) %>% 
      rbind(S_df_l)
    
    as.tbl(df_l)
    unique(df_l$Proteins)
    row_names = c('Cy5 BSA 10ng/ul','Cy5 BSA 15ng/ul','Cy5 BSA 5ng/ul')
    
    plot_data = df_l# %>% 
      filter(Proteins %in% row_names)
    as.tbl(plot_data) 
    
    ggplot(plot_data, aes(x = Correction, y = Intensity, fill = Correction)) + 
      geom_boxplot() + 
      geom_jitter(size = 0.5) + 
      
      facet_grid(Normalisation ~ Proteins) + 
      theme(axis.text = element_text(size = 12), axis.title = element_blank(), legend.position = "none")
      
  
    })
  
  ### Differential Analysis ####
  
  output$cont_matrix_comp_ui = renderUI({
    selectInput('cont_matrix_comp','Control',c("NA" = 'All',input$condition_select),'All')
  }) 
  
  
  eBayes_test = reactive({ withProgress(message = 'eBayes',{ 
    df = data() %>% column_to_rownames('protein')     
    
    (selected_cols = intersect(selected_metadata()$Name,colnames(df)))
    
    df = df[,selected_cols]

    cont_matrix_list = cont_matrix_function(df,selected_metadata(),input)
    design = cont_matrix_list$design
    cont.matrix = cont_matrix_list$cont.matrix
    
    fit = lmFit(df,design, weights = as.numeric(arrayw_df()[,selected_cols]))
    fit


    fit2 = contrasts.fit(fit,cont.matrix)
    fit2 = eBayes(fit2)
    fit2
    
    Sig_Proteins <- topTable(fit2, adjust.method = input$mtc,number = dim(df)[1])
    threshold <- Sig_Proteins$adj.P.Val < as.numeric(input$pvalue_select)
    length(which(threshold))
    Sig_Proteins <- cbind(Sig_Proteins, threshold) %>% 
      rownames_to_column('protein') 
    
    if('logFC' %in% colnames(Sig_Proteins)){
      Sig_Proteins$threshold[Sig_Proteins$logFC > -input$fc_cutoff & Sig_Proteins$logFC < 0 & Sig_Proteins$threshold == 'TRUE'] = 'FALSE'

      Sig_Proteins$threshold[Sig_Proteins$logFC < input$fc_cutoff & Sig_Proteins$logFC > 0 & Sig_Proteins$threshold == 'TRUE'] = 'FALSE'
    }
    
    list(df = Sig_Proteins,cont_matrix = cont_matrix_list$cmd, comparison_list = cont_matrix_list$comparison_list)
    })})
  
  output$cont_matrix_text_ui = renderUI({   
    if(length(input$condition_select) > 1){
      showTab('sig_panel','Table')
      showTab('sig_panel','Plots')
      #output$cont_html = renderPrint({
      #  paste(eBayes_test()$comparison_list,collapse = '<br>')
      #})
      
      lst = list(tags$h5(HTML(paste(eBayes_test()$comparison_list,collapse = '<br>'))))
    }else{
      hideTab('sig_panel','Table')
      hideTab('sig_panel','Plots')
      
      lst = list(span(tags$h4('A minimum of two conditions is required to do determine differential expression'), style="color:orange"))
    }
    do.call(tagList,lst)
  })
  
  output$eBays_table = DT::renderDataTable({
    eBayes_test()$df
  },rownames = FALSE)
  
  eBayes_sig_data = reactive({ 
    sig_df = eBayes_test()$df# %>%  
      #rownames_to_column('protein')
    as.tbl(sig_df)
    df = data()
    sig_protein = sig_df %>% 
      filter(threshold == TRUE) %>% 
      pull(protein)
    sig_protein
    as.tbl(sig_df)
    
    as.tbl(df)
    
    data = df %>% 
      filter(protein %in% sig_protein)
    data
    
  })
  
  output$eBayes_Heatmap = renderPlot({
    df = eBayes_sig_data()
  })
  
  output$eBayes_Heatmap_ui = renderUI({   
    df = eBayes_sig_data() %>%     
      column_to_rownames('protein')
    #colnames(df)  
    m = as.matrix(df)
    m
    rownames(df)
    m[is.na(m)] = 0
    m[is.infinite(m)] = 0
    m = m[,selected_metadata()$Name]
 
    if(dim(m)[1] == 0){
      tags$h4('No significant proteins')
    }else{
      id = 'EBayes'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,selected_metadata(),selected_metadata(),rownames(m),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    }
 

    
  })
  
  
  volcano_plot_function = function(sig_df,input){
    plot_height = 600
    type = values$volcano_type
    if(values$volcano_type == 'gg plotly'){
      type = 'ggplot'
    }
    
    
    (comp_cols = grep('_vs_',colnames(sig_df),value = T))
    if(length(comp_cols) >0){
      plot_height = 300 * length(comp_cols)
      (other_cols = colnames(sig_df)[!colnames(sig_df) %in% comp_cols])
      
      df_l = sig_df %>% 
        gather(comparison,logFC,comp_cols)
      as.tbl(df_l)
    }else{
      df_l = sig_df %>% 
        dplyr::mutate(comparison = 'vs')
        
    }
     
    if(type == 'ggplot'){
      p = ggplot(df_l) + 
        geom_point(aes(x = logFC,y = -log10(adj.P.Val), col = comparison, group = protein, shape = threshold)) + 
        geom_vline(aes(xintercept = as.numeric(input$fc_cutoff))) + 
        geom_vline(aes(xintercept = as.numeric(-input$fc_cutoff))) + 
        geom_hline(aes(yintercept = -log10(as.numeric(input$pvalue_select)))) +
        facet_grid(comparison ~ .)
    }
    

    if(type == 'EnhancedVolcano'){
      plot_height = 600
      p = EnhancedVolcano(df_l,
                      lab = df_l$protein,
                      #selectLab =df_l$comparison,
                      x = 'logFC',
                      y = 'adj.P.Val',
                      pCutoff = as.numeric(input$pvalue_select),
                      #cutoffLineCol = 'red2',
                      FCcutoff = as.numeric(input$fc_cutoff))
    }
    
    list(p = p, plot_height = plot_height)
  }

  output$volcano_plot_ui = renderUI({
    sig_df = eBayes_test()$df
    #%>%   
    #  rownames_to_column('protein')
    result_list = volcano_plot_function(sig_df,input)
    id = 'eBayes'
    
    name = paste('volcano',values$volcano_type,sep ='_')
    volcano_plot_Server(id,name,result_list$p,values$volcano_type,result_list$plot_height)
    if(values$volcano_type == 'gg plotly'){
      plotly_UI(id,name)
    }else{
      plot_UI(id,name,NULL,result_list$plot_height)
    }
  })

  output$sig_test_ui = renderUI({
    (conditions = input$condition_select) 
    print(conditions)
    if(length(conditions) > 1){
      do.call(tagList,SigTest_UI())
    }else{
      span(tags$h4('A minimum of two conditions is required to do determine differential expression'), style="color:orange")
    }
  })
  
  
  output$stat_MA_plot = renderPlotly({  
      
    df = eBayes_test()$df
    #%>%   
    #  rownames_to_column('protein')
    
    plot_df = df %>% 
      
      left_join(proteins())
    
    as.tbl(plot_df)
    
    if('logFC' %in% colnames(plot_df)){

      MA_plot = ggplot(plot_df) + 
        geom_point(aes(y = logFC, x = AveExpr,col = threshold, shape = Category, group = protein), size = 3) + 
        geom_hline(aes(yintercept = 0))
    }else{
      (comp_cols = grep('_vs_',colnames(plot_df),value = T))
      if(length(comp_cols) >0){
        plot_height = 300 * length(comp_cols)
        (other_cols = colnames(plot_df)[!colnames(plot_df) %in% comp_cols])
        
        df_l = plot_df %>% 
          gather(comparison,logFC,comp_cols)
        as.tbl(df_l)
      }
      MA_plot = ggplot(df_l) + 
        geom_point(aes(y = logFC, x = AveExpr,col = threshold, shape = comparison,group = protein), size = 3) + 
        geom_hline(aes(yintercept = 0))
    }

    
    MA_plot
  })
  
  


  
  #### Panel Labels ####
  
  next_tab_UI = function(id,name){
      if(is.null(values[[id]])){
        cmd = paste0(".tabbable > .nav > li > a[data-value='",id,"'] {background-color: #6b8eb7;   color:white}")
        lst = list(
          
          tags$style(HTML(cmd)),
          span(tags$h4('Next >>'), style="color:white")
        )
      }else{
        lst = list(tags$h5(name))
      }
      lst
  }
  
  output$metadata_label = renderUI({
    lst = next_tab_UI('metadata','MetaData')
    do.call(tagList,lst)
  })
  
  output$probe_label = renderUI({
    lst = next_tab_UI('probes','Probes')

    do.call(tagList,lst)
    
  })
  
  output$protein_label = renderUI({
    lst = next_tab_UI('proteins','Proteins')
    do.call(tagList,lst)
    
  })
  
})
