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
  
  values = reactiveValues(
    target_file = NULL,
    spot_file = NULL,
    protein_file = NULL,
    targets = NULL,
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
  #hideTab('main','All Methods')
  
  output$debug_ui = renderUI({
    if(!grepl('public',getwd())){
      actionButton('debug','Debug')
    }
  })
  
  observeEvent(input$debug,{
    browser()
  })
  
  
  output$update_ui = renderUI({
    if(grepl('devel',getwd())){
      actionButton('update','Update')
    }else{
      if(!grepl('public',getwd())){
        actionButton('push','git push')
      }
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
    values$app_version = app_version
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
    values$spot_collapse_digits = spot_collapse_digits
    values$cont_matrix_comp = cont_matrix_comp 
    print('readme_markdown_ui')
    includeMarkdown("Instructions.md")
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
      radioButtons('app_version','App Version',c('basic','pro'),app_version,inline = T)
    }
  })
  
  observeEvent(input$app_version,{
    values$app_version = input$app_version
  })
  
  # output$sep_categories_ui = renderUI({
  #   if(values$app_version == 'pro'){
  #     radioButtons('sep_categories','Separate plots by category',c(F,T),T,inline = T)
  #   }
  # })
  # 
  # observeEvent(input$sep_categories,{
  #   values$sep_categories = input$sep_categories
  # })
  
  
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

  
  
  #### UI objects #####
  
  output$main_header_options_ui = renderUI({
    do.call(tagList,Main_header_UI(values))
  })
    
  #### Inputs Options #####
    ##### _Select Datasets #####
    
    output$select_datasets_ui = renderUI({ 
      if(is.null(input$gpr_files$datapath)){
        if(values$app_version == 'pro'){
          selectInput('dataset','Dataset',c(pro_data_list),pro_dataset)
        }else{
          selectInput('dataset','Dataset',c(basic_data_list),basic_dataset)
        }
      }else{
        if(values$app_version == 'pro'){
          selectInput('dataset','Dataset',c('Upload',pro_data_list),'Upload')
        }else{
          selectInput('dataset','Dataset',c('Upload',basic_data_list),'Upload')
        }
        #selectInput('dataset','Dataset',c('Upload',paper_data_list),'Upload')
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
    values$spot_file = NULL 
    values$proteins_file = NULL
    values$target_file = NULL
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
        values$target_file = 'dataset'
        values$spot_file = 'dataset'
        values$protein_file = 'dataset'
        getwd()
        (path = input$dataset)
        file_list = list.files(path)
        file_list = file_list[!file_list %in% c('targets.txt','spots.txt','proteins.txt')]
        file_list
        file_path_list = file.path(path,file_list)
     
      }
      file_path_list
      list(name = file_list,path = file_path_list)
    })
   
    test_files = reactive({#withProgress(message = 'testing array files',{ 
      req(array_file_list())  
      #updateTabsetPanel(session,'main',selected = 'Instructions')
      #updateTabItems(inputId = "main", selected = "Instructions")
      # 
   
    #Tab('main','targets')
      values$targets = NULL
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
      showTab('main','targets')
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
        file_error = 'There are differing number of spots on the arrays, only the intersection spots will be used'
        file_error_list[['rows']] = file_error
        
      } 
      #&
      #    length(unique(col_list)) == 1 &
      #    #length(unique(colnames_list)) == 1 &
      #    length(unique(rownames_list)) == 1
      # ){
      #   # showTab('main','File Details')
      #   # showTab('main','Targets')files
      # 
      #   #showTab('main','Spots')
      #   #showTab('main','Proteins')
      #   #showTab('main','Plots')
      #   #showTab('main','Data')
      #   file_error = FALSE
      #   
      # }else{
      #   # hideTab('main','Targets')
      #   # hideTab('main','Spots')
      #   # hideTab('main','Proteins')
      #   # hideTab('main','Plots')
      #   # hideTab('main','Data')
      #   file_error = 'The array files are too disparate to compare, check the number of spots in each file'
      # }
      
      if(!TRUE %in% grepl('635|535',col_names)){
        file_error = 'No array channels found in the uploaded files'
        file_error_list[['cols']] = file_error
      }
      #file_error
      
      #if(length(file_error_list) == 0){
      #  hideTab('main','targets')
      #}
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
  
        #hideTab('main','files')
        updateTabItems(session,"main", "Targets")
        df = test_files()$file_df 
        as.tbl(df)
        
        cmd = paste(dim(df)[1],'Files with',
                    paste(unique(df$Number.of.Spots),collapse=', '),
                    'spots and ',
                    paste(unique(df$Number.of.Metric.Columns),collapse =', '),'columns')
   
      
      print(cmd)
    })
    
    output$test_files_text_rows_ui = renderUI({ 
   
      if(length(test_files()$file_error) > 0){
        if('rows' %in% names(test_files()$file_error)){
          #updateTabItems(session,"main", "File Details")
          span(tags$h4(test_files()$file_error$rows), style="color:red")
        }
      }
        

    }) 
    
    output$test_files_text_cols_ui = renderUI({ 
      
      if(length(test_files()$file_error) > 0){
        if('cols' %in% names(test_files()$file_error)){
          #updateTabItems(session,"main", "File Details")
          hideTab('main','targets')
          
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
      'spots and ',
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
    
    
    
  #---------------------------Read in data and filter spots----------------------------------------------------
    
    
    #### _EListRaw ####
     

    

    

      

    
    E = reactive({withProgress(message = 'Generating EListRaw object',{
      print('E')
      file_path_list = array_file_list()$path  
      file_path_list
          # if(input$spot_filtering == 'wtflags(0.1)'){
          #   E <- read.maimages(file_path_list,
          #                      columns=list(E=input$foreground_column, Eb=input$background_column),
          #                      annotation=c(input$select_annotation), 
          #                      wt.fun=wtflags(0.1))
          # }
          # if(input$spot_filtering == 'wtarea(150)'){
          #   E <- read.maimages(file_path_list,
          #                      columns=list(E=input$foreground_column, Eb=input$background_column),
          #                      annotation=c(input$select_annotation), 
          #                      wt.fun=wtarea(150))
          # }
          if(input$spot_filtering == TRUE){
            E <- read.maimages(file_path_list,
                               columns=list(E=input$foreground_column, Eb=input$background_column),
                               annotation=c(input$select_annotation), 
                               wt.fun=sd_filter)
          }
          if(input$spot_filtering == FALSE){
            E <- read.maimages(file_path_list,
                               columns=list(E=input$foreground_column, Eb=input$background_column),
                               annotation=c(input$select_annotation))
          }

        E
    
    })})
    


    #### _targets #####
    
    #values$target_file is a reactive value, that serves as a global variabe, which can be affected by different reactive elements
    
    # inputs that reset values$target file
    observeEvent(c(input$reset_targets,
                   input$gpr_files),{
                   values$target_file = NULL
                   })
    
    
    # assigns input$target file to values$target file
    observeEvent(input$target_file,{
      values$target_file = input$target_file
    })
    
    output$target_file_upload_ui = renderUI({
      input$dataset
      input$reset_targets
      input$gpr_files
      
      fileInput(
        inputId = "target_file", 
        label = "Upload Sample File", 
        accept = c(".txt")
      )
    })
    
    target_names = reactive({ 
      target_names = targets()$Name
      target_names
    })
    
    
    # reactive object that decided which targets file to upload. 
    targets_upload = reactive({ withProgress(message = 'uploding targets',{  
      input$reset_targets     
      input$dataset
      input$gpr_files
      target_file_path = NULL
      
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
      if(is.null(values$target_file)){
        df = df_files
        
      }else{
        if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'targets.txt')) & values$target_file == 'dataset'){
          target_file_path = file.path(input$dataset,'targets.txt')
        }else{
          target_file_path = values$target_file$datapath
        }
          df_upload = read.csv(target_file_path,sep ='\t',stringsAsFactors = F)
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
                error = "There is no intersect between the uploaded target file and the array files"
                df = df_files
              }
              
          
          }else{
            error = 'There is no FileName column in the uploaded targets file.'
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
    
    output$target_upload_error_ui = renderUI({
      if(!is.null(targets_upload()$error)){
        output$target_upload_error_text = renderPrint(cat(targets_upload()$error))
        output$target_upload_df = DT::renderDataTable(targets_upload()$df_upload,rownames = FALSE)
        
        lst = list(span(tags$h4(htmlOutput('target_upload_error_text')), style="color:red"),
                   tags$h2('Uploaded Targets Table'),
                   DT::dataTableOutput('target_upload_df'),
                   tags$h2('Targets Table'))
        do.call(tagList,lst)
      }
    })
    
    targets = reactive(targets_upload()$df)
    
    output$select_conditions_column_ui = renderUI({
      req(targets())
      selectInput('select_conditions_column','Select Condition Column',colnames(targets()),colnames(targets()))
    })
    
    target_conditions = reactive({
      df = targets()
      df$Condition = df[,input$select_conditions_column]
      df
    })
    
    selected_targets = reactive({
      target_conditions() %>% filter(Condition %in% input$condition_select)
    })
    
    selected_target_names = reactive({
      target_names = selected_targets()$Name
      target_names
    })
    
    output$target_table = DT::renderDataTable({
      showTab('main','probes')
      values$targets = 'hit'
      targets()
    },rownames = FALSE)
    
    output$target_table_ui = renderUI({
      result_list = targets_upload()  
      
      if(!is.null(result_list$error) | !is.null(result_list$warning)){
        output$target_upload_df = DT::renderDataTable({
          result_list$upload_df
        })
        
        lst = list(
          span(tags$h4(result_list$error), style="color:red"),
          span(tags$h4(result_list$warning), style="color:orange"),
          tags$h3('Uploaded Sample Table'),
          DT::dataTableOutput('target_upload_df'),
          tags$h3('Array Sample Table'),
          DT::dataTableOutput('target_table')
        )
      }else{
        DT::dataTableOutput('target_table')
      }
    }) 
    
    output$download_targets <- downloadHandler(
      filename = function(){"targets.txt"}, 
      content = function(fname){
        write.table(target_conditions(), fname,sep = '\t', row.names = FALSE)
      }
    ) 
    
    output$condition_select_ui = renderUI({
      req(target_conditions())
      df = target_conditions()
      selectInput('condition_select','Select Conditions',unique(df$Condition),unique(df$Condition),multiple = T,width = 1200)
    })
    #### _spots ####
    
    observeEvent(input$reset_spots,{
      values$spot_file = NULL
    })
    
    observeEvent(input$spot_file,{
      values$spot_file = input$spot_file
    })
    
    output$spot_file_upload_ui = renderUI({
      input$gpr_files
      input$dataset
      input$reset_spots
        fileInput(
          inputId = "spot_file", 
          label = "Upload Spots File", 
          accept = c(".txt")
        )
    })
    
    output$spot_columns_ui = renderUI({
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
      selectInput('spot_column','Spot Column',columns,selected)
    })
    

    
    spot_names = reactive({
      input$reset_spots
      req(input$spot_column)
      req(E())
      spot_names = E()$genes[[input$spot_column]]

      spot_names
    })
    
    
    spot_upload = reactive({  withProgress(message = 'Upload Spots',{
      print('spot_upload')
      req(spot_names())  
      input$reset_spots
      input$dataset
      input$gpr_files
      spot_file_path = NULL
      
      df = E()$genes
      df$spot = spot_names()
      
      spot_names = spot_names()
   
      error = NULL
      warning = NULL
      upload_df = NULL
      if(!is.null(values$spot_file)){
        if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'spots.txt'))){
          spot_file_path = file.path(input$dataset,'spots.txt')
        }
        if(!is.null(input$spot_file$datapath)){
            spot_file_path = input$spot_file$datapath
        }
        if(!is.null(spot_file_path)){
          
          upload_df = read.csv(spot_file_path,sep ='\t',stringsAsFactors = F)
          if('spot' %in% colnames(upload_df)){
            if('Category' %in% colnames(upload_df)){
              print('col hit')
              
              #upload_df_trim = upload_df %>% 
              #  dplyr::select(spot,Category) %>% 
              #  distinct()
            }else{
              error = 'No Category column in uploaded spot file'
            }
          }else{
            error = 'No spot column in uploaded spot file'
            
          }
          
          if(length(intersect(df$spot,upload_df$spot)) == 0){
            error = 'There are no overlapping spots between the uploded spot file and the uploaded array files'
          }else{
            if(length(setdiff(df$spot,upload_df$spot)) > 0){
              warning = 'Not all the spots in the array files are in the uploaded spot file.'
            }
          }
          
          # if(TRUE %in% duplicated(upload_df$spot)){
          #   warning = "There are duplicates in the spot column, the duplicates have been removed" 
          #   upload_df_trim = upload_df_trim %>% 
          #     filter(!duplicated(spot))
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
    
    output$spot_remove_ui = renderUI({withProgress(message = 'remove probes',{
      print('spot_remove') 
      #req(spot_upload()) 
      #req(input$reset_spots)
      #req(input$dataset)
      #req(input$gpr_files)
      df = spot_upload()$df
      #as.tbl(df)
      proteins = unique(df$spot)
      (empty_spots = grep('empty|EMPTY',df$spot,value = T))
      
      control = unique(c(empty_spots,df$spot[df$Category == 'remove']))
      control
      selectInput('select_remove','Probes to Remove before Normalisation',proteins,control,multiple = T, width = 1200)
    })})
    
    output$spot_control_ui = renderUI({withProgress(message = 'control probes',{
      print('spot control') 
      #req(spot_upload())
      #req(input$reset_spots)
      #req(input$dataset)
      #input$gpr_files
      df = spot_upload()$df
      as.tbl(df)
      proteins = unique(df$spot)
      control = unique(df$spot[df$Category == 'control'])
      selectInput('select_control_spot','Control Probes (labelled but not removed)',proteins,control,multiple = T, width = 1200)
    })})
    
    spots = reactive({withProgress (message = 'assigning labels to probes',{
      print('spots')
      req(spot_upload())
      df = spot_upload()$df
      df$Category = 'analyte'
      df$Category[df$spot == ''] = 'remove'
      df$Category[df$spot %in% input$select_remove] = 'remove'
      df$Category[df$spot %in% input$select_control_spot] = 'control'
      #df$Category[df$Category == 'remove' & !df$spot %in% input$select_remove] = ''
      df
    })})
    
    removed_spots = reactive({
      print('removed_spots')
      req(spots())
      spots() %>% 
        filter(!Category %in% c('remove')) %>% 
        pull(spot)
    })
    
    
    output$spot_table = DT::renderDataTable({
      print('spot_table')
      req(spot_upload())
      req(spots())
      showTab('main','proteins')
      showTab('main','data')
      
      values$probes = 'hit'
      spots()
    },rownames = FALSE)
    
    output$spot_table_ui = renderUI({
      result_list = spot_upload()
      
      if(!is.null(result_list$error) | !is.null(result_list$warning)){
        output$spot_upload_df = DT::renderDataTable({
          result_list$upload_df
        })
        
        lst = list(
          span(tags$h4(result_list$error), style="color:red"),
          span(tags$h4(result_list$warning), style="color:orange"),
          tags$h3('Uploaded Spot Table'),
          DT::dataTableOutput('spot_upload_df'),
          tags$h3('Array Spot Table'),
          DT::dataTableOutput('spot_table')
        )
      }else{
        DT::dataTableOutput('spot_table')
      }
    })
    
 
    
    output$download_spots <- downloadHandler(
      filename = function(){"spots.txt"}, 
      content = function(fname){
        write.table(spots(), fname,sep = '\t',row.names = F)
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
      if("Name" %in% colnames(spots())){ 
        selected = 'Name'
      }
      if("ID" %in% colnames(spots())){
        selected = 'ID'
      }
      selectInput('protein_column','Protein Column',colnames(spots()),input$spot_column)
    })
    
    observeEvent(input$reset_proteins,{
      values$protein_file = NULL
    })
    
    
    
    observeEvent(input$protein_file,{
      values$protein_file = input$protein_file
    })
    
     
    protein_upload = reactive({ withProgress(message = 'Uploading Proteins',{
      req(data_full()) 
      input$reset_targets
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
              #  dplyr::select(spot,Category)
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
      
      if(length(selected_targets()$Condition) >1){
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
    
    array_HeatMap_function <- function(m,target_names,selected_targets,spot_names,pallete,cluster) {
          
          colnames(m) = target_names$Name
          m = m[,selected_targets$Name]
          
          remove = c()
          if(FALSE %in% is.finite(m)){
            m[!is.finite(m)] = 0
            remove = c(remove,'infinite')
            
          }
          if(TRUE %in% is.infinite(m)){
            m[is.na(m)] = 0
            remove = c(remove,'na')
          }
          
          title = ''
          if(length(remove) > 0){
            title = paste('replaced ',paste(remove,collapse = ' and '), "values with zero's")
          }
          plot_height = 300+(dim(m)[1]*10)
          plot_width = 600 + (dim(m)[2]*5) 
          if(values$app_version == 'basic'){
            if(dim(m)[1] > max_heatmap_rows){
              cluster = 'dend'
            }
          }
          if(cluster == 'dend'){
            plot_height = 300
            ht = dend_function(m,target_names,pallete)
          }else{
            plot_height = 300+(dim(m)[1]*10)
            ht = Heatmap_function(m,selected_targets,spot_names,pallete,cluster)
          }
     list(p = ht,plot_height = plot_height, plot_width = plot_width, warning = title,type = cluster)  
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
    
    

    
    dend_function = function(m,targets,pallete){
      ## stupid toy example
      groupCodes = targets$Condition
      groupCodes
      palette
      conditions = unique(targets$Condition)
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
          #code <- substr(label, 1, 1)
          ## use the following line to reset the label to one letter code
          # attr(x, "label") <- code
          attr(x, "nodePar") <- list(lab.col=colorCodes[label])
        }
        return(x)
      }
      
      d <- dendrapply(as.dendrogram(hc), labelCol)
      
      d
      
    }
    
    dend_function_old = function(m,targets){
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
    
    
    foreground_table = reactive({
      df = as.data.frame(E()$E)  
      dim(df)
      colnames(df) = target_names()
      df$spot = spot_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(spot,everything())
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
      ht_list = array_HeatMap_function(m,targets(),selected_targets(),spot_names(),input$r_col,values$heatmap_order)
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
    })
    
    triplicate_CV_function = function(df){
      #df = foreground_table() 
      #as.tbl(df)
      
      df_l = df %>% 
        gather(Name,value,-spot)
      as.tbl(df_l)
      
      # df_cv_t = df_l %>% 
      #   filter(Name == '10289345_06') %>% 
      #   filter(spot == 'CT62')
      # 
      # as.tbl(df_cv_t)
      
      df_cv = df_l %>% 
        group_by(spot,Name) %>%
          summarise(count = n(),
                    mean = mean(value,na.rm = T),
                    median = median(value,na.rm = T),
                    sd = sd(value,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(CV = 100*(sd/mean),
               diff_mm = abs(mean-median)) 
        
      as.tbl(df_cv)
      
      df_cv_mean = df_cv %>% 
        group_by(spot) %>% 
          summarise(mean = mean(CV,na.rm = T),
                    median = median(CV,na.rm = T),
                    max = max(CV,na.rm = T),
                    min = min(CV,na.rm = T)) %>% 
        ungroup()
      
      list(df_cv = df_cv, df_cv_mean = df_cv_mean)
   
    }
    
    triplicate_cv_plot_function = function(df,spots,targets){withProgress(message = 'Calculating CV',{
      #df = foreground_table() 
      df_list = triplicate_CV_function(df)
      df_cv = df_list$df_cv
      if('Name' %in% colnames(spots)){
        spots = spots %>% 
          dplyr::select(-Name)
      }
      
      df_cv = df_cv %>% 
        left_join(spots) %>% 
        left_join(targets)
      
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
      spots = spots()
      targets = targets()
      plot_list = triplicate_cv_plot_function(df,spots(),targets())
    
      id = 'foreground'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)

      do.call(tagList,CV_UI(id,name,values))
    })})
  
    background_table = reactive({
      df = as.data.frame(E()$Eb)
      colnames(df) = target_names()
      df$spot = spot_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(spot,everything())
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
      ht_list = array_HeatMap_function(m,targets(),selected_targets(),spot_names(),input$r_col,values$heatmap_order)
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))

    })
    
    output$background_triplicate_cv_plot_ui = renderUI({withProgress(message = 'Generating Plots',{
      df = background_table()  
      spots = spots()
      targets = targets()
      plot_list = triplicate_cv_plot_function(df,spots(),targets())
      
      id = 'background'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)
      
      do.call(tagList,CV_UI(id,name,values))
    })})
    
    # output$background_triplicate_cv_plot_ui = renderUI({withProgress(message = 'Generating Plots',{
    #   df = background_table()
    #   spots = spots()
    #   targets = targets()
    #   plot_list = triplicate_cv_plot_function(df,spots(),targets())
    #   plot_list$p
    #   id = 'background'
    #   name = 'triplicate_CV'
    #   plot_Server(id,'triplicate_CV',plot_list$p)
    #   plot_Server(id,'triplicate_CV_density',plot_list$d)
    #   plot_Server(id,'triplicate_diff',plot_list$b)
    #   
    #   lst = list(plot_UI(id,'triplicate_CV',"Boxplots of CV's for probe replicates"),
    #              plot_UI(id,'triplicate_CV_density',"Density plot of CV's for probe replicates"),
    #              plot_UI(id,'triplicate_diff','Boxplot of the absolute difference between probe replicate means and medians'))
    #   lst
    #   do.call(tagList,lst)
    # })})

    spot_filtering_E = reactive({
      df = E()$weights %>%  
        as.data.frame
      colnames(df) = target_names()
      df$spot = spot_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(spot,everything())
      df
    })
    #output$spot_filtering_E_table = DT::renderDataTable({
    #  df = spot_filtering_E()
    #})
    
    output$spot_filtering_table_ui = renderUI({
      df = spot_filtering_E()
      id = 'spot_filtering'
      name = 'table'
      table_Server(id,name,df)
      table_UI(id,name)
    })
    
    output$spot_filtering_E_heatmap_ui = renderUI({ 
      
    
    
      if('weights' %in% names(E())){
        m = spot_filtering_E() %>% 
          dplyr::select(-spot) %>% 
          as.matrix()
        
        if(length(unique(as.numeric(m))) >1){
        
          id = 'spot_filtering'
          name = 'Hcluster'
          ht_list = array_HeatMap_function(m,targets(),selected_targets(),spot_names(),input$r_col,values$heatmap_order)
          ht_plot_Server(id,name,ht_list)
          do.call(tagList,plot_UI(id,name,ht_list$warning))
          
          #plot_height = data_heatmap_Server('spot_filtering',m,targets(),selected_targets(),spot_names(),input$r_col,input$heatmap_order)
          
          #do.call(tagList,data_heatmap_UI('spot_filtering',plot_height))
        }else{
          tags$h4('No spots were filtered')
        }
      }else{
        tags$h4('No spot filtering was applied to the array data')
      }
  
    })
    
    ### Pipeline #####
    output$pipeline_ui = renderUI({
      do.call(tagList,Pipeline_UI(values))
    })
    
    #----------------------------Visualization of Raw data---------------------------------------------
    


    
    array_boxplot_function = function(data,target_names,spot_names,targets,selected_targets,
                                      log_rb,input){
      df = data.frame(data)
      #if(log_rb == TRUE){
      #  df = log2(df)
      #}
      colnames(df) <- target_names
      df$Proteins = spot_names
      as.tbl(df)   
      
      
      df_l = df %>% 
        gather(Name,`Expression Intensity`,c(target_names)) %>% 
        left_join(targets) %>% 
        filter(Name %in% selected_targets$Name)
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
     
    array_boxplot_function_2 = function(data,target_names,spot_names,
                                        targets,selected_targets,
                                        spots,
                                        log_rb,input,values){
      df = data.frame(data)
      #if(log_rb == TRUE){
      #  df = log2(df)
      #}
      colnames(df) <- target_names
      df$spot = spot_names
      as.tbl(df)   
      
      
      if("Name" %in% colnames(spots)){
        spots = spots %>% 
          dplyr::select(-Name)
      }
      
      df_l = df %>% 
        gather(Name,`Expression Intensity`,c(target_names)) %>% 
        left_join(targets) %>% 
        left_join(spots) %>% 
        filter(Name %in% selected_targets$Name) 
      as.tbl(df_l)
      p = ggplot(df_l)
      if(values$collapse_boxplots == F){
        #p = p + geom_boxplot(aes(x = Name,y = `Expression Intensity`, fill = Condition, col = Category))
        
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
      #if(length(unique(df_l$Group)) > 1){
      #  p = p + facet_grid(Group ~ .)
      #  p = p + facet_grid(Category ~ ., scales = 'free')
      #}
      plot_height = 400
      if(values$sep_categories == TRUE){
        if(length(unique(df_l$Category)) > 1){
          p = p + facet_grid(Category ~ ., scales = 'free')
          plot_height = 300 * length(unique(df_l$Category))
        }
      }
      list(p = p, plot_height = plot_height)
    }
    
    CV_df_function  = function(df,targets){
      
      df_l = df %>% 
        gather(Name,value,-spot) %>% 
        left_join(targets)
      
      as.tbl(df_l)
      
      df_cv = df_l %>% 
        filter(!is.na(value)) %>% 
        group_by(spot,Condition) %>% 
        summarise(mean = mean(value,na.rm = T),
                  sd = sd(value,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(CV = sd/mean*100)
      
      df_cv
    }
    
    CV_plot_function = function(data,targets,spots){
      CV_df = data %>%  
        left_join(spots)
      q = quantile(CV_df$CV,na.rm = T)
      
      if(length(unique(CV_df$Category)) == 1){
        p = ggplot(CV_df) + 
          geom_boxplot(aes(y = CV,x = Condition)) + 
          ylim(q[1],q[3])
        
      }else{
        p = ggplot(CV_df) + 
          geom_boxplot(aes(y = CV,x = Condition, fill = Category)) + 
          ylim(q[1],q[3])
      }
      
      p
    }
    
    missingness_function = function(df,targets){
      
      
      df[df <= 0] = NA
      as.tbl(df)
      
      df_l = df %>% 
        #rownames_to_column('spot') %>% 
        gather(Name,value,-spot) %>% 
        filter(!is.na(value))
      as.tbl(df_l)
      
      df_count = df_l %>% 
        group_by(Name) %>% 
        summarise(`Percentage missing` = (dim(df)[1]-n())/dim(df)[1]*100) %>% 
        ungroup() %>% 
        left_join(targets)
      df_count  
      
      p = ggplot(df_count) +
        geom_col(aes(x = Name,y = `Percentage missing`, fill = Condition))
      p
    }
    
    MA_plot_function = function(df,spots){
      if('spot' %in% colnames(df)){
        spot_col = df$spot
        df = df %>% dplyr::select(-spot)
      }else{
        spot_col = rownames(df)
      }
      
      Rfit = lmFit(df)
      Rfit2 = eBayes(Rfit)
      Rfit2_df = as.data.frame(Rfit2)
      as.tbl(Rfit2_df)
      dim(Rfit2_df)
      #rownames(Rfit2_df) = rownames(df)
      Rfit2_df$spot = spot_col
      
      as.tbl(Rfit2_df)
      
      plot_df = Rfit2_df %>% 
        
        left_join(spots)
      
      as.tbl(plot_df)
      
      
      if(length(unique(plot_df$Category))>1){
        MA_plot = ggplot(plot_df) + 
          geom_point(aes(y = log(F), x = Amean,col = Category,group = spot))
      }else{
        MA_plot = ggplot(plot_df) + 
          geom_point(aes(y = log(F), x = Amean,group = spot))
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
    
    cont_matrix_function = function(df,targets,input){
      time = factor(paste(targets$Condition), levels = unique(targets$Condition))
      time
      design = model.matrix(~0+time)
      colnames(design) = levels(time)
      
      conditions = unique(targets$Condition)
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
                                   target_names(),spot_names(),
                                   target_conditions(),selected_targets(),
                                   spots(),
                                   log_rb,input,values)
      p
      id = 'RAW'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Raw Data Expression Intensity Boxplot',result_list$plot_height))
    })
    
    RAW_df = reactive({
      df = E()$E  
      colnames(df) = target_names()
      df = df %>% 
        as.data.frame()
      df$spot = spot_names()
      df
      #CV_df = CV_df_function(df,targets())
      #CV_df
    })
    
    
    output[['RAW-table']] = DT::renderDataTable({
      RAW_df()
    },rownames = FALSE)
    
    output[['RAW-CV_plot_ui']] = renderUI({  
      df = RAW_df()
      data = CV_df_function(df,targets()) 
      
      p = CV_plot_function(data,targets(),spots())
      p = gg_fill_function(p)
      
      id = 'RAW'
      name = 'CV'
      title = "RAW data CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    
    output[['RAW-MA_plot_ui']] = renderUI({
   
      
      df = RAW_df()
      p = MA_plot_function(df,spots())
      #p = gg_col_function(p)
      
      id = 'RAW'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW-missing_plot_ui']] = renderUI({  
 
      df = RAW_df()
      p = missingness_function(df,targets())
      p = gg_fill_function(p)
      
      plot_Server('RAW','missingness',p)
      do.call(tagList,plot_UI('RAW','missingness','Percentage of missing values per array'))
     
    })
    
    output[['RAW-Heatmap_ui']] = renderUI({ 
      df = E()$E      
      colnames(df) = target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input) 
      
      id = 'RAW'
      name = 'Hcluster'
      

      ht_list = array_HeatMap_function(m,targets(),selected_targets(),spot_names(),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list) 
      #title = paste('removed ',paste(ht_list$removed,collapse = ' and '), 'values')
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = RAW_df()  
      spots = spots()
      targets = targets()
      plot_list = triplicate_cv_plot_function(df,spots(),targets())
      
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
                                             target_names(),spot_names(),target_conditions(),selected_targets(),spots(),
                                             log_rb,input,values)
      
      id = 'RAW_filter'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Raw Data Expression Intensity Boxplot',result_list$plot_height))
    })
    
    E_filter_df = reactive({
      df = E_filter() 
      colnames(df) = target_names()
      df = df %>% 
        as.data.frame
      df$spot = spot_names()
      df
    })
    
    output[['RAW_filter-table']] = DT::renderDataTable({
      E_filter_df()
    },rownames = FALSE)
    
    output[['RAW_filter-CV_plot_ui']] = renderUI({     
      df = E_filter_df() 
      data = CV_df_function(df,targets())
      p = CV_plot_function(data,targets(),spots())
      p = gg_fill_function(p)
      
      id = 'RAW_filter'
      name = 'CV'
      title = "RAW data CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    output[['RAW_filter-MA_plot_ui']] = renderUI({
      
      df = E_filter_df()  
      p = MA_plot_function(df,spots())

      id = 'RAW_filter'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW_filter-missing_plot_ui']] = renderUI({   
      df = E_filter_df()
      p = missingness_function(df,targets())
      p = gg_fill_function(p)
      
      id = 'RAW_filter'
      name = 'missingness' 
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Percentage of missing values per array'))
    })
    
    output[['RAW_filter-Heatmap_ui']] = renderUI({ 
      df = E_filter()     
      colnames(df) = target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      
      id = 'RAW_filter'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,targets(),selected_targets(),spot_names(),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW_filter-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = E_filter_df()  
      spots = spots()
      targets = targets()
      plot_list = triplicate_cv_plot_function(df,spots(),targets())
      
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
                                             target_names(),spot_names(),target_conditions(),selected_targets(),spots(),
                                             log_rb,input,values)
      
      id = 'RAW_corr'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Background Corrected Expression Intensity Boxplot',result_list$plot_height))
    })
    
    E_corr_df = reactive({
      df = E_corr()$E
      colnames(df) = target_names()
      df = df %>% 
        as.data.frame
      df$spot = spot_names()
      df
    })
    
    output[['RAW_corr-table']] = DT::renderDataTable({
      E_corr_df()
    },rownames = FALSE)
    
    output[['RAW_corr-CV_plot_ui']] = renderUI({     
      df = E_corr_df() 
      data = CV_df_function(df,targets())
      p = CV_plot_function(data,targets(),spots())
      p = gg_fill_function(p)
      
      id = 'RAW_corr'
      name = 'CV'
      title = "Background Corrected CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    output[['RAW_corr-MA_plot_ui']] = renderUI({
      
      df = E_corr_df()  
      p = MA_plot_function(df,spots())
      
      id = 'RAW_corr'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW_corr-missing_plot_ui']] = renderUI({   
      df = E_corr_df()
      p = missingness_function(df,targets())
      p = gg_fill_function(p)
      
      id = 'RAW_corr'
      name = 'missingness' 
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Percentage of missing values per array'))
    })
    
    output[['RAW_corr-Heatmap_ui']] = renderUI({ 
      df = E_corr()$E      
      colnames(df) = target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      
      id = 'RAW_corr'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,targets(),selected_targets(),spot_names(),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW_corr-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = E_corr_df()  
      spots = spots()
      targets = targets()
      plot_list = triplicate_cv_plot_function(df,spots(),targets())
      
      id = 'RAW_corr'
      name = 'triplicate_CV'
      
      CV_Server(id,name,plot_list)
      
      do.call(tagList,CV_UI(id,name,values))
    })})
    
    output$Raw_corr_tabs_ui = renderUI({ 
      do.call(tagList,PlotTabs_UI(id = "RAW_corr",values))
    })
    

    #---------------------------------Normalization------------------------------------
    

    pre_norm_function = function(data,spot_names,target_names,selected_target_names,removed_spots,log_rb){
      df = as.data.frame(data)
      dim(df)
      colnames(df) <- target_names
      df$spot = spot_names
      
      df_f = df %>% 
        dplyr::filter(spot %in% removed_spots)
      
      df_m = df_f %>% 
        dplyr::select(-spot)
      
      df_m = df_m %>% 
        dplyr::select(one_of(selected_target_names))
      m = as.matrix(df_m)
      if(log_rb == T){
        m = log2(m)
      }
      m
      list(m = m, spots = df_f$spot)
    }
    
    norm_function = function(m,method,spots){
      E_norm = normalizeBetweenArrays(m,method = method)
      E_norm = as.data.frame(E_norm)
      E_norm$spot <- spots
      E_norm
    }
    
    E_norm = reactive({   withProgress(message = 'Normalisation',{
      
        data = E_corr()$E  
        spot_names = spot_names()
        target_names = target_names()
        removed_spots = removed_spots()
        log_rb = values$log_rb
      
        norm_list = pre_norm_function(E_corr()$E,spot_names(),target_names(),selected_target_names(),removed_spots(),values$log_rb)
        E_norm = norm_function(norm_list$m,input$normalisation_method,norm_list$spots)
        E_norm

    })})
    
    output[['RAW_norm-table']] = DT::renderDataTable({
        E_norm()
    },rownames = FALSE)
    
    #----------------------------Visualization of Normalized data---------------------------------------------
    
    
    output[['RAW_norm-boxplot_ui']] = renderUI({   
       
      data = E_norm() %>% 
        dplyr::select(-spot)
      log_rb = values$log_rb
      #p = array_boxplot_function(data,target_names(),spot_names(),target_conditions(),selected_targets(),log_rb,input)
      result_list = array_boxplot_function_2(data,
                                             selected_target_names(),E_norm()$spot,target_conditions(),selected_targets(),spots(),
                                             log_rb,input,values)
      
      
      id = 'RAW_norm'
      name = 'boxplot'
      
      plot_Server(id,name,result_list$p, result_list$plot_height)
      do.call(tagList,plot_UI(id,name,'Background Corrected & Normalised Expression Intensity Boxplot',result_list$plot_height))
    })
    
    output[['RAW_norm-CV_plot_ui']] = renderUI({     
      df = E_norm() 
      data = CV_df_function(df,targets())
      #proteins = proteins() %>% 
      #  dplyr::rename('spot' = protein)
      
      
      p = CV_plot_function(data,targets(),spots())
      p = gg_fill_function(p)
      
      id = 'RAW_norm'
      name = 'CV'
      title = "Background Corrected & Normalised CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    output[['RAW_norm-MA_plot_ui']] = renderUI({
      
      df = E_norm()  
      p = MA_plot_function(df,spots())
      
      id = 'RAW_norm'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['RAW_norm-missing_plot_ui']] = renderUI({   
      df = E_norm()
      p = missingness_function(df,targets())
      p = gg_fill_function(p)
      
      id = 'RAW_norm'
      name = 'missingness' 
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Percentage of missing values per array'))
    })
    
    output[['RAW_norm-Heatmap_ui']] = renderUI({ 
      df = E_norm() %>%  
        dplyr::select(-spot)
      colnames(df) = selected_target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      
      id = 'RAW_norm'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,selected_targets(),selected_targets(),E_norm()$spot,input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output[['RAW_norm-triplicate_cv_plot_ui']] = renderUI({withProgress(message = 'Generating Plots',{
      df = E_norm()  
      spots = spots()
      targets = targets()
      plot_list = triplicate_cv_plot_function(df,spots(),targets())
      
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
        data = E_norm() %>% dplyr::select(-spot)
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
            left_join(target_conditions())
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
    
    protein_collapse_function = function(df,spots,input){
      
      data = df %>% 
        dplyr::select(-spot)
      colnames(data)
      as.tbl(data)
      colnames(data)

      
      df_spots = df  %>% 
        left_join(spots)
      as.tbl(df_spots)
      colnames(df_spots)
      if(values$spot_collapse_digits == TRUE){
        df_spots$protein <- gsub("\\.[[:digit:]]*$", "", df_spots[,input$protein_column])
      }else{
        df_spots$protein = df_spots[,input$protein_column]
      }
      df_collapse = df_spots %>% 
        dplyr::select(one_of(c('protein','Category',colnames(data))))
      colnames(df_collapse)
      
      data <- as.data.frame(df_collapse) %>% group_by(protein,Category) %>%
        summarise_all(funs(mean))
      data
    }
    
    data_full = reactive({withProgress(message = 'Generating Final Data',{ 
      data = E_norm()
      req(input$protein_column)
      req(E_norm())
      req(spots())
      df = protein_collapse_function(E_norm(),spots(),input)
      df
    })})
    
    
    output$drop_cols_ui = renderUI({
        df = proteins() 
        selectInput('drop_col','Drop by',colnames(df),'Category')
    })
    
    output$drop_rows_ui = renderUI({
        df = proteins()
        selection = unique(df[,input$drop_col])
        selectInput('drop_row',paste(input$drop_col,' to drop'),selection,'control',multiple = T,width = 1200)
    })
    
    protein_filter_function = function(data,proteins_df,input){
      (var <- rlang::parse_quosures(paste(input$drop_col))[[1]])

      drops = proteins_df %>% 
        filter(!!var %in% input$drop_row) %>% 
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
      df = protein_filter_function(data,proteins(),input)
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
      p = array_boxplot_function(df,colnames(df),rownames(df),target_conditions(),selected_targets(),values$log_rb,input)
      
      id = 'Data'
      name = 'boxplot'
      
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,'Data Expression Intensity Boxplot'))
    })
    
    data_df = reactive({
      df = data() %>%  
        dplyr::rename(spot = protein)
    })
    
    output[['Data-CV_plot_ui']] = renderUI({     
      df = data_df()  
      data = CV_df_function(df,targets())
      proteins = proteins() %>% 
        dplyr::rename('spot' = protein)
      
      p = CV_plot_function(data,targets(),proteins)
      p = gg_fill_function(p)
      
      p
      id = 'Data'
      name = 'CV'
      title = "Background Corrected & Normalised CV's across arrays for all probes"
      plot_Server(id,name,p)
      do.call(tagList,plot_UI(id,name,title))
      
    })
    
    output[['Data-MA_plot_ui']] = renderUI({
      
      df = data_df()  
      proteins = proteins() %>% 
        dplyr::rename('spot' = protein)
      p = MA_plot_function(df,proteins)
      
      id = 'Data'
      name = 'MA'
      title = NULL
      plotly_Server(id,name,p)
      do.call(tagList,plotly_UI(id,name,title))
      
    })
    
    
    output[['Data-missing_plot_ui']] = renderUI({   
      df = data_df()
      p = missingness_function(df,targets())
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
      colnames(df) = selected_target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      m
      id = 'Data'
      name = 'Hcluster'

      ht_list = array_HeatMap_function(m,selected_targets(),selected_targets(),data()$protein,input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
    })
    
    output$Data_tabs_ui = renderUI({
      #do.call(tagList,PlotTabs_UI(id = "Data"))
      
      ns <- NS("Data")
      
      lst = list(
        tabsetPanel(
          tabPanel('Table',
                   column(6,tags$h4(htmlOutput('data_dim_text'))),
                   column(2,downloadButton('download_data',"Data Table")),
                   column(2,downloadButton('download_ExpSet',"ExpSet")),
                   column(2,downloadButton('download_MSnSet',"MSnSet")),
                   
                   column(12,DT::dataTableOutput('data_table'))
          ),
          tabPanel('Boxplot',
                   uiOutput(ns('boxplot_ui'))
                   ),
          tabPanel('CV',
                   uiOutput(ns('CV_plot_ui'))
                   ),
          tabPanel('Missing Values',
                   uiOutput(ns('missing_plot_ui'))
          ),
          tabPanel("MA Plots",
                   uiOutput(ns('MA_plot_ui')),
          ),
          tabPanel('Clustering',
                   column(12,uiOutput(ns('Heatmap_ui')))
          ))
      )
      do.call(tagList,lst)
    })

    
    output$data_table = DT::renderDataTable({
        data()
    },rownames = FALSE)
    
    output$download_data <- downloadHandler(
        filename = function(){"data.txt"}, 
        content = function(fname){
            write.table(data(), fname,sep = '\t', col.names=NA,)
        }
    )
    
    output$data_dim_text = renderPrint({
      cat(paste(dim(data())[1],'proteins and ',dim(data())[2],'samples'))
    })
    
    # expression_set_function = function(data,targets,proteins){
    #   m = as.matrix(data)
    #   p = targets()
    #   rownames(p) = p$Name
    #   p$sample_name = rownames(p)
    #   phenoData = new('AnnotatedDataFrame',data = p)
    #   phenoData
    #   features = proteins()[proteins()$protein %in% rownames(data),]
    #   
    #   features
    #   rownames(features) = features$protein
    #   
    #   featureData =  new('AnnotatedDataFrame',data = features)
    #   featureData
    #   expSet = ExpressionSet(assayData = as.matrix(data),
    #                          phenoData = phenoData,
    #                          featureData = featureData)
    #   
    #   expSet
    #   
    #   
    #   
    #   MSnSet = MSnSet(m,featureData,phenoData)
    #   MSnSet
    #   
    #   list(ExpressionSet = expSet, MSnSet = MSnSet)
    # }
    
    expression_set_function = function(data,samples,features){
      #library(MSnbase)
      df = data[rownames(features),rownames(samples)]
      m <- df %>% mutate_all(as.numeric) %>% 
        as.matrix()
      
      samples$sample_name = rownames(samples)
      samples = samples %>% 
        dplyr::select(sample_name,everything())
      
      phenoData = new('AnnotatedDataFrame',data = samples)
      phenoData
      
      features$feature_name = rownames(features)
      features = features %>% 
        dplyr::select(feature_name,everything())
      
      #features$features = rownames(features)
      featureData =  new('AnnotatedDataFrame',data = features)
      featureData
      
      MSnSet = MSnSet(m,featureData,phenoData)
      
      
      MSnSet
    }
    
    data_MSnSet = reactive({  
      data = data() %>%   
        column_to_rownames('protein')
      colnames(data)
      rownames(data)
      targets = target_conditions()
      rownames(targets) = targets$Name
      proteins = proteins()
      rownames(proteins) = proteins$protein
      features = proteins
      features = features[data()$protein,]
      dim(features)
      samples = targets
      samples = samples[colnames(data),]
      
      arrayw_df = arrayw_df()
      w = t(arrayw_df) %>% 
        as.data.frame %>% 
        rownames_to_column('Name')
      
      samples =samples %>% 
        left_join(w)
      rownames(samples) = samples$Name
      
      #samples$array_weight = arrayw_df()[1,]
      dim(samples)
      dim(data)
      expression_set_function(data,samples,features)
       
    })
    
    output$download_MSnSet <- downloadHandler(
      filename = function(){"MSnSet.rds"}, 
      content = function(fname){
        saveRDS(data_MSnSet(),fname)
      }
    )
    
    
    ##### Optimal Cutpoints ######
    output$threshold_control_col_ui = renderUI({
      (selection = unique(target_conditions()$Condition))
      selected = selection[1]
      if('Control' %in% selection){
        selected = 'Control'
      }
      selectInput('threshold_control_column','Control Condition',selection,selected)
    })
    
    threshold_function = function(data,targets,input){  
      thres.data = as.data.frame(t(data)) %>% 
        rownames_to_column('Name') %>% 
        left_join(targets %>% dplyr::select(Name,Condition)) %>% 
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
      targets = target_conditions()
      
      #threshold_df = threshold_function(data,targets)
      #threshold_df
      threshold_df = tryCatch({threshold_function(data,targets,input)}, error = function(e) {NULL})
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
        targets = target_conditions()
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
        targets = target_conditions()
        rownames(targets) = targets$Name
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
        samples = targets
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
        (select_cols = intersect(selected_targets()$Name,colnames(m)))
        m = m[,select_cols]
        
        id = 'Cutoff'
        name = 'Hcluster'
        ht_list = array_HeatMap_function(m,targets(),samples,features$protein,input$r_col,values$heatmap_order)
        ht_list$p
        ht_plot_Server(id,name,ht_list)
        do.call(tagList,plot_UI(id,name,ht_list$warning))
      }
      # df = E_norm() %>% 
      #   dplyr::select(-spot)
      # colnames(df) = target_names()
      # m = as.matrix(df)
      # m = log_min_function(m,input)
      # m = neg_corr_function(m,input)
      # 
      # id = 'RAW_norm'
      # name = 'Hcluster'
      # ht_list = array_HeatMap_function(m,samples,features$protein,input$r_col,input$r_col,input$heatmap_order)
      # ht_list$p
      # ht_plot_Server(id,name,ht_list$p,ht_list$plot_height,ht_list$plot_width)
      # do.call(tagList,plot_UI(id,name))
      
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
  
    
  multi_norm_function = function(corr_data,spot_names,target_names,selected_target_names,removed_spots,log_rb,method,proteins,input){
    
    E_norm_list = pre_norm_function(corr_data$E$E,spot_names,target_names,selected_target_names,removed_spots,log_rb)
    E_norm = norm_function(E_norm_list$m,method,E_norm_list$spots)
    E_proteins = protein_collapse_function(E_norm,spots(),input)
    E = protein_filter_function(E_proteins,proteins,input)
    
    E = as.data.frame(E) %>% dplyr::select(-one_of('protein','Category'))
    
    S_norm_list = pre_norm_function(corr_data$S$E,spot_names,target_names,selected_target_names,removed_spots,log_rb)
    S_norm = norm_function(S_norm_list$m,method,S_norm_list$spots)
    S_proteins = protein_collapse_function(S_norm,spots(),input)
    S = protein_filter_function(S_proteins,proteins,input)

    S = as.data.frame(S) %>% dplyr::select(-one_of('protein','Category'))
    
    N_norm_list = pre_norm_function(corr_data$N$E,spot_names,target_names,selected_target_names,removed_spots,log_rb)
    N_norm = norm_function(N_norm_list$m,method,N_norm_list$spots)
    N_proteins = protein_collapse_function(N_norm,spots(),input)
    N = protein_filter_function(N_proteins,proteins,input)

    N = as.data.frame(N) %>% dplyr::select(-one_of('protein','Category'))
    
    M_norm_list = pre_norm_function(corr_data$M$E,spot_names,target_names,selected_target_names,removed_spots,log_rb)
    M_norm = norm_function(M_norm_list$m,method,M_norm_list$spots)
    M_proteins = protein_collapse_function(M_norm,spots(),input)
    M = protein_filter_function(M_proteins,proteins,input)

    M = as.data.frame(M) %>% dplyr::select(-one_of('protein','Category'))

    
    list(E = E,
         S = S,
         M = M,
         N = N)
  }
  norm = reactive({withProgress(message = 'multi normalisation',{
    corr_data = corr()    

    spot_names = spot_names()
    target_names = target_names()
    removed_spots = removed_spots()
    log_rb = values$log_rb
    proteins = proteins()
    
    
    method = "none"
    E = multi_norm_function(corr_data,spot_names(),target_names(),selected_target_names(),removed_spots(),values$log_rb,method,proteins(),input)
    
    method = "quantile"
    Q = multi_norm_function(corr_data,spot_names(),target_names(),selected_target_names(),removed_spots(),values$log_rb,method,proteins(),input)
    method = "cyclicloess"
    C = multi_norm_function(corr_data,spot_names(),target_names(),selected_target_names(),removed_spots(),values$log_rb,method,proteins(),input)
    method = "scale"
    S = multi_norm_function(corr_data,spot_names(),target_names(),selected_target_names(),removed_spots(),values$log_rb,method,proteins(),input)
    
    
    
    
    list(E = E, 
         Q = Q,
         C = C,
         S = S)
    
  })})

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
    Rfit2 = eBayes_single_function(norm$E)
    Sfit2 = eBayes_single_function(norm$S)
    Mfit2 = eBayes_single_function(norm$M)
    Nfit2 = eBayes_single_function(norm$N)
    
    list(E = Rfit2,
         S = Rfit2,
         M = Mfit2,
         N = Nfit2)
  }
  
  eBayes_function = function(data){
    df = data %>% 
      as.data.frame# %>% 
      #column_to_rownames('protein')  
    
    (selected_cols = intersect(selected_targets()$Name,colnames(df)))
    
    df = df[,selected_cols]
    
    cont_matrix_list = cont_matrix_function(df,selected_targets(),input)
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
   
    df = norm$E
    df$protein = data()$protein
    Rfit2 <- eBayes_function(df)
    
    df = norm$S
    df$protein = data()$protein
    Sfit2 <- eBayes_function(df)
    
    df = norm$M
    df$protein = data()$protein
    Mfit2 <- eBayes_function(df)
    
    df = norm$N
    df$protein = data()$protein
    Nfit2 <- eBayes_function(df)
    
    list(E = Rfit2,
         S = Rfit2,
         M = Mfit2,
         N = Nfit2)
  }
  
  cont_matix = reactive({
    df = norm()$E 
    as.tbl(df)
    result_list = cont_matrix_function(df,targets(),input)
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
  
  # multi_df = reactive({
  #   norm = norm()$E
  #   E = multi_DE_function(norm()E,cont_matrix())
  # })
  
  

  # MA_plots = renderPlot({
  #   df = Rfit()$E$E
  #   as.tbl(df)  
  # })
  
  

  # multi_Amean_function = function(fit,norm,column_name,table){
  #   Amean <- cbind(fit$E[[table]][column_name], fit$S[[table]][column_name], fit$M[[table]][column_name], fit$N[[table]][column_name])
  #   dim(Amean)
  #   colnames(Amean) <- c("Rawdata","Subtraction","Movingminimum","Normexp")
  #   as.tbl(as.data.frame(Amean))
  #   Amean_l = Amean %>% 
  #     as.data.frame() %>% 
  #     gather(Correction,A)
  #   
  #   Amean_l$A[is.infinite(Amean_l$A)] = NA
  #   Amean_l$Normalisation = norm
  #   Amean_l
  # }
  # 
  # Amean_data = reactive({withProgress(message = 'A data',{
  #   if(input$multi_DE == FALSE){
  #     fit = Rfit()
  #   }else{
  #     fit = DE_Rfit()
  #   }
  #   if(input$multi_top == FALSE){
  #     E_Amean = multi_Amean_function(fit$E,"None",'Amean','df')  
  #     S_Amean = multi_Amean_function(fit$S,"Scale",'Amean','df')
  #     Q_Amean = multi_Amean_function(fit$Q,"Quantile",'Amean','df')
  #     C_Amean = multi_Amean_function(fit$C,"Cyclicloess",'Amean','df')
  #   }else{
  #     E_Amean = multi_Amean_function(fit$E,"None",'AveExpr','top_df')  
  #     S_Amean = multi_Amean_function(fit$S,"Scale",'AveExpr','top_df')
  #     Q_Amean = multi_Amean_function(fit$Q,"Quantile",'AveExpr','top_df')
  #     C_Amean = multi_Amean_function(fit$C,"Cyclicloess",'AveExpr','top_df')
  #   }
  #   
  #   Amean = rbind(E_Amean,S_Amean) %>% 
  #     rbind(Q_Amean) %>% 
  #     rbind(C_Amean)
  #   
  #   as.tbl(as.data.frame(Amean))
  #   Amean$Correction = factor(Amean$Correction, levels = unique(Amean$Correction))
  #   Amean$Normalisation = factor(Amean$Normalisation, levels = unique(Amean$Normalisation))
  #   Amean
  #   #Amean$A
  #   
  # })})
  
  
  
  output$MA_correction_ui = renderUI({
    df = multi_fit_data() 
    selectInput('MA_correction','Background Correction',unique(df$Correction),unique(df$Correction),multiple = T)
  })
  
  output$MA_normalisation_ui = renderUI({
    df = multi_fit_data()
    selectInput('MA_normalisation','Normalisation',unique(df$Normalisation),unique(df$Normalisation),multiple = T)
  })
  
  
  
  
  # output$Amean_plot_ui = renderUI({
  #     plot_height = single_plot_height * length(input$MA_normalisation)
  #     output$Amean_plot = renderPlot({
  #   
  #         
  #         Amean = Amean_data() %>% 
  #           filter(Correction %in% input$MA_correction, 
  #                  Normalisation %in% input$MA_normalisation)
  #         
  #         
  #     
  #     
  #           p = ggplot(Amean, aes(x= Correction, y=A, fill=Correction))+
  #             geom_boxplot()+
  #             #theme_classic()+
  #             theme(axis.text = element_text(size = 12), axis.title.x = element_blank(), legend.position = "none") + 
  #             facet_grid(Normalisation ~ .)
  #           q = quantile(Amean$A,na.rm = T)
  #           if(input$MA_quantile == T){
  #             p = p + 
  #               ylim(q[1],q[3])
  #           }
  #           p
  #         
  #       },height = plot_height)
  #     
  #     plotOutput('Amean_plot',height = plot_height)
  # })
  
  # multi_M_function = function(data,norm,column_name,table){
  #   Rfit2 = data$E
  #   Sfit2 = data$S
  #   Mfit2 = data$M
  #   Nfit2 = data$N
  #   
  #   M <- cbind(Rfit2[[table]][column_name], Sfit2[[table]][column_name], Mfit2[[table]][column_name], Nfit2[[table]][column_name])
  #   M <- as.data.frame(M)
  #   colnames(M) = c("Rawdata","Subtraction","Movingminimum","Normexp")
  #   #M = log2(M)
  #   M_l = M %>% 
  #     gather('Correction','M')
  #   M_l$Normalisation = norm
  #   M_l
  # }
  
  multi_collate_fit_function = function(data,norm,table){
    print('hit') 
    Rfit2 = data$E[[table]]
    Rfit2$Correction = 'Rawdata'
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
    #if(input$multi_DE == FALSE){
    #  fit = Rfit()
    #}else{
    #  fit = DE_Rfit() 
    #}
    
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
  
  # M_plot_data = reactive({withProgress(message = 'M data',{
  #    
  #   if(input$multi_DE == FALSE){
  #     fit = Rfit()
  #   }else{
  #     fit = DE_Rfit() 
  #   }
  # 
  #   if(input$multi_top == FALSE){
  #     E_M_l = multi_M_function(fit$E,"None",'F','df')
  #     S_M_l = multi_M_function(fit$S,"Scale",'F','df')
  #     Q_M_l = multi_M_function(fit$Q,"Quantile",'F','df')
  #     C_M_l = multi_M_function(fit$C,"Cyclicloess",'F','df')
  #   }else{
  #     E_M_l = multi_M_function(fit$E,"None",'logFC','top_df')
  #     S_M_l = multi_M_function(fit$S,"Scale",'logFC','top_df')
  #     Q_M_l = multi_M_function(fit$Q,"Quantile",'logFC','top_df')
  #     C_M_l = multi_M_function(fit$C,"Cyclicloess",'logFC','top_df')
  #   }
  #   
  #   M_l = E_M_l %>% 
  #     rbind(S_M_l) %>% 
  #     rbind(Q_M_l) %>% 
  #     rbind(C_M_l)
  #   
  #   as.tbl(M_l)
  #   
  #   M_l$Correction = factor(M_l$Correction, levels = unique(M_l$Correction))
  #   M_l$Normalisation = factor(M_l$Normalisation, levels = unique(M_l$Normalisation))
  #   
  #   M_l
  # })})
  # 
  
  output$M_plot_ui = renderUI({   
    plot_height = single_plot_height * length(input$MA_normalisation)
  
    output$M_plot = renderPlot({
      
      #multi_fit_data
      #M_l = M_plot_data()  %>% 
      #  filter(Correction %in% input$MA_correction, 
      #         Normalisation %in% input$MA_normalisation)
      
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
      
      #multi_fit_data
      #M_l = M_plot_data()  %>% 
      #  filter(Correction %in% input$MA_correction, 
      #         Normalisation %in% input$MA_normalisation)
      
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
  
 
  # MA_data = reactive({
  #   A = Amean_data()
  #   M = M_plot_data()
  #   
  #   dim(A)
  #   dim(M)
  #   
  #   MA = A
  #   MA$M = M$M
  #   as.tbl(MA)
  #   
  #   MA
  # })


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
    
    #df = multi_collate_fit_function(data,norm,table)
    
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
    E_df = as.data.frame(data$E)
    E_df_l = E_df %>% 
      rownames_to_column('Proteins') %>% 
      gather('Target','Intensity',colnames(E_df))
    E_df_l$Correction = 'None'
    
    S_df = as.data.frame(data$S)
    S_df_l = S_df %>% 
      rownames_to_column('Proteins') %>% 
      gather('Target','Intensity',colnames(S_df))
    S_df_l$Correction = 'Substraction'
    
    M_df = as.data.frame(data$M)
    M_df_l = M_df %>% 
      rownames_to_column('Proteins') %>% 
      gather('Target','Intensity',colnames(M_df))
    M_df_l$Correction = 'Movingminimum'
    
    N_df = as.data.frame(data$N)
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
    
    (selected_cols = intersect(selected_targets()$Name,colnames(df)))
    
    df = df[,selected_cols]

    cont_matrix_list = cont_matrix_function(df,selected_targets(),input)
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
    sig_df = eBayes_test()$df %>%  
      rownames_to_column('protein')
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
    colnames(df)  
    #colnames(df) = target_names()
    m = as.matrix(df)
    m
    rownames(df)
    #m = log_min_function(m,input)
    m[is.na(m)] = 0
    m[is.infinite(m)] = 0
    m = m[,selected_targets()$Name]
    #plot_height = 300 + (dim(m)[1]*10)
    # output$eBayes_Heatmap = renderPlot({
    #   Heatmap_function(m,target_conditions(),rownames(m))
    # },height = plot_height)
    if(dim(m)[1] == 0){
      tags$h4('No significant proteins')
    }else{
      id = 'EBayes'
      name = 'Hcluster'
      ht_list = array_HeatMap_function(m,target_conditions(),selected_targets(),rownames(m),input$r_col,values$heatmap_order)
      ht_list$p
      ht_plot_Server(id,name,ht_list)
      #plot_height = data_heatmap_Server('eBayes',m,target_conditions(),selected_targets(),rownames(m),input$r_col,input$heatmap_order)
      do.call(tagList,plot_UI(id,name,ht_list$warning))
      
      #do.call(tagList,data_heatmap_UI('eBayes',plot_height))
    }
 
    # if(dim(m)[1] < max_heatmap_rows){
    #   plot_height = 300 + (dim(m)[1]*10)
    #   output$eBayes_Heatmap = renderPlot({
    #     Heatmap_function(m,target_conditions(),rownames(m),input$r_col,input$heatmap_order)
    #   },height = plot_height)
    #   plotOutput('eBayes_Heatmap',height = plot_height)
    # }else{
    #   output$eBayes_dend = renderPlot({withProgress(message = 'generating dendrogram',{
    #     dend_function(m,target_conditions())
    #   })})
    #   plotOutput('eBayes_dend')
    # }

    #plotOutput('eBayes_Heatmap')
    
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
        geom_point(aes(x = logFC,y = -log10(adj.P.Val), col = comparison, group = protein)) + 
        geom_vline(aes(xintercept = as.numeric(input$fc_cutoff))) + 
        geom_vline(aes(xintercept = as.numeric(input$fc_cutoff))) + 
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
    sig_df = eBayes_test()$df %>%   
      rownames_to_column('protein')
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
     
    df = eBayes_test()$df %>%   
      rownames_to_column('protein')
    
    plot_df = df %>% 
      
      left_join(proteins())
    
    as.tbl(plot_df)
    
    

      MA_plot = ggplot(plot_df) + 
        geom_point(aes(y = logFC, x = AveExpr,col = threshold, shape = Category, group =), size = 3) + 
        geom_hline(aes(yintercept = 0))

    
    MA_plot
  })
  
  
  #targets()
  
  #E()$targets
  
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
  
  output$target_label = renderUI({
    lst = next_tab_UI('targets','Samples')
    # if(is.null(values$targets)){
    #   lst = list(
    #     tags$style(HTML("
    #       .tabbable > .nav > li > a[data-value='targets'] {background-color: #6b8eb7;   color:white}
    #     ")),
    #     span(tags$h4('Next >>'), style="color:white")
    #   )
    # }else{
    #   lst = list(tags$h5('Samples'))
    # }
    do.call(tagList,lst)
  })
  
  output$probe_label = renderUI({
    lst = next_tab_UI('probes','Probes')
    
    # if(is.null(values$probes)){
    #   span(tags$h4('Next >>'), style="color:#21b8cd")
    # }else{
    #   tags$h5('Probes')
    # }
    do.call(tagList,lst)
    
  })
  
  output$protein_label = renderUI({
    lst = next_tab_UI('proteins','Proteins')
    # if(is.null(values$proteins)){
    #   span(tags$h4('Next >>'), style="color:#21b8cd")
    # }else{
    #   tags$h5('Proteins')
    # }
    do.call(tagList,lst)
    
  })
  
})
