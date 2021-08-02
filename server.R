#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

  
  #### Debugging and updating #####
  hideTab('main','All Methods')
  
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
  output$readme_markdown_ui = renderUI({
    hideTab('main','Plots')
    hideTab('main','Data')
    print('readme_markdown_ui')
    includeMarkdown("README.md")
  })
  
  #### Functions #####
  
  values = reactiveValues(
    target_file = NULL,
    spot_file = NULL,
    protein_file = NULL
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
    

    
    myfun <- function(x, threshold = 2*sd){
        okred <- abs(x[,"F635 Median"]) < threshold*abs(x[,"B635Median"])
        +  as.numeric(okred) #Spot filtering 
    }
    
    myfun_2 <- function(x, threshold, foreground_column, background_column){
      okred <- abs(x[,foreground_column]) < threshold*abs(x[,background_column])
      +  as.numeric(okred) #Spot filtering 
    }
    
    sd_filter <- function(x){
      threshold = 2*sd(abs(x[,background_column()]))
      okred <- abs(x[,foreground_column()]) < threshold*abs(x[,background_column()])
      +  as.numeric(okred) #Spot filtering 
    } 
    
    sd_filter_2 <- function(x){
      threshold = 2*sd
      okred <- abs(x[,foreground_column()]) < threshold*abs(x[,background_column()])
      +  as.numeric(okred) #Spot filtering 
    } 
    
    
    CV <- function(x){
      100*(sd(x)/mean(x))
    }

    output$example_data_text_ui = renderUI({
        if(is.null(input$gpr_files$datapath)){
            span(tags$h4('Example data automatically uploaded at start.'), style="color:red")
            
        }
    })
    
    #### Inputs #####
    ##### _Select Datasets #####
    
    output$select_datasets_ui = renderUI({
      if(is.null(input$gpr_files$datapath)){
        selectInput('dataset','Dataset',paper_data_list)
      }else{
        selectInput('dataset','Dataset',c('Upload',paper_data_list),'Upload')
      }
      
    })
    
    
    ##### _Test Files #####
    
    array_file_list = reactive({
      req(input$dataset) 
      if(input$dataset == 'Upload'){
        if(!is.null(input$gpr_files$datapath)){
          file_list = input$gpr_files$name
          file_path_list = input$gpr_files$datapath
        }
      }else{
        getwd()
        (path = input$dataset)
        file_list = list.files(path)
        file_list = file_list[!file_list %in% c('targets.txt','spots.txt','proteins.txt')]
        file_list
        #gpr_files = grep('.gpr',file_list,value = T)
        file_path_list = file.path(path,file_list)
        #gpr_files_path_list
        #file_path_list
      }
      file_path_list
      list(name = file_list,path = file_path_list)
    })
    
    test_files = reactive({#withProgress(message = 'testing array files',{
      req(array_file_list())
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Testing file", value = 0)
      
      (file_name_list = array_file_list()$name)
      file_path_list = array_file_list()$path
      row_list = c()
      col_list = c()
      colnames_list = c()
      rownames_list = c()
      i = 1
      n = length(file_name_list)
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
        colnames_list = c(colnames_list,paste(colnames(df),collapse =', '))
      }
      
      file_df = data.frame(File_name = file_name_list,
                           `Number of Spots` = row_list,
                           `Number of Metric Columns` = col_list)
      file_df
      
      
      
      if(length(unique(row_list)) == 1 &
         length(unique(col_list)) == 1 &
         #length(unique(colnames_list)) == 1 &
         length(unique(rownames_list)) == 1
      ){
        showTab('main','File Details')
        showTab('main','Targets')
        #showTab('main','Spots')
        #showTab('main','Proteins')
        #showTab('main','Plots')
        #showTab('main','Data')
        file_error = FALSE
        
      }else{
        hideTab('main','Targets')
        hideTab('main','Spots')
        hideTab('main','Proteins')
        hideTab('main','Plots')
        hideTab('main','Data')
        file_error = TRUE
      }
      list(file_df = file_df,file_error = file_error,col_names = col_names,type = type)
    })#})
    
    output$test_text = renderText({
      
      
    })
    
    data_col_names = reactive({ 
      paste(test_files()$col_names)
    })
    
    output$data_columns_text = renderPrint({
      cat(paste(data_col_names(),collapse =', '))
    })
    
    output$test_files_table = DT::renderDataTable({
      df = test_files()$file_df  
      df_g = df %>% group_by(Number.of.Spots,Number.of.Metric.Columns) %>% 
        summarise(FileNames = paste(File_name,collapse = ' ,')) %>% 
        ungroup()
      
      df_g
    })
    
    output$test_file_text = renderText({ 
      if(test_files()$file_error == TRUE){
        showTab('main','File Details')
        updateTabItems(session,"main", "File Details")
        print('The array files are too disparate to compare, check the number of spots in each file')
      }else{
        hideTab('main','File Details')
        updateTabItems(session,"main", "Targets")
        print('')
      }
    })
    
    
    ###_Column Selections ####
    
    # data_col_names = reactive({
    #   if(!is.null(input$gpr_files$datapath)){
    #     file_path = input$gpr_files$datapath[1]
    #   }else{
    #     path = data_dir
    #     file_list = list.files(path)
    #     gpr_files = grep('.gpr',file_list,value = T)
    #     gpr_files_path_list = file.path(path,gpr_files)
    #     gpr_files_path_list
    #     file_path = gpr_files_path_list[1]
    #   }
    #   
    #     #if(grepl('.txt',input$gpr_files$datapath[1])){
    #     if(input$array_type == 'Genepix'){
    #         print('txt')
    #         df = read.table(file_path,sep ='\t',stringsAsFactors = F)
    #         (col_names = paste(unlist(df[1,])))
    #     }else{
    #       
    #         file_1 = read_table(file_path)
    #         file_1
    #         i = max(grep("Block",file_1[[1]]))
    #         df = read.csv(file_path,skip = i,sep ='\t',header = F, stringsAsFactors = F)
    #         as.tbl(df)
    #         col_names = paste(df[1,])
    #         #df[[1]][1]
    #         #col_names = df[[1]][grep('"Block"\t"Column"',df[[1]])]
    #         #col_names = paste(unlist(strsplit(col_names,'\t')))
    #         #col_names = gsub('\"','',col_names)
    #      
    #     }
    # 
    #     col_names
    # })
    
    output$array_type_ui = renderUI({
      #if(!is.null(input$gpr_files$datapath)){
      selected = 'Mapix'
        if(TRUE %in% grepl('txt',input$gpr_files$datapath)){
          selected = "Genepix"
        }else{
          selected = 'Mapix'
        }
        radioButtons('array_type','Array',c('Mapix','Genepix'),selected,inline = T)
      #}
    })
    
    
    output$array_colours_ui = renderUI({
      #if(!is.null(input$gpr_files$datapath)){
        col_names = data_col_names() 
        colour_list = c()
        colour_list_test = c('532','635')
        for(col_test in colour_list_test){
          if(TRUE %in% grepl(col_test,col_names)){
            colour_list = c(colour_list,col_test)
          }
        }
        colour_list
        #colour_list = c('532','635')
        selectInput('array_colours', 'Array Colours',colour_list,colour_list[1],multiple = F)
      #}
    })
    
    output$array_column_ui = renderUI({ 
      #if(!is.null(input$gpr_files$datapath)){
      if(test_files()$type != 'flat'){
          colour_list = c('Median','Mean')
          selected = 'Median'
        }else{
          colour_list = c('(mean) [mean]','(med) [med]','(mean) [med]','(med) [mean]')
          selected = '(med) [med]'
        }
        selectInput('array_column', 'Array Column',colour_list,selected)
      #}
    })
    
    output$foreground_column_ui = renderUI({
 
            col_names = data_col_names()
            col_names
            (selected = paste0('F',input$array_colours,' ',input$array_column))

            if(test_files()$type == 'flat'){
              #if(grepl('.txt',input$gpr_files$datapath[1])){
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
            #}
            #if(selected = 'error'){
              selectInput('foreground_column', 'Foreground',col_names,selected)
            #}
        #}

      #selectInput('foreground_column', 'Foreground',col_names,selected)
    })

    output$background_column_ui = renderUI({
        #if(!is.null(input$gpr_files$datapath)){
            col_names = data_col_names()
            col_names
            selected = paste0('B',input$array_colours,' ',input$array_column)

            if(test_files()$type == 'flat'){

              #if(grepl('.txt',input$gpr_files$datapath[1])){
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

            #}
            selectInput('background_column', 'Background',col_names,selected)
        #}
    })

    output$annotation_columns_ui = renderUI({
        #if(!is.null(input$gpr_files$datapath)){
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
                    #}
                    selectInput('select_annotation','Annotation Columns',col_names,annotation,multiple = T)
                }
            }
        #}
    })
    
    # output$foreground_column_ui = renderUI({ 
    #   if(!is.null(test_files())){
    #     col_names = data_col_names() 
    #     col_names
    #     test_files()$type
    # 
    #     
    #     if(test_files()$type == 'flat'){
    #       selected = paste0("Raw intensity ",input$array_column,' {',input$array_colours,'}')
    #     }else{
    #       (selected = grep('F',grep('Median',col_names,value = T),value = T)[1])
    #     }
    #     selectInput('foreground_column', 'Foreground',col_names,selected)
    #   }
    # })
    # 
    # output$background_column_ui = renderUI({
    #   if(!is.null(test_files())){
    #     col_names = data_col_names() 
    #     col_names
    #     test_files()$type
    #     
    #     
    #     if(test_files()$type == 'flat'){
    #       selected = "Background (med) [med] {635}"
    #     }else{
    #       (selected = grep('B',grep('Median',col_names,value = T),value = T)[1])
    #     }
    #     selectInput('background_column', 'Background',col_names,selected)
    #   }
    #   
    # })
    # 
    # output$annotation_columns_ui = renderUI({
    #   if(!is.null(input$background_column)){
    #     if(!is.null(input$foreground_column)){
    #       
    #       (col_names = data_col_names())
    #       (col_names = col_names[!col_names %in% c(input$background_column,input$foreground_column)])
    #       if(test_files()$type == 'flat'){
    #         annotation = c("Grid","Column","Row","Annotation", "Name")
    #       }else{
    #         annotation=c("Block","Column","Row","ID","Name")
    #       }
    #       selectInput('select_annotation','Annotation Columns',col_names,annotation,multiple = T)
    #     }
    #   }
    #   
    # })

    
    #### E ####
     
    foreground_column = reactive(input$foreground_column)
    background_column = reactive(input$background_column)
    
    observeEvent(input$gpr_files$datapath,{
      values$spot_file = NULL 
      values$proteins_file = NULL
      values$target_file = NULL
      
    })
    

      

    
    E = reactive({   
        #req(input$gpr_files)
        #upload = list()
        # if(!is.null(input$gpr_files$datapath)){
        #   file_path_list = input$gpr_files$datapath
        #   #values$spot_file = NULL 
        #   #values$proteins_file = NULL
        #   #values$target_file = NULL
        # }else{
        #   path = data_dir
        #   file_list = list.files(path)
        #   gpr_files = grep('.gpr',file_list,value = T)
        #   gpr_files_path_list = file.path(path,gpr_files)
        #   gpr_files_path_list
        #   file_path_list = gpr_files_path_list
        # }
      
      file_path_list = array_file_list()$path  
      file_path_list
          if(input$spot_filtering == 'wtflags(0.1)'){
            E <- read.maimages(file_path_list,
                               columns=list(E=input$foreground_column, Eb=input$background_column),
                               annotation=c(input$select_annotation), 
                               wt.fun=wtflags(0.1))
          }
          if(input$spot_filtering == 'wtarea(150)'){
            E <- read.maimages(file_path_list,
                               columns=list(E=input$foreground_column, Eb=input$background_column),
                               annotation=c(input$select_annotation), 
                               wt.fun=wtarea(150))
          }
          if(input$spot_filtering == 'sd_filter'){
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
        # }else{
        #     
        #     
        #         path = data_dir
        #         file_list = list.files(path)
        #         gpr_files = grep('.gpr',file_list,value = T)
        #         gpr_files_path_list = file.path(path,gpr_files)
        #         gpr_files_path_list
        #   
        #         
        #     
        #         if(input$spot_filtering == TRUE){
        #         
        #             E <- read.maimages(gpr_files_path_list,
        #                                columns=list(E="F635 Median", Eb="B635 Median"),
        #                                annotation=c("Block", "Column", "Row", "ID"),
        #                                             wt.fun=myfun())
        #         }else{
        #           E <- read.maimages(gpr_files_path_list,
        #                              columns=list(E="F532 Median", Eb="B532 Median"),
        #                              annotation=c("Block", "Column", "Row", "ID"))
        #         }
        # 
        #             E
        #     
        #   
        # }
        E
    
    })
    

    
    #---------------------------Read in data and filter spots----------------------------------------------------
    
    #first make a targets file (.txt) with the following colunms
    #Name, filename, and Disease state (include any other extra information if necessary)

    # output$protein_file_upload_ui = renderUI({
    #     if(!is.null(input$gpr_files$datapath)){
    #         fileInput(
    #             inputId = "protein_file", 
    #             label = "Upload Protein File", 
    #             #multiple = TRUE,
    #             accept = c(".txt")
    #         )
    #     }
    # })
    # 
    # 
    # 
    # observeEvent(input$reset_spots,{
    #   values$spot_file = NULL
    # })
    # observeEvent(input$reset_proteins,{
    #   values$proteins_file = NULL
    # })
    # 
    # 
    # 
    # observeEvent(input$spot_file,{
    #   values$spot_file = input$spot_file
    # })
    # 
    # observeEvent(input$protein_file,{
    #   values$protein_file = input$protein_file
    # })
    
    #### _targets ####
    
    # observeEvent(input$reset_targets,{
    #   values$target_file = NULL
    # })
    # 
    # observeEvent(input$target_file,{
    #   values$target_file = input$target_file
    # })
    # 
    # output$target_file_upload_ui = renderUI({
    #   if(!is.null(input$gpr_files$datapath)){
    #     fileInput(
    #       inputId = "target_file", 
    #       label = "Upload Targets File", 
    #       #multiple = TRUE,
    #       accept = c(".txt")
    #     )
    #   }
    # })
    # 
    # target_names = reactive({
    #   #basename(colnames(E()$E))
    #   #as.tbl(targets())
    #   
    #   target_names = targets()$Name
    #   target_names
    # })
    
    observeEvent(c(input$dataset,
                   input$reset_target,
                   input$gpr_files),{
                     values$target_file = NULL
                   })
    
    observeEvent(input$target_file,{
      values$target_file = input$target_file
    })
    
    output$target_file_upload_ui = renderUI({
      input$dataset
      input$reset_target
      input$gpr_files
      
      fileInput(
        inputId = "target_file", 
        label = "Upload Targets File", 
        accept = c(".txt")
      )
    })
    
    target_names = reactive({ 
      target_names = targets()$Name
      target_names
    })
    
    targets_upload = reactive({   
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
      df_upload = NULL
      if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'targets.txt')) & is.null(values$target_file)){
        target_file_path = file.path(input$dataset,'targets.txt')
        #df = read.csv(file.path(input$dataset,'targets.txt'),sep ='\t')
      }else{
        if(!is.null(values$target_file)){
          
          target_file_path = values$target_file$datapath
        }
      }
        if(!is.null(target_file_path)){
  
          
          df_upload = read.csv(target_file_path,sep ='\t',stringsAsFactors = F)
          
    
          dim(df_upload)
          
          if('FileName' %in% colnames(df_upload)){
            if(!TRUE %in% duplicated(df_upload$Name)){
              df = df_files %>% 
                dplyr::select(-Name) %>% 
                left_join(df_upload %>% 
                            filter(!duplicated(FileName))) %>%
                filter(FileName %in% file_names)
              df$Name[is.na(df$Name)] = df$FileName[is.na(df$Name)]
            }else{
              error = "There are duplicates in Name column of the uploaded targets file."
              df = df_files
            }
          }else{
            error = 'There is no FileName column in the uploaded targets file.'
            df = df_files
          }
          
        }else{
          df = df_files
        }
        
        if(!'Condition' %in% colnames(df)){
          df$Condition = 'Condition'
        }
        if(!'Name' %in% colnames(df)){
          df$Name = df$File
        }
        #df$Name = as.character(df$Name)
   
      df$Name = as.character(df$Name)
      list(df = df, df_upload = df_upload, error = error)
    }) 
    
    output$target_upload_error_ui = renderUI({
      if(!is.null(targets_upload()$error)){
        output$target_upload_error_text = renderPrint(cat(targets_upload()$error))
        output$target_upload_df = DT::renderDataTable(targets_upload()$df_upload)
        
        lst = list(span(tags$h4(htmlOutput('target_upload_error_text')), style="color:red"),
                   tags$h2('Uploaded Targets Table'),
                   DT::dataTableOutput('target_upload_df'),
                   tags$h2('Targets Table'))
        do.call(tagList,lst)
      }
    })
    
    targets = reactive(targets_upload()$df)
    # targets = reactive({ 
    #   if(is.null(input$gpr_files$datapath)){
    #     #df = readTargets(file.path(data_dir,'targets.txt'))
    #     df = read.csv(file.path(data_dir,'targets.txt'),sep = '\t')
    #   }else{
    #     if(!is.null(values$target_file)){
    #       file_names = input$gpr_files$name
    #       (n = length(file_names))
    #       df_files = data.frame(FileName = file_names,
    #                             Name = tools::file_path_sans_ext(file_names)
    #       )
    #       dim(df_files)
    #       as.tbl(df_files)
    #       #df = readTargets(values$target_file$datapath,sep ='\t',row.names = FALSE)
    #       df_upload = read.csv(values$target_file$datapath,sep ='\t')
    #       dim(df_upload)
    #       duplicated(df_upload$FileName)
    #       df = df_files %>% 
    #         dplyr::select(-Name) %>% 
    #         left_join(df_upload %>% 
    #                     filter(!duplicated(FileName)))
    #       as.tbl(df)
    #       
    #       df = df %>% filter(FileName %in% input$gpr_files$name)
    #       dim(df)
    #       #df
    #       #df = read_table2(values$target_file$datapath)
    #       str(df)
    #     }else{
    #       #file_names = basename(E()$targets$FileName)
    #       file_names = input$gpr_files$name
    #       n = length(file_names)
    #       df_files = data.frame(FileName = file_names,
    #                             Name = tools::file_path_sans_ext(file_names)
    #       )
    #       dim(df_files)
    #       as.tbl(df_files)
    #       
    #       df = df_files
    #       
    #       #df$Group = 'Group'
    #       #df$Condition = 'Condition'
    #       
    #       
    #     }
    #   }
    #   if(!'Group' %in% colnames(df)){
    #     df$Group = 'Group'
    #   }
    #   if(!'Condition' %in% colnames(df)){
    #     df$Condition = 'Condition'
    #   }
    #   if(!'Name' %in% colnames(df)){
    #     df$Name = df$File
    #   }
    #   df$Name = as.character(df$Name)
    #   df
    # })
    
    output$select_conditions_column_ui = renderUI({
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
    
    output$target_table = DT::renderDataTable({
      selected_targets()
    })
    
    output$download_targets <- downloadHandler(
      filename = function(){"targets.txt"}, 
      content = function(fname){
        write.table(target_conditions(), fname,sep = '\t', row.names = FALSE)
      }
    ) 
    
    output$condition_select_ui = renderUI({
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
      #if(!is.null(input$gpr_files$datapath)){
        fileInput(
          inputId = "spot_file", 
          label = "Upload Spots File", 
          #multiple = TRUE,
          accept = c(".txt")
        )
      #}
    })
    
    output$spot_columns_ui = renderUI({
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
    
    # spot_names = reactive({
    #   if(is.null(input$gpr_files$datapath)){
    #     spot_names = paste(E()$genes$ID)
    #   }else{
    #     #colnames(df) <- c(targets()$Name)
    #     if(grepl('.txt',input$gpr_files$datapath[1])){
    #       spot_names = paste(E()$genes$Annotation)
    #     }else{
    #       spot_names = paste(E()$genes$ID)
    #     }
    #   }
    #   spot_names
    # })
    
    spot_names = reactive({
      input$reset_spots
      
      #if(test_files()$type == 'flat'){
      #  spot_names = paste(E()$genes$Annotation)
      #}else{
      #  spot_names = paste(E()$genes$ID)
      #}
      
      spot_names = E()$genes[[input$spot_column]]
      #i = 1

      spot_names
    })
    
    
    
    # spot_upload = reactive({
    #   if(is.null(input$gpr_files$datapath)){
    #     df = read.table(file.path(data_dir,'spots.txt'),header = T)
    #   }else{
    #     if(!is.null(values$spot_file)){
    #       df = read.table(input$spot_file$datapath,header = T)
    #     }else{
    #       df = E()$genes
    #       df$spot = spot_names()
    #       df$Category = character(dim(df)[1])
    #       df
    #     }
    #   }
    #   df
    # })
    
    spot_upload = reactive({  
      input$reset_spots
      input$dataset
      input$gpr_files
      spot_file_path = NULL
      
      df = E()$genes
      df$spot = spot_names()
      
      spot_names = spot_names()
      if(input$remove_spot_duplicates == T){
        while(TRUE %in% duplicated(spot_names)){
          print(paste('hit :',i))
          print(length(spot_names[duplicated(spot_names) == TRUE]))
          spot_names[duplicated(spot_names)] = paste0(spot_names[duplicated(spot_names)],'_',i)
          i = i + 1
        }
        TRUE %in% duplicated(spot_names)
        df$unique_spot = spot_names
      }
     
      
      
      if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'spots.txt')) & is.null(values$spot_file)){
        #df = read.csv(file.path(input$dataset,'spots.txt'),sep ='\t')
        spot_file_path = file.path(input$dataset,'spots.txt')
      }else{
        if(!is.null(values$spot_file)){
          spot_file_path = input$spot_file$datapath
        }
      }
      if(!is.null(spot_file_path)){

          upload_df = read.csv(spot_file_path,sep ='\t',stringsAsFactors = F)
          
          df = df %>% 
            left_join(upload_df)
      }
      
      if(!'Category' %in% colnames(df)){
        df$Category = character(dim(df)[1])
      }
      df
    })
    
    output$both_spot_control_ui = renderUI({
      input$reset_spots
      input$dataset
      input$gpr_files
      df = spot_upload()
      as.tbl(df)
      proteins = df$spot
      control = df$spot[df$Category == 'remove' | df$spot == 'EMPTY']
      selectInput('select_remove','Spots to Remove',proteins,control,multiple = T, width = 1200)
    })
    
    spots = reactive({
      df = spot_upload()
      df$Category[df$spot %in% input$select_remove] = 'remove'
      df$Category[df$Category == 'remove' & !df$spot %in% input$select_remove] = ''
      df
    })
    
    removed_spots = reactive({
      spots() %>% 
        filter(!Category %in% c('remove')) %>% 
        pull(spot)
    })
    
    
    output$spot_table = DT::renderDataTable({
      spots()
    })
    
    output$download_spots <- downloadHandler(
      filename = function(){"spots.txt"}, 
      content = function(fname){
        write.table(spots(), fname,sep = '\t',row.names = F)
      }
    )
    
    #### _proteins ####
    
    output$protein_file_upload_ui = renderUI({
      input$dataset
      input$reset_proteins
      input$gpr_files
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
      selectInput('protein_column','Protein Column',colnames(spots()),selected)
    })
    
    observeEvent(input$reset_proteins,{
      values$proteins_file = NULL
    })
    
    
    
    observeEvent(input$protein_file,{
      values$protein_file = input$protein_file
    })
    
    # protein_upload = reactive({
    #   
    #   
    #   if(is.null(input$gpr_files$datapath)){
    #     df = read.table(file.path(data_dir,'proteins.txt'),header = T)
    #   }else{
    #     if(!is.null(values$protein_file)){
    #       df = read.table(input$protein_file$datapath,header = T)
    #     }else{
    #       # df = E()$genes
    #       # df$spot = spot_names()
    #       # df$Category = character(dim(df)[1])
    #       # df
    #       
    #       df = data_full()
    #       
    #       df = data.frame(protein = df$protein,
    #                       Category = character(dim(df)[1]))
    #       df
    #     }
    #   }
    #   df
    #   
    # })
    
    protein_upload = reactive({ 
      #req(data_full())
      input$reset_targets
      input$dataset
      input$gpr_files
      #if(!is.null(values$data)){
      
     
      df = data.frame(protein = data_full()$protein)
      error = NULL
      upload_df = NULL
      protein_file_path = NULL
      if(input$dataset != 'Upload' & file.exists(file.path(input$dataset,'proteins.txt')) & is.null(values$protein_file)){
        #df = read.csv(file.path(input$dataset,'proteins.txt'),sep ='\t')
        protein_file_path = file.path(input$dataset,'proteins.txt')
      }else{
        if(!is.null(values$protein_file)){
          protein_file_path = input$protein_file$datapath
        }
      }
      if(!is.null(protein_file_path)){
      
          upload_df = read.csv(protein_file_path,sep ='\t',stringsAsFactors = F)
          
          if(!TRUE %in% duplicated(upload_df$Name)){
            
            df = df %>% 
              left_join(upload_df)
          }else{
            error = 'There are duplicates in the Name column'
            df = df
          }
      }
      
      if(!'Category' %in% colnames(df)){
     
        df$Category = character(dim(df)[1])

      }
      
      list(df = df, upload_df = upload_df,error = error)
    })
    
    output$protein_control_ui = renderUI({
      #if(!is.null(values$data)){
      input$reset_proteins
      df = protein_upload()$df
      as.tbl(df)
      proteins = df$protein
      control = df$protein[df$Category == 'control']
      selectInput('select_controls','Controls',proteins,control,multiple = T, width = 1200)
      #}
    })
    
    proteins = reactive({
      #if(!is.null(protein_upload())){
      #req(input$select_controls)
      df = protein_upload()$df
      df$Category[df$protein == ''] = 'EMPTY'
      df$Category[df$protein == 'EMPTY'] = 'EMPTY'
      if(!is.null(input$select_controls)){
        df$Category[df$protein %in% input$select_controls] = 'control'
        df$Category[df$Category == 'control' & !df$protein %in% input$select_controls] = 'analyte'
      }
      df
      #}
    })
    
    


    
    output$proteins_table = DT::renderDataTable({
      proteins() 
    })
    
    output$download_proteins <- downloadHandler(
      filename = function(){"proteins.txt"}, 
      content = function(fname){
        write.table(proteins(), fname,sep = '\t',row.names = F)
      }
    )
    
    
    #### Data #####
    
    output$foreground_table = DT::renderDataTable({
      df = as.data.frame(E()$E)  
      dim(df)
      colnames(df) = target_names()
      df$spot = spot_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(spot,everything())
      df
    }) 
    
    fbs_heatmap_function = function(m,spots){
      col_fun = colorRamp2(c(0,max(as.numeric(m),na.rm = T)), c("white","red"))
      
      Heatmap(m,
              col = col_fun,
              row_labels = spots,
              row_names_side = "left",
              column_names_side = "top",
              cluster_columns = FALSE,
              cluster_rows = FALSE)
    }
    
    output$foreground_heatmap_ui = renderUI({
      #input$ 
      m = E()$E  
      

      colnames(m)
      colnames(m) = target_names()
      m = m[,selected_targets()$Name]

      plot_height = 300+(dim(m)[1]*10)
      output$foreground_heatmap = renderPlot({
        #fbs_heatmap_function(m,spot_names())
        Heatmap_function(m,selected_targets(),spot_names(),input$heatmap_order)
    
      },height = plot_height)
      
      plotOutput('foreground_heatmap',height = plot_height)
    })
    
    output$background_table = DT::renderDataTable({
      df = as.data.frame(E()$Eb)
      colnames(df) = target_names()
      df$spot = spot_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(spot,everything())
      df
    })
    
    output$background_heatmap_ui = renderUI({
      m = E()$Eb
      col_fun = colorRamp2(c(0,max(as.numeric(m))), c("white", "red"))
      
      colnames(m)
      colnames(m) = target_names()
      m = m[,selected_targets()$Name]
      
      plot_height = 300+(dim(m)[1]*10)
      output$background_heatmap = renderPlot({
        fbs_heatmap_function(m,spot_names())
    
      },height = plot_height)
      
      plotOutput('background_heatmap',height = plot_height)
    })
    
    spot_filtering_threshold = reactive({
      (threshold = 2*sd(E()$Eb))
      df = E()$Eb %>% 
        as.data.frame
      as.tbl(df)
      df_l = df %>% 
        as.tibble %>% 
        rownames_to_column('spot') %>% 
        
        gather(key,value,colnames(df))
      as.tbl(df_l)
    })
    
    output$spot_filtering_threshold_text = renderText({
      paste('Spot filtering threshold = ',spot_filtering_threshold())
    })
     
    spot_filtering = reactive({
      threshold = spot_filtering_threshold()
      
      df = as.data.frame(abs(E()$E) < threshold*abs(E()$Eb))
      as.tbl(as.data.frame(df))
      dim(df) 
      #df = E()$weights %>%  
      #  as.data.frame
      colnames(df) = target_names()
      df$spot = spot_names()
      as.tbl(as.data.frame(df))
      df = df %>% 
        dplyr::select(spot,everything())
      df
      
    })
    
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
    
    output$spot_filtering_table = DT::renderDataTable({
      # myfun <- function(x, threshold = 2*sd){
      #   okred <- abs(x[,"F635 Median"]) < threshold*abs(x[,"B635Median"])
      #   +  as.numeric(okred) #Spot filtering 
      # }
      
     df = spot_filtering()
    })
    output$spot_filtering_heatmap_ui = renderUI({  

      
      
      m = spot_filtering() %>% 
        dplyr::select(-spot) %>% 
        as.matrix()
      

      m[m == TRUE] = 'TRUE'
      m[m != TRUE] = 'FALSE'
 
      colnames(m)
      colnames(m) = target_names()
      m = m[,selected_targets()$Name]
    plot_height = 300+(dim(m)[1]*10)
    output$spot_filtering_heatmap = renderPlot({
      
      Heatmap(m,
              name = 'spot',

              row_labels = spot_names(),
              row_names_side = "left",
              column_names_side = "top",
              cluster_columns = FALSE,
              cluster_rows = FALSE)
    },height = plot_height)
    
    plotOutput('spot_filtering_heatmap',height = plot_height)
    })
    
    
    output$spot_filtering_E_table = DT::renderDataTable({
      # myfun <- function(x, threshold = 2*sd){
      #   okred <- abs(x[,"F635 Median"]) < threshold*abs(x[,"B635Median"])
      #   +  as.numeric(okred) #Spot filtering 
      # }
      
      df = spot_filtering_E()
    })
    output$spot_filtering_E_heatmap_ui = renderUI({ 
      
      
      
      m = spot_filtering_E() %>% 
        dplyr::select(-spot) %>% 
        as.matrix()
      
      
      #m[m == TRUE] = 'TRUE'
      #m[m != TRUE] = 'FALSE'
      
      colnames(m)
      colnames(m) = target_names()
      m = m[,selected_targets()$Name]
      plot_height = 300+(dim(m)[1]*10)
      output$spot_filtering_E_heatmap = renderPlot({
        
        Heatmap(m,
                name = 'spot',
                
                row_labels = spot_names(),
                row_names_side = "left",
                column_names_side = "top",
                cluster_columns = FALSE,
                cluster_rows = FALSE)
      },height = plot_height)
      
      plotOutput('spot_filtering_E_heatmap',height = plot_height)
    })
    
    signal2noise = reactive({
      fg = E()$E
      bg = E()$Eb
      View(bg)
      dim(bg)[2]
      bg_sd = bg %>% 
        as.data.frame %>% 
        mutate_all(as.numeric) %>% 
        rowwise() %>% 
        mutate(row_sd = mean(.,na.rm = T))
      View(bg_df)
      bg_sd$row_sd
      
      sd(bg[2,])
    })
    
    #----------------------------Visualization of Raw data---------------------------------------------
    

    
    boxplot_function_1_col_array = function(data,target_names,spot_names,targets,selected_targets,log_rb,input){
      df = data.frame(data)
      if(log_rb == TRUE){
        df = log2(df)
      }
      colnames(df) <- target_names
      df$Proteins = spot_names
      as.tbl(df)   
      
      
      df_l = df %>% 
        gather(Name,`Expression Intensity`,c(target_names)) %>% 
        left_join(targets) %>% 
        filter(Name %in% selected_targets$Name)
      as.tbl(df_l)
      p = ggplot(df_l)
      if(input$collapse_boxplots == F){
        
        if(length(unique(df_l$Condition)) > 1){
          p = p + geom_boxplot(aes(x = Name,y = `Expression Intensity`, col = Condition))
          
        }else{
          p = p + geom_boxplot(aes(x = Name,y = `Expression Intensity`))
          
        }
      }else{
        p = p + geom_boxplot(aes(x = Condition,y = `Expression Intensity`, col = Condition))
        
      }
      p = gg_col_function(p)
      if(length(unique(df_l$Group)) > 1){
        p = p + facet_grid(Group ~ .)
      }
      
      p 
    }
    
    
    output$E_boxplot = renderPlot({ 
      input$collapse_boxplots
      boxplot_function_1_col_array(E()$E,target_names(),spot_names(),target_conditions(),selected_targets(),input$log_rb,input)
    })
    
    CV_df_function  = function(df,targets){
  
 
      
      df_l = df %>% 
        gather(Name,value,-spot) %>% 
        left_join(targets())
      
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

    E_CV_df = reactive({
      df = E()$E  
      targets = targets()
      target_names = target_names()
      spot_names = spot_names()
      
      colnames(df) = target_names()
      rownames(df) = spot_names()
      df = df %>% 
        as.data.frame %>% 
        rownames_to_column('spot')
      
      CV_df = CV_df_function(df,targets)
      CV_df
    })
    
    output$E_CV_plot = renderPlot({
      CV_df = E_CV_df() %>%  
        left_join(spots())
      as.tbl(CV_df)
      unique(CV_df$Category)
      q = quantile(CV_df$CV,na.rm = T)
      ggplot(CV_df) + 
        geom_boxplot(aes(y = CV,x = Condition, fill = Category)) + 
        ylim(q[1],q[3])
    })
    
    missingness_function = function(df,targets){

      
      df[df <= 0] = NA
      as.tbl(df)
      
      df_l = df %>% 
        rownames_to_column('spot') %>% 
        gather(Name,value,colnames(df)) %>% 
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
      p = gg_fill_function(p) 
      p
    }
    
    cont_matrix_function = function(df,targets){
      time = factor(paste(targets$Condition), levels = unique(targets$Condition))
      time
      design = model.matrix(~0+time)
      colnames(design) = levels(time)
      
      conditions = unique(targets$Condition)
      comparison_list = c()
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
      comparison_list
      design
      cmd = paste0('cont.matrix = makeContrasts(',paste(comparison_list,collapse=', '),', levels=design)')
      #print(cmd)
      eval(parse(text = cmd))
      cont.matrix
      result = list(design = design, cont.matrix = cont.matrix,cmd = cmd)
      return(result)
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
    
    output$R_MA_plot = renderPlotly({
      df = E()$E  
      rownames(df) = spot_names()
      spots = spots()
      MA_plot_function(df,spots())
    })
    
    output$R_missing_plot = renderPlot({
      df = E()$E   
      colnames(df) = target_names()
      rownames(df) = spot_names()
      df = as.data.frame(df)
      
      missingness_function(df,targets())
    })
    
    #---------------------------------Background Correction----------------------------
    
    Heatmap_function = function(m, targets,spots,cluster = 'Cluster'){ 
      
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
      (condition_col = brewer.pal(n = length(unique(targets$Condition)), name = input$r_col)[1:length(unique(targets$Condition))])
      (names(condition_col) = unique(targets$Condition))
      column_ha = HeatmapAnnotation(Condition = ha_annotation, col = list(Condition = condition_col))
      
      sample_order = targets %>% 
        arrange(Condition)
      
      
      if(cluster == 'Cluster'){
        Heatmap(m,
                col = col_fun,
                row_labels = spots,
                column_dend_height = unit(4, "cm"), 
                row_dend_width = unit(4, "cm"),
                column_names_side = "top",
                top_annotation = column_ha
        )
      }else{
        Heatmap(m,
                col = col_fun,
                row_labels = spots,
                row_names_side = "left",
                column_names_side = "top",
                cluster_columns = FALSE,
                cluster_rows = FALSE,
                top_annotation = column_ha,
                column_order = sample_order$Name)
      }
      
    }
    
    dend_function = function(m,targets){
      m
 
      
       dend <- m %>% scale %>% dist %>% 
         hclust %>% as.dendrogram
      #%>%
      #  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
      #  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
      #  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
      # plot the dend in usual "base" plotting engine:
      #plot(dend)
      library(dendextend)
      library(ggdendro)
      ggd1 <- as.ggdend(dend)
      ggplot(ggd1) 
      
      x <- as.matrix(scale(m))
      dd.row <- as.dendrogram(hclust(dist(t(x))))
      ddata_x <- dendro_data(dd.row)
      #as.tbl(ddata_x)
      
      ddata_x$labels = ddata_x$labels %>%
        mutate(Name = label) %>% 
        left_join(targets)
      #ddata_x$labels
      ddata_x$leaf_labels = ddata_x$labels$label
      
      #da
      p2 <- ggplot(segment(ddata_x)) +
        geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
      p2 = p2 + geom_text(data=label(ddata_x),
                     aes(label=label, x=x, y=y-2, colour=ddata_x$labels$Condition, angle = 90))
      p2
    }
    
    log_min_function = function(m,input){
      if(input$min_corr == TRUE){
        m[m < 1] = 1
      }
      if(input$log_rb == TRUE){
        m = log2(m)
      }
      m
    }
    
    neg_corr_function = function(m,input){
      if(input$min_corr == TRUE){
        m[is.na(m)] = 0
        m[is.infinite(m)] = 0 
        m[m < 0] = 0
      }
      m
    }
    
    output$E_Heatmap_ui = renderUI({ 
      df = E()$E 
      colnames(df) = target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      #m[is.na(m)] = 0
      #m[is.infinite(m)] = 0
      m = m[,selected_targets()$Name]
      # plot_height = 300 + (dim(m)[1]*10)
      # output$E_Heatmap = renderPlot({
      #   Heatmap_function(m,target_conditions(),spot_names(),input$heatmap_order)
      # },height = plot_height)
      # 
      # plotOutput('E_Heatmap')
      
      if(dim(m)[1] < max_heatmap_rows){
        plot_height = 300 + (dim(m)[1]*10)
        output$E_Heatmap = renderPlot({
          Heatmap_function(m,target_conditions(),spot_names(),input$heatmap_order)
        },height = plot_height)
        plotOutput('E_Heatmap',height = plot_height)
      }else{
        output$E_dend = renderPlot({withProgress(message = 'generating dendrogram',{
          dend_function(m,target_conditions())
        })})
        plotOutput('E_dend') 
      }
      
    })
    
    E_filter_before = reactive({
      E = E()
      if(!is.null(E()$weights) & input$apply_spot_filtering == T){
        E_fg = E$E
        E_fg
        E_weights = E()$weights
        E_filter = E_fg * E_weights
        E_filter[E_filter == 0] = NA
        E$E = E_filter
      }
      E$E
      E
    })
    
    E_corr = reactive({
        #backgroundCorrect(E_filter_before(), method = input$backgroundCorrect_method, offset = 0)
      backgroundCorrect(E_filter_before(), method = input$backgroundCorrect_method)
      
    })
    
    output$E_corr_boxplot = renderPlot({
 
      data = E_corr()$E
      
      input$collapse_boxplots
      boxplot_function_1_col_array(data,target_names(),spot_names(),target_conditions(),selected_targets(),input$log_rb,input)
      
      
    })
    
    output$E_corr_missing_plot = renderPlot({
      df = E_corr()$E
      colnames(df) = target_names()
      rownames(df) = spot_names()
      df = as.data.frame(df)
      as.tbl(df)
      missingness_function(df,targets())
    })
    output$E_corr_MA_plot = renderPlotly({
      df = E_corr()$E
      rownames(df) = spot_names()
      MA_plot_function(df,spots())
    })
    
    output$E_corr_Heatmap_ui = renderUI({
      df = E_corr()$E   
      colnames(df) = target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      m = m[,selected_targets()$Name]
      if(TRUE %in% is.na(m) | TRUE %in% is.infinite(m)){
        span(tags$h5("Negative values cannot be log2 transformed, NA's produced"), style="color:red")
      }else{
        if(dim(m)[1] < max_heatmap_rows){
          plot_height = 300 + (dim(m)[1]*10)
          output$E_corr_Heatmap = renderPlot({
            Heatmap_function(m,target_conditions(),spot_names(),input$heatmap_order)
          },height = plot_height)
          plotOutput('E_corr_Heatmap',height = plot_height)
        }else{
          output$E_corr_dend = renderPlot({withProgress(message = 'generating dendrogram',{
            dend_function(m,target_conditions())
          })})
          plotOutput('E_corr_dend') 
        }
        
      }
      
    })
    
    E_corr_CV_df = reactive({
      df = E_corr()$E
      targets = targets()
      target_names = target_names()
      spot_names = spot_names()
      
      colnames(df) = target_names()
      rownames(df) = spot_names()
      
      
      df = df %>% 
        as.data.frame %>% 
        rownames_to_column('spot')
      
      CV_df = CV_df_function(df,targets)
      CV_df
    })
    
    output$E_corr_CV_plot = renderPlot({
      CV_df = E_corr_CV_df() %>%  
        left_join(spots())
      as.tbl(CV_df)
      unique(CV_df$Category)
      q = quantile(CV_df$CV,na.rm = T)
      ggplot(CV_df) + 
        geom_boxplot(aes(y = CV,x = Condition, fill = Category)) + 
        ylim(q[1],q[3])
    })
    
    #----------------------------------Spot Filtering ----------------------------------
    
    
    E_filter = reactive({
      
      E_filter_before()$E
      # E = E_corr()$E
      # if(!is.null(E_corr()$weights)){
      #   E_weights = E_corr()$weights
      #   
      #   E_filter = E * E_weights
      #   E_filter[E_filter == 0] = NA
      # }else{
      #   E_filter = E
      # }
      # 
      # E_filter
    })
    
    output$E_filter_boxplot = renderPlot({
      
      data = E_filter()
      
      input$collapse_boxplots
      boxplot_function_1_col_array(data,target_names(),spot_names(),target_conditions(),selected_targets(),input$log_rb,input)
      
      
    })
    
    output$E_filter_missing_plot = renderPlot({
      df = E_filter()
      colnames(df) = target_names()
      rownames(df) = spot_names()
      df = as.data.frame(df)
      
      missingness_function(df,targets())
    })
    
    output$E_filter_MA_plot = renderPlotly({
      df = E_filter()
      rownames(df) = spot_names()
      MA_plot_function(df,spots())
    })
    
    output$E_filter_Heatmap_ui = renderUI({
      df = E_filter() 
      colnames(df) = target_names()
      m = as.matrix(df)
      m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      m = m[,selected_targets()$Name]
      if(TRUE %in% is.na(m) | TRUE %in% is.infinite(m)){
        span(tags$h5("Negative values cannot be log2 transformed, NA's produced"), style="color:red")
      }else{
      #   plot_height = 300 + (dim(m)[1]*10)
      #   output$E_corr_Heatmap = renderPlot({
      #     Heatmap_function(m,target_conditions(),spot_names(),input$heatmap_order)
      #   },height = plot_height)
      #   
      #   plotOutput('E_corr_Heatmap')
      # }
      
      if(dim(m)[1] < max_heatmap_rows){
        plot_height = 300 + (dim(m)[1]*10)
        output$E_filter_Heatmap = renderPlot({
          Heatmap_function(m,target_conditions(),spot_names(),input$heatmap_order)
        },height = plot_height)
        plotOutput('E_filter_Heatmap',height = plot_height)
      }else{
        output$E_filter_dend = renderPlot({withProgress(message = 'generating dendrogram',{
          dend_function(m,target_conditions())
        })})
        plotOutput('E_filter_dend') 
      }
      }
      
    })
    
    
    E_filter_CV_df = reactive({
      df = E_filter() 
      targets = targets()
      target_names = target_names()
      spot_names = spot_names()
      
      colnames(df) = target_names()
      rownames(df) = spot_names()
      
      
      df = df %>% 
        as.data.frame %>% 
        rownames_to_column('spot')
      
      CV_df = CV_df_function(df,targets)
      CV_df
    })
    
    output$E_filter_CV_plot = renderPlot({
      CV_df = E_filter_CV_df() %>%  
        left_join(spots())
      as.tbl(CV_df)
      unique(CV_df$Category)
      q = quantile(CV_df$CV,na.rm = T)
      ggplot(CV_df) + 
        geom_boxplot(aes(y = CV,x = Condition, fill = Category)) + 
        ylim(q[1],q[3])
    })
    #---------------------------------Normalization------------------------------------
    
    # E_df = reactive({
    #     E_norm = data.frame(log2(E()$E))
    #     rownames(E_norm) <- spot_names() #ID represents column name of protein IDs/names 
    #     colnames(E_norm) <- target_names()
    #     E_norm
    # })
    pre_norm_function = function(data,spot_names,target_names,removed_spots,log_rb){
      df = as.data.frame(data)
      dim(df)
      colnames(df) <- target_names
      df$spot = spot_names
      
      df_f = df %>% 
        dplyr::filter(spot %in% removed_spots)
      
      df_m = df_f %>% 
        dplyr::select(-spot)
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
    
    E_norm = reactive({   
      
        data = E_corr()$E
        spot_names = spot_names()
        target_names = target_names()
        removed_spots = removed_spots()
        log_rb = input$log_rb
      
        norm_list = pre_norm_function(E_corr()$E,spot_names(),target_names(),removed_spots(),input$log_rb)
        E_norm = norm_function(norm_list$m,input$normalisation_method,norm_list$spots)
        E_norm
        
        #E_norm = E_norm[,c(targets()$Name[1:10],'spot')]
        #E_norm
    })
    
    output$E_norm_table = DT::renderDataTable({
        E_norm()
    })
    
    #----------------------------Visualization of Normalized data---------------------------------------------
    
    output$E_norm_boxplot = renderPlot({
        # boxplot(data.frame(log2(E_norm())), 
        #         main = "Normalised Rawdata",
        #         ylab="Expression Intensity",
        #         col = "white",
        #         font =12,
        #         frame = FALSE) 
      data = E_norm() %>% 
        dplyr::select(-spot)
      
      input$collapse_boxplots
      boxplot_function_1_col_array(data,target_names(),E_norm()$spot,target_conditions(),selected_targets(),FALSE,input)
      

    })
    
    output$E_norm_missing_plot = renderPlot({ 
      df = E_norm()
      if(TRUE %in% duplicated(df$spot)){
        df = df %>% 
          dplyr::select(-spot)
      }else{
        df = E_norm() %>% 
          column_to_rownames('spot')
      }
      
      #View(df[df$spot %in% df$spot[duplicated(df$spot)],])
      #colnames(df) = target_names()
      #rownames(df) = spot_names()
      df = as.data.frame(df)
      
      missingness_function(df,targets())
    })
    
    output$E_norm_MA_plot = renderPlotly({
      df = E_norm()
      spots = spots()
      MA_plot_function(df,spots())
    })
    
    output$norm_Heatmap_ui = renderUI({ 
      df = E_norm()
      
      m = as.matrix(df %>% 
                      dplyr::select(-spot))
      
      #m = log_min_function(m,input)
      m = neg_corr_function(m,input)
      m = m[,selected_targets()$Name]
      if(TRUE %in% is.na(m) | TRUE %in% is.infinite(m)){
        span(tags$h5("Negative values cannot be log2 transformed, NA's produced"), style="color:red")
      }else{
      
        # plot_height = 300 + (dim(m)[1]*10)
        #   output$norm_Heatmap = renderPlot({
        #     Heatmap_function(m,target_conditions(),df$spot,input$heatmap_order)
        #   },height = plot_height)
        #   
        #   plotOutput('norm_Heatmap')
          
          if(dim(m)[1] < max_heatmap_rows){
            plot_height = 300 + (dim(m)[1]*10)
            output$E_norm_Heatmap = renderPlot({
              Heatmap_function(m,target_conditions(),df$spot,input$heatmap_order)
            },height = plot_height)
            plotOutput('E_norm_Heatmap',height = plot_height)
          }else{
            output$E_norm_dend = renderPlot({withProgress(message = 'generating dendrogram',{
              dend_function(m,target_conditions())
            })})
            plotOutput('E_norm_dend') 
          }
      }
    
    })
    
    E_norm_CV_df = reactive({
      df = E_norm() 
      targets = targets()
  
      head(df)
      CV_df = CV_df_function(df,targets)
      CV_df
    })
    
    output$E_norm_CV_plot = renderPlot({
      CV_df = E_norm_CV_df() %>%  
        left_join(spots())
      as.tbl(CV_df)
      unique(CV_df$Category)
      q = quantile(CV_df$CV,na.rm = T)
      ggplot(CV_df) + 
        geom_boxplot(aes(y = CV,x = Condition, fill = Category)) + 
        ylim(q[1],q[3])
    })
    
    #---------------------------------Array weights----------------------------------
      arrayw = reactive({
        data = E_norm() %>% dplyr::select(-spot)
        aw = arrayWeights(data)
        names(aw) = colnames(data)
        aw
        

    })
    
    arrayw_df = reactive({
        df = as.data.frame(t(arrayw()))
        rownames(df) = 'Array Weights'
        str(df)
        df
    })
    
    output$arrayw_table = DT::renderDataTable({
       arrayw_df()
    })
    
    output$download_arrayw <- downloadHandler(
        filename = function(){"arrayw.txt"}, 
        content = function(fname){
            write.table(arrayw_df(), fname,sep = '\t', col.names=NA)
        }
    )
    
    output$arrayw_barplot = renderPlot({
        #barplot(arrayw(), xlab="Array", ylab="Weight", col="cornflowerblue", las=2)
        #abline(h=0.5, lwd=1, lty=2)
        
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
           
            p = p + geom_hline(yintercept = input$array_weight_threshold, linetype = 'dashed')# +
            #ggtitle('Weights')
        p = gg_fill_function(p)
        p
        
    
    }) 
    
    #---------------------Condense and clean up data--------------------------------
    
    CV <- function(x) ( 100*(sd(x)/mean(x)))
    
    protein_collapse_function = function(df,spots,input){
      #var <- rlang::parse_quosures(paste(input$drop_col))[[1]])
      #df = E_norm()
      data = df %>% 
        dplyr::select(-spot)
      colnames(data)
      as.tbl(data)
      colnames(data)

      
      df_spots = df  %>% 
        left_join(spots)
      as.tbl(df_spots)
      colnames(df_spots)
      #data$spot_collapse <- gsub("\\.[[:digit:]]*$", "", data$spot)
      df_spots$protein <- gsub("\\.[[:digit:]]*$", "", df_spots[,input$protein_column])
      df_collapse = df_spots %>% 
        dplyr::select(one_of(c('protein',colnames(data))))
      colnames(df_collapse)
      
      data <- as.data.frame(df_collapse) %>% group_by(protein) %>%
        summarise_all(funs(mean))
      

      data
    }
    
    data_full = reactive({   
      data = E_norm()
      protein_collapse_function(E_norm(),spots(),input)
        # #data <- tibble::rownames_to_column(as.data.frame(E_norm()), var = "spot")
        # data = E_norm()
        # data$spot
        # colnames(data)
        # data <- data %>% mutate_at(vars(2:ncol(data %>% dplyr::select(-spot))), as.numeric)
        # #data$Spot
        # data$spot <- gsub("\\.[[:digit:]]*$", "", data$spot)
        # duplicated(data$spot)
        # if(input$spot_collapse == 'mean'){
        #   data <- as.data.frame(data) %>% group_by(spot) %>%
        #     summarise_all(funs(mean))
        # }
        # if(input$spot_collapse == 'median'){
        #   data <- as.data.frame(data) %>% group_by(spot) %>%
        #     summarise_all(funs(mean))
        # }
        # if(input$spot_collapse == 'sum'){
        #   data <- as.data.frame(data) %>% group_by(spot) %>%
        #     summarise_all(funs(sum))
        # }
        # 
        #  
        # #Name all your controls and empty spots to be dropped from the condensed dataset
        # 
        # data <- tibble::column_to_rownames(data, var = "spot")
        # data
    })
    
    
    output$drop_cols_ui = renderUI({
        df = proteins() 
        selectInput('drop_col','Drop by',colnames(df),'Category')
    })
    
    output$drop_rows_ui = renderUI({
        df = proteins()
        selection = unique(df[,input$drop_col])
        #selection = rownames(data_full())
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
    
    data = reactive({ 
      proteins_df = proteins()  
      data = data_full()
      df = protein_filter_function(data_full(),proteins(),input)
      df$protein
      dim(df)
      keep_weight = arrayw_df() %>% 
        gather() %>% 
        filter(value >= input$array_weight_threshold) %>% 
        pull(key)
      keep_weight
      
      if(input$drop_by_weight == TRUE){
        df = df %>% 
          dplyr::select(one_of(c('protein',keep_weight)))
      }
      dim(df)
      df
      #drops <- c("control 1", "control2", "control3")
        #drops = input$drop_row
        # df = proteins()
        # as.tbl(df)
        # drops = df[df[,input$drop_col] == input$drop_row,'spot']
        # drops
        # data = data_full()
        # data <- data[!(row.names(data) %in% drops), ]
        # data
        
        # (var <- rlang::parse_quosures(paste(input$drop_col))[[1]])
        # df = proteins() 
        # as.tbl(df)
        # drops = df %>% 
        #   filter(!!var %in% input$drop_row) %>% 
        #   pull(protein)
        # 
        # drops
        # data = data_full() 
        # data = data %>%  
        #   filter(!rownames(data) %in% drops)
        # 
        # rownames(data)
        # data
    })
    
    #----------------------------Visualization of Condensed dataset-----------------------------------------
    
    output$data_boxplot = renderPlot({
        # boxplot(data.frame(data()), 
        #         main = "Normalized Data",
        #         ylab="Expression Intensity",
        #         col = "white",
        #         font =12,
        #         frame = FALSE)
        # 
        df = data() %>% 
          dplyr::select(-protein)
        # col_names = colnames(df)
        # df$Proteins = rownames(df)
        input$collapse_boxplots
        boxplot_function_1_col_array(df,colnames(df),rownames(df),target_conditions(),selected_targets(),log_rb = FALSE,input)
          
        
        # df_l = df %>% 
        #     gather(Name,`Expression Intensity`,col_names) %>% 
        #     left_join(targets())
        # as.tbl(df_l)
        # p = ggplot(df_l) + 
        #     geom_boxplot(aes(x = Name,y = `Expression Intensity`, col = Condition)) + 
        #     #theme(axis.text.x = element_text(angle = 90)) + 
        #     facet_grid(Group ~ .) + 
        #     xlab('Sample Name') + 
        #     ggtitle('Selected  Data')
        # 
        # p = gg_col_function(p)
        # 
        # p 
    })
    
    output$data_Heatmap_ui = renderUI({
      df = data()  
      as.tbl(df)
      
      
      m = as.matrix(df %>% 
                      dplyr::select(-protein))
      #m[is.na(m)] = 0
      #m[is.infinite(m)] = 0
      m = neg_corr_function(m,input)
      
      (select_cols = intersect(selected_targets()$Name,colnames(m)))
      
      m = m[,select_cols]
      
      # plot_height = 300 + (dim(m)[1]*10)
      # output$data_Heatmap = renderPlot({
      #   Heatmap_function(m,target_conditions(),df$protein,input$heatmap_order)
      # },height = plot_height)
      # 
      # plotOutput('data_Heatmap')
      
      if(dim(m)[1] < max_heatmap_rows){
        plot_height = 300 + (dim(m)[1]*10)
        output$data_Heatmap = renderPlot({
          Heatmap_function(m,target_conditions(),df$protein,input$heatmap_order)
        },height = plot_height)
        plotOutput('data_Heatmap',height = plot_height)
      }else{
        output$data_dend = renderPlot({withProgress(message = 'generating dendrogram',{
          dend_function(m,target_conditions())
        })})
        plotOutput('data_dend') 
      }
      
    })
    
    data_CV_df = reactive({
      df = data() %>% 
        dplyr::rename(spot = protein)
      targets = targets()
      
      head(df)
      CV_df = CV_df_function(df,targets)
      CV_df
    })
    
    output$data_CV_plot = renderPlot({
      CV_df = data_CV_df() %>%  
        dplyr::rename(protein = spot) %>% 
        left_join(proteins())
      as.tbl(CV_df)
      unique(CV_df$Category)
      q = quantile(CV_df$CV,na.rm = T)
      ggplot(CV_df) + 
        geom_boxplot(aes(y = CV,x = Condition, fill = Category)) + 
        ylim(q[1],q[3])
    })
    
    output$data_missing_plot = renderPlot({
      df = data() %>% 
        column_to_rownames('protein')
      
      #colnames(df) = target_names()
      #rownames(df) = spot_names()
      df = as.data.frame(df)
      
      missingness_function(df,targets())
    })
    
    output$data_MA_plot = renderPlotly({
      df = data() %>% 
        column_to_rownames('protein')
      spots = proteins() %>% 
        mutate(spot = protein)
      #duplicated(df$protein)
      #  rownames_to_column('protein')
      MA_plot_function(df,spots)
    })
    
    output$data_table = DT::renderDataTable({
        data()
    })
    
    output$download_data <- downloadHandler(
        filename = function(){"data.txt"}, 
        content = function(fname){
            write.table(data(), fname,sep = '\t', col.names=NA,)
        }
    )
    
    output$data_dim_text = renderPrint({
      print(paste(dim(data())[1],'proteins and ',dim(data())[2],'targets'))
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
    
    threshold = reactive({  
      input$threshold_control_column 
      data = data() %>% 
        column_to_rownames('protein')
      targets = target_conditions()
      
      #threshold_df = threshold_function(data,targets)
      #threshold_df
      threshold_df = tryCatch({threshold_function(data,targets,input)}, error = function(e) {NULL})
      threshold_df
    })
    
    output$threshold_ui = renderUI({ 
      threshold_df = threshold()
      threshold_df
      if(is.null(threshold_df)){
        lst = list(tags$h3('No cutoffs fullfill the conditions for threshold estimation'))
      }else{
        output$threshold_G_df = DT::renderDataTable({
          threshold_df
        })
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
        })
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
      
    })
    
    output$download_threshold_MSnSet <- downloadHandler(
      filename = function(){"threshold_MSnSet.rds"}, 
      content = function(fname){
        saveRDS(threshold_MSnSet(),fname)
      }
    )
    
    # output$download_ExpSet <- downloadHandler(
    #     filename = function(){"ExpSet.rds"}, 
    #     content = function(fname){
    #         saveRDS(exp_set()$ExpressionSet,fname)
    #     }
    # )
    
    

  
  output$threshold_Heatmap_ui = renderUI({ 
    msn_set = threshold_MSnSet() 
    m = exprs(msn_set)
    m
    samples  = msn_set@phenoData@data
    features = msn_set@featureData@data
    m[is.na(m)] = 0
    (select_cols = intersect(selected_targets()$Name,colnames(m)))
    m = m[,select_cols]
    # plot_height = 300 + (dim(m)[1]*10)
    # output$threshold_Heatmap = renderPlot({
    #   Heatmap_function(m,samples,features$protein,input$heatmap_order)
    # },height = plot_height)
    # 
    # plotOutput('threshold_Heatmap')
    
    if(dim(m)[1] < max_heatmap_rows){
      plot_height = 300 + (dim(m)[1]*10)
      output$threshold_Heatmap = renderPlot({
        Heatmap_function(m,samples,features$protein,input$heatmap_order)
      },height = plot_height)
      plotOutput('threshold_Heatmap',height = plot_height)
    }else{
      output$threshold_dend = renderPlot({withProgress(message = 'generating dendrogram',{
        dend_function(m,samples)
      })})
      plotOutput('threshold_dend') 
    }
    
  })


  #### Comparing Methods ####
  
    
  corr = reactive({ 
    E = E_filter_before()
    
    E_corr <-  backgroundCorrect(E, method = "none")
    S_corr <-  backgroundCorrect(E, method = "subtract")
    M_corr <-  backgroundCorrect(E, method = "movingmin")
    N_corr <-  backgroundCorrect(E, method = "normexp")
    
    list(E = E_corr,
         S = S_corr,
         M = M_corr,
         N = N_corr)
  })
  
    
  multi_norm_function = function(corr_data,spot_names,target_names,removed_spots,log_rb,method,proteins,input){
    
    E_norm_list = pre_norm_function(corr_data$E$E,spot_names,target_names,removed_spots,log_rb)
    E_norm = norm_function(E_norm_list$m,method,E_norm_list$spots)
    E_proteins = protein_collapse_function(E_norm,spots(),input)
    E = protein_filter_function(E_proteins,proteins,input)
    
    E = as.data.frame(E) %>% dplyr::select(-one_of('protein'))
    
    S_norm_list = pre_norm_function(corr_data$S$E,spot_names,target_names,removed_spots,log_rb)
    S_norm = norm_function(S_norm_list$m,method,S_norm_list$spots)
    S_proteins = protein_collapse_function(S_norm,spots(),input)
    S = protein_filter_function(S_proteins,proteins,input)

    S = as.data.frame(S) %>% dplyr::select(-one_of('protein'))
    
    N_norm_list = pre_norm_function(corr_data$N$E,spot_names,target_names,removed_spots,log_rb)
    N_norm = norm_function(N_norm_list$m,method,N_norm_list$spots)
    N_proteins = protein_collapse_function(N_norm,spots(),input)
    N = protein_filter_function(N_proteins,proteins,input)

    N = as.data.frame(N) %>% dplyr::select(-one_of('protein'))
    
    M_norm_list = pre_norm_function(corr_data$M$E,spot_names,target_names,removed_spots,log_rb)
    M_norm = norm_function(M_norm_list$m,method,M_norm_list$spots)
    M_proteins = protein_collapse_function(M_norm,spots(),input)
    M = protein_filter_function(M_proteins,proteins,input)

    M = as.data.frame(M) %>% dplyr::select(-one_of('protein'))

    
    list(E = E,
         S = S,
         M = M,
         N = N)
  }
  norm = reactive({
    corr_data = corr()    

    spot_names = spot_names()
    target_names = target_names()
    removed_spots = removed_spots()
    log_rb = input$log_rb
    proteins = proteins()
    
    
    method = "none"
    E = multi_norm_function(corr_data,spot_names(),target_names(),removed_spots(),input$log_rb,method,proteins(),input)
    
    method = "quantile"
    Q = multi_norm_function(corr_data,spot_names(),target_names(),removed_spots(),input$log_rb,method,proteins(),input)
    method = "cyclicloess"
    C = multi_norm_function(corr_data,spot_names(),target_names(),removed_spots(),input$log_rb,method,proteins(),input)
    method = "scale"
    S = multi_norm_function(corr_data,spot_names(),target_names(),removed_spots(),input$log_rb,method,proteins(),input)
    
    
    
    
    list(E = E, 
         Q = Q,
         C = C,
         S = S)
    
  })

  multi_fit_function = function(norm){
    
    Rfit <- lmFit(norm$E)
    Rfit2 <- eBayes(Rfit)
    Rfit2 <- as.data.frame(Rfit2)
    
    Sfit <- lmFit(norm$S)
    Sfit2 <- eBayes(Sfit)
    Sfit2 <- as.data.frame(Sfit2)
    
    Mfit <- lmFit(norm$M)
    Mfit2 <- eBayes(Mfit)
    Mfit2 <- as.data.frame(Mfit2)
    
    Nfit <- lmFit(norm$N)
    Nfit2 <- eBayes(Nfit)
    Nfit2 <- as.data.frame(Nfit2)
    
    list(E = Rfit2,
         S = Rfit2,
         M = Mfit2,
         N = Nfit2)
  }
   
  Rfit = reactive({
    list(E = multi_fit_function(norm()$E),
         Q = multi_fit_function(norm()$Q),
         C = multi_fit_function(norm()$C),
         S = multi_fit_function(norm()$S)
    )
    
  })
  

  MA_plots = renderPlot({
    df = Rfit()$E$E
    as.tbl(df)  
  })
  
  multi_Amean_function = function(fit,norm){
    Amean <- cbind(fit$E$Amean, fit$S$Amean, fit$M$Amean, fit$N$Amean)
    dim(Amean)
    colnames(Amean) <- c("Rawdata","Subtraction","Movingminimum","Normexp")
    as.tbl(as.data.frame(Amean))
    Amean_l = Amean %>% 
      as.data.frame() %>% 
      gather(Correction,Amean)
    
    Amean_l$Amean[is.infinite(Amean_l$Amean)] = NA
    Amean_l$Normalisation = norm
    Amean_l
  }
  
  Amean_data = reactive({
     
    E_Amean = multi_Amean_function(Rfit()$E,"None")  
    S_Amean = multi_Amean_function(Rfit()$S,"Scale")
    Q_Amean = multi_Amean_function(Rfit()$Q,"Quantile")
    C_Amean = multi_Amean_function(Rfit()$C,"Cyclicloess")
    
    Amean = rbind(E_Amean,S_Amean) %>% 
      rbind(Q_Amean) %>% 
      rbind(C_Amean)
    
    as.tbl(as.data.frame(Amean))
    Amean$Correction = factor(Amean$Correction, levels = unique(Amean$Correction))
    Amean$Normalisation = factor(Amean$Normalisation, levels = unique(Amean$Normalisation))
    Amean
    #Amean$A
    
  })
  
  
  output$MA_correction_ui = renderUI({
    df = Amean_data()
    selectInput('MA_correction','Background Correction',unique(df$Correction),unique(df$Correction),multiple = T)
  })
  
  output$MA_normalisation_ui = renderUI({
    df = Amean_data()
    selectInput('MA_normalisation','Normalisation',unique(df$Normalisation),unique(df$Normalisation),multiple = T)
  })
  
  
  
  
  output$Amean_plot_ui = renderUI({
      plot_height = single_plot_height * length(input$MA_normalisation)
      output$Amean_plot = renderPlot({
    
          
          Amean = Amean_data() %>% 
            filter(Correction %in% input$MA_correction, 
                   Normalisation %in% input$MA_normalisation)
          
          
      
      
            p = ggplot(Amean, aes(x= Correction, y=Amean, fill=Correction))+
              geom_boxplot()+
              #theme_classic()+
              theme(axis.text = element_text(size = 12), axis.title.x = element_blank(), legend.position = "none") + 
              facet_grid(Normalisation ~ .)
            q = quantile(Amean$Amean,na.rm = T)
            if(input$MA_quantile == T){
              p = p + 
                ylim(q[1],q[3])
            }
            p
          
        },height = plot_height)
      
      plotOutput('Amean_plot',height = plot_height)
  })
  
  multi_M_function = function(data,norm){
    Rfit2 = data$E
    Sfit2 = data$S
    Mfit2 = data$M
    Nfit2 = data$N
    
    M <- cbind(Rfit2$F, Sfit2$F, Mfit2$F, Nfit2$F)
    M <- as.data.frame(M)
    colnames(M) = c("Rawdata","Subtraction","Movingminimum","Normexp")
    #M = log2(M)
    M_l = M %>% 
      gather('Correction','M')
    M_l$Normalisation = norm
    M_l
  }
  
  M_plot_data = reactive({
    E_M_l = multi_M_function(Rfit()$E,"None")
    S_M_l = multi_M_function(Rfit()$S,"Scale")
    Q_M_l = multi_M_function(Rfit()$Q,"Quantile")
    C_M_l = multi_M_function(Rfit()$C,"Cyclicloess")
    
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
      
    
      M_l = M_plot_data()  %>% 
        filter(Correction %in% input$MA_correction, 
               Normalisation %in% input$MA_normalisation)
      if(input$log_rb_M == TRUE){
        p = ggplot(M_l, aes(x= Correction, y=log(M), fill=Correction))+
          geom_boxplot()+
          #theme_classic()+
          theme(axis.text = element_text(size = 12), axis.title.x = element_blank(), legend.position = "none") +
          facet_grid(Normalisation ~ .)
        q = quantile(log(M_l$M),na.rm = T)
        if(input$MA_quantile == T){
          p = p + 
            ylim(q[1],q[3])
        }
      }else{
        p = ggplot(M_l, aes(x= Correction, y=M, fill=Correction))+
          geom_boxplot()+
          #theme_classic()+
          theme(axis.text = element_text(size = 12), axis.title.x = element_blank(), legend.position = "none") +
          facet_grid(Normalisation ~ .)
        q = quantile(M_l$M,na.rm = T)
        if(input$MA_quantile == T){
          p = p + 
            ylim(q[1],q[3])
        }
      }
      p
    },height = plot_height)
  
    plotOutput('M_plot',height = plot_height)
  })
  
 
MA_data = reactive({
  A = Amean_data()
  M = M_plot_data()
  
  dim(A)
  dim(M)
  
  MA = A
  MA$M = M$M
  as.tbl(MA)
  
  MA
})


  output$MA_plot_ui = renderUI({
    plot_height = single_plot_height * length(input$MA_normalisation)
    output$MA_plot = renderPlot({
      MA = MA_data() %>% 
        filter(Correction %in% input$MA_correction, 
               Normalisation %in% input$MA_normalisation)
      as.tbl(MA)
      if(input$log_rb_M == TRUE){
      
        ggplot(MA) + 
          geom_point(aes(x = Amean, y = log(M))) + 
          facet_grid(Normalisation ~ Correction)
      }else{
        ggplot(MA) + 
          geom_point(aes(x = Amean, y = M)) + 
          facet_grid(Normalisation ~ Correction)
      }
    },height = plot_height)
  
    plotOutput('MA_plot',height = plot_height)
  })
    
  
  multi_precision_function = function(data,norm){
    #data = E_fit()
    Rfit2 = data$E
    Sfit2 = data$S
    Mfit2 = data$M
    Nfit2 = data$N
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
    
    colnames(Precision_all) <- c("Amean","Rawdata","Subtraction","Moving minimum","Normexp")
    
    Precision_all <- as.data.frame(Precision_all)
    Precision_melt <- reshape2::melt(Precision_all, id.vars="Amean")
    Precision_melt <- cbind(Precision$Raw_A, Precision_melt)
    colnames(Precision_melt) <- c("N", "Amean", "Background_correction","Variance")
    
    as.tbl(Precision_melt)
    Precision_melt$Normalisation = norm 
    Precision_melt
  }
  
  precision_data = reactive({
    E_Precision_melt = multi_precision_function(Rfit()$E,"None")
    S_Precision_melt = multi_precision_function(Rfit()$S,"Scale")
    Q_Precision_melt = multi_precision_function(Rfit()$Q,"Quantile")
    C_Precision_melt = multi_precision_function(Rfit()$C,"Cyclicloess")
    
    Precision_melt = rbind(E_Precision_melt) %>% 
      rbind(S_Precision_melt) %>% 
      rbind(Q_Precision_melt) %>% 
      rbind(C_Precision_melt)
    Precision_melt
    
    Precision_melt$Background_correction = factor(Precision_melt$Background_correction, levels = unique(Precision_melt$Background_correction))
    Precision_melt$Normalisation = factor(Precision_melt$Normalisation, levels = unique(Precision_melt$Normalisation))
    
    Precision_melt
  })
  
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
  
  eBayes_test = reactive({ 
    df = data() %>% column_to_rownames('protein') 
    
    (selected_cols = intersect(selected_targets()$Name,colnames(df)))
    
    df = df[,selected_cols]

    cont_matrix_list = cont_matrix_function(df,selected_targets())
    design = cont_matrix_list$design
    cont.matrix = cont_matrix_list$cont.matrix
    
    fit = lmFit(df,design, weights = as.numeric(arrayw_df()[,selected_cols]))
    fit


    fit2 = contrasts.fit(fit,cont.matrix)
    fit2 = eBayes(fit2)
    fit2
    
    Sig_Proteins <- topTable(fit2, adjust.method = input$mtc, number =30)
    threshold <- Sig_Proteins$adj.P.Val < 0.05 
    length(which(threshold))
    Sig_Proteins <- cbind(Sig_Proteins, threshold)
    Sig_Proteins
    
    list(df = Sig_Proteins,cont_matrix = cont_matrix_list$cmd)
    })
  
  output$cont_matrix_text = renderText({
    eBayes_test()$cont_matrix
  })
  
  output$eBays_table = DT::renderDataTable({
    eBayes_test()$df
  })
  
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
    plot_height = 300 + (dim(m)[1]*10)
    # output$eBayes_Heatmap = renderPlot({
    #   Heatmap_function(m,target_conditions(),rownames(m))
    # },height = plot_height)
    
    if(dim(m)[1] < max_heatmap_rows){
      plot_height = 300 + (dim(m)[1]*10)
      output$eBayes_Heatmap = renderPlot({
        Heatmap_function(m,target_conditions(),rownames(m))
      },height = plot_height)
      plotOutput('eBayes_Heatmap',height = plot_height)
    }else{
      output$eBayes_dend = renderPlot({withProgress(message = 'generating dendrogram',{
        dend_function(m,target_conditions())
      })})
      plotOutput('eBayes_dend') 
    }
    
    #plotOutput('eBayes_Heatmap')
    
  })
  
  
  #targets()
  
  #E()$targets
  
})
