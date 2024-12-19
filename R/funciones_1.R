# to_days <- function(df_h,fun = sum){
#   #dates <- df_h[[1]]
#   colnames(df_h)[1] <- 'dates'
#   df_h %>% mutate(dates = as.POSIXct(format(dates,'%Y-%m-%d'))) %>% 
#     group_by(dates) %>% summarise_all(fun) %>% return()
# }
to_hours <- function(df_h,funs = sum,complete = F,...){
  to_aggregate(df_h, funs = funs, new_step = 'hours', complete = complete,...)
}

to_days <- function(df_h,funs = sum,complete = F,...){
  to_aggregate(df_h, funs = funs, new_step = 'days', complete = complete,...)
}

to_months <- function(df_h,funs = sum,complete = F,...){
  to_aggregate(df_h, funs = funs, new_step = 'month', complete = complete,...)
}

to_years <- function(df_h,funs = sum,complete = F,...){
  to_aggregate(df_h, funs = funs, new_step = 'years', complete = complete,...)
}

to_regime <- function(df_m,funs = mean,format_time = '2000-%m-01',...){
  to_aggregate(df_m, funs = funs, format_time = format_time,...)
}


## additional arguments will be passed to all funs
to_aggregate <- function(df_h,funs = sum,format_time = NULL, new_step = NULL,complete = F,...){
  
  ### evaluate new time step
  if(is.null(format_time)){
    if(new_step %in% c('hour','hours'))  format_time <- '%Y-%m-%d %H:00:00' 
    else if(new_step %in% c('day','days'))  format_time <- '%Y-%m-%d'  # recommended only 2 options (for seq.posixct)
    else if(new_step %in% c('month','months'))   format_time <- '%Y-%m-01'
    else if(new_step %in% c('year','years'))   format_time <- '%Y-01-01'
  }
  
  #dates <- df_h[[1]]
  colnames(df_h)[1] <- 'dates'
  names_0 <- names(df_h)
  # if (length(funs) == 1) funs = rep(funs,length(df_h)-1)   # recycling funciones
  if (length(funs) == 1) funs = lapply(2:length(df_h),function(i) funs)   # recycling funciones
  
  ###
  names_funs <- funs %>% as.character()
  nums_funs <- as.numeric(factor(names_funs))
  ###
  
  df_h <- df_h %>% mutate(dates = as.POSIXct(format(dates,format_time))) %>%    # TO CHANGE for a gen fun
    group_by(dates) 
  
  df_h <- lapply(1:max(nums_funs),function(k){
    i_val <- which(nums_funs == k)
    fun <- funs[[i_val[1]]]
    df_h %>% select(c(1,1+i_val)) %>% summarise_all(fun,...) 
  }) %>% 
    Reduce(function(x,y) cbind(x,y[,-1]),.) %>% select(names_0)
  
  # df_h <- df_h %>% mutate(dates = as.POSIXct(format(dates,'%Y-%m-01'))) %>%    # TO CHANGE for a gen fun
  #   group_by(dates) %>% summarise_all(fun) 

  if (complete & !is.null(new_step)) {   
    df_h <- data.frame(dates = seq.POSIXt(min(df_h$dates),max(df_h$dates),new_step)) %>%  # TO CHANGE for a gen fun
      left_join(df_h) 
  }
  
  return(df_h)
}

### CREATe function complete
# time_step : 'hour','hours','day', 'days','month', 'months'   see ?seq.POSIXt
complete_dates <- function(df_h,time_step = NULL,fill_strip = NULL){
  if(is.null(time_step))   time_step <- abs(df_h[[1]][2] - df_h[[1]][1])
  nombre <- names(df_h)[1]
  if(is.null(fill_strip)){
    date_0 <- min(df_h[[1]],na.rm = T)
    date_f <- max(df_h[[1]],na.rm = T)
  } else {
    date_0 <- min(df_h[[1]],na.rm = T) %>% first_date_period(period = fill_strip)
    date_f <- max(df_h[[1]],na.rm = T) %>% last_date_period(perio = fill_strip)
  }

  tibble(!!nombre := seq.POSIXt(date_0,date_f, time_step)) %>%  
      left_join(df_h) 
  
}

# merge to_days and to_months   only changes '%Y-%m-01'
### TO CHANGE 'days' for a more general function



# summarise_at_fun <- function(variable, func, data){
#   data2 <- data %>%
#     summarise_at(vars(variable), funs(get(func)(.)))
#   return(data2)
# }



tables_daily_by_year <- function(df_h, fun = sum,path_file = NULL,new_dir = NULL,...){
  if (!is.null(path_file)) paste0(path_file,'/')
  if (!is.null(new_dir)){
    shell(paste0('mkdir ',paste0(path_file,new_dir)))
    path_file <- paste0(path_file,new_dir,'/')
  } 
  
  # to days
  colnames(df_h)[1] <- 'dates'
  df_h <- df_h %>% mutate(dates = as.POSIXct(format(dates,'%Y-%m-%d'))) %>% 
    group_by(dates) %>% summarise_all(fun,...)
  #
  
  df_h <- data.frame(dates = seq.POSIXt(min(df_h$dates),max(df_h$dates),'days')) %>%
    left_join(df_h)    ### TO CHANGE 'days' for a more general function
  
  # add variables year, month and day to df
  df_h <- df_h %>% mutate(year = as.numeric(format(dates,'%Y'))) %>% 
    mutate(month = as.numeric(format(dates,'%m'))) %>% 
    mutate(day = as.numeric(format(dates,'%d')))
  
  # table_0 <- matrix(nrow = 31,ncol = 12)
  # colnames(table_0) <- month.abb
  # rownames(table_0) <- 1:31
  
  df_h %>% group_by(year) %>% group_split() %>% 
  sapply(function(df){
    table <- spread(df %>% select(-c('dates','year')) ,month,value = Rain)
    colnames(table)[-1] <- month.abb[colnames(table)[-1] %>% as.numeric]   # delete [...] if you complete de absent intermediate dates
    write.csv(table,file = paste0(path_file,df$year[1],'.csv'),row.names = F)
    T
  })
    
}

tables_daily_mul <- function(df_h, file, path = NULL,begin_row = 1, complete = T,...){
  require(dplyr)
  require(tidyr)
  if (!is.null(path)) paste0(path,'/')
  file <- paste0(path,file)
  
  colnames(df_h)[1] <- 'dates'
  if (complete) {
    df_h <- data.frame(dates = seq.POSIXt(min(df_h$dates),max(df_h$dates),'days')) %>%
      left_join(df_h)    ### TO CHANGE 'days' for a more general function
  }
  
  list_df_h <- df_h %>% mutate(year = as.numeric(format(dates,'%Y'))) %>% 
    mutate(month = as.numeric(format(dates,'%m'))) %>% 
    mutate(day = as.numeric(format(dates,'%d'))) %>% 
    gather('station','var', -c('dates','year','month','day')) %>% 
    group_by(station) %>% named_group_split(F)
  # names(list_df_h) <- sapply(list_df_h, function(x) x[['station']][1])
  
  list_df_h <- list_df_h %>% lapply(function(df_h_0){
    list_years <- df_h_0 %>% select(-dates) %>% 
      group_by(year) %>% spread(month,var) %>% #select(-c('station')) %>% # group_split(.keep = F) %>%
      rename_at(as.character(1:12), ~ month.abb) %>%
      named_group_split(.keep = F)
    
    # names(list_years) <- list_years %>% sapply(function(x) x[['year']][1])
    # df %>%
    #   group_split(Cluster) %>%
    #   setNames(sort(unique(df$Cluster)))
  })
  
  #### WRITE TO EXCEL
  wb <- openxlsx::createWorkbook("data_completada")
  style_header <- openxlsx::createStyle(fgFill = "#EE7710",fontColour = '#FFFFFF',halign = 'center',
                                        border = c("top", "bottom", "left", "right"),fontName = 'Arial',
                                        textDecoration = 'bold')
  style_cell_table <- openxlsx::createStyle(fontName = 'Arial',numFmt = '0.0',fontSize = 10,halign = 'center',
                                            border = c("top", "bottom", "left", "right"))
  style_cell_integer <- openxlsx::createStyle(fontName = 'Arial',numFmt = '0',fontSize = 10,halign = 'center',
                                              border = c("top", "bottom", "left", "right"))
  style_completed <- openxlsx::createStyle(fgFill = "#9BC2E6", fontName = 'Arial',fontSize = 18,halign = 'center',
                                           border = c("top", "bottom", "left", "right"))
  style_title <- openxlsx::createStyle(fontName = 'Arial',fontSize = 14,halign = 'center',
                                       textDecoration = 'bold')
  
  for(name_station in names(list_df_h)){
    list_station <- list_df_h[[name_station]]
    list_station %>% names() %>%lapply(function(year){
      table <- list_station[[year]]
      name_0 <- paste(name_station,year,sep = '-')
      openxlsx::addWorksheet(wb, name_0, gridLines = FALSE)
      openxlsx::writeData(wb, sheet = name_0, 
                          table,startRow = begin_row, rowNames = FALSE,
                          keepNA = TRUE,
                          na.string = 'S/D',headerStyle = style_header)   ## adding table
      openxlsx::addStyle(wb,name_0,style_cell_table,
                         rows = begin_row + 1:(nrow(table)), #%>% # cause header row
                           # c( nrow(table) + 1:nrow(sumario)),   
                         cols = 1:ncol(table),gridExpand = T)
      openxlsx::conditionalFormatting(wb,name_0,cols = 1:ncol(table),
                                      rows = begin_row + 1:(nrow(table)),
                                      type = 'contains',rule = 'S/D')

    })
  }
  openxlsx::saveWorkbook(wb, file,overwrite = TRUE)
  
  T
}


named_group_split <- function(.tbl, .keep = T,...) {
  # grouped <- group_by(.tbl, ...)
  grouped <- .tbl
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split(.keep = .keep,...) %>% 
    rlang::set_names(names)
}


to_p24_day <- function(df_h){
  df_h <- to_days(df_h)
  df_h %>% mutate(dates = as.POSIXct(format(dates,'%Y-%m-01'))) %>% 
    group_by(dates) %>% summarise_all(max)
}


# df_h is monthly
tables_monthly <- function(df_h,path_file = NULL,name = NULL,complete = T){
  require(tidyr)
  if (!is.null(path_file)) paste0(path_file,'/')
  # if (!is.null(new_dir)){
  #   shell(paste0('mkdir ',paste0(path_file,new_dir)))
  #   path_file <- paste0(path_file,new_dir,'/')
  # } 
  if (complete) {
    df_h <- data.frame(dates = seq.POSIXt(min(df_h$dates),max(df_h$dates),'months')) %>%
      left_join(df_h)    ### TO CHANGE 'days' for a more general function
  }
  
  df_h <- df_h %>% mutate(year = as.numeric(format(dates,'%Y'))) %>% 
    mutate(month = as.numeric(format(dates,'%m'))) # %>% 
    # mutate(day = as.numeric(format(dates,'%d')))
  
  table <- spread(df_h %>% select(-1) ,month,value = 1)
  colnames(table)[-1] <- month.abb
  write.csv(table,file = paste0(path_file,name,'.csv'),row.names = F)
  return()
}





# df_h is monthly
# df_h_0 is incomplete
tables_monthly_mul <- function(df_h,df_0,path_file = NULL,name,complete = T,begin_row = 1){
  if (!is.null(path_file)) paste0(path_file,'/')
  # if (!is.null(new_dir)){
  #   shell(paste0('mkdir ',paste0(path_file,new_dir)))
  #   path_file <- paste0(path_file,new_dir,'/')
  # } 
  if (complete) {
    df_h <- data.frame(dates = seq.POSIXt(min(df_h$dates),max(df_h$dates),'months')) %>%
      left_join(df_h)    ### TO CHANGE 'days' for a more general function
  }
  
  name_0 <- names(df_h)[2]
  df_h <- df_h %>% mutate(year = as.numeric(format(dates,'%Y'))) %>% 
    mutate(month = as.numeric(format(dates,'%m'))) # %>% 
  df_0 <- df_0 %>% mutate(year = as.numeric(format(dates,'%Y'))) %>% 
    mutate(month = as.numeric(format(dates,'%m'))) # %>% 
  # mutate(day = as.numeric(format(dates,'%d')))
  
  table <- spread(df_h %>% select(-1) ,month,value = 1)
  table_0 <- spread(df_0 %>% select(-1) ,month,value = 1)
  colnames(table)[-1] <- month.abb
  colnames(table_0)[-1] <- month.abb
  
  mat_na <- is.na(table_0)
  dimnames(mat_na) <- NULL
  df_na <- reshape2::melt(mat_na) %>% rename(row = Var1,col = Var2) %>% 
    filter(value)
  
  #### WRITE TO EXCEL
  wb <- openxlsx::createWorkbook("data_completada")
  openxlsx::addWorksheet(wb, name_0, gridLines = FALSE)
  openxlsx::writeData(wb, sheet = 1, table,startRow = begin_row, rowNames = FALSE,
                      na.string = 'S/D')
  style_cell_completed <- openxlsx::createStyle(fgFill = "#9BC2E6")
  openxlsx::addStyle(wb,name_0,style_cell_completed,
                     rows = begin_row - 1 + 1 + df_na$row,cols = df_na$col)
  
  openxlsx::saveWorkbook(wb, paste0(path_file,name_0,'.xlsx'),overwrite = TRUE)
  # write.csv(table,file = paste0(path_file,name,'.csv'),row.names = F)
  return()
}

# df_h is monthly
# df_h_0 is incomplete
tables_monthly_mul_2 <- function(df_h,df_0 = NULL,file, path = NULL,funs_anual = sum, complete = T,
                                 begin_row = 1, type_style = 'completed',
                                 indexes = NULL,names_sheets = 'names',resume_table = T,
                                 title_anexo = NULL,subtitle_var = NULL,text_subtitle2 = NULL, 
                                 add_name_df = T,add_years = F,
                                 df_info_stas = NULL, replace_subtitle_var = T ,...){
  require(dplyr)
  require(tidyr)
  if (!is.null(path)) paste0(path,'/')
  file <- paste0(path,file)
  if(length(subtitle_var)==1) subtitle_var <- rep(subtitle_var,ncol(df_h)-1)
  # else if (!is.null(names(subtitle_var)))
    
  # if (!is.null(new_dir)){
  #   shell(paste0('mkdir ',paste0(path_file,new_dir)))
  #   path_file <- paste0(path_file,new_dir,'/')
  # } 
  if (length(funs_anual) == 1) {
    funs_anual = lapply(2:length(df_h),function(i) funs_anual)   # recycling funciones
  }
  
  if (complete) {
    df_h <- data.frame(dates = seq.POSIXt(min(df_h$dates),max(df_h$dates),'months')) %>%
      left_join(df_h)    ### TO CHANGE 'days' for a more general function
  }
  if (is.null(indexes)) indexes <- 1:(ncol(df_h)-1)

  # name_0 <- names(df_h)[2]
  names_df_h <- names(df_h)[-1]
  
  if(names_sheets == 'names') names_sheets <- names_df_h 
  else if(names_sheets == 'indexes') names_sheets <- as.character(indexes)
  else stop('names_sheets should be "names" or "indexes"')
  
  
  if (!is.null(df_info_stas)){
    if (length(df_info_stas) == 1){
      df_info_stas <- rep(df_info_stas,length(df_h) - 1) %>% do.call(cbind,.) %>% as.data.frame()
    } 
    if(replace_subtitle_var) df_info_stas[2,] <- subtitle_var
  }
  
  ## this if divide df in numeric and no numeric variables( i.e. wind dir)
  if (any(!sapply(df_h[,-1],is.numeric))){
    list_groups <- list(df_h[,c(T,sapply(df_h[,-1],is.numeric))],
                        df_h[,c(T,!sapply(df_h[,-1],is.numeric))])
  } else list_groups <- list(df_h)
  
  list_table <- lapply(list_groups,function(df_h){     # var df_h is used in order to not change the following code { }
    list_df_h <- df_h %>% gather('station','var',-1) %>% 
      mutate(year = as.numeric(format(dates,'%Y'))) %>% 
      mutate(month = as.numeric(format(dates,'%m'))) %>% 
      mutate(station = factor(station,levels = names_df_h))  %>% ### to avoid desorder  # added
      group_by(station)
    list_table <- spread(list_df_h %>% dplyr::select(-dates) ,month,value = 2) %>%
      named_group_split() %>% as.list()   # list of tibbles wasnt the same as a list
  }) %>% do.call(c,.)

  list_table <- list_table %>% mapply(.,funs_anual,SIMPLIFY = F,FUN = function(table,fun_anual) {
    table <- table[,-1]
    colnames(table)[-1] <- month.abb    # to change months names
    colnames(table)[1] <- "A\U00f1o"         # classic table
    
    if(is.numeric(table[[2]][1])) {
      table$Anual <- apply(table[,2:13],1,fun_anual,...)
    }
    i_del <- rowSums(!is.na(table[,2:13])) == 0
    table <- table[!i_del,] 
    table       
  })  %>% .[names_df_h]      # 29.05.21
  # names(list_table) <- names_df_h
  
  # for(table in list_table){
  #   colnames(table)[-1] <- month.abb
  # } 
  
  if(!is.null(df_0)){
    
    list_df_0 <- df_0 %>% gather('station','var',-1) %>% mutate(var = is.na(var)) %>% # quit last to not  transform here
      mutate(year = as.numeric(format(dates,'%Y'))) %>% 
      mutate(month = as.numeric(format(dates,'%m'))) %>% 
      mutate(station = factor(station,levels = names_df_h)) %>%   ### to avoid desorder  # added
      group_by(station)
    list_table_0 <- spread(list_df_0 %>% dplyr::select(-dates) ,month,value = 2) %>%
      named_group_split() %>% as.list()
    list_table_0 <- list_table_0 %>% mapply(., list_table,SIMPLIFY = F,FUN = function(table_0,table) {
      valid_years <- table[[1]]
      
      table_0 <- table_0[,-1]
      colnames(table_0)[-1] <- month.abb
      
      table_0 <- table_0 %>% filter(year %in% valid_years)
      
      mat_na <- table_0 %>% mutate(year = FALSE) %>% as.matrix  #
      # mat_na <- is.na(table_0)                                ## to consider completed strips (cantos)
      dimnames(mat_na) <- NULL
      df_na <- reshape2::melt(mat_na) %>% rename(row = Var1,col = Var2) %>% 
        filter(value)
    })  # %>% .[names_df_h]    # 29.05.21
    # names(list_table_0) <- names_df_h
    
    # df_0 <- df_0 %>% mutate(year = as.numeric(format(dates,'%Y'))) %>% 
    #   mutate(month = as.numeric(format(dates,'%m'))) # %>% 
    # # mutate(day = as.numeric(format(dates,'%d')))
    # table_0 <- spread(df_0 %>% select(-1) ,month,value = 1)
    # colnames(table_0)[-1] <- month.abb
    # mat_na <- is.na(table_0)
    # dimnames(mat_na) <- NULL
    # df_na <- reshape2::melt(mat_na) %>% rename(row = Var1,col = Var2) %>% 
    #   filter(value)
  }

  
  # to use for summarizing
  # list_df_h <- list_df_h %>% group_split()
  
  
  
  #### MATRIX INFO STAS ####
  if(!is.null(df_info_stas)){
    names_info <- c('Estaci\U00F3n','Par\U00E0metro','Norte','Este',
                    'Altitud','Dpto','Prov','Dist') %>%
      paste(' :')
    matrix_info <- matrix(nrow = 3,ncol = 13)
    matrix_info[1:2,1] <- names_info[1:2]
    matrix_info[,8] <- names_info[3:5]
    matrix_info[,12] <- names_info[6:8]
    matrix_info[,10] <- c('m','m','msnm')
  }
  
  
  
  #### WRITE TO EXCEL ####
  wb <- openxlsx::createWorkbook("data_completada")
  
  # style_rounding_table <- openxlsx::createStyle(numFmt = '0.0')
  style_header <- openxlsx::createStyle(fgFill = "#EE7710",fontColour = '#FFFFFF',halign = 'center',
                                        border = c("top", "bottom", "left", "right"),fontName = 'Arial',
                                        textDecoration = 'bold')
  style_cell_table <- openxlsx::createStyle(fontName = 'Arial',numFmt = '0.0',fontSize = 10,halign = 'center',
                                            border = c("top", "bottom", "left", "right"))
  style_cell_integer <- openxlsx::createStyle(fontName = 'Arial',numFmt = '0',fontSize = 10,halign = 'center',
                                            border = c("top", "bottom", "left", "right"))
  style_completed <- openxlsx::createStyle(fgFill = "#9BC2E6", fontName = 'Arial',fontSize = 10,halign = 'center',
                                           border = c("top", "bottom", "left", "right"), numFmt = '0.0')
  style_incomplete <- openxlsx::createStyle(fgFill = "#C6EFCE", fontColour = '#006100', numFmt = '0.0', 
                                            fontName = 'Arial',fontSize = 10,halign = 'center',
                                           border = c("top", "bottom", "left", "right"))
  style_title <- openxlsx::createStyle(fontName = 'Arial',fontSize = 14,halign = 'center',
                                       textDecoration = 'bold')
  
  if(resume_table) openxlsx::addWorksheet(wb, '\U00EDndice', gridLines = FALSE)
  
  lapply(2:ncol(df_h),function(i){
    name_0 <- names_sheets[i - 1]
    table <- list_table[[i - 1]]
    # df_h_0 <- list_df_h[[i - 1]]
    # sumario <- df_h_0 %>% group_by(month) %>%
    #                       summarise('N? datos' = sum(!is.na(var)),
    #                                 '3er Cuartil' = quantile(var,0.75,na.rm = T),
    #                                 Mediana = median(var),
    #                                 '1er Cuartil' = quantile(var,0.25, na.rm = T),
    #                                 'M?ximo' = max(var,na.rm = T),
    #                                 Promedio = mean(var,na.rm = T),
    #                                 'M?nimo' = min(var,na.rm = T),
    #                                 'Desv. Est.' = sd(var,na.rm = T),
    #                                 'Coef. Asi.' = moments::skewness(var,na.rm = T),
    #                                 'Coef. Var.' = Promedio/get('Desv. Est.')
    
    #                                 )
    # if(is.numeric(table[[2]][1])) 
    sumario <- summarise_table(table)
    
    openxlsx::addWorksheet(wb, name_0, gridLines = FALSE)
    openxlsx::writeData(wb, sheet = name_0, table,startRow = begin_row, rowNames = FALSE,
                        keepNA = TRUE,
                        na.string = 'S/D',headerStyle = style_header)   ## adding table
    # if(is.numeric(table[[2]][1])){
      openxlsx::writeData(wb, sheet = name_0, sumario,startRow = begin_row + nrow(table) + 1, 
                          rowNames = TRUE,colNames = FALSE, keepNA = TRUE,
                          na.string = 'S/D')   ### adding sumario
    # }
    openxlsx::addStyle(wb,name_0,style_cell_table,
                       rows = begin_row + 1:(nrow(table)) %>% # cause header row
                         c( nrow(table) + 1:nrow(sumario)),   
                       cols = 1:ncol(table),gridExpand = T)
    openxlsx::addStyle(wb,name_0,style_cell_integer,
                       rows = begin_row + 1:(nrow(table)),  
                       cols = 1,gridExpand = T)
    openxlsx::addStyle(wb,name_0,style_cell_integer,
                       rows = begin_row + nrow(table) + 1,  
                       cols = 2:ncol(table),gridExpand = T)
    # openxlsx::addStyle(wb,name_0,style_border,
    #                    rows = begin_row + 1:(nrow(table)),   # cause header row
    #                    cols = 1:(ncol(table)+1),gridExpand = T)
    # rounding style
    openxlsx::setRowHeights(wb,name_0,rows =  nrow(table) + nrow(sumario),
                            heights = 14.4)
    # openxlsx::setColWidths(wb,name_0,cols = 1,
    #                         widths = 8.89)     # talvez borrar
    openxlsx::setColWidths(wb,name_0,cols = 2:14 ,widths = 6)
    
    openxlsx::conditionalFormatting(wb,name_0,cols = 1:ncol(table),
                                    rows = begin_row + 1:(nrow(table)),
                                    type = 'contains',rule = 'S/D',
                                    style = openxlsx::createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C"))
    
    if(!is.null(df_0)){
      if (type_style == 'completed') style_colored <- style_completed
      else  if(type_style == 'incomplete') style_colored <- style_incomplete
      df_na <- list_table_0[[i - 1]]
      openxlsx::addStyle(wb,name_0,style_colored,
                         rows = begin_row - 1 + 1 + df_na$row,cols = df_na$col)
    }
    
    if(!is.null(df_info_stas)){
      matrix_info[1:2,2] <- df_info_stas[[i-1]][1:2]
      matrix_info[,9] <- df_info_stas[[i-1]][3:5]
      matrix_info[,13] <- df_info_stas[[i-1]][6:8]
      openxlsx::writeData(wb,name_0,matrix_info,startRow = 5,colNames = F)
    }
    
    #### TO ADD TTILES SUBTITLES ####
    vale <- F
    if(!is.null(title_anexo)){
      openxlsx::writeData(wb,name_0,paste0(title_anexo,indexes[i - 1]),startRow = 1)
      openxlsx::mergeCells(wb,name_0,rows = 1,cols = 1:ncol(table))
      vale <- T
    }
    if(!is.null(subtitle_var)){
      openxlsx::writeData(wb,name_0,subtitle_var[i-1],startRow = 2)
      openxlsx::mergeCells(wb,name_0,rows = 2,cols = 1:ncol(table))
      vale <- T
    }
    if(!is.null(text_subtitle2) || add_name_df || add_years){
      if(!is.null(text_subtitle2))   text_1 <- paste0(text_subtitle2,ifelse(add_name_df,names_df_h[i-1],''))
      else   text_1 <- NULL
      if (add_years)   text_years <- paste0('(',min(table[[1]]),'-',max(table[[1]]),')')
      else  text_years <- NULL
      
      openxlsx::writeData(wb,name_0,paste(text_1,text_years),startRow = 3)
      openxlsx::mergeCells(wb,name_0,rows = 3,cols = 1:ncol(table))
      vale <- T
    }
      
    if(vale){
      openxlsx::addStyle(wb,name_0,style_title,
                         rows = 1:3,  
                         cols = 1:ncol(table),gridExpand = T)
    }
    
  })
  
  if(resume_table){
    # =CONCATENAR('01'!A2,"  ",'01'!A3," ",'01'!A4)
    names_sheets <- names_sheets[1:length(names_df_h)]   # if someone inputs a longer indexes 
    space <- '" "'
    gen_code <- sub('Anexo ','',title_anexo)
    codes <- indexes[1:length(names_df_h)] %>%    # if someone inputs a longer indexes 
      paste0(gen_code,.)
    info_sheets <- sapply(names_sheets,function(name_sheet){
      paste0("=CONCATENATE('" ,name_sheet,"'!A2,",space,",'" ,name_sheet,"'!A3,",space,",'",name_sheet,"'!A4)")
    })
    df_resumen <- data.frame(codes,info_sheets)
    names(df_resumen) <- c('Tabla','')
    # df_resumen[[1]] <- as.formula(df_resumen[[1]])
    # class(df_resumen[[1]]) <- c(class(df_resumen[[1]]), "formula")
    class(df_resumen[[2]]) <- c(class(df_resumen[[2]]), "formula")
    df_resumen %>% openxlsx::writeData(wb,'\U00EDndice',.,startRow = 4)
    
    ###---- TITLE  ---- ###
    openxlsx::writeData(wb,'\U00EDndice',paste('\U00EDNDICE DE ',toupper(title_anexo)),startRow = 1)
    openxlsx::mergeCells(wb,'\U00EDndice',rows = 1,cols = 1:2)
    openxlsx::mergeCells(wb,'\U00EDndice',rows = 2,cols = 1:2)   # maybe it is not necessary
    openxlsx::addStyle(wb,'\U00EDndice',style_title,
                       rows = 1:2,  
                       cols = 1:2,gridExpand = T)
    openxlsx::setColWidths(wb,'\U00EDndice',cols = 2 ,widths = 72.67)
    
  }
  
  
    
  openxlsx::saveWorkbook(wb, file,overwrite = TRUE)
  # write.csv(table,file = paste0(path_file,name,'.csv'),row.names = F)
  return()
}


summarise_table <- function(table_data, ignore_first_col = T){
  if(ignore_first_col){
    table_data <- table_data[,-1]
  }
  if(!is.numeric(table_data[[1]])){
    sumario <- table_data %>% summarise_all(~sum(!is.na(.x))) %>% 
      as.matrix()
    rownames(sumario) <- 'N\U00B0 datos'
    return(sumario)
  }
  
  lapply(1:ncol(table_data),function(i){
    var <- table_data[,i,drop = F]
    names(var) <- 'var'
    # var <- table_data[[i]]
    var %>% summarise('N\U00B0 datos' = sum(!is.na(var)),
               '3er Cuartil' = quantile(var,0.75,na.rm = T),
               Mediana = median(var,na.rm=T),
               '1er Cuartil' = quantile(var,0.25, na.rm = T),
               'M\U00E1ximo' = max(var,na.rm = T),
               Promedio = mean(var,na.rm = T),
               'M\U00EDnimo' = min(var,na.rm = T),
               'Desv. Est.' = sd(var,na.rm = T),
               'Coef. Asi.' = moments::skewness(var,na.rm = T),
               'Coef. Var.' = Promedio/get('Desv. Est.')
               ) %>% # mutate('Coef. Var.' = Promedio/get('Desv. Est.'))
      t()
  }) %>% Reduce(function(x,y) cbind(x,y),.)
} 

mean_c <- function(vector, na_max = 3,...){
  if (sum(is.na(vector)) > na_max || all(is.na(vector)))  return(NA)
  else return(mean(vector,na.rm = T))
}

min_c <- function(vector, na_max = 3,...){
  if (sum(is.na(vector)) > na_max || all(is.na(vector)))  return(NA)
  else return(min(vector,na.rm = T))
}

max_c <- function(vector, na_max = 3,...){
  if (sum(is.na(vector)) > na_max || all(is.na(vector)))  return(NA)
  else return(max(vector,na.rm = T))
}

sum_c <- function(vector, na_max = 3,complete_mean = T,...){
  n_na <- sum(is.na(vector)) 
  length_vector <- length(vector)
  if (n_na > na_max   || n_na == length_vector)  return(NA)
  else if (complete_mean){
    return(sum(vector,na.rm = T)*length_vector/(length_vector-n_na))
  } else return(sum(vector,na.rm = T))
}


getmode <- function(v,...) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmodes <- function(x,...) {
  # a <- table(x)
  a <- table(x,useNA = "always")
  as.numeric(names(a)[a == max(a)])
}


compass_to_angles <- function(compass_direction){
  direcs <- setNames( seq(0, 337.5 , by=22.5),
                  c("N","NNE","NE", "ENE", "E", "ESE", "SE", "SSE", 
                    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))
  direcs[compass_direction]
}

angles_to_compass <- function(angles_direction){
  direcs <- setNames( nm = seq(0, 337.5 , by=22.5),
                      object = c("N","NNE","NE", "ENE", "E", "ESE", "SE", "SSE", 
                        "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))
  direcs[as.character(angles_direction)]
}

## the function takes NA like another value, very logical !!
# it is recommended to keep na.rm = F
get_dir <- function(vect,na.rm = F,...){
  if(na.rm)   vect <- na.omit(vect)
  # x = seq(0,360,22.5)
  # sapply(vector, function(v_n){
  #   index <- which.min(abs(x-v_n))
  #   x[index]
  # }) %>% ifelse(.==360,0,.)
  ## probably it will be faster with:
  values <- round(vect/22.5,0)*22.5 
  # values <- ifelse(values == 360,0,values)
  values <- values%%360
  
  getmode(values)
}


## the function takes NA like another value, very logical !!
# it is recommended to keep na.rm = F
get_dir_2 <- function(vect,na.rm = F,out_compass = T,...){
  if(na.rm)   vect <- na.omit(vect)
  ## probably it will be faster with:
  values <- round(vect/22.5,0)*22.5 
  # values <- ifelse(values == 360,0,values)
  values <- values%%360
  
  modes <- getmodes(values)
  # result <- 
    sapply(modes,function(y){
    if (!is.na(y))  sum(values %in% seq(y-22.5,y+22.5,22.5)%%360)
    else sum(values %in% y)
  }) %>%
    which.max() %>% modes[.]
  
  # if(out_compass) return(angles_to_compass(result)) else return(result)
  # df_rep <- rle(values) %>% (function(x) data.frame(lengths = x$lengths, values = x$values))
  # df_rep %>% filter(values %in% modes) %>% slice(which.max(lengths)) %>% .[[2]]
}





cor_matrix <- function(data){
  cor_mat <- matrix(nrow = ncol(data),ncol = ncol(data))
  for(i in (1:ncol(data))){
    cor_mat[i,i] <- 1
    for(j in i:ncol(data)){
      index <- cor(data[,i],data[,j],use = 'na.or.complete')
      cor_mat[i,j] <- index
      cor_mat[j,i] <- index
    }
    data[,i] 
  }
  colnames(cor_mat) <- names(data)
  rownames(cor_mat) <- names(data)
  return(cor_mat)
}

# df_p with first column of dates
complete_by_mean_month <- function(df_p,na_max_year = NULL){
  name_0 <- names(df_p)[1]
  names(df_p)[1] <- 'dates'
  df_mean_m <- df_p %>% mutate(dates = as.numeric(format(dates,'%m'))) %>% 
    group_by(dates) %>% summarise_all(mean,na.rm=T) %>% arrange(dates) 
  df_p <- df_p %>% mutate(month = as.numeric(format(dates,'%m')))
  for (i in 2:(ncol(df_p)-1)){
    name_var <- names(df_p)[i]
    n_na <- df_p[,c(1,i)] %>% mutate(year = trunc(dates,'year')) %>% 
      group_by(year) %>% mutate(n_na = sum(is.na(!!sym(name_var)))) %>% .$n_na
    if (!is.null(na_max_year))   index_na_year <- (n_na <= na_max_year) 
    else index_na_year <- TRUE  
    index_replace <- is.na(df_p[[i]])   & index_na_year   
    df_p[[i]][index_replace] <- df_mean_m[[i]][df_p$month[index_replace]] 
  }
  
  names(df_p)[1] <- name_0
  df_p %>% dplyr::select(-month)
}



# round(date_00,units = 'months')   # trunc
# cut(tm, "weeks")
first_date_period <- function(date_0,period = 'month'){
  if(period %in% c('hour','hours'))  format_time <- '%Y-%m-%d %H:00:00' 
  else if(period %in% c('day','days'))  format_time <- '%Y-%m-%d'  # recommended only 2 options (for seq.posixct)
  else if(period %in% c('month','months'))   format_time <- '%Y-%m-01'
  else if(period %in% c('year','years'))   format_time <- '%Y-01-01'
  
  as.POSIXct(format(date_0,format_time))
}

# first_date_period(as.POSIXct('2000-01-02'),'month')
last_date_period <- function(date_0,period = 'month'){
  if(period %in% c('hour','hours'))  {
    format_time <- '%Y-%m-%d %H:00:00' 
    # var_lt <- 'hour'
    date_0 <- as.POSIXlt(format(date_0,format_time))
    date_0$hour <- date_0$hour + 1
  }
  else if(period %in% c('day','days')){
    format_time <- '%Y-%m-%d'  # recommended only 2 options (for seq.posixct)
    var_lt <- 'mday'
    date_0 <- as.POSIXlt(format(date_0,format_time))
    date_0$mday <- date_0$mday + 1
  }
  else if(period %in% c('month','months')){
    format_time <- '%Y-%m-01'
    var_lt <- 'mon'
    date_0 <- as.POSIXlt(format(date_0,format_time))
    date_0$mon <- date_0$mon + 1
  }   
  else if(period %in% c('year','years')){
    format_time <- '%Y-01-01'
    var_lt <- 'year'
    date_0 <- as.POSIXlt(format(date_0,format_time))
    date_0$year <- date_0$year + 1
  }   

  as.POSIXct(format(date_0,format_time)) - 1 
}



expand_bbox <- function(x, p_x = 0.2, p_y = 0.2, same_dis = F){
  if ('bbox' %in% slotNames(x)){
    x@bbox <- expand_bbox_0(x@bbox, p_x = p_x, p_y = p_y, same_dis = same_dis)
    return(x)
  }
  else {
    x <- expand_bbox_0(x, p_x = p_x, p_y = p_y,same_dis = same_dis)
    return(x)
  }
}

expand_bbox_0 <- function(bbox,p_x = 0.2,p_y = 0.2,same_dis = F){
  if (same_dis) p_y = p_x*diff(bbox[1,])/diff(bbox[2,])
  bbox[1,] <- bbox[1,] + c(-1,1)*p_x*diff(bbox[1,])
  bbox[2,] <- bbox[2,] + c(-1,1)*p_y*diff(bbox[2,])
  bbox
}


trim_log <- function(vec, value = NA){
  vec_l <- vector(mode = 'logical',length = length(vec))
  # if(anyNA(vec)) {
    vec_l[min(which(!is.na(vec))):max(which(!is.na(vec)))] <- T
  # } else vec_l <- !vec_l
  vec_l
}
