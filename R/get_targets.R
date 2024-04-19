#' get daily targets
#' 
#' @param fcre_file link to EDI data file for fcre
#' @param bvre_file link to data file(s) for bvre, order EDI then L1
#' @param method how should the P1D data be generated from the HF data, either subset or aggregate
#' @return a targets dataframe in VERA format

get_targets_P1D <- function(fcre_file, bvre_file, method = 'subet', subset_time = '12:00:00') {
  
  standard_names <- data.frame(variable_new = c('Temp_C_mean', 'SpCond_uScm_mean', 'Chla_ugL_mean', 'fDOM_QSU_mean'),
                               variable = c('EXOTemp', 'EXOSpCond', 'EXOChla', 'EXOfDOM'))
  
  # Load FCR data
  fcre_df <- readr::read_csv(fcre_file, show_col_types = FALSE) |>
    dplyr::mutate(site_id = "fcre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  sampledate = as.Date(DateTime)) |> 
    dplyr::filter(Site == 50)
  
  if (length(bvre_file) == 2) {
    # Load bvre data
    bvre_current <- readr::read_csv(bvre_file[1], show_col_types = FALSE) |>
      dplyr::mutate(site_id = "bvre",
                    DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                    DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                    sampledate = as.Date(DateTime))  |> 
      dplyr::filter(Site == 50)
    
    bvre_historical <- readr::read_csv(bvre_file[2], show_col_types = FALSE) |>
      dplyr::mutate(site_id = "bvre",
                    DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                    DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                    sampledate = as.Date(DateTime)) |>
      dplyr::rename(LvlDepth_m_13 = Depth_m_13)  |> 
      dplyr::filter(Site == 50)
    
    bvre_df <- dplyr::bind_rows(bvre_current, bvre_historical)
  } 
  
  if (length(bvre_file) == 1) {
    fcre_df <- readr::read_csv(bvre_file, show_col_types = FALSE) |>
      dplyr::mutate(site_id = "bvre",
                    DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                    DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                    sampledate = as.Date(DateTime))
  }
  
  
  
  
  ## only use complete days (remove only partially sampled days) # 144 sample events per day (6*24)
  fcre_remove_days <- fcre_df |>
    group_by(sampledate) |>
    summarize(n_samples = n_distinct(DateTime)) |>
    filter(n_samples < 144) |>
    filter(sampledate == Sys.Date() | n_samples < 144/2)
  
  
  bvre_remove_days <- bvre_df |>
    group_by(sampledate) |>
    summarize(n_samples = n_distinct(DateTime)) |>
    filter(n_samples < 144) |>
    filter(sampledate == Sys.Date() | n_samples < 144/2)
  
  if (method == 'aggregate') {
    # Format data to combine
    # FCR
    fcre_sum <- fcre_df |>
      filter(!sampledate %in% fcre_remove_days$sampledate) |>  # filter for complete days
      dplyr::group_by(sampledate, site_id) |>
      dplyr::summarise(# Cond_uScm_mean = mean(EXOCond_uScm_1, na.rm = T),
        Temp_C_mean = mean(EXOTemp_C_1, na.rm = T),
        SpCond_uScm_mean = mean(EXOSpCond_uScm_1, na.rm = T),
        Chla_ugL_mean = mean(EXOChla_ugL_1, na.rm = T),
        fDOM_QSU_mean = mean(EXOfDOM_QSU_1, na.rm = T),
        # Turbidity_FNU_mean = mean(EXOTurbidity_FNU_1, na.rm = T),
        # Bloom_binary_mean = as.numeric(mean(Chla_ugL_mean, na.rm = T)>20), 
        .groups = 'drop')
    
    # bvre
    bvre_sum <- bvre_df |>
      filter(!sampledate %in% bvre_remove_days$sampledate) |> # filter for complete days
      dplyr::group_by(sampledate, site_id) |> #daily mean
      dplyr::summarise(# Cond_uScm_mean = mean(EXOCond_uScm_1.5, na.rm = T),
        Temp_C_mean = mean(EXOTemp_C_1.5, na.rm = T),
        SpCond_uScm_mean = mean(EXOSpCond_uScm_1.5, na.rm = T),
        Chla_ugL_mean = mean(EXOChla_ugL_1.5, na.rm = T),
        fDOM_QSU_mean = mean(EXOfDOM_QSU_1.5, na.rm = T),
        # Turbidity_FNU_mean = mean(EXOTurbidity_FNU_1.5, na.rm = T),
        # Bloom_binary_mean = as.numeric(mean(Chla_ugL_mean, na.rm = T)>20), 
        .groups = 'drop')
    
    
    ## build DO for each site separately and then combine
    fcre_DO <- fcre_df |>
      filter(!sampledate %in% fcre_remove_days$sampledate) |>  # filter for complete days
      select(DateTime, RDO_mgL_9_adjusted, EXODO_mgL_1) |>
      pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
      mutate(sampledate = as.Date(DateTime),
             depth_m = as.numeric(str_split_i(variable, "_", 3)), 
             depth_m = ifelse(str_detect(variable, 'EXO'), 1.6, depth_m),
             variable = 'DO_mgL_mean') |> 
      summarise(obs_avg = mean(observation, na.rm = TRUE), .by = c('sampledate', 'variable', 'depth_m')) |>
      mutate(datetime=ymd_hms(paste0(sampledate,"","00:00:00"))) |>
      select(datetime, depth_m, observation = obs_avg, variable)
    
    fcre_DO$site_id <- 'fcre'
    
    
    bvre_DO <- bvre_df |>
      filter(!sampledate %in% bvre_remove_days$sampledate) |> # filter for complete days
      select(DateTime, RDO_mgL_13, EXODO_mgL_1.5) |>
      pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
      mutate(sampledate = as.Date(DateTime),
             depth_m = as.numeric(str_split_i(variable, "_", 3)), 
             depth_m = ifelse(str_detect(variable, 'EXO'), 1.5, depth_m),
             variable = 'DO_mgL_mean') |> 
      summarise(obs_avg = mean(observation, na.rm = TRUE), .by = c('sampledate', 'variable', 'depth_m')) |>
      mutate(datetime=ymd_hms(paste0(sampledate,"","00:00:00"))) |>
      select(datetime, depth_m, observation = obs_avg, variable, observation = obs_avg)
    
    bvre_DO$site_id <- 'bvre'
    
    
    combined_DO <- bind_rows(fcre_DO, bvre_DO) |>
      mutate(observation = ifelse(is.nan(observation), NA, observation))
    
    #depth is 1.5 at bvre and 1.6 and FCR
    
    #Combine all and format
    targets_P1D <- fcre_sum |>
      dplyr::bind_rows(bvre_sum) |>
      dplyr::rename(datetime = sampledate) |>
      tidyr::pivot_longer(cols = Temp_C_mean:fDOM_QSU_mean, names_to = "variable", values_to = "observation") |>
      dplyr::mutate(depth_m = NA,
                    depth_m = ifelse(site_id == "fcre", fcre_depths[1], depth_m),
                    depth_m = ifelse(site_id == "bvre", bvre_depths[1], depth_m)) |>
      dplyr::select(datetime, site_id, depth_m, observation, variable) |>
      dplyr::mutate(observation = ifelse(!is.finite(observation),NA,observation)) |>
      bind_rows(combined_DO) # append DO data
  }
  
  if (method == 'subset') {
    # FCR
    fcre_subset <- fcre_df |>
      filter(!sampledate %in% fcre_remove_days$sampledate,  # filter for complete days
             str_detect(DateTime, pattern = subset_time)) |> 
      dplyr::select(any_of(c('DateTime', 'EXOTemp_C_1', 'EXOSpCond_uScm_1','EXOChla_ugL_1','EXOfDOM_QSU_1'))) |>
      pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
      mutate(datetime = DateTime,
             depth_m = as.numeric(str_split_i(variable, "_", 3)), 
             depth_m = ifelse(str_detect(variable, 'EXO'), 1.6, depth_m),
             variable = str_split_i(variable, "_", 1),
             site_id = 'fcre') |>
      full_join(standard_names, by = 'variable') |> 
      select(datetime, site_id, depth_m, observation, variable = variable_new)  
    
    # bvre
    bvre_subset <- bvre_df |>
      filter(!sampledate %in% bvre_remove_days$sampledate,  # filter for complete days
             str_detect(DateTime, pattern = subset_time)) |> 
      dplyr::select(any_of(c('DateTime', 'EXOTemp_C_1.5', 'EXOSpCond_uScm_1.5','EXOChla_ugL_1.5','EXOfDOM_QSU_1.5'))) |>
      pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
      mutate(datetime = DateTime,
             depth_m = as.numeric(str_split_i(variable, "_", 3)), 
             depth_m = ifelse(str_detect(variable, 'EXO'), 1.5, depth_m),
             variable = str_split_i(variable, "_", 1),
             site_id = 'fcre') |>
      full_join(standard_names, by = 'variable') |> 
      select(datetime, site_id, depth_m, observation, variable = variable_new)  
    
    
    ## build DO for each site separately and then combine
    fcre_DO_subset <- fcre_df |>
      filter(!sampledate %in% fcre_remove_days$sampledate,  # filter for complete days
             str_detect(DateTime, pattern = subset_time)) |> 
      select(DateTime, RDO_mgL_9_adjusted, EXODO_mgL_1) |>
      pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
      mutate(sampledate = as.Date(DateTime),
             depth_m = as.numeric(str_split_i(variable, "_", 3)), 
             depth_m = ifelse(str_detect(variable, 'EXO'), 1.6, depth_m),
             variable = 'DO_mgL_mean') |> 
      mutate(datetime=ymd_hms(paste0(sampledate,"","00:00:00"))) |>
      select(datetime, depth_m, observation, variable)
    
    fcre_DO_subset$site_id <- 'fcre'
    
    
    bvre_DO_subset <- bvre_df |>
      filter(!sampledate %in% bvre_remove_days$sampledate,  # filter for complete days
             str_detect(DateTime, pattern = subset_time)) |> 
      select(DateTime, RDO_mgL_13, EXODO_mgL_1.5) |>
      pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
      mutate(sampledate = as.Date(DateTime),
             depth_m = as.numeric(str_split_i(variable, "_", 3)), 
             # depth_m = ifelse(str_detect(variable, 'EXO'), 1.5, depth_m),
             variable = 'DO_mgL_mean') |> 
      mutate(datetime=ymd_hms(paste0(sampledate,"","00:00:00"))) |>
      select(datetime, depth_m, observation, variable)
    
    bvre_DO_subset$site_id <- 'bvre'
    
    
    combined_DO <- bind_rows(fcre_DO_subset, bvre_DO_subset) |>
      mutate(observation = ifelse(is.nan(observation), NA, observation))
    

    #Combine all and format
    targets_P1D <- fcre_subset |>
      dplyr::bind_rows(bvre_subset) |> 
      dplyr::select(datetime, site_id, depth_m, observation, variable) |>
      dplyr::mutate(observation = ifelse(!is.finite(observation),NA,observation)) |>
      bind_rows(combined_DO) # append DO data
  } 
  return(targets_P1D)
}

# ================================================================#

# =================== Temperature profiles ==================

get_temp_profiles <- function(current_file = NULL, historic_file){
  source('R/find_depths.R')
 
  if (current_file != 'none') {
    message('reading ', current_file)
    current_df <- readr::read_csv(current_file, show_col_types = F) |>
      dplyr::filter(Site == 50) |>
      dplyr::select(Reservoir, DateTime,
                    dplyr::starts_with('ThermistorTemp'))
    
    if (current_df$Reservoir[1] == 'BVR') {
      bvr_depths <- find_depths(data_file = current_file,
                                depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                                output <- NULL,
                                date_offset <- "2021-04-05",
                                offset_column1<- "Offset_before_05APR21",
                                offset_column2 <- "Offset_after_05APR21") |>
        dplyr::filter(variable == 'ThermistorTemp') |>
        dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)
      
      current_df_1 <- current_df  |>
        tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                            names_to = c('variable','Position'),
                            names_sep = '_C_',
                            values_to = 'observation') |>
        dplyr::mutate(date = lubridate::as_date(DateTime),
                      Position = as.numeric(Position)) |>
        na.omit() |>
        dplyr::left_join(bvr_depths,
                         by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
        dplyr::group_by(date, Reservoir, depth_bin) |>
        dplyr::summarise(observation = mean(observation, na.rm = T),
                         n = dplyr::n(),
                         .groups = 'drop') |>
        dplyr::mutate(observation = ifelse(n < 144/3, NA, observation), # 144 = 24(hrs) * 6(10 minute intervals/hr)
                      Reservoir = 'bvre') |>
        
        dplyr::rename(site_id = Reservoir,
                      datetime = date,
                      depth = depth_bin) |>
        dplyr::select(-n) |> 
        dplyr::mutate(depth = as.character(depth))
    }
    
    # read in differently for FCR
    if (current_df$Reservoir[1] == 'FCR') {
      current_df_1 <- current_df |>
        tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                            names_to = 'depth',
                            names_prefix = 'ThermistorTemp_C_',
                            values_to = 'observation') |>
        dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                         'fcre',
                                         ifelse(Reservoir == 'BVR',
                                                'bvre', NA)),
                      date = lubridate::as_date(DateTime)) |>
        na.omit() |>
        dplyr::group_by(date, Reservoir, depth) |>
        dplyr::summarise(observation = mean(observation, na.rm = T),
                         n = dplyr::n(),
                         .groups = 'drop') |>
        dplyr::mutate(observation = ifelse(n < 144/2, NA, observation),
                      depth = as.character(depth)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
        dplyr::rename(site_id = Reservoir,
                      datetime = date) |>
        dplyr::select(-n)
    }
    message('Current file ready')
  } else {
    current_df_1 <- NULL
    message('No current file')
  }
  
  # read in historical data file
  # EDI
  # infile <- tempfile()
  # try(download.file(historic_file, infile, method="curl"))
  # if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  
  historic_df <- readr::read_csv(historic_file, show_col_types = FALSE) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp'))
  
  # Extract depths for BVR
  if (historic_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = historic_file,
                              depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                              output <- NULL,
                              date_offset <- "2021-04-05",
                              offset_column1<- "Offset_before_05APR21",
                              offset_column2 <- "Offset_after_05APR21") |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)
    
    historic_df_1 <- historic_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(date = lubridate::as_date(DateTime),
                    Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
      dplyr::group_by(date, Reservoir, depth_bin) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation), # 144 = 24(hrs) * 6(10 minute intervals/hr)
                    Reservoir = 'bvre') |>
      dplyr::rename(site_id = Reservoir,
                    datetime = date,
                    depth = depth_bin) |>
      dplyr::select(-n) |> 
      dplyr::mutate(depth = as.character(depth))
  }
  
  if (historic_df$Reservoir[1] == 'FCR') {
    historic_df_1 <- historic_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = 'depth',
                          names_prefix = 'ThermistorTemp_C_',
                          values_to = 'observation') |>
      dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                       'fcre',
                                       ifelse(Reservoir == 'BVR',
                                              'bvre', NA)),
                    date = lubridate::as_date(DateTime)) |>
      dplyr::group_by(date, Reservoir, depth)  |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 6/2, NA, observation)) |> # 6 = 6(10 minute intervals/hr)
      dplyr::rename(site_id = Reservoir,
                    datetime = date)|>
      dplyr::select(-n) |> 
      dplyr::mutate(depth = as.character(depth))
  }
  
  message('EDI file ready')
  
  ## manipulate the data files to match each other
  
  
  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(historic_df_1, current_df_1) |>
    dplyr::mutate(variable = 'Temp_C_mean',
                  depth = as.numeric(ifelse(depth == "surface", 0.1, depth))) |>
    rename(depth_m = depth)
  
  final_df <- final_df |>
    mutate(observation = ifelse(is.nan(observation), NA, observation)) |>
    drop_na(depth_m)
  ## Match data to flare targets file
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily
  # for hourly
  
  ## return dataframe formatted to match FLARE targets
  return(final_df)
}


calc_strat_dates <- function(density_diff = 0.1,
                             temp_profiles) {
  
  ## extract the depths that will be used to calculate the density difference (surface, bottom)
  depths_use <- temp_profiles |>
    na.omit() |> 
    dplyr::group_by(datetime, site_id) |>
    dplyr::summarise(top = min(as.numeric(depth_m, na.rm = T)),
                     bottom = max(as.numeric(depth_m, na.rm = T)),.groups = 'drop') |>
    tidyr::pivot_longer(cols = top:bottom, 
                        names_to = 'location',
                        values_to = 'depth_m')
  
  sites <- distinct(depths_use, site_id) |> pull()
  
  strat_dates <- NULL
  
  for (site in sites) {
    temp_profile_site <- filter(temp_profiles, site_id == site)
    # need a full timeseries
    all_dates <- data.frame(datetime = seq.Date(min(temp_profile_site$datetime), 
                                                max(temp_profile_site$datetime),
                                                'day'))
    density_obs <-
      filter(depths_use, site_id == site) |> 
      inner_join(na.omit(temp_profile_site), by = join_by(datetime, site_id, depth_m)) |> 
      mutate(density = rLakeAnalyzer::water.density(observation)) |> 
      select(datetime, site_id, density, observation, location) |> 
      pivot_wider(values_from = c(density, observation), names_from = location, id_cols = c(datetime, site_id)) |> 
      full_join(all_dates, by = 'datetime') |> 
      mutate(dens_diff = density_bottom - density_top,
             strat = ifelse(abs(dens_diff > 0.1) & observation_top > observation_bottom, 1, 0),
             strat = imputeTS::na_interpolation(strat, option = 'linear'))
    
    
    # extract the dates of the stratified periods
    #using a loop function to go through each year and do the rle function
    
    strat <- data.frame(year = unique(year(density_obs$datetime)), 
                        length = NA,
                        start = NA,
                        end = NA)
    
    for (i in 1:nrow(strat)) {
      year_use <- strat$year[i]
      
      temp.dens <- density_obs %>%
        filter(year(datetime) == year_use)
      
      if (nrow(temp.dens) >= 300) {
        #run length encoding according to the strat var
        temp.rle <- rle(temp.dens$strat)
        
        #what is the max length for which the value is "norm"
        strat$length[i] <- max(temp.rle$lengths[temp.rle$values==1], 
                               na.rm = T)
        
        #stratification dates
        rle.strat <- data.frame(strat = temp.rle$values, 
                                lengths = temp.rle$lengths)
        
        # Get the end of ech run
        rle.strat$end <- cumsum(rle.strat$lengths)
        # Get the start of each run
        rle.strat$start <- rle.strat$end - rle.strat$lengths + 1
        
        # Sort rows by whehter it is stratified or not
        rle.strat <- rle.strat[order(rle.strat$strat), ]
        
        start.row <- rle.strat$start[which(rle.strat$length == max(rle.strat$lengths)
                                           & rle.strat$strat == 1)] 
        #gets the row with the start date
        #of the run which has the max length and is 1
        
        end.row <- rle.strat$end[which(rle.strat$length == max(rle.strat$lengths)
                                       & rle.strat$strat == 1)] 
        #gets the row with the end date
        #of the run which has the max length and is TRuE
        
        strat$start[which(strat$year == year_use)] <- as.character(temp.dens$datetime[start.row])
        strat$end[which(strat$year == year_use)] <- as.character(temp.dens$datetime[end.row])
        
        strat$site_id <- site
      }
     
    } 
    strat_dates <- bind_rows(strat, strat_dates)
    message(site)
  }
  
  return(na.omit(strat_dates))
}


get_targets_sample  <- function(infiles, start_date, end_date) {
  
  # list of standardised column names
  standard_names <- c(site_id = "Reservoir", 
                      depth_m = "Depth_m",
                      datetime = "DateTime")
  
  final_df <- NULL
  
  for (i in 1:length(infiles)) {
    
    df <- read_csv(infiles[i], show_col_types = F, progress = F) |> 
      filter(Site == 50) |> 
      rename(any_of(standard_names))  |> 
      mutate(site_id = ifelse(site_id == 'BVR', 'bvre', ifelse(site_id == 'FCR', 'fcre', site_id))) |> 
      filter(site_id %in% c('fcre', 'bvre')) |> 
      select(-Site)
    
    df_flags <- df |> 
      select(any_of(c('datetime', 'site_id', 'depth_m')) | contains('Flag')) |> 
      pivot_longer(cols = contains('Flag'),
                   names_to = 'variable', 
                   values_to = 'flag_value', 
                   names_prefix = 'Flag_')
    
    df_observations <- df |> 
      select(-contains('Flag')) |> 
      pivot_longer(cols = -any_of(c('site_id', 'datetime', 'depth_m', 'Rep')),
                   names_to = 'variable', 
                   values_to = 'observation', 
                   names_prefix = 'Flag_')
    
    df_long <- inner_join(df_observations, df_flags, 
                          # by = c('site_id', 'datetime', 'depth_m', 'variable'),
                          relationship = 'many-to-many') |> 
      na.omit() |> 
      filter(!flag_value %in% c(9, 5, 2)) |> 
      select(-contains('flag')) |> 
      group_by(pick(any_of(c('site_id', 'datetime', 'depth_m', 'variable')))) |> 
      summarise(observation = mean(observation), .groups = 'drop')
    
    
    # Combine with other dataframe
    final_df <- bind_rows(final_df, df_long)
    
  }
  
 return(final_df) 
}
