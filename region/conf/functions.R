## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).

## Biodiversity goal/subgoals:
HAB <- function(layers) {

  scen_year <- layers$data$scenario_year


  ## Corals
  coral_status <- layers$data$hab_coral_status %>% # AlignManyDataYears wasn't working
    dplyr::select(-layer) %>%
    group_by(region_id) %>%
    summarize(status = weighted.mean(status, area_km)) %>%  # Area weighted average of zone scores
    mutate(habitat = "coral",
           year = 2020)

  coral_trend  <- layers$data$hab_coral_trend %>% # AlignManyDataYears wasn't working
    dplyr::select(-layer)

  ## Rainforest
  rainforest_status <- layers$data$hab_rainforest_status %>% # AlignManyDataYears wasn't working
    dplyr::select(-layer)

  rainforest_trend  <- AlignManyDataYears("hab_rainforest_trend") %>%
    filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, habitat, year = scenario_year, trend)

  ## Calculate status
  hab_status <- coral_status %>%
    rbind(rainforest_status) %>%
    group_by(region_id) %>%
    summarize(status = mean(status) * 100) %>% # Average coral status and rainforest status
    mutate(dimension = 'status') %>%
    ungroup()

  ## Trend
  hab_trend <- coral_trend %>%
    rbind(rainforest_trend) %>%
    group_by(region_id) %>%
    summarize(score = mean(trend)) %>% # Average coral trend and rainforest trend
    mutate(dimension = 'trend') %>%
    ungroup()

  ## Calculate scores
  hab_score <- hab_status %>%
    dplyr::select(region_id, score = status, dimension) %>%
    bind_rows(hab_trend) %>%
    mutate(goal = "HAB")


  # ## create weights file for pressures/resilience calculations
  #
  # weights <- extent %>%
  #   filter(
  #     habitat %in% c(
  #       'seagrass',
  #       'saltmarsh',
  #       'mangrove',
  #       'coral',
  #       'seaice_edge',
  #       'soft_bottom'
  #     )
  #   ) %>%
  #   dplyr::filter(extent > 0) %>%
  #   dplyr::mutate(boolean = 1) %>%
  #   dplyr::mutate(layer = "element_wts_hab_pres_abs") %>%
  #   dplyr::select(rgn_id = region_id, habitat, boolean, layer)
  #
  # write.csv(weights,
  #           sprintf(here("region/temp/element_wts_hab_pres_abs_%s.csv"), scen_year),
  #           row.names = FALSE)
  #
  # layers$data$element_wts_hab_pres_abs <- weights


  # return scores
  return(hab_score)

}

SPP <- function(layers) {

  scen_year <- layers$data$scenario_year

  # Load species status
  spp_status <- layers$data$spp_status %>%
    dplyr::select(-layer)

  # Load trend data and calculate score
  spp_trend <- layers$data$spp_trend %>%
    dplyr::select(-layer) %>%
    mutate(dimension = "trend",
           goal = "SPP") %>%
    rename("score" = "trend_score")

  # Find scores
  spp_scores <- spp_status %>%
    mutate(score = 100 * status,
           dimension = "status",
           goal = "SPP") %>%
    dplyr::select(-status) %>%
    bind_rows(spp_trend)

  return(spp_scores)
}

BD <- function(scores) {
  bd <- scores %>%
    dplyr::filter(goal %in% c('HAB', 'SPP')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, bd[, c('region_id', 'goal', 'dimension', 'score')]))
}

## Habitat Services goal/subgoals
CS <- function(layers) {

  scen_year <- layers$data$scenario_year

  ## Coastal protection
  cs_status <- layers$data$hs_carbon_storage_status %>%
    mutate(dimension = "status",
           status = status * 100) %>%
    dplyr::select(-layer)

  ## Trend
  cs_trend <- layers$data$hab_rainforest_trend %>% # use same layer as habitats subgoal
    mutate(dimension = "trend") %>%
    dplyr::select(region_id, score=trend, dimension)

  ## CS scores
  cs_score <- cs_status %>%
    dplyr::select(region_id, score=status, dimension) %>%
    bind_rows(cs_trend) %>%
    mutate(goal = "CS") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(cs_score)

  ### OLD - not sure if I need some of this ...
  scen_year <- layers$data$scenario_year

  # layers for carbon storage
  extent_lyrs <-
    c('hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent')
  health_lyrs <-
    c('hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health')
  trend_lyrs <-
    c('hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend')

  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    dplyr::filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    dplyr::filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))

  ## join layer data
  d <-  extent %>%
    dplyr::full_join(health, by = c("region_id", "habitat")) %>%
    dplyr::full_join(trend, by = c("region_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)

  ## limit to CS habitats and add rank
  d <- d %>%
    dplyr::mutate(rank = habitat.rank[habitat],
                  extent = ifelse(extent == 0, NA, extent))

  # status
  status <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) / (sum(
      extent * rank, na.rm = TRUE
    ))) * 100,
    dimension = 'status') %>%
    ungroup()
  ## temporary if empty
  if (dim(status)[1] < 1) {
    status <- data.frame(region_id = 1,
                         score = NA,
                         dimension = "status")
  }

  # trend

  trend <- d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent *
                                                                               rank, na.rm = TRUE)),
                     dimension = 'trend') %>%
    dplyr::ungroup()
  ## temporary if empty
  if (dim(trend)[1] < 1) {
    trend <- data.frame(region_id = 1,
                        score = NA,
                        dimension = "trend")
  }

  scores_CS <- rbind(status, trend)  %>%
    dplyr::mutate(goal = 'CS') %>%
    dplyr::select(goal, dimension, region_id, score)


  ## create weights file for pressures/resilience calculations
  weights <- extent %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(rank = habitat.rank[habitat]) %>%
    dplyr::mutate(extent_rank = extent * rank) %>%
    dplyr::mutate(layer = "element_wts_cs_km2_x_storage") %>%
    dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf(here("region/temp/element_wts_cs_km2_x_storage_%s.csv"), scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cs_km2_x_storage <- weights


  # return scores
  return(scores_CS)

}

CP <- function(layers) {

  scen_year <- layers$data$scenario_year

  ## Coastal protection
  cp_status <- layers$data$hs_coastal_protection_status %>%
    mutate(dimension = "status",
           status = status * 100) %>%
    dplyr::select(-layer)

  ## Trend
  cp_trend <- layers$data$hab_coral_trend %>% # use same layer as habitats subgoal
    mutate(dimension = "trend") %>%
    dplyr::select(region_id, score=trend, dimension)

  ## CP scores
  cp_score <- cp_status %>%
    dplyr::select(region_id, score=status, dimension) %>%
    bind_rows(cp_trend) %>%
    mutate(goal = "CP") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(cp_score)

}

HS <- function(scores) {

  hs <- scores %>%
    filter(goal %in% c('CP', 'CS'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "HS") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, hs))
}

## Sense of Place goal/subgoals
ICO <- function(layers) {

  scen_year <- layers$data$scenario_year

  ico_status <-
    AlignDataYears(layer_nm = "ico_status", layers_obj = layers) %>%
    dplyr::select(-layer_name, -ico_status_year)

  # calculate score
  ico_score <- ico_status %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status")

  # calculate trend
  trend_years <- (scen_year - 4):(scen_year)

  ico_trend <- CalculateTrend(status_data = ico_score, trend_years = trend_years)

  #combine trend and score
  ico_scores <- ico_score %>%
    filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, score, dimension) %>%
    rbind(ico_trend) %>%
    mutate(goal = 'ICO') %>%
    arrange(goal, dimension, region_id) %>%
    distinct()

  return(ico_scores)

}

LSP <- function(layers) {

  scen_year <- layers$data$scenario_year

  # pull in lsp status layer
  lsp_marine <- AlignDataYears(layer_nm = "lsp_offshore", layers_obj = layers) %>%
    dplyr::select(-layer_name)

  lsp_land <- AlignDataYears(layer_nm = "lsp_inland", layers_obj = layers) %>%
    dplyr::select(-layer_name)

  # combine lsp layers

  lsp_status <- lsp_marine %>%
    bind_rows(lsp_land) %>%
    dplyr::select(-lsp_offshore_year,
                  -lsp_inland_year) %>%
    group_by(region_id, scenario_year) %>%
    dplyr::summarize(status = mean(status) * 100) %>%
    dplyr::select(region_id, status,
                  year = scenario_year) %>%
    dplyr::mutate(dimension = "status")

  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  lsp_trend <-
    CalculateTrend(status_data = lsp_status, trend_years = trend_years)


  # return scores
  lsp_scores <- lsp_status %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(score = status) %>%
    dplyr::select(-year, -status) %>%
    dplyr::bind_rows(lsp_trend) %>%
    mutate(goal = "LSP")

  return(lsp_scores)
}

SP <- function(scores) {
  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  sp <- scores %>%
    dplyr::filter(goal %in% c('ICO', 'LSP'),
                  dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, sp))
}

## All other goals
CW <- function(layers) {

  scen_year <- layers$data$scenario_year

  ### Function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }

  ## Layers
  # Debris
  cw_debris <- layers$data$cw_debris_status %>%
    mutate(cat = "debris") %>%
    dplyr::select(-layer)

  # Soil Contamination
  cw_sc <- layers$data$cw_contamination_status %>%
    mutate(cat = "contamination") %>%
    dplyr::select(-layer)


  ## Get data together:
  cw_data <- cw_debris %>%
    rbind(cw_sc)

  ## Calculate the status
  cw_status <- cw_data %>%
    group_by(region_id, year) %>%
    summarize(status = geometric.mean2(status)) %>%
    dplyr::mutate(status = status * 100) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::ungroup()

  ## Calculate the overall trend
  trend_data <- cw_status %>%
    filter(!is.na(status)) %>%
    dplyr::select(-dimension)

  trend_years <- (scen_year - 4):(scen_year)

  cw_trend <-
    CalculateTrend(status_data = trend_data, trend_years = trend_years)

  ## Calculate scores
  cw_score <- cw_status %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::select(region_id, score = status, dimension) %>%
    bind_rows(cw_trend) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(cw_score)

}

RS <- function(layers) {

  scen_year <- layers$data$scenario_year

  ### Function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }

  # Research layers
  rs_lyrs <- c(
    "rs_employment_status",
    "rs_sci_papers_status"
  )

  # Get data together:
  rs_data <- AlignManyDataYears(rs_lyrs) %>%
    dplyr::select(-data_year)

  # Calculate the status
  rs_status <- rs_data %>%
    group_by(region_id, scenario_year) %>%
    summarize(status = geometric.mean2(status)) %>%
    dplyr::mutate(status = status * 100) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::ungroup()

  # Calculate the trend
  trend_data <- rs_status %>%
    filter(!is.na(status)) %>%
    dplyr::select(-dimension)

  trend_years <- (scen_year - 4):(scen_year)

  rs_trend <-
    CalculateTrend(status_data = trend_data, trend_years = trend_years)

  ## Calculate scores
  rs_score <- rs_status %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, score = status, dimension) %>%
    bind_rows(rs_trend) %>%
    dplyr::mutate(goal = "RS") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(rs_score)
}

TR <- function(layers) {
  ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S
  pct_ref <- 90

  scen_year <- layers$data$scenario_year


  ## read in layers
  tourism <-
    AlignDataYears(layer_nm = "tr_jobs_pct_tourism", layers_obj = layers) %>%
    dplyr::select(-layer_name)
  sustain <-
    AlignDataYears(layer_nm = "tr_sustainability", layers_obj = layers) %>%
    dplyr::select(-layer_name)

  tr_data  <-
    dplyr::full_join(tourism, sustain, by = c('rgn_id', 'scenario_year'))

  tr_model <- tr_data %>%
    dplyr::mutate(E   = Ep,
                  S   = (S_score - 1) / (7 - 1),
                  # scale score from 1 to 7.
                  Xtr = E * S)


  # assign NA for uninhabitated islands (i.e., islands with <100 people)
  if (conf$config$layer_region_labels == 'rgn_global') {
    unpopulated = layers$data$uninhabited %>%
      dplyr::filter(est_population < 100 | is.na(est_population)) %>%
      dplyr::select(rgn_id)
    tr_model$Xtr = ifelse(tr_model$rgn_id %in% unpopulated$rgn_id,
                          NA,
                          tr_model$Xtr)
  }



  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    dplyr::filter(scenario_year >=2008) %>%
    dplyr::mutate(Xtr_q = quantile(Xtr, probs = pct_ref / 100, na.rm = TRUE)) %>%
    dplyr::mutate(status  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) %>% # rescale to qth percentile, cap at 1
    dplyr::ungroup()

  # get status
  tr_status <- tr_model %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score = status) %>%
    dplyr::mutate(score = score * 100) %>%
    dplyr::mutate(dimension = 'status')
  ## temporary if empty
  if (dim(tr_status)[1] < 1) {
    tr_status <- data.frame(region_id = 1,
                            score = NA,
                            dimension = "status")
  }

  # calculate trend

  trend_data <- tr_model %>%
    dplyr::filter(!is.na(status))

  trend_years <- (scen_year - 4):(scen_year)

  tr_trend <-
    CalculateTrend(status_data = trend_data, trend_years = trend_years)
  ## temporary if empty
  if (dim(tr_trend)[1] < 1) {
    tr_trend <- data.frame(region_id = 1,
                           score = NA,
                           dimension = "trend")
  }

  # bind status and trend by rows
  tr_score <- dplyr::bind_rows(tr_status, tr_trend) %>%
    dplyr::mutate(goal = 'TR')


  # return final scores
  scores <- tr_score %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)
}

RAO <- function(layers) {

  scen_year <- layers$data$scenario_year

  # Calculate status
  rao_status <- layers$data$rao_status %>%
    dplyr::mutate(status = status * 100) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id, year, status, dimension)

  # Find trend
  rao_trend <- layers$data$rao_trend %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::select(region_id, year, score = trend_score, dimension)

  # Calculate scores
  rao_score <- rao_status %>%
    filter(year == scen_year) %>%
    dplyr::select(region_id, score = status, dimension) %>%
    bind_rows(rao_trend) %>%
    dplyr::mutate(goal = "RAO") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(rao_score)

}

## Final Scores

PreGlobalScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    dplyr::select(id_num = rgn_id, val_chr = label)

  # limit to just desired regions and global (region_id==0)
  scores <- subset(scores, region_id %in% c(rgns[, 'id_num'], 0))

  # apply NA to Antarctica
  id_ant <- subset(rgns, val_chr == 'Antarctica', id_num, drop = TRUE)
  scores[scores$region_id == id_ant, 'score'] = NA

  return(scores)
}

FinalizeScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    dplyr::select(id_num = rgn_id, val_chr = label)


  # add NAs to missing combos (region_id, goal, dimension)
  d <- expand.grid(list(
    score_NA  = NA,
    region_id = c(rgns[, 'id_num'], 0),
    dimension = c(
      'pressures',
      'resilience',
      'status',
      'trend',
      'future',
      'score'
    ),
    goal      = c(conf$goals$goal, 'Index')
  ),
  stringsAsFactors = FALSE)
  head(d)
  d <- subset(d,!(
    dimension %in% c('pressures', 'resilience', 'trend') &
      region_id == 0
  ) &
    !(
      dimension %in% c('pressures', 'resilience', 'trend', 'status') &
        goal == 'Index'
    ))
  scores <-
    merge(scores, d, all = TRUE)[, c('goal', 'dimension', 'region_id', 'score')]

  # order
  scores <- dplyr::arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score <- round(scores$score, 2)

  return(scores)
}
