## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, ICO is the Iconic Species sub-goal of Sense of Place (SP).

### Function to calculate geometric mean:
geometric.mean2 <- function (x, na.rm = TRUE) {
  if (is.null(nrow(x))) {
    exp(mean(log(x), na.rm = TRUE))
  }
  else {
    exp(apply(log(x), 2, mean, na.rm = na.rm))
  }
}

## Biodiversity goal/subgoals:
HAB <- function(layers) {

  scen_year <- layers$data$scenario_year

  ## Corals
  coral_status <- AlignDataYears(layer_nm = 'hab_coral_status', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    group_by(region_id) %>%
    summarize(status = weighted.mean(status, area_km)) %>%  # Area weighted average of zone scores
    mutate(habitat = "coral") %>%
    dplyr::select(region_id, habitat, status)

  coral_trend  <- AlignDataYears(layer_nm = 'hab_coral_trend', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, habitat, trend)

  ## Rainforest
  rainforest_status <- AlignDataYears(layer_nm = 'hab_rainforest_status', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, habitat, status)

  rainforest_trend  <- AlignManyDataYears("hab_rainforest_trend") %>%
    filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, habitat, trend)

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
    mutate(goal = "HAB") %>%
    dplyr::select(region_id, goal, dimension, score)

  ## Create weights file for pressures/resilience calculations
  ## This identifies what regions contain what habitats

  hab_weights <- coral_status %>%
    rbind(rainforest_status) %>%
    select(-status) %>%
    dplyr::mutate(boolean = 1) %>%
    dplyr::mutate(layer = "element_wts_hab_pres_abs") %>%
    dplyr::select(rgn_id = region_id, habitat, boolean, layer)

  write.csv(hab_weights,
            sprintf(here("region/temp/element_wts_hab_pres_abs_%s.csv"), scen_year),
            row.names = FALSE)

  layers$data$element_wts_hab_pres_abs <- hab_weights

  return(hab_score)

}

SPP <- function(layers) {

  scen_year <- layers$data$scenario_year

  # Load species status
  spp_status <- AlignDataYears(layer_nm = 'spp_status', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, status)

  # Load trend data and calculate score
  spp_trend <- AlignDataYears(layer_nm = 'spp_trend', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "trend") %>%
    dplyr::select(region_id, dimension, score = trend_score)

  # Find scores
  spp_scores <- spp_status %>%
    mutate(score = 100 * status,
           dimension = "status") %>%
    dplyr::select(-status) %>%
    bind_rows(spp_trend) %>%
    mutate(goal = "SPP") %>%
    dplyr::select(region_id, goal, dimension, score)

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

  ## Non-weighting simple method
  # Calculate status
  cs_status <- AlignDataYears(layer_nm = 'hs_carbon_storage_status', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(status = status * 100) %>%
    mutate(dimension = 'status') %>%
    dplyr::select(region_id, dimension, score = status)

  # Calculate trend
  cs_trend <- AlignDataYears(layer_nm = 'hab_rainforest_trend', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "trend") %>%
    dplyr::select(region_id, dimension, score = trend)

  # Calculate score
  cs_score <- cs_status %>%
    rbind(cs_trend) %>%
    mutate(goal = "CS") %>%
    dplyr::select(region_id, goal, dimension, score)

  ### If using the weighting method - uncomment this section
  ## Gather health, extent, and rank layers
  # Carbon Storage health (uses status layer)
  # cs_health <- AlignDataYears(layer_nm = 'hs_carbon_storage_status', layers_obj = layers) %>%
  #   filter(scenario_year == scen_year) %>%
  #   dplyr::select(region_id, habitat, health = status)
  #
  # # Set habitat rank - only include CS habitats (rainforest)
  # habitat.rank <- c('rainforest' = 4)
  #
  # # Rainforest extent
  # cs_extent <- AlignDataYears(layer_nm = "hab_rainforest_extent", layers_obj = layers) %>%
  #   filter(scenario_year == scen_year) %>%
  #   dplyr::select(region_id, habitat, extent = km2)
  #
  # ## Join layers
  # cs_data <-  cs_extent %>%
  #   dplyr::full_join(cs_health, by = c("region_id", "habitat")) %>%
  #   dplyr::mutate(rank = habitat.rank[habitat])
  #
  # ## Calculate current status from health, extent, and rank
  # cs_status <- cs_data %>%
  #   dplyr::group_by(region_id) %>%
  #   dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) /
  #                                   (sum(
  #                                     extent * rank, na.rm = TRUE
  #                                   ))) * 100) %>%
  #   dplyr::mutate(dimension = 'status') %>%
  #   ungroup()
  #
  # ## Calculate Trend
  # cs_trend <- AlignDataYears(layer_nm = 'hab_rainforest_trend', layers_obj = layers) %>% # use same layer as habitats subgoal
  #   filter(scenario_year == scen_year) %>%
  #   mutate(dimension = "trend") %>%
  #   dplyr::select(region_id, score=trend, dimension)
  #
  # ## CS scores
  # cs_score <- cs_status %>%
  #   dplyr::select(region_id, score, dimension) %>%
  #   bind_rows(cs_trend) %>%
  #   mutate(goal = "CS") %>%
  #   dplyr::select(region_id, goal, dimension, score)
  #
  # ## Create weights file for pressures/resilience calculations
  #
  # cs_weights <- cs_extent %>%
  #   dplyr::filter(extent > 0) %>%
  #   dplyr::mutate(rank = habitat.rank[habitat]) %>%
  #   dplyr::mutate(extent_rank = extent * rank) %>%
  #   dplyr::mutate(layer = "element_wts_cp_km2_x_protection") %>%
  #   dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)
  #
  # write.csv(
  #   cs_weights,
  #   sprintf(here("region/temp/element_wts_cs_km2_x_storage_%s.csv"), scen_year),
  #   row.names = FALSE
  # )
  #
  # ## Add weights to layers folder
  # add_cs_weights <- cs_weights %>%
  #   dplyr::select(-layer)
  #
  # layers$data$element_wts_cs_km2_x_storage <- add_cs_weights

  ## Return scores
  return(cs_score)

}

CP <- function(layers) {

  scen_year <- layers$data$scenario_year

  ## Non-weighting simple method
  # Calculate status
  cp_status <- AlignDataYears(layer_nm = 'hs_coastal_protection_status', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(status = status * 100) %>%
    mutate(dimension = 'status') %>%
    dplyr::select(region_id, dimension, score = status)

  # Calculate trend
  cp_trend <- AlignDataYears(layer_nm = 'hab_coral_trend', layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "trend") %>%
    dplyr::select(region_id, dimension, score = trend)

  # Calculate score
  cp_score <- cp_status %>%
    rbind(cp_trend) %>%
    mutate(goal = "CP") %>%
    dplyr::select(region_id, goal, dimension, score)

  ### If using the weighting method - uncomment this section
  ## Gather health, extent, and rank layers
  # Coastal protection health (uses status layer)
  # cp_health <- AlignDataYears(layer_nm = 'hs_coastal_protection_status', layers_obj = layers) %>%
  #   filter(scenario_year == scen_year) %>%
  #   dplyr::select(region_id, habitat, health = status)
  #
  # # Set habitat rank - only include CP habitats (coral)
  # habitat.rank <- c('coral' = 4) # Global uses a rank of 4 for corals
  #
  # # Coral extent
  # cp_extent <- AlignDataYears(layer_nm = "hab_coral_extent", layers_obj = layers) %>%
  #   filter(scenario_year == scen_year) %>%
  #   dplyr::select(region_id, habitat, extent = km2)
  #
  # ## Join layers
  # cp_data <-  cp_extent %>%
  #   dplyr::full_join(cp_health, by = c("region_id", "habitat")) %>%
  #   dplyr::mutate(rank = habitat.rank[habitat])
  #
  # ## Calculate current status from health, extent, and rank
  # cp_status <- cp_data %>%
  #   dplyr::group_by(region_id) %>%
  #   dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) /
  #                                   (sum(
  #                                     extent * rank, na.rm = TRUE
  #                                   ))) * 100) %>%
  #   dplyr::mutate(dimension = 'status') %>%
  #   ungroup()
  #
  # ## Calculate Trend
  # cp_trend <- AlignDataYears(layer_nm = 'hab_coral_trend', layers_obj = layers) %>% # use same layer as habitats subgoal
  #   filter(scenario_year == scen_year) %>%
  #   mutate(dimension = "trend") %>%
  #   dplyr::select(region_id, score=trend, dimension)
  #
  # ## CP scores
  # cp_score <- cp_status %>%
  #   dplyr::select(region_id, score, dimension) %>%
  #   bind_rows(cp_trend) %>%
  #   mutate(goal = "CP") %>%
  #   dplyr::select(region_id, goal, dimension, score)
  #
  # ## Create weights file for pressures/resilience calculations
  #
  # cp_weights <- cp_extent %>%
  #   dplyr::filter(extent > 0) %>%
  #   dplyr::mutate(rank = habitat.rank[habitat]) %>%
  #   dplyr::mutate(extent_rank = extent * rank) %>%
  #   dplyr::mutate(layer = "element_wts_cp_km2_x_protection") %>%
  #   dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)
  #
  # write.csv(
  #   cp_weights,
  #   sprintf(here("region/temp/element_wts_cp_km2_x_protection_%s.csv"), scen_year),
  #   row.names = FALSE
  # )
  #
  # ## Add weights to layers folder
  # add_cp_weights <- cp_weights %>%
  #   dplyr::select(-layer)
  #
  # layers$data$element_wts_cp_km2_x_protection <- add_cp_weights

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

  ## Layers
  # Debris
  cw_debris <- AlignDataYears(layer_nm = 'cw_debris_status', layers_obj = layers) %>%
    mutate(cat = "debris") %>%
    dplyr::select(region_id, scenario_year, cat, status)

  # Soil Contamination
  cw_sc <- AlignDataYears(layer_nm = 'cw_contamination_status', layers_obj = layers) %>%
    mutate(cat = "contamination") %>%
    dplyr::select(region_id, scenario_year, cat, status)


  ## Get data together:
  cw_data <- cw_debris %>%
    rbind(cw_sc)

  ## Calculate the status
  cw_status <- cw_data %>%
    group_by(region_id, scenario_year) %>%
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
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, score = status, dimension) %>%
    bind_rows(cw_trend) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(cw_score)

}

RS <- function(layers) {

  scen_year <- layers$data$scenario_year

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

  scen_year <- layers$data$scenario_year

  # Placeholder Status
  tr_status <- AlignDataYears(layer_nm = "tr_status_placeholder", layers_obj = layers) %>%
    dplyr::select(region_id, scenario_year, score = status)

  # Calculate trend - NA for now
  tr_trend <- data.frame(region_id = 1,
                         scenario_year = 2020,
                         score = NA,
                         dimension = "trend")

  # Calculate score
  tr_score <- tr_status %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    rbind(tr_trend) %>%
    mutate(goal = "TR") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(tr_score)

}

RAO <- function(layers) {

  scen_year <- layers$data$scenario_year

  # Placeholder status
  rao_status <- AlignDataYears(layer_nm = "rao_status_placeholder", layers_obj = layers) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id, scenario_year, score = status, dimension)

  # Calculate trend - NA for now
  rao_trend <- data.frame(region_id = 1,
                          scenario_year = 2020,
                          score = NA,
                          dimension = "trend")

  # Calculate scores
  rao_score <- rao_status %>%
    filter(scenario_year == scen_year) %>%
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
