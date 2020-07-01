## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).


FIS <- function(layers) {

  scen_year <- layers$data$scenario_year

  #catch data
  c <-
    AlignDataYears(layer_nm = "fis_meancatch", layers_obj = layers) %>%
    dplyr::select(
      region_id = rgn_id,
      year = scenario_year,
      stock_id_taxonkey,
      catch = mean_catch
    )

  #  b_bmsy data

  b <-
    AlignDataYears(layer_nm = "fis_b_bmsy", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, stock_id, year = scenario_year, bbmsy)

  # The following stocks are fished in multiple regions and often have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.

  high_bmsy_filter <- dplyr::filter(b, bbmsy>1.5 & year == 2015) %>%
    dplyr::group_by(stock_id) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    data.frame() %>%
    dplyr::filter(n>3)

   high_bmsy <- high_bmsy_filter$stock_id

   b <- b %>%
     dplyr::mutate(bbmsy = ifelse(stock_id %in% high_bmsy &
                             bbmsy > 1, 1, bbmsy))

   # # no underharvest penalty
   # b <- b %>%
   #   dplyr::mutate(bbmsy = ifelse(bbmsy > 1, 1, bbmsy))


  # separate out the stock_id and taxonkey:
  c <- c %>%
    dplyr::mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
    dplyr::mutate(taxon_key = stringr::str_sub(stock_id_taxonkey,-6,-1)) %>%
    dplyr::mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey) -
                               7)) %>%
    dplyr::mutate(catch = as.numeric(catch)) %>%
    dplyr::mutate(year = as.numeric(as.character(year))) %>%
    dplyr::mutate(region_id = as.numeric(as.character(region_id))) %>%
    dplyr::mutate(taxon_key = as.numeric(as.character(taxon_key))) %>%
    dplyr::select(region_id, year, stock_id, taxon_key, catch)

  # general formatting:
  b <- b %>%
    dplyr::mutate(bbmsy = as.numeric(bbmsy)) %>%
    dplyr::mutate(region_id = as.numeric(as.character(region_id))) %>%
    dplyr::mutate(year = as.numeric(as.character(year))) %>%
    dplyr::mutate(stock_id = as.character(stock_id))


  ####
  # STEP 1. Calculate scores for Bbmsy values
  ####
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  b$score = ifelse(
    b$bbmsy < lowerBuffer,
    b$bbmsy,
    ifelse (b$bbmsy >= lowerBuffer &
              b$bbmsy <= upperBuffer, 1, NA)
  )
  b$score = ifelse(!is.na(b$score),
                   b$score,
                   ifelse(
                     1 - alpha * (b$bbmsy - upperBuffer) > beta,
                     1 - alpha * (b$bbmsy - upperBuffer),
                     beta
                   ))


  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####
  data_fis <- c %>%
    dplyr::left_join(b, by = c('region_id', 'stock_id', 'year')) %>%
    dplyr::select(region_id, stock_id, year, taxon_key, catch, bbmsy, score)


  ###
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  ###

  ## this takes the mean score within each region and year
  ## assessments prior to 2018 used the median
  data_fis_gf <- data_fis %>%
    dplyr::group_by(region_id, year) %>%
    dplyr::mutate(mean_score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup()

  ## this takes the mean score across all regions within a year
  # (when no stocks have scores within a region)
  data_fis_gf <- data_fis_gf %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(mean_score_global = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mean_score = ifelse(is.na(mean_score), mean_score_global, mean_score)) %>%
    dplyr::select(-mean_score_global)

  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************

  penaltyTable <- data.frame(TaxonPenaltyCode = 1:6,
                             penalty = c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

  data_fis_gf <- data_fis_gf %>%
    dplyr::mutate(TaxonPenaltyCode = as.numeric(substring(taxon_key, 1, 1))) %>%
    dplyr::left_join(penaltyTable, by = 'TaxonPenaltyCode') %>%
    dplyr::mutate(score_gf = mean_score * penalty) %>%
    dplyr::mutate(method = ifelse(is.na(score), "Mean gapfilled", NA)) %>%
    dplyr::mutate(gapfilled = ifelse(is.na(score), 1, 0)) %>%
    dplyr::mutate(score = ifelse(is.na(score), score_gf, score))


  gap_fill_data <- data_fis_gf %>%
    dplyr::select(region_id,
           stock_id,
           taxon_key,
           year,
           catch,
           score,
           gapfilled,
           method) %>%
    dplyr::filter(year == scen_year)

  write.csv(gap_fill_data, here('region/temp/FIS_summary_gf.csv'), row.names = FALSE)

  status_data <- data_fis_gf %>%
    dplyr::select(region_id, stock_id, year, catch, score)


  ###
  # STEP 4. Calculate status for each region
  ###

  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year

  status_data <- status_data %>%
    dplyr::group_by(year, region_id) %>%
    dplyr::mutate(SumCatch = sum(catch)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wprop = catch / SumCatch)

  status_data <- status_data %>%
    dplyr::group_by(region_id, year) %>%
    dplyr::summarize(status = prod(score ^ wprop)) %>%
    dplyr::ungroup()


  ###
  # STEP 5. Get yearly status and trend
  ###

  status <-  status_data %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(score     = round(status * 100, 1),
           dimension = 'status') %>%
    dplyr::select(region_id, score, dimension)


  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  trend <-
    CalculateTrend(status_data = status_data, trend_years = trend_years)


  # assemble dimensions
  scores <- rbind(status, trend) %>%
    dplyr::mutate(goal = 'FIS') %>%
    dplyr::filter(region_id != 255)
  scores <- data.frame(scores)

  return(scores)
}


MAR <- function(layers) {
  scen_year <- layers$data$scenario_year

  harvest_tonnes <-
    AlignDataYears(layer_nm = "mar_harvest_tonnes", layers_obj = layers)

  sustainability_score <-
    AlignDataYears(layer_nm = "mar_sustainability_score", layers_obj = layers)

  reference_point <-
    AlignDataYears(layer_nm = "mar_capacity", layers_obj = layers)

  rky <-  harvest_tonnes %>%
    dplyr::left_join(sustainability_score,
              by = c('rgn_id', 'taxa_code', 'scenario_year')) %>%
    dplyr::select(rgn_id, scenario_year, taxa_code, taxa_group, tonnes, sust_coeff)

  # fill in gaps with no data
  rky <- tidyr::spread(rky, scenario_year, tonnes)
  rky <- tidyr::gather(rky, "scenario_year", "tonnes",-(1:4)) %>%
    dplyr::mutate(scenario_year = as.numeric(scenario_year))

  # adjustment for seaweeds based on protein content
  rky <- rky %>%
    dplyr::mutate(tonnes = ifelse(taxa_group == "AL", tonnes*0.2, tonnes)) %>%
    dplyr::select(-taxa_group)

  # 4-year rolling mean of data
  m <- rky %>%
    dplyr::group_by(rgn_id, taxa_code, sust_coeff) %>%
    dplyr::arrange(rgn_id, taxa_code, scenario_year) %>%
    dplyr::mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm = TRUE, partial =
                                        TRUE, align = "right")) %>%
    dplyr::ungroup()


  # smoothed mariculture harvest * sustainability coefficient
  m <- m %>%
    dplyr::mutate(sust_tonnes = sust_coeff * sm_tonnes)


  # aggregate all weighted timeseries per region, and divide by potential mariculture

  ry = m %>%
    dplyr::group_by(rgn_id, scenario_year) %>%
    dplyr::summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm = TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
    dplyr::left_join(reference_point, by = c('rgn_id', 'scenario_year')) %>%
    dplyr::mutate(mar_score = sust_tonnes_sum / potential_mar_tonnes) %>%
    dplyr::ungroup()

  ## add in methods to deal with weirdness


  ry = ry %>%
    dplyr::mutate(status = ifelse(mar_score > 1,
                           1,
                           mar_score)) %>%
    dplyr::mutate(status = ifelse(is.na(status),
                                  0,
                                  status)) %>%
    dplyr::mutate(status = ifelse(sust_tonnes_sum < 100 & potential_mar_tonnes < 100,
                  NA,
                  status))

  ## Add all other regions/countries with no mariculture production to the data table
  ## Uninhabited or low population countries that don't have mariculture, should be given a NA since they are too small to ever be able to produce and sustain a mariculture industry.
  ## Countries that have significant population size and fishing activity (these two are proxies for having the infrastructure capacity to develop mariculture), but don't produce any mariculture, are given a '0'.
  all_rgns <- expand.grid(rgn_id = georegions$rgn_id, scenario_year = min(ry$scenario_year):max(ry$scenario_year))

  all_rgns <- all_rgns[!(all_rgns$rgn_id %in% ry$rgn_id),]

  uninhabited <- read.csv("https://raw.githubusercontent.com/OHI-Science/ohiprep/master/globalprep/spatial/v2017/output/rgn_uninhabited_islands.csv")

  uninhabited <- uninhabited %>%
    dplyr::filter(rgn_nam != "British Indian Ocean Territory") # remove British Indian Ocean Territory which has fishing activity and a population size of 3000 inhabitants

  ## Combine all regions with mariculture data table
  ry_all_rgns <- all_rgns %>%
    dplyr::mutate(status = 0) %>%
    dplyr::mutate(status = ifelse(rgn_id %in% uninhabited$rgn_id, NA, status)) %>%
    dplyr::bind_rows(ry) %>%
    dplyr::arrange(rgn_id)


  status <- ry_all_rgns %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score = status, dimension) %>%
    dplyr::mutate(score = round(score * 100, 2))


  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  trend <- CalculateTrend(status_data = dplyr::filter(ry_all_rgns, !is.na(status)), trend_years = trend_years)


  # return scores
  scores = rbind(status, trend) %>%
    dplyr::mutate(goal = 'MAR')

  return(scores)
}


FP <- function(layers, scores) {

  scen_year <- layers$data$scenario_year

  w <-
    AlignDataYears(layer_nm = "fp_wildcaught_weight", layers_obj = layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, w_fis)

  # scores
  s <- scores %>%
    dplyr::filter(goal %in% c('FIS', 'MAR')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::left_join(w, by = "region_id")  %>%
    dplyr::mutate(w_mar = 1 - w_fis) %>%
    dplyr::mutate(weight = ifelse(goal == "FIS", w_fis, w_mar))


  ## Some warning messages due to potential mismatches in data:
  ## In the future consider filtering by scenario year so it's easy to see what warnings are attributed to which data
  # NA score but there is a weight
  tmp <-
    dplyr::filter(s,
           goal == 'FIS' &
             is.na(score) & (!is.na(w_fis) & w_fis != 0) & dimension == "score")
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a FIS weight but no score: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <-
    dplyr::filter(s,
           goal == 'MAR' &
             is.na(score) & (!is.na(w_mar) & w_fis != 0) & dimension == "score")
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a MAR weight but no score: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  # score, but the weight is NA or 0
  tmp <-
    dplyr::filter(
      s,
      goal == 'FIS' &
        (!is.na(score) &
           score > 0) &
        (is.na(w_fis) | w_fis == 0) & dimension == "score" & region_id != 0
    )
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a FIS score but weight is NA or 0: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <-
    dplyr::filter(
      s,
      goal == 'MAR' &
        (!is.na(score) &
           score > 0.05) &
        (is.na(w_mar) | w_mar == 0) & dimension == "score" & region_id != 0
    )
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a MAR score but weight is NA or 0: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  s <- s  %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


AO <- function(layers) {
  Sustainability <- 1.0

  scen_year <- layers$data$scenario_year

  r <- AlignDataYears(layer_nm = "ao_access", layers_obj = layers) %>%
    dplyr::rename(region_id = rgn_id, access = value) %>%
    na.omit()

  ry <-
    AlignDataYears(layer_nm = "ao_need", layers_obj = layers) %>%
    dplyr::rename(region_id = rgn_id, need = value) %>%
    dplyr::left_join(r, by = c("region_id", "scenario_year"))

  # model
  ry <- ry %>%
    dplyr::mutate(Du = (1 - need) * (1 - access)) %>%
    dplyr::mutate(status = (1 - Du) * Sustainability)

  # status
  r.status <- ry %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, status) %>%
    dplyr::mutate(status = status * 100) %>%
    dplyr::select(region_id, score = status) %>%
    dplyr::mutate(dimension = 'status')

  # trend

  trend_years <- (scen_year - 4):(scen_year)

  r.trend <- CalculateTrend(status_data = ry, trend_years = trend_years)
  ## temporary if empty
  if (dim(r.trend)[1] < 1) {
    r.trend <- data.frame(region_id = 1,
                          score = NA,
                          dimension = "trend")
  }


  # return scores
  scores <- rbind(r.status, r.trend) %>%
    dplyr::mutate(goal = 'AO')

  return(scores)
}


CS <- function(layers) {
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

  ## read in layers
  scen_year <- layers$data$scenario_year

  # layers for coastal protection
  extent_lyrs <-
    c(
      'hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_coral_extent',
      'hab_seaice_extent'
    )
  health_lyrs <-
    c(
      'hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_coral_health',
      'hab_seaice_health'
    )
  trend_lyrs <-
    c(
      'hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend',
      'hab_coral_trend',
      'hab_seaice_trend'
    )


  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    dplyr::filter(!(habitat %in% "seaice_edge")) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    dplyr::filter(!(habitat %in% "seaice_edge")) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::filter(!(habitat %in% "seaice_edge")) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))

  ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    dplyr::filter(habitat %in% c('mangrove_inland1km', 'mangrove_offshore'))

  if (nrow(mangrove_extent) > 0) {
    mangrove_extent <- mangrove_extent %>%
      dplyr::group_by(region_id) %>%
      dplyr::summarize(extent = sum(extent, na.rm = TRUE)) %>%
      dplyr::mutate(habitat = 'mangrove') %>%
      dplyr::ungroup()
  }

  extent <- extent %>%
    dplyr::filter(!habitat %in% c('mangrove', 'mangrove_inland1km', 'mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore

  ## join layer data
  d <-  extent %>%
    dplyr::full_join(health, by = c("region_id", "habitat")) %>%
    dplyr::full_join(trend, by = c("region_id", "habitat"))

  # Removing countries within the Baltic, Iceland, and North Sea regions (UK, Germany, Denmark)
  # because seaice edge is due to ice floating into the environment and does not provide coastal protection
  # for these regions

  floaters <- c(174, 178, 222, 70, 69, 189, 143, 180, 176, 175)


   d <- d %>%
    dplyr::filter(!(region_id %in% floaters & habitat == "seaice_shoreline"))

  ## set ranks for each habitat
  habitat.rank <- c(
    'coral'            = 4,
    'mangrove'         = 4,
    'saltmarsh'        = 3,
    'seagrass'         = 1,
    'seaice_shoreline' = 4
  )

  ## limit to CP habitats and add rank
  d <- d %>%
    dplyr::filter(habitat %in% names(habitat.rank)) %>%
    dplyr::mutate(rank = habitat.rank[habitat],
           extent = ifelse(extent == 0, NA, extent))


  # status
  scores_CP <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) /
                             (sum(
                               extent * rank, na.rm = TRUE
                             ))) * 100) %>%
    dplyr::mutate(dimension = 'status') %>%
    ungroup()

  # trend
  d_trend <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

  if (nrow(d_trend) > 0) {
    scores_CP <- dplyr::bind_rows(
      scores_CP,
      d_trend %>%
        dplyr::group_by(region_id) %>%
        dplyr::summarize(
          score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent * rank, na.rm =
                                                                    TRUE)),
          dimension = 'trend'
        )
    )
  } else {
    # if no trend score, assign NA
    scores_CP <- dplyr::bind_rows(scores_CP,
                                  d %>%
                                    dplyr::group_by(region_id) %>%
                                    dplyr::summarize(score = NA,
                                              dimension = 'trend'))
  }

  ## finalize scores_CP
  scores_CP <- scores_CP %>%
    dplyr::mutate(goal = 'CP') %>%
    dplyr::select(region_id, goal, dimension, score)


  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(rank = habitat.rank[habitat]) %>%
    dplyr::mutate(extent_rank = extent * rank) %>%
    dplyr::mutate(layer = "element_wts_cp_km2_x_protection") %>%
    dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf(here("region/temp/element_wts_cp_km2_x_protection_%s.csv"), scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cp_km2_x_protection <- weights

  # return scores
  return(scores_CP)

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


LIV <- function(layers) {

  # NOTE: scripts and related files for calculating these subgoals is located:
  # region/archive
  # These data are no longer available and status/trend have not been updated since 2013

  scen_year <- layers$data$scenario_year

  ## status data
  status_liv <-
    AlignDataYears(layer_nm = "liv_status", layers_obj = layers) %>%
    dplyr::select(-layer_name, -liv_status_year) %>%
    dplyr::mutate(goal = "LIV") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, goal, score = status) %>%
    dplyr::mutate(dimension = 'status')

  # trend data
  trend_liv <-
    AlignDataYears(layer_nm = "liv_trend", layers_obj = layers) %>%
    dplyr::select(-layer_name, -liv_trend_year) %>%
    dplyr::mutate(goal = "LIV") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, goal, score = trend) %>%
    dplyr::mutate(dimension = 'trend')


  scores <- rbind(status_liv, trend_liv) %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)
}


ECO <- function(layers) {

  # NOTE: scripts and related files for calculating these subgoals is located:
  # region/archive
  # These data are no longer available and status/trend have not been updated since 2013

  scen_year <- layers$data$scenario_year

  ## status data
  status_eco <-
    AlignDataYears(layer_nm = "eco_status", layers_obj = layers) %>%
    dplyr::select(-layer_name, -eco_status_year) %>%
    dplyr::mutate(goal = "ECO") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, goal, score = status) %>%
    dplyr::mutate(dimension = 'status')

# trend data
  trend_eco <-
    AlignDataYears(layer_nm = "eco_trend", layers_obj = layers) %>%
    dplyr::select(-layer_name, -eco_trend_year) %>%
    dplyr::mutate(goal = "ECO") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, goal, score = trend) %>%
    dplyr::mutate(dimension = 'trend')


    scores <- rbind(status_eco, trend_eco) %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)
}

LE <- function(scores, layers) {

  s <- scores %>%
    dplyr::filter(goal %in% c('LIV', 'ECO'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "LE") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))

}

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
  s <- scores %>%
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
  return(rbind(scores, s))
}


CW <- function(layers) {

  scen_year <- layers$data$scenario_year

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }


  # layers
  trend_lyrs <-
    c('cw_chemical_trend',
      'cw_nutrient_trend',
      'cw_trash_trend',
      'cw_pathogen_trend')
  prs_lyrs <-
    c('po_pathogens',
      'po_nutrients_3nm',
      'po_chemicals_3nm',
      'po_trash')

  # get data together:
  prs_data <- AlignManyDataYears(prs_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, value = pressure_score)

  d_pressures <- prs_data %>%
    dplyr::mutate(pressure = 1 - value) %>%  # invert pressure
    dplyr::mutate(pressure = ifelse(pressure == 0 , pressure + 0.01, pressure)) %>% # add small modifier to zeros to
    dplyr::group_by(region_id) %>%                                                  # prevent zeros with geometric mean
    dplyr::summarize(score = geometric.mean2(pressure, na.rm = TRUE)) %>% # take geometric mean
    dplyr::mutate(score = score * 100) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::ungroup()


  # get trend data together:
  trend_data <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, value = trend)

  d_trends <- trend_data %>%
    dplyr::mutate(trend = -1 * value)  %>%  # invert trends
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = mean(trend, na.rm = TRUE)) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::ungroup()


  # return scores
  scores <- rbind(d_pressures, d_trends) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()


  return(scores)
}


HAB <- function(layers) {
  scen_year <- layers$data$scenario_year


  extent_lyrs <-
    c(
      'hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_coral_extent',
      'hab_seaice_extent',
      'hab_softbottom_extent'
    )
  health_lyrs <-
    c(
      'hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_coral_health',
      'hab_seaice_health',
      'hab_softbottom_health'
    )
  trend_lyrs <-
    c(
      'hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend',
      'hab_coral_trend',
      'hab_seaice_trend',
      'hab_softbottom_trend'
    )

  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))


  # join and limit to HAB habitats
  d <- health %>%
    dplyr::full_join(trend, by = c('region_id', 'habitat')) %>%
    dplyr::full_join(extent, by = c('region_id', 'habitat')) %>%
    dplyr::filter(
      habitat %in% c(
        'coral',
        'mangrove',
        'saltmarsh',
        'seaice_edge',
        'seagrass',
        'soft_bottom'
      )
    ) %>%
    dplyr::mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    dplyr::filter(!is.na(w))

  if (sum(d$w %in% 1 & is.na(d$trend)) > 0) {
    warning(
      "Some regions/habitats have extent data, but no trend data.  Consider estimating these values."
    )
  }

  if (sum(d$w %in% 1 & is.na(d$health)) > 0) {
    warning(
      "Some regions/habitats have extent data, but no health data.  Consider estimating these values."
    )
  }


  ## calculate scores
  status <- d %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(!is.na(health)) %>%
    dplyr::summarize(score = pmin(1, sum(health) / sum(w)) * 100,
              dimension = 'status') %>%
    ungroup()

  trend <- d %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(!is.na(trend)) %>%
    dplyr::summarize(score =  sum(trend) / sum(w),
              dimension = 'trend')  %>%
    dplyr::ungroup()

  scores_HAB <- rbind(status, trend) %>%
    dplyr::mutate(goal = "HAB") %>%
    dplyr::select(region_id, goal, dimension, score)


  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    filter(
      habitat %in% c(
        'seagrass',
        'saltmarsh',
        'mangrove',
        'coral',
        'seaice_edge',
        'soft_bottom'
      )
    ) %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(boolean = 1) %>%
    dplyr::mutate(layer = "element_wts_hab_pres_abs") %>%
    dplyr::select(rgn_id = region_id, habitat, boolean, layer)

  write.csv(weights,
            sprintf(here("region/temp/element_wts_hab_pres_abs_%s.csv"), scen_year),
            row.names = FALSE)

  layers$data$element_wts_hab_pres_abs <- weights


  # return scores
  return(scores_HAB)
}


SPP <- function(layers) {

  scen_year <- layers$data$scenario_year

  # Load conservation scores and calculate status
  spp_status <- layers$data$spp_status %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::filter(!is.na(status)) %>%
    group_by(region_id, class) %>% # first find the average status by class
    summarize(status = mean(status)) %>%
    group_by(region_id) %>%
    summarize(status = mean(status)) # find overall average of the 10 classes

  # Load trend data and calculate score
  spp_trend <- layers$data$spp_trend %>%
    dplyr::filter(year == scen_year) %>%
    group_by(region_id, class) %>% # first find average score by class
    summarize(trend = mean(trend_score, na.rm=T)) %>%
    group_by(region_id) %>%
    summarize(trend = mean(trend, na.rm=T)) %>% # find average of the 10 classes
    mutate(dimension = "trend",
           goal = "SPP") %>%
    rename("score" = "trend")

  # Adjust the calculated status to score by incorporating the threshold at which the region would get a zero
  # If over 75% of all species in the region were critically endangered this would get a score of zero
  spp_scores <- spp_status %>%
    mutate(score = 100 * (0.75 - status) / 0.75,
           dimension = "status",
           goal = "SPP") %>%
    dplyr::select(-status) %>%
    bind_rows(spp_trend)

  return(spp_scores)
}

BD <- function(scores) {
  d <- scores %>%
    dplyr::filter(goal %in% c('HAB', 'SPP')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[, c('region_id', 'goal', 'dimension', 'score')]))
}

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
