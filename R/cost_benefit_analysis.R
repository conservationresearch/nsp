#This file contains the main cost-benefit functions 
#Dylan Cole
#September 2021
#Incorporated code written by Laura Keating (July 2021)

#####Cost Benefit Analysis #####

#### Benefit Options #####
#Users are able to select different metrics to examine the cost/benefit effectiveness

######## Analysis for calculating cost benefit ratios based only on simple conservation gains ########
#' Simple cost benefit analysis
#' 
#' This function performs the simplest cost benefit analysis where the benefits are quantified only by Conservation Gain plus Conservation Dependence. 
#' 
#' @param org_programs A vector of the conservation program names
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param functional_score_max The maximum possible score for a spatial unit
#' @return Returns a list of dataframes containing all the different results
#' @examples
#' # cba_GplusD(org_programs,inputs,functional_score_max)
#' @export

cba_GplusD<-function(org_programs, inputs, functional_score_max){
  analysisoption<<-1
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank"
                                 )
  results_overall$org_program <- org_programs 
  
  # Summary object for national BCR results
  results_BCR_national <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_BCR_national) <- c("org_program", "mean", "P5", "P50", "P95")
  results_BCR_national$org_program <- org_programs 
  
  # Summary set up for the global results 
  # will be the same as the national results
  results_BCR_global <- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_total<- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_organization <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_national <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_global <- results_BCR_national
  
  # initialize something to hold all the simulated inputs for sensitivity 
  # and the results too if needed for each program
  dat_list <- list() 
  
  ##### Main Simulation #####
  for (i in 1:length(org_programs)) {
    # Identify which program we are doing now
    org_program <- as.character(org_programs[i])
    print(paste("Running cba_GplusD simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainPlusDependence
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$OrgBenefit
    
    ######## OLD - Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    #GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    #GS_numerator_assessed <- dat$GSGainPlusDependence * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    #n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    #GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    #dat$GSGlobalGainPlusDependence <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Relabelling for consistency
    #dat$GS_Benefit_global <- dat$GSGlobalGainPlusDependence
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    #dat$benefit_global_org <- dat$GS_Benefit_global * dat$OrgBenefit
    
    ######## NEW - Calculate the global benefit for each iteration ########
    
    dat$benefit_global_org <- dat$benefit_national_org * dat$species_range_pct_in_nation/100
    
    ######## Calculate the costs ########
    
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    # Calculate the maximum amount of external funding that our
    # organization will accept (i.e. multiply the maximum funding proportion
    # of total cost with the total project cost).
    
    dat$max_external_funding <- dat$max_prop_total_external_funding*dat$cost_total_project
    
    # Take the minimum of the fundability and the maximum amount of external
    # funding. The resulting vector is the external funding amounts.
    
    dat$external_funding <- pmin(dat$max_external_funding, dat$fundability)
    
    # Calculate the organization project cost as the total project cost minus
    # external funding.
    
    dat$cost_organization <- dat$cost_total_project - dat$external_funding
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) 
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_global$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
    # Costs
    
    results_cost_total[which(results_cost_total$org_program == org_program), "mean"] <- mean(dat$cost_total_project) 
    results_cost_total[which(results_cost_total$org_program == org_program), "P5"] <- quantile(dat$cost_total_project, 0.05)
    results_cost_total[which(results_cost_total$org_program == org_program), "P50"] <- quantile(dat$cost_total_project, 0.50)
    results_cost_total[which(results_cost_total$org_program == org_program), "P95"] <- quantile(dat$cost_total_project, 0.95)
    
    results_cost_organization[which(results_cost_organization$org_program == org_program), "mean"] <- mean(dat$cost_organization)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P5"] <- quantile(dat$cost_organization, 0.05)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P50"] <- quantile(dat$cost_organization, 0.50)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P95"] <- quantile(dat$cost_organization, 0.95)
    
    # Benefits
    
    results_benefit_national[which(results_benefit_national$org_program == org_program), "mean"] <- mean(dat$benefit_national_org)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P5"] <- quantile(dat$benefit_national_org, 0.05)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P50"] <- quantile(dat$benefit_national_org, 0.50)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P95"] <- quantile(dat$benefit_national_org, 0.95)
    
    results_benefit_global[which(results_benefit_global$org_program == org_program), "mean"] <- mean(dat$benefit_global_org)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P5"] <- quantile(dat$benefit_global_org, 0.05)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P50"] <- quantile(dat$benefit_global_org, 0.50)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P95"] <- quantile(dat$benefit_global_org, 0.95)
    
    # Store the dat in a list
    dat_list[[i]] <- dat 
    
  }
  
  ######## Calculating ranking #####
  
  # Calculate where would rank for each of the national and global
  results_overall$BCR_national_EV_rank <- rank(-results_overall$BCR_national_EV)
  results_overall$BCR_global_EV_rank <- rank(-results_overall$BCR_global_EV)
  
  return(list(results_overall, 
              results_cost_total, results_cost_organization,
              results_benefit_national, results_benefit_global, 
              results_BCR_national,
              results_BCR_global,
              dat_list))
  
} #End of cba_GplusD function


######## Analysis for calculating cost benefit ratios based on conservation gains relative to long term potential #########
#' Cost benefit analysis with long term potential
#' 
#' This function performs the cost benefit analysis where benefits, calculated as Conservation Gains plus Conservation Dependence, are relative to
#' long-term potential Green Score metric.
#' 
#' @param org_programs A vector of the conservation program names
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param functional_score_max The maximum possible score for a spatial unit
#' @return Returns a list of dataframes containing all the different results
#' @examples
#' # cba_GplusD_LongTermPot(org_programs,inputs,functional_score_max)
#' @export

cba_GplusD_LongTermPot <- function(org_programs, inputs, functional_score_max){ 
  analysisoption<<-2
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank" 
                                 )
  results_overall$org_program <- org_programs 
  
  # Summary object for national BCR results
  results_BCR_national <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_BCR_national) <- c("org_program", "mean", "P5", "P50", "P95")
  results_BCR_national$org_program <- org_programs 
  
  # Summary set up for the global results 
  # will be the same as the national results
  results_BCR_global <- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_total<- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_organization <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_national <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_global <- results_BCR_national
  
  # initialize something to hold all the simulated inputs for sensitivity 
  # and the results too if needed for each program
  dat_list <- list() 
  for (i in 1:length(org_programs)) {
    # Identify which program we are doing now
    org_program <- as.character(org_programs[i])
    print(paste("Running cba_GplusD_LongTermPot simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainPlusDependence / dat$GSlongtermAspiration * 100
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$OrgBenefit
    
    ######## OLD - Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    #GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    #GS_numerator_assessed <- dat$GSGainPlusDependence * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    #n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    #GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    #dat$GSGlobalGainPlusDependence <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Rescaling to long term potential
    #dat$GS_Benefit_global <- dat$GSGlobalGainPlusDependence / dat$GSlongtermAspiration * 100
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    #dat$benefit_global_org <- dat$GS_Benefit_global * dat$OrgBenefit
    
    ######## NEW - Calculate the global benefit for each iteration ########
    
    dat$benefit_global_org <- dat$benefit_national_org * dat$species_range_pct_in_nation/100
    
    ######## Calculate the costs ########
    
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    # Calculate the maximum amount of external funding that our
    # organization will accept (i.e. multiply the maximum funding proportion
    # of total cost with the total project cost).
    
    dat$max_external_funding <- dat$max_prop_total_external_funding*dat$cost_total_project
    
    # Take the minimum of the fundability and the maximum amount of external
    # funding. The resulting vector is the external funding amounts.
    
    dat$external_funding <- pmin(dat$max_external_funding, dat$fundability)
    
    # Calculate the organization project cost as the total project cost minus
    # external funding.
    
    dat$cost_organization <- dat$cost_total_project - dat$external_funding
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) 
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_global$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
    # Costs
    
    results_cost_total[which(results_cost_total$org_program == org_program), "mean"] <- mean(dat$cost_total_project) 
    results_cost_total[which(results_cost_total$org_program == org_program), "P5"] <- quantile(dat$cost_total_project, 0.05)
    results_cost_total[which(results_cost_total$org_program == org_program), "P50"] <- quantile(dat$cost_total_project, 0.50)
    results_cost_total[which(results_cost_total$org_program == org_program), "P95"] <- quantile(dat$cost_total_project, 0.95)
    
    results_cost_organization[which(results_cost_organization$org_program == org_program), "mean"] <- mean(dat$cost_organization)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P5"] <- quantile(dat$cost_organization, 0.05)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P50"] <- quantile(dat$cost_organization, 0.50)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P95"] <- quantile(dat$cost_organization, 0.95)
    
    # Benefits
    
    results_benefit_national[which(results_benefit_national$org_program == org_program), "mean"] <- mean(dat$benefit_national_org)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P5"] <- quantile(dat$benefit_national_org, 0.05)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P50"] <- quantile(dat$benefit_national_org, 0.50)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P95"] <- quantile(dat$benefit_national_org, 0.95)
    
    results_benefit_global[which(results_benefit_global$org_program == org_program), "mean"] <- mean(dat$benefit_global_org)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P5"] <- quantile(dat$benefit_global_org, 0.05)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P50"] <- quantile(dat$benefit_global_org, 0.50)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P95"] <- quantile(dat$benefit_global_org, 0.95)
    
    # Store the dat in a list
    dat_list[[i]] <- dat
    
  }
  
  ######## Calculating ranking #####
  
  # Calculate where would rank for each of the national and global
  results_overall$BCR_national_EV_rank <- rank(-results_overall$BCR_national_EV)
  results_overall$BCR_global_EV_rank <- rank(-results_overall$BCR_global_EV)

  return(list(results_overall, 
              results_cost_total, results_cost_organization,
              results_benefit_national, results_benefit_global, 
              results_BCR_national,
              results_BCR_global,
              dat_list)) 
} # End of cba_GplusD_LongTermPot function


######## Analysis for calculating cost benefit ratios based only on conservation gains binned into high, medium, low, zero gains ########
#' Simple cost benefit analysis with binning based on benefits
#'
#' This function performs the cost benefit analysis and bins the results prior to ranking the cost-benefit ratios, giving priority to those programs
#' with the largest benefit, calculated as Conservation Gains plus Conservation Dependence, regardless of the program cost. 
#' 
#' @param org_programs A vector of the conservation program names
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param functional_score_max The maximum possible score for a spatial unit
#' @return Returns a list of dataframes containing all the different results
#' @examples
#' # cba_GplusD_BinnedByBenefit(org_programs,inputs,functional_score_max)
#' @export

cba_GplusD_BinnedByBenefit <- function(org_programs, inputs, functional_score_max){
  analysisoption<<-3
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 9))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank", 
                                 "mean_cgain_national_org", "mean_cgain_global_org",
                                 "bin_cgainNational", "bin_cgainGlobal")
  results_overall$org_program <- org_programs 
  
  # Summary object for national BCR results
  results_BCR_national <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_BCR_national) <- c("org_program", "mean", "P5", "P50", "P95")
  results_BCR_national$org_program <- org_programs 
  
  # Summary set up for the global results 
  # will be the same as the national results
  results_BCR_global <- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_total<- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_organization <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_national <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_global <- results_BCR_national
  
  # initialize something to hold all the simulated inputs for sensitivity 
  # and the results too if needed for each program
  dat_list <- list() 
  for (i in 1:length(org_programs)) {
    # Identify which program we are doing now
    org_program <- as.character(org_programs[i])
    print(paste("Running cba_GplusD_BinnedByBenefit simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainPlusDependence
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$OrgBenefit
    
    ######## OLD - Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    #GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    #GS_numerator_assessed <- dat$GSGainPlusDependence * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    #n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    #GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    #dat$GSGlobalGainPlusDependence <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Relabelling for consistency
    #dat$GS_Benefit_global <- dat$GSGlobalGainPlusDependence
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    #dat$benefit_global_org <- dat$GS_Benefit_global * dat$OrgBenefit
    
    ######## NEW - Calculate the global benefit for each iteration ########
    
    dat$benefit_global_org <- dat$benefit_national_org * dat$species_range_pct_in_nation/100
    
    ######## Calculate the costs ########
    
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    # Calculate the maximum amount of external funding that our
    # organization will accept (i.e. multiply the maximum funding proportion
    # of total cost with the total project cost).
    
    dat$max_external_funding <- dat$max_prop_total_external_funding*dat$cost_total_project
    
    # Take the minimum of the fundability and the maximum amount of external
    # funding. The resulting vector is the external funding amounts.
    
    dat$external_funding <- pmin(dat$max_external_funding, dat$fundability)
    
    # Calculate the organization project cost as the total project cost minus
    # external funding.
    
    dat$cost_organization <- dat$cost_total_project - dat$external_funding
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) 
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_global$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
    # Costs
    
    results_cost_total[which(results_cost_total$org_program == org_program), "mean"] <- mean(dat$cost_total_project) 
    results_cost_total[which(results_cost_total$org_program == org_program), "P5"] <- quantile(dat$cost_total_project, 0.05)
    results_cost_total[which(results_cost_total$org_program == org_program), "P50"] <- quantile(dat$cost_total_project, 0.50)
    results_cost_total[which(results_cost_total$org_program == org_program), "P95"] <- quantile(dat$cost_total_project, 0.95)
    
    results_cost_organization[which(results_cost_organization$org_program == org_program), "mean"] <- mean(dat$cost_organization)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P5"] <- quantile(dat$cost_organization, 0.05)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P50"] <- quantile(dat$cost_organization, 0.50)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P95"] <- quantile(dat$cost_organization, 0.95)
    
    # Benefits
    
    results_benefit_national[which(results_benefit_national$org_program == org_program), "mean"] <- mean(dat$benefit_national_org)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P5"] <- quantile(dat$benefit_national_org, 0.05)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P50"] <- quantile(dat$benefit_national_org, 0.50)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P95"] <- quantile(dat$benefit_national_org, 0.95)
    
    results_benefit_global[which(results_benefit_global$org_program == org_program), "mean"] <- mean(dat$benefit_global_org)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P5"] <- quantile(dat$benefit_global_org, 0.05)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P50"] <- quantile(dat$benefit_global_org, 0.50)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P95"] <- quantile(dat$benefit_global_org, 0.95)
    
    # Store the dat in a list
    dat_list[[i]] <- dat
    
    #Calculating mean national gains for binning
    results_overall$mean_cgain_national_org[which(results_overall$org_program == org_program)] <- mean(dat$benefit_national_org)
    
    #Calcuating mean current national GS for conditions within binning
    results_overall$mean_GScurrentNational[which(results_overall$org_program == org_program)] <- mean(dat$GScurrentNational)
    
    # Create national conservation gains bins based on IUCN GS Tables
    if ((results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] >= 40) |
        (results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] > 0) &&
        (results_overall$mean_GScurrentNational[which(results_overall$org_program == org_program)] == 0)  | 
        (results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] > 1 * 
         results_overall$mean_GScurrentNational[which(results_overall$org_program == org_program)]))
    {results_overall$bin_cgainNational[which(results_overall$org_program==org_program)]<-1
    } else if (results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] >= 10 && 
               results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] < 40){
      results_overall$bin_cgainNational[which(results_overall$org_program==org_program)]<- 2
    } else if (results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] > 0 && 
               results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] <= 10) {
      results_overall$bin_cgainNational[which(results_overall$org_program==org_program)]<- 3
    } else if (results_overall$mean_cgain_national_org[which(results_overall$org_program==org_program)] == 0)  {
      results_overall$bin_cgainNational[which(results_overall$org_program==org_program)]<- 4
    }
    
    #Calculating mean global gains for binning
    results_overall$mean_cgain_global_org[which(results_overall$org_program == org_program)] <- mean(dat$benefit_global_org)
    
    #Calcuating mean current global GS for conditions within binning
    results_overall$mean_GScurrentGlobal[which(results_overall$org_program == org_program)] <- mean(dat$GScurrentGlobal)
    
    #Create global conservation gains bins based on IUCN GS Tables
    if ((results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] >= 40) |
        (results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] > 0) &&
        (results_overall$mean_GScurrentGlobal[which(results_overall$org_program == org_program)] == 0)  | 
        (results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] > 1 * 
         results_overall$mean_GScurrentGlobal[which(results_overall$org_program == org_program)]))
    {results_overall$bin_cgainGlobal[which(results_overall$org_program==org_program)]<- 1
    } else if (results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] >= 10 && 
               results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] < 40){
      results_overall$bin_cgainGlobal[which(results_overall$org_program==org_program)]<- 2
    } else if (results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] > 0 && 
               results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] <= 10) {
      results_overall$bin_cgainGlobal[which(results_overall$org_program==org_program)]<- 3
    } else if (results_overall$mean_cgain_global_org[which(results_overall$org_program==org_program)] == 0)  {
      results_overall$bin_cgainGlobal[which(results_overall$org_program==org_program)]<- 4
    }
    
    
  }
  
  #Ranking of programs first by bin then by BCR
  results_overall$BCR_national_EV_rank<-data.table::frank(results_overall, bin_cgainNational, -BCR_national_EV, ties.method="average")
  results_overall$BCR_global_EV_rank<-data.table::frank(results_overall, bin_cgainGlobal, -BCR_global_EV,ties.method="average")
  
  #Relabeling numeric bins to categorical names
  results_overall$bin_cgainNational[which(results_overall$bin_cgainNational==1)] <- "High"
  results_overall$bin_cgainNational[which(results_overall$bin_cgainNational==2)] <- "Medium"
  results_overall$bin_cgainNational[which(results_overall$bin_cgainNational==3)] <- "Low"
  results_overall$bin_cgainNational[which(results_overall$bin_cgainNational==4)] <- "Zero"
  
  results_overall$bin_cgainGlobal[which(results_overall$bin_cgainGlobal==1)] <- "High"
  results_overall$bin_cgainGlobal[which(results_overall$bin_cgainGlobal==2)] <- "Medium"
  results_overall$bin_cgainGlobal[which(results_overall$bin_cgainGlobal==3)] <- "Low"
  results_overall$bin_cgainGlobal[which(results_overall$bin_cgainGlobal==4)] <- "Zero"
 
  
  return(list(results_overall, 
              results_cost_total, results_cost_organization,
              results_benefit_national, results_benefit_global, 
              results_BCR_national,
              results_BCR_global,
              dat_list)) 
} # End of cba_GplusD_BinnedByBenefit function


######## Analysis for calculating cost benefit ratios based on conservation gains relative to long term potential binned by current GS ########
#' Cost benefit analysis with benefits relative to long-term potential, binned by current Green Score status
#' 
#' This function performs the cost benefit analysis with the benefits, calculated as the Conservation Gain plus Conservation Dependence, relative to
#' long-term potential of each species. Prior to ranking programs, this function bins conservation programs into groups depending on the species
#' current Green Score. Species with a lower current Green Score are giving priority and will therefore rank higher, even if the cost benefit ratio
#' is comparatively lower. 
#' 
#' @param org_programs A vector of the conservation program names
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs  
#' @param functional_score_max The maximum possible score for a spatial unit
#' @return Returns a list of dataframes containing all the different results
#' @examples
#' cba_GplusD_LongTermPot_BinnedByGS(org_programs, inputs, functional_score_max)
#' @export

cba_GplusD_LongTermPot_BinnedByGS<-function(org_programs, inputs, functional_score_max){
  analysisoption<<-4
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank"
                                 )
  results_overall$org_program <- org_programs 
  
  # Summary object for national BCR results
  results_BCR_national <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_BCR_national) <- c("org_program", "mean", "P5", "P50", "P95")
  results_BCR_national$org_program <- org_programs 
  
  # Summary set up for the global results 
  # will be the same as the national results
  results_BCR_global <- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_total<- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_organization <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_national <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_global <- results_BCR_national
  
  #Initializing columns for binning process
  results_overall$mean_GScurrentNational <- NA
  results_overall$mean_GScurrentGlobal <- NA
  
  # initialize something to hold all the simulated inputs for sensitivity 
  # and the results too if needed for each program
  dat_list <- list() 
  for (i in 1:length(org_programs)) {
    # Identify which program we are doing now
    org_program <- as.character(org_programs[i])
    print(paste("Running cba_GplusD_LongTermPot_BinnedByGS simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainPlusDependence / dat$GSlongtermAspiration * 100
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$OrgBenefit
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    #GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    #GS_numerator_assessed <- dat$GSGainPlusDependence * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    #n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    #GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    #dat$GSGlobalGainPlusDependence <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Rescaling to long term potential
    #dat$GS_Benefit_global <- dat$GSGlobalGainPlusDependence / dat$GSlongtermAspiration * 100
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    #dat$benefit_global_org <- dat$GS_Benefit_global * dat$OrgBenefit
    
    ######## NEW - Calculate the global benefit for each iteration ########
    
    dat$benefit_global_org <- dat$benefit_national_org * dat$species_range_pct_in_nation/100
    
    ######## Calculate the costs ########
    
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    # Calculate the maximum amount of external funding that our
    # organization will accept (i.e. multiply the maximum funding proportion
    # of total cost with the total project cost).
    
    dat$max_external_funding <- dat$max_prop_total_external_funding*dat$cost_total_project
    
    # Take the minimum of the fundability and the maximum amount of external
    # funding. The resulting vector is the external funding amounts.
    
    dat$external_funding <- pmin(dat$max_external_funding, dat$fundability)
    
    # Calculate the organization project cost as the total project cost minus
    # external funding.
    
    dat$cost_organization <- dat$cost_total_project - dat$external_funding
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) 
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_global$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
    # Costs
    
    results_cost_total[which(results_cost_total$org_program == org_program), "mean"] <- mean(dat$cost_total_project) 
    results_cost_total[which(results_cost_total$org_program == org_program), "P5"] <- quantile(dat$cost_total_project, 0.05)
    results_cost_total[which(results_cost_total$org_program == org_program), "P50"] <- quantile(dat$cost_total_project, 0.50)
    results_cost_total[which(results_cost_total$org_program == org_program), "P95"] <- quantile(dat$cost_total_project, 0.95)
    
    results_cost_organization[which(results_cost_organization$org_program == org_program), "mean"] <- mean(dat$cost_organization)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P5"] <- quantile(dat$cost_organization, 0.05)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P50"] <- quantile(dat$cost_organization, 0.50)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P95"] <- quantile(dat$cost_organization, 0.95)
    
    # Benefits
    
    results_benefit_national[which(results_benefit_national$org_program == org_program), "mean"] <- mean(dat$benefit_national_org)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P5"] <- quantile(dat$benefit_national_org, 0.05)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P50"] <- quantile(dat$benefit_national_org, 0.50)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P95"] <- quantile(dat$benefit_national_org, 0.95)
    
    results_benefit_global[which(results_benefit_global$org_program == org_program), "mean"] <- mean(dat$benefit_global_org)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P5"] <- quantile(dat$benefit_global_org, 0.05)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P50"] <- quantile(dat$benefit_global_org, 0.50)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P95"] <- quantile(dat$benefit_global_org, 0.95)
    
    # Store the dat in a list
    dat_list[[i]] <- dat
    
    
    results_overall$mean_cgain_national_org[which(results_overall$org_program == org_program)] <- mean(dat$benefit_national_org)
    #Calculating mean currentGS for national binning
    results_overall$mean_GScurrentNational[which(results_overall$org_program == org_program)] <- mean(dat$GScurrentNational)
    
    # Create national bins based on IUCN GS Tables 
    if (results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] == 0){
      results_overall$bin_GScurrentNational[which(results_overall$org_program==org_program)]<-1
    } else if (results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] > 0 && 
               results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] < 20){
      results_overall$bin_GScurrentNational[which(results_overall$org_program==org_program)]<-2
    } else if (results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] >= 20 && 
               results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] < 50) {
      results_overall$bin_GScurrentNational[which(results_overall$org_program==org_program)]<-3
    } else if (results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] >= 50 && 
               results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] < 80) {
      results_overall$bin_GScurrentNational[which(results_overall$org_program==org_program)]<-4
    } else if (results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] >= 80 && 
               results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] < 100) {
      results_overall$bin_GScurrentNational[which(results_overall$org_program==org_program)]<-5
    } else if (results_overall$mean_GScurrentNational[which(results_overall$org_program==org_program)] == 100) {
      results_overall$bin_GScurrentNational[which(results_overall$org_program==org_program)]<-6
    }
    
    
    results_overall$mean_cgain_global_org[which(results_overall$org_program == org_program)] <- mean(dat$benefit_global_org)
    #Calculating mean currentGS for global binning
    results_overall$mean_GScurrentGlobal[which(results_overall$org_program == org_program)]<- mean(dat$GScurrentGlobal)
    
    #Creating global bins based on IUCN GS Tables 
    if (results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] == 0){
      results_overall$bin_GScurrentGlobal[which(results_overall$org_program==org_program)]<-1
    } else if (results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] > 0 && 
               results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] < 20){
      results_overall$bin_GScurrentGlobal[which(results_overall$org_program==org_program)]<-2
    } else if (results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] >= 20 && 
               results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] < 50) {
      results_overall$bin_GScurrentGlobal[which(results_overall$org_program==org_program)]<-3
    } else if (results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] >= 50 && 
               results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] < 80) {
      results_overall$bin_GScurrentGlobal[which(results_overall$org_program==org_program)]<-4
    } else if (results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] >= 80 && 
               results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] < 100) {
      results_overall$bin_GScurrentGlobal[which(results_overall$org_program==org_program)]<-5
    } else if (results_overall$mean_GScurrentGlobal[which(results_overall$org_program==org_program)] == 100) {
      results_overall$bin_GScurrentGlobal[which(results_overall$org_program==org_program)]<-6
    }
  }
  
  ######## Calculating ranking ########
  results_overall$BCR_national_EV_rank<-data.table::frank(results_overall, bin_GScurrentNational, -BCR_national_EV, ties.method="average")
  results_overall$BCR_global_EV_rank<-data.table::frank(results_overall, bin_GScurrentGlobal, -BCR_global_EV, ties.method="average")
  
  #Relabeling numeric ranks to categorical names
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==1)] <- "Extinct in the Wild"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==2)] <- "Critically Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==3)] <- "Largely Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==4)] <- "Moderately Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==5)] <- "Slightly Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==6)] <- "Fully Recovered"
  
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==1)] <- "Extinct in the Wild"
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==2)] <- "Critically Depleted"
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==3)] <- "Largely Depleted"
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==4)] <- "Moderately Depleted"
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==5)] <- "Slightly Depleted"
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==6)] <- "Fully Recovered"
  
  return(list(results_overall, 
              results_cost_total, results_cost_organization,
              results_benefit_national, results_benefit_global, 
              results_BCR_national,
              results_BCR_global,
              dat_list)) 
} # End of cba_GplusD_LongTermPot_BinnedByGS function


######## Analysis for calculating cost benefit ratios based on conservation gains relative to current GS plus subjective epsilon ########
#' Cost benefit analysis with benefits relative to current Green Status score plus epsilon
#' 
#' This function performs the cost benefit analysis using an advanced method wherein the benefits, calculated as Conservation Gain plus Conservation 
#' Dependence, are calculated relative to the current Green Score of the species while also incorporating epsilon. Epsilon is subjective value that is
#' added to the current Green Score in order to accommodate species that have a current Green Score of zero.
#' 
#' @param org_programs A vector of the conservation program names
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs 
#' @param functional_score_max The maximum possible score for a spatial unit
#' @return Returns a list of dataframes containing all the different results
#' @examples
#' cba_GplusD_CurrentGS_epsilon(org_programs, inputs, functional_score_max)
#' @export

cba_GplusD_CurrentGS_epsilon<-function(org_programs, inputs, functional_score_max){
  analysisoption<<-5
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank")
  results_overall$org_program <- org_programs 
  
  # Summary object for national BCR results
  results_BCR_national <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_BCR_national) <- c("org_program", "mean", "P5", "P50", "P95")
  results_BCR_national$org_program <- org_programs 
  
  # Summary set up for the global results 
  # will be the same as the national results
  results_BCR_global <- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_total<- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_organization <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_national <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_global <- results_BCR_national
  
  # initialize something to hold all the simulated inputs for sensitivity 
  # and the results too if needed for each program
  dat_list <- list() 
  for (i in 1:length(org_programs)) {
    # Identify which program we are doing now
    org_program <- as.character(org_programs[i])
    print(paste("Running cba_GplusD_CurrentGS_epsilon simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate organization portion of the benefit
    
    # Adding epsilon to account for extirpated species 
    dat$GS_Benefit_national <- dat$GSGainPlusDependence 
    
    dat$benefit_national_org <-  (dat$GS_Benefit_national * dat$OrgBenefit ) / (dat$GScurrentNational + epsilon)
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    #GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    #GS_numerator_assessed <- dat$GSGainPlusDependence * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    #n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    #GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    #dat$GSGlobalGainPlusDependence <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Calcuate global benefit relative to current global GS (adding 0.1 for extirpated species)
    #dat$GS_Benefit_global <- dat$GSGlobalGainPlusDependence / (dat$GScurrentGlobal + epsilon) 
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    #dat$benefit_global_org <- dat$GS_Benefit_global * dat$OrgBenefit
    
    ######## NEW - Calculate the global benefit for each iteration ########
    
    dat$benefit_global_org <- dat$benefit_national_org * dat$species_range_pct_in_nation/100
    
    ######## Calculate the costs ########
    
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    # Calculate the maximum amount of external funding that our
    # organization will accept (i.e. multiply the maximum funding proportion
    # of total cost with the total project cost).
    
    dat$max_external_funding <- dat$max_prop_total_external_funding*dat$cost_total_project
    
    # Take the minimum of the fundability and the maximum amount of external
    # funding. The resulting vector is the external funding amounts.
    
    dat$external_funding <- pmin(dat$max_external_funding, dat$fundability)
    
    # Calculate the organization project cost as the total project cost minus
    # external funding.
    
    dat$cost_organization <- dat$cost_total_project - dat$external_funding
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) 
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_global$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
    # Costs
    
    results_cost_total[which(results_cost_total$org_program == org_program), "mean"] <- mean(dat$cost_total_project) 
    results_cost_total[which(results_cost_total$org_program == org_program), "P5"] <- quantile(dat$cost_total_project, 0.05)
    results_cost_total[which(results_cost_total$org_program == org_program), "P50"] <- quantile(dat$cost_total_project, 0.50)
    results_cost_total[which(results_cost_total$org_program == org_program), "P95"] <- quantile(dat$cost_total_project, 0.95)
    
    results_cost_organization[which(results_cost_organization$org_program == org_program), "mean"] <- mean(dat$cost_organization)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P5"] <- quantile(dat$cost_organization, 0.05)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P50"] <- quantile(dat$cost_organization, 0.50)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P95"] <- quantile(dat$cost_organization, 0.95)
    
    # Benefits
    
    results_benefit_national[which(results_benefit_national$org_program == org_program), "mean"] <- mean(dat$benefit_national_org)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P5"] <- quantile(dat$benefit_national_org, 0.05)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P50"] <- quantile(dat$benefit_national_org, 0.50)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P95"] <- quantile(dat$benefit_national_org, 0.95)
    
    results_benefit_global[which(results_benefit_global$org_program == org_program), "mean"] <- mean(dat$benefit_global_org)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P5"] <- quantile(dat$benefit_global_org, 0.05)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P50"] <- quantile(dat$benefit_global_org, 0.50)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P95"] <- quantile(dat$benefit_global_org, 0.95)
    
    # Store the dat in a list
    dat_list[[i]] <- dat
    
  }
  
  ######## Calculating ranking #####
  
  # Calculate where would rank for each of the national and global
  results_overall$BCR_national_EV_rank <- rank(-results_overall$BCR_national_EV)
  results_overall$BCR_global_EV_rank <- rank(-results_overall$BCR_global_EV)
  
  return(list(results_overall, 
              results_cost_total, results_cost_organization,
              results_benefit_national, results_benefit_global, 
              results_BCR_national,
              results_BCR_global,
              dat_list)) 
} # End cba_GplusD_CurrentGS_epsilon function


######## Analysis for calculating cost benefit ratios based on conservation gains relative to long term potential gains relative to current GS plus arbitrary epsilon ########
#' Cost benefit analysis with benefits relative to both long term potential and current Green Score plus epsilon
#' 
#' This function performs the cost benefit analysis using an advanced method wherein the benefits, calculated as Conservation Gain plus Conservation
#' Dependence, are relative to both the long term potential and current the Green Score of the species while also incorporating epsilon. Epsilon is 
#' subjective value that is added to the current Green Score in order to accommodate species that have a current Green Score of zero.
#' 
#' @param org_programs A vector of the conservation program names
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param functional_score_max The maximum possible score for a spatial unit
#' @return Returns a list of dataframes containing all the different results
#' @examples
#' cba_GplusD_LongTermPot_CurrentGS_epsilon(org_programs,inputs,functional_score_max)
#' @export

cba_GplusD_LongTermPot_CurrentGS_epsilon<-function(org_programs, inputs, functional_score_max){
  analysisoption<<-6
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank" 
                                 )
  results_overall$org_program <- org_programs 
  
  # Summary object for national BCR results
  results_BCR_national <- as.data.frame(matrix(nrow = length(org_programs), ncol = 5))
  colnames(results_BCR_national) <- c("org_program", "mean", "P5", "P50", "P95")
  results_BCR_national$org_program <- org_programs 
  
  # Summary set up for the global results 
  # will be the same as the national results
  results_BCR_global <- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_total<- results_BCR_national
  
  # Summary set up for the cost-specific results 
  # will be the same as the national results
  results_cost_organization <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_national <- results_BCR_national
  
  # Summary set up for the benefit-specific results 
  # will be the same as the national results
  results_benefit_global <- results_BCR_national
  
  # initialize something to hold all the simulated inputs for sensitivity 
  # and the results too if needed for each program
  dat_list <- list() 
  
  #TESTING TESTING
  
  
  #####Main Simulation #####
  for (i in 1:length(org_programs)) {
    # Identify which program we are doing now
    org_program <- as.character(org_programs[i])
    print(paste("Running cba_GplusD_LongTermPot_CurrentGS_epsilon simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- (dat$GSGainPlusDependence / dat$GSlongtermAspiration) * 100
    
    dat$benefit_national_org <-  (dat$GS_Benefit_national * dat$OrgBenefit) / (dat$GScurrentNational + epsilon)
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    #GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    #GS_numerator_assessed <- dat$GSGainPlusDependence * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    #n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    #GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    #dat$GSGlobalGainPlusDependence <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Rescaling to long term potential and current global GS plus epsilon
    #dat$GS_Benefit_global <- (dat$GSGlobalGainPlusDependence / dat$GSlongtermAspiration) * 100 
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program. Followed by rescaling to currentGS plus epsilon
    
    #dat$benefit_global_org <- (dat$GS_Benefit_global * dat$OrgBenefit) / (dat$GScurrentGlobal + epsilon)

    ######## NEW - Calculate the global benefit for each iteration ########
    
    dat$benefit_global_org <- dat$benefit_national_org * dat$species_range_pct_in_nation/100
    
    ######## Calculate the costs ########
    
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    # Calculate the maximum amount of external funding that our
    # organization will accept (i.e. multiply the maximum funding proportion
    # of total cost with the total project cost).
    
    dat$max_external_funding <- dat$max_prop_total_external_funding*dat$cost_total_project
    
    # Take the minimum of the fundability and the maximum amount of external
    # funding. The resulting vector is the external funding amounts.
    
    dat$external_funding <- pmin(dat$max_external_funding, dat$fundability)
    
    # Calculate the organization project cost as the total project cost minus
    # external funding.
    
    dat$cost_organization <- dat$cost_total_project - dat$external_funding
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) 
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "SD"] <- sd(dat$BCR_national)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "SD"] <- sd(dat$BCR_global)
    
    results_overall[which(results_BCR_global$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
    # Costs
    
    results_cost_total[which(results_cost_total$org_program == org_program), "mean"] <- mean(dat$cost_total_project) 
    results_cost_total[which(results_cost_total$org_program == org_program), "P5"] <- quantile(dat$cost_total_project, 0.05)
    results_cost_total[which(results_cost_total$org_program == org_program), "P50"] <- quantile(dat$cost_total_project, 0.50)
    results_cost_total[which(results_cost_total$org_program == org_program), "P95"] <- quantile(dat$cost_total_project, 0.95)
    results_cost_total[which(results_cost_total$org_program == org_program), "SD"] <- sd(dat$cost_total_project)
     
    results_cost_organization[which(results_cost_organization$org_program == org_program), "mean"] <- mean(dat$cost_organization)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P5"] <- quantile(dat$cost_organization, 0.05)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P50"] <- quantile(dat$cost_organization, 0.50)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "P95"] <- quantile(dat$cost_organization, 0.95)
    results_cost_organization[which(results_cost_organization$org_program == org_program), "SD"] <-sd(dat$cost_organization)
    # Benefits
    
    results_benefit_national[which(results_benefit_national$org_program == org_program), "mean"] <- mean(dat$benefit_national_org)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P5"] <- quantile(dat$benefit_national_org, 0.05)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P50"] <- quantile(dat$benefit_national_org, 0.50)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "P95"] <- quantile(dat$benefit_national_org, 0.95)
    results_benefit_national[which(results_benefit_national$org_program == org_program), "SD"] <- sd(dat$benefit_national_org) 
    
    results_benefit_global[which(results_benefit_global$org_program == org_program), "mean"] <- mean(dat$benefit_global_org)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P5"] <- quantile(dat$benefit_global_org, 0.05)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P50"] <- quantile(dat$benefit_global_org, 0.50)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "P95"] <- quantile(dat$benefit_global_org, 0.95)
    results_benefit_global[which(results_benefit_global$org_program == org_program), "SD"] <-sd(dat$benefit_global_org)
    # Store the dat in a list
    dat_list[[i]] <- dat
    
  }
  
  ######## Calculating ranking #####
  
  # Calculate where would rank for each of the national and global
  results_overall$BCR_national_EV_rank <- rank(-results_overall$BCR_national_EV)
  results_overall$BCR_global_EV_rank <- rank(-results_overall$BCR_global_EV)
  
  return(list(results_overall, 
              results_cost_total, results_cost_organization,
              results_benefit_national, results_benefit_global, 
              results_BCR_national,
              results_BCR_global,
              dat_list)) 
} # End of cba_GplusD_LongTermPot_CurrentGS_epsilon function

