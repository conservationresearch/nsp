#This file contains the main cost-benefit functions 
#Dylan Cole
#September 2021
#Incorporated code written by Laura Keating (July 2021) 



#####Cost Benefit Analysis #####

#### Benefit Options #####
#Users are able to select different metrics to examine the cost/benefit effectiveness

######## Analysis for calculating cost benefit ratios based only on conservation gains ########
#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
cba_cgain<-function(org_programs, inputs, functional_score_max, sensitivity){
  
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 7))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank", 
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
    print(paste("Running cgain simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainGplusD
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$organization_portion_benefit
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Relabelling for consistency
    dat$GS_Benefit_global <- dat$GSGlobalGainWithDynamicBaseline
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    dat$benefit_global_org <- dat$GS_Benefit_global * dat$organization_portion_benefit
    
    ######## Calculate the costs ########
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
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
  
  ##### Sensitivity Analysis ########
} #End of cba_cgain function

#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
######## Analysis for calculating cost benefit ratios based on conservation gains relative to long term aspirations
cba_cgain_longtermasp <- function(org_programs, inputs, functional_score_max, sensitivity){ 
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 7))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank", 
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
    print(paste("Running cgain_longtermasp simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainGplusD / dat$GSlongtermAspiration * 100
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$organization_portion_benefit
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Rescaling to long term aspirations
    dat$GS_Benefit_global <- dat$GSGlobalGainWithDynamicBaseline / dat$GSlongtermAspiration * 100
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    dat$benefit_global_org <- dat$GS_Benefit_global * dat$organization_portion_benefit
    
    
    ######## Calculate the costs ########
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
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
} # End of cba_cgain_longtermasp function simulation

#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
######## Analysis for calculating cost benefit ratios based only on conservation gains binned into high, medium, low, zero gains ########
cba_cgain_binnedbycgain <- function(org_programs, inputs, functional_score_max, sensitivity){
  
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
    print(paste("Running cgain_binnedbycgain simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainGplusD
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$organization_portion_benefit
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Relabelling for consistency
    dat$GS_Benefit_global <- dat$GSGlobalGainWithDynamicBaseline
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    dat$benefit_global_org <- dat$GS_Benefit_global * dat$organization_portion_benefit
    
    ######## Calculate the costs ########
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
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
} # End of cba_cgain_binnedbycgain

#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
######## Analysis for calculating cost benefit ratios based on conservation gains relative to long term aspirations binned by current GS ########

cba_cgain_longtermasp_binnedbyGS<-function(org_programs, inputs, functional_score_max, sensitivity){
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
    print(paste("Running cgain_longtermasp_binnedbyGS simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- dat$GSGainGplusD / dat$GSlongtermAspiration * 100
    
    dat$benefit_national_org <-  dat$GS_Benefit_national * dat$organization_portion_benefit
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Rescaling to long term aspirations
    dat$GS_Benefit_global <- dat$GSGlobalGainWithDynamicBaseline / dat$GSlongtermAspiration * 100
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    dat$benefit_global_org <- dat$GS_Benefit_global * dat$organization_portion_benefit
    
    
    ######## Calculate the costs ########
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
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
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==2)] <- "Crtically Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==3)] <- "Largely Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==4)] <- "Moderately Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==5)] <- "Slightly Depleted"
  results_overall$bin_GScurrentNational[which(results_overall$bin_GScurrentNational==6)] <- "Fully Recovered"
  
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==1)] <- "Extinct in the Wild"
  results_overall$bin_GScurrentGlobal[which(results_overall$bin_GScurrentGlobal==2)] <- "Crtically Depleted"
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
} # End of cba_cgain_longtermasp_binnedbyGS function simulation

#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
####### Analysis for calculating cost benefit ratios based on conservation gains relative to current GS for extant species only ########

cba_cgain_currentGS<-function(org_programs, inputs, functional_score_max, sensitivity){
  
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 7))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank", 
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
    # Cannot divide by 0, so if 0 is found in GScurrentNational, skips this species. 
    if (inputs$highP95[which(inputs$subcategory=="GScurrentNational" & inputs$species==org_program)] == 0
        && inputs$baseP50[which(inputs$subcategory=="GScurrentNational" & inputs$species==org_program)] == 0
        && inputs$lowP5[which(inputs$subcategory=="GScurrentNational" & inputs$species==org_program)] == 0)
    {print(paste("Skipping cgain_currentGS simulation for", org_program))
      next}
    else {
      print(paste("Running cgain_currentGS simulation for", org_program))
      
      # Do parameter prep from the input distributions to get one set of parameters
      # to use for each iteration
      dat <- parameter_prep(inputs = inputs,
                            org_program = org_program,
                            number_of_simulations = number_of_simulations)
      
      ######## Calculate the national benefit for each iteration ########
      
      # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
      # Calculate our portion of the benefit
      
      dat$GS_Benefit_national <- dat$GSGainGplusD
      
      dat$benefit_national_org <-  (dat$GS_Benefit_national * dat$organization_portion_benefit) / dat$GScurrentNational
      
      ######## Calculate the global benefit for each iteration ########
      
      # Calculate what the assessed denominator is
      GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
      
      # Calculate what the assessed numerator is
      GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
      
      # Calculate how many spatial units would be in the larger scale if they are
      # proportional to the smaller scale and the range
      n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
      
      # Calculate what the scaled denominator is
      GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
      
      # Calculate new value for scaled global
      dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
      
      # Relabel for consistency
      dat$GS_Benefit_global <- dat$GSGlobalGainWithDynamicBaseline 
      
      # Adjust the global benefit of the organization's program to reflect the organization portion by
      # multiplying the organizations global benefit vector by the vector with the percent
      # contribution from the organization. The result is the organization's global benefit of
      # the program.
      
      dat$benefit_global_org <- (dat$GS_Benefit_global * dat$organization_portion_benefit) / dat$GScurrentGlobal
      
      ######## Calculate the costs ########
      # Note: this is currently done as prework in the paramater draws function
      # But might be clearer if pull it out and put here
      
      ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
      
      # Divide the organization's national benefit of the program by the
      # organizational project cost to get the national Benefit to Cost ratio (BCR).
      dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
      
      #Do the same for the organization's global benefit.
      dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
      
      ######## Calculate the BCR probability-weighted average and credible intervals. ########
      # Store the various pieces of the results separately for easy use later
      
      # National
      results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
      results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
      results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
      results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
      
      results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
      
      # Global
      
      results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
      results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
      results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
      results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
      
      results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
      
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
      
    } }
  
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
  
} #End cba_cgain_currentGS function simulation

#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
####### Analysis for calculating cost benefit ratios based on conservation gains relative to long term aspirations gains relative to current GS for extant species only  ########

cba_cgain_longtermasp_currentGS<-function(org_programs, inputs, functional_score_max, sensitivity){
  
  ##### Creating empty df to hold results #####
  
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 7))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank", 
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
    if (inputs$highP95[which(inputs$subcategory=="GScurrentNational" & inputs$species==org_program)] == 0
        && inputs$baseP50[which(inputs$subcategory=="GScurrentNational" & inputs$species==org_program)] == 0
        && inputs$lowP5[which(inputs$subcategory=="GScurrentNational" & inputs$species==org_program)] == 0)
    {print(paste("Skipping cgain_longtermasp_currentGS simulation for", org_program))
      next}
    else {print(paste("Running cgain_longtermasp_currentGS simulation for", org_program))
      
      # Do parameter prep from the input distributions to get one set of parameters
      # to use for each iteration
      dat <- parameter_prep(inputs = inputs,
                            org_program = org_program,
                            number_of_simulations = number_of_simulations)
      
      ######## Calculate the national benefit for each iteration ########
      
      # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
      # Calculate our portion of the benefit
      
      dat$GS_Benefit_national <- (dat$GSGainGplusD / dat$GSlongtermAspiration) * 100
      
      dat$benefit_national_org <-  (dat$GS_Benefit_national * dat$organization_portion_benefit ) / dat$GScurrentNational
      
      ######## Calculate the global benefit for each iteration ########
      
      # Calculate what the assessed denominator is
      GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
      
      # Calculate what the assessed numerator is
      GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
      
      # Calculate how many spatial units would be in the larger scale if they are
      # proportional to the smaller scale and the range
      n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
      
      # Calculate what the scaled denominator is
      GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
      
      # Calculate new value for scaled global
      dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
      
      # Rescaling to long term aspirations and current global GS 
      dat$GS_Benefit_global <- (dat$GSGlobalGainWithDynamicBaseline / dat$GSlongtermAspiration) * 100 
      
      # Adjust the global benefit of the organization's program to reflect the organization portion by
      # multiplying the organizations global benefit vector by the vector with the percent
      # contribution from the organization. The result is the organization's global benefit of
      # the program.
      
      dat$benefit_global_org <- (dat$GS_Benefit_global * dat$organization_portion_benefit) / dat$GScurrentGlobal
      
      
      ######## Calculate the costs ########
      # Note: this is currently done as prework in the paramater draws function
      # But might be clearer if pull it out and put here
      
      ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
      
      # Divide the organization's national benefit of the program by the
      # organizational project cost to get the national Benefit to Cost ratio (BCR).
      dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
      
      #Do the same for the organization's global benefit.
      dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
      
      ######## Calculate the BCR probability-weighted average and credible intervals. ########
      # Store the various pieces of the results separately for easy use later
      
      # National
      results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
      results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
      results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
      results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
      
      results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
      
      # Global
      
      results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
      results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
      results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
      results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
      
      results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
      
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
      
    } }
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
  
} # End of cba_cgain_longtermasp_currentGS function simulation

#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
####### Analysis for calculating cost benefit ratios based on conservation gains relative to current GS plus arbitrary epsilon ########

cba_cgain_currentGS_epsilon<-function(org_programs, inputs, functional_score_max, sensitivity){
  
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 7))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank", 
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
    print(paste("Running cgains_currentGS_epsilon simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate organization portion of the benefit
    
    # Adding 0.1 to account for extirpated species 
    dat$GS_Benefit_national <- dat$GSGainGplusD 
    
    dat$benefit_national_org <-  (dat$GS_Benefit_national * dat$organization_portion_benefit ) / (dat$GScurrentNational + 0.1)
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Calcuate global benefit relative to current global GS (adding 0.1 for extirpated species)
    dat$GS_Benefit_global <- dat$GSGlobalGainWithDynamicBaseline / (dat$GScurrentGlobal + 0.1)
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program.
    
    dat$benefit_global_org <- dat$GS_Benefit_global * dat$organization_portion_benefit
    
    ######## Calculate the costs ########
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
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
} #End cba_cgain_currentgs_epsilon function simulation

#' This function performs the cost benefit analysis for an organizations new species prioritization process. 
#' 
#' 
#' @param org_programs A vector of the species names included in the analysis
#' @param inputs A dataframe containing the organization's different programs costs and benefits for each species 
#' @param functional_score_max TO DO
#' @param sensitivity TO DO
#' @return TO DO
#' 
####### Analysis for calculating cost benefit ratios based on conservation gains relative to long term aspirations gains relative to current GS plus arbitrary epsilon ########

cba_cgain_longtermasp_currentGS_epsilon<-function(org_programs, inputs, functional_score_max, sensitivity){
  
  ##### Creating empty df to hold results #####
  results_overall <- as.data.frame(matrix(nrow = length(org_programs), ncol = 7))
  colnames(results_overall) <- c("org_program", "BCR_national_EV","BCR_national_EV_rank",  
                                 "BCR_global_EV","BCR_global_EV_rank", 
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
  
  #####Main Simulation #####
  for (i in 1:length(org_programs)) {
    # Identify which program we are doing now
    org_program <- as.character(org_programs[i])
    print(paste("Running cgain_longtermasp_currentGS_epsilon simulation for", org_program))
    
    # Do parameter prep from the input distributions to get one set of parameters
    # to use for each iteration
    dat <- parameter_prep(inputs = inputs,
                          org_program = org_program,
                          number_of_simulations = number_of_simulations)
    
    ######## Calculate the national benefit for each iteration ########
    
    # G relative to dynamic baseline done in the pre-work, for clarity may want to pull out TO DO
    # Calculate our portion of the benefit
    
    dat$GS_Benefit_national <- (dat$GSGainGplusD / dat$GSlongtermAspiration) * 100
    
    dat$benefit_national_org <-  (dat$GS_Benefit_national * dat$organization_portion_benefit) / (dat$GScurrentNational+0.1)
    
    ######## Calculate the global benefit for each iteration ########
    
    # Calculate what the assessed denominator is
    GS_denominator_assessed <- dat$GSnSpatialUnits * functional_score_max
    
    # Calculate what the assessed numerator is
    GS_numerator_assessed <- dat$GSGainGplusD * GS_denominator_assessed/100
    
    # Calculate how many spatial units would be in the larger scale if they are
    # proportional to the smaller scale and the range
    n_spatial_units_scaled <-  dat$GSnSpatialUnits / (dat$species_range_pct_in_nation/100)
    
    # Calculate what the scaled denominator is
    GS_denominator_scaled <- functional_score_max * n_spatial_units_scaled
    
    # Calculate new value for scaled global
    dat$GSGlobalGainWithDynamicBaseline <- GS_numerator_assessed / GS_denominator_scaled * 100
    
    # Rescaling to long term aspirations and current global GS plus epsilon
    dat$GS_Benefit_global <- (dat$GSGlobalGainWithDynamicBaseline / dat$GSlongtermAspiration) * 100 
    
    # Adjust the global benefit of the organization's program to reflect the organization portion by
    # multiplying the organizations global benefit vector by the vector with the percent
    # contribution from the organization. The result is the organization's global benefit of
    # the program. Followed by rescaling to currentGS plus 0.1 for extirpated species
    
    dat$benefit_global_org <- (dat$GS_Benefit_global * dat$organization_portion_benefit) / (dat$GScurrentGlobal+0.1)
    
    
    ######## Calculate the costs ########
    # Note: this is currently done as prework in the paramater draws function
    # But might be clearer if pull it out and put here
    
    ######## Calculate the Benefit to Cost Ratios (BCRs) for each iteration ########
    
    # Divide the organization's national benefit of the program by the
    # organizational project cost to get the national Benefit to Cost ratio (BCR).
    dat$BCR_national <- dat$benefit_national_org / dat$cost_organization
    
    #Do the same for the organization's global benefit.
    dat$BCR_global <- dat$benefit_global_org / dat$cost_organization
    
    ######## Calculate the BCR probability-weighted average and credible intervals. ########
    # Store the various pieces of the results separately for easy use later
    
    # National
    results_BCR_national[which(results_BCR_national$org_program == org_program), "mean"] <- mean(dat$BCR_national) # TO DO: Consider deleting these and just storing in overall unless planning to plot theses
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P5"] <- quantile(dat$BCR_national, 0.05)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P50"] <- quantile(dat$BCR_national, 0.50)
    results_BCR_national[which(results_BCR_national$org_program == org_program), "P95"] <- quantile(dat$BCR_national, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_national_EV"] <- mean(dat$BCR_national)
    
    # Global
    
    results_BCR_global[which(results_BCR_global$org_program == org_program), "mean"] <- mean(dat$BCR_global)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P5"] <- quantile(dat$BCR_global, 0.05)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P50"] <- quantile(dat$BCR_global, 0.50)
    results_BCR_global[which(results_BCR_global$org_program == org_program), "P95"] <- quantile(dat$BCR_global, 0.95)
    
    results_overall[which(results_BCR_national$org_program == org_program), "BCR_global_EV"] <- mean(dat$BCR_global)
    
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
} # End of cba_cgain_longtermasp_currentGS_epsilon function simulation

