# This file contains functions related to prepping the input data
# Dylan Cole
# August 2021
# Primarily using code written by Laura Keating (July 2021) with some changes/cleanup

#####   parameter_prep   #####

#' Create a data frame with the number of iterations as rows and the inputs as columns.
#' Populate with randomly drawn values from the relevant distributions for each uncertain input.
#' Also does some preliminary calculations on costs and includes the results as they are used
#' for calculating the fundability parameter draws.
#' 
#' 
#' 
#' 
#' 
#' 

####Inputs loaded in here? wrapper script should include argument about where to find data?
# Need to change once all ready for manuscript to 
inputs<-read.csv(file="C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Inputs/inputs_manuscript.csv")

#Outlining what are organization_programs
#org_program <- unique(inputs$species) 
#org_programs <- org_programs[which(org_programs != "N/A")]

##### Main function #####
parameter_prep <- function(inputs, org_program, G_and_A_prop,
                           number_of_simulations = 10000){
  
  # Calls the specific species data when the input species is equal to the org_program species in question
  inputs_parameter_prep <- inputs[which(inputs$species == org_program),]
  
  
  ##### Create a data frame to store the results #####
  
  dat_cost <- as.data.frame(matrix(nrow=number_of_simulations, ncol=11))
  colnames(dat_cost) <- c("cost_CB", "cost_RR",
                          "max_prop_total_external_funding", "G_and_A_prop_of_total",
                          "cost_total_project_without_G_and_A", "G_and_A",
                          "cost_total_project",  "fundability",
                          "max_external_funding", "external_funding", "cost_organization")
  
  dat_benefit <- as.data.frame(matrix(nrow=number_of_simulations, ncol=6))
  colnames(dat_benefit) <- c("GSGainWithDynamicBaseline", "GSlongtermAspiration", 
                             "GScurrentNational", "GScurrentGlobal",
                             "organization_portion_benefit", 
                             "species_range_pct_in_nation")
  
  ######  Write functions for different types of simulations we will do below #####
  
  simulate_bounded_continuous_distribution <- function(P5, P50, P95, lower_bound, upper_bound, number_of_simulations){
    
    # If all three quantiles are the same, use a uniform distribution
    if(P5 == P50 && P50 == P95){
      results_vector <- rep(P50, number_of_simulations)
    }
    
    # If two of the three quantiles are the same, use a 2 term metalog
    if(xor(P5 == P50,P50 == P95)){
      metalog_dist <- rmetalog::metalog(c(P5,P50,P95),
                                        term_limit = 3,
                                        bounds=c(lower_bound,upper_bound), 
                                        boundedness = 'b',
                                        probs = c(0.05, 0.5, 0.95))
      
      results_vector <- rmetalog::rmetalog(metalog_dist, number_of_simulations, term = 2)
    }
    
    # If all three quantiles are different, use a 3 term metalog
    if(P5 != P50 && P50 != P95){
      metalog_dist <- rmetalog::metalog(c(P5,P50,P95),
                                        term_limit = 3,
                                        bounds=c(lower_bound,upper_bound), 
                                        boundedness = 'b',
                                        probs = c(0.05, 0.5, 0.95))
      
      results_vector <- rmetalog::rmetalog(metalog_dist, number_of_simulations, term = 3)
    }
    return(results_vector)
  }
  
  simulate_lowerbounded_continuous_distribution <- function(P5, P50, P95, lower_bound, number_of_simulations){
    
    # If all three quantiles are the same, use a uniform distribution
    if(P5 == P50 && P50 == P95){ 
      results_vector <- rep(P50, number_of_simulations)
    }
    # If two of the three quantiles are the same, use a 2 term metalog
    if(xor(P5 == P50,P50 == P95)){ 
      metalog_dist <- rmetalog::metalog(c(P5,P50,P95),
                                        term_limit = 3,
                                        bounds=c(lower_bound), 
                                        boundedness = 'sl',
                                        probs = c(0.05, 0.5, 0.95))
      
      results_vector <- rmetalog::rmetalog(metalog_dist, number_of_simulations, term = 2)
    }
    
    # If all three quantiles are different, use a 3 term metalog
    if(P5 != P50 && P50 != P95){ 
      metalog_dist <- rmetalog::metalog(c(P5,P50,P95),
                                        term_limit = 3,
                                        bounds=c(lower_bound), 
                                        boundedness = 'sl',
                                        probs = c(0.05, 0.5, 0.95))
      
      results_vector <- rrmetalog::metalog(metalog_dist, number_of_simulations, term = 3)
    }
    return(results_vector)
  }
  
  # Write a function to apply a metalog distribution to the benefit inputs   
  
  metalogSelectBenefit <- function(P5, P50, P95, number_of_simulations){
    if(P5 != P50 | P50 != P95){
      metalog_dist <- rmetalog::metalog(c(P5,P50,P95), 
                                        term_limit = 3,
                                        bounds=c(max(-100, P5 - abs(2*P5-0.0001)), min(2*P95, 100)),  # changed from before to be more realistic for GplusD; added the -0.0001 in case the P5 is 0 since P5 and lower bound can not be the same
                                        boundedness = 'b',
                                        probs = c(0.05, 0.5, 0.95))
      results_vector <- rmetalog::rmetalog(metalog_dist, number_of_simulations, term = 3)    
    }
    
    # If low, base, and high are all the same then use a uniform distribution
    if(P5 == P50 && P50 == P95){ # then this is a uniform distribution
      results_vector <- rep(P50, number_of_simulations)
    }
    return(results_vector)
  }
  
  ######  Fill in dat_cost with the relevant parameter draws #####
  
  # Captive breeding costs
  
  cost_CB_total_low <- as.numeric(inputs_parameter_prep$lowP5[which(inputs_parameter_prep$category == "Costs" &
                                                                      inputs_parameter_prep$subcategory== "Captive Breeding")])
  
  cost_CB_total_base <- as.numeric(inputs_parameter_prep$baseP50[which(inputs_parameter_prep$category == "Costs" &
                                                                         inputs_parameter_prep$subcategory== "Captive Breeding")])
  
  cost_CB_total_high <- as.numeric(inputs_parameter_prep$highP95[which(inputs_parameter_prep$category == "Costs" &
                                                                         inputs_parameter_prep$subcategory== "Captive Breeding")])
  
  dat_cost$cost_CB <- simulate_bounded_continuous_distribution(P5 = cost_CB_total_low, 
                                                               P50 = cost_CB_total_base, 
                                                               P95 = cost_CB_total_high, 
                                                               lower_bound = 0, 
                                                               upper_bound = 2*cost_CB_total_high,
                                                               number_of_simulations = number_of_simulations)
  
  # Release and Research costs
  
  cost_RR_total_low <- as.numeric(inputs_parameter_prep$lowP5[which(inputs_parameter_prep$category == "Costs" &
                                                                      inputs_parameter_prep$subcategory== "Release and Research")])
  
  cost_RR_total_base <- as.numeric(inputs_parameter_prep$baseP50[which(inputs_parameter_prep$category == "Costs" &
                                                                         inputs_parameter_prep$subcategory== "Release and Research")])
  
  cost_RR_total_high <- as.numeric(inputs_parameter_prep$highP95[which(inputs_parameter_prep$category == "Costs" &
                                                                         inputs_parameter_prep$subcategory== "Release and Research")])
  
  dat_cost$cost_RR <- simulate_bounded_continuous_distribution(P5 = cost_RR_total_low, 
                                                               P50 = cost_RR_total_base, 
                                                               P95 = cost_RR_total_high, 
                                                               lower_bound = 0, 
                                                               upper_bound = 2*cost_RR_total_high,
                                                               number_of_simulations = number_of_simulations)
  
  
  
  ### 
  ### Check with Jana to determine if we actually want this included in the manuscript code or just in our CZ version
  # Maximum proportion of total external funding allowed
  max_pct_total_external_funding_low <- 100 - as.numeric(as.character(inputs$highP95[which(inputs$type == "CZ Skin in the Game")]))
  max_pct_total_external_funding_base <- 100 - as.numeric(as.character(inputs$baseP50[which(inputs$type == "CZ Skin in the Game")]))
  max_pct_total_external_funding_high <- 100 - as.numeric(as.character(inputs$lowP5[which(inputs$type == "CZ Skin in the Game")]))
  
  dat_cost$max_prop_total_external_funding <- simulate_bounded_continuous_distribution(P5 = max_pct_total_external_funding_low,
                                                                                       P50 = max_pct_total_external_funding_base,
                                                                                       P95 = max_pct_total_external_funding_high,
                                                                                       lower_bound = 0,
                                                                                       upper_bound = 100,
                                                                                       number_of_simulations = number_of_simulations)/100 # convert the results from percent to proportion
  
  
  #Do we need this??? Should be already factored into the cost they are providing in the Excel inputs file
  #NEED TO FIGURE THIS PART OUT - UNCERTAINITY LIES IN THE % of TOTAL COST THAT WILL BE ADDED ON AS EXTRA. IE. Will need an additional 15% of
  #total cost to go to G&A - so first need to calculate the project cost then take % of that and add it on to get total project cost. 
  # General and Admin costs
  #G_and_A_prop_total_low <- as.numeric(as.character(inputs$lowP5[which(inputs$type == "General and Administrative")]))/100 # note, our input here didn't have uncertainty but we built in the flexibility to change it later if we want to.
  #G_and_A_prop_total_base <- as.numeric(as.character(inputs$baseP50[which(inputs$type == "General and Administrative")]))/100
  #G_and_A_prop_total_high <- as.numeric(as.character(inputs$highP95[which(inputs$type == "General and Administrative")]))/100
  
  #dat_cost$G_and_A_prop_of_total <- simulate_bounded_continuous_distribution(P5 = G_and_A_prop_total_low, 
  #                                                                          P50 = G_and_A_prop_total_base,
  #                                                                         P95 = G_and_A_prop_total_high, 
  #                                                                        lower_bound = 0,
  #                                                                       upper_bound = 1,
  #                                                                      number_of_simulations = number_of_simulations)
  
  # Add the cost inputs together to get a total project cost 
  dat_cost$G_and_A_prop_of_total<-G_and_A_prop_of_total
  dat_cost$cost_total_project_without_G_and_A <-  dat_cost$cost_CB + dat_cost$cost_RR
  dat_cost$G_and_A <- dat_cost$cost_total_project_without_G_and_A*G_and_A_prop_of_total
  dat_cost$cost_total_project <- dat_cost$cost_total_project_without_G_and_A + dat_cost$G_and_A
  
  
  
  
  # Calculate fundability (i.e. external funding potential)
  
  fundability_low <- as.numeric(inputs_parameter_prep$lowP5[which(inputs_parameter_prep$subcategory== "Fundability")])
  fundability_base <- as.numeric(inputs_parameter_prep$baseP50[which(inputs_parameter_prep$subcategory== "Fundability")])
  fundability_high <- as.numeric(inputs_parameter_prep$highP95[which(inputs_parameter_prep$subcategory== "Fundability")])
  
  if(fundability_low == 0){
    fundability_low <- 0.1 # can't have it be 0 as lower bound is 0 so make it 10 cents
  }
  if(fundability_base == 0){
    fundability_base <- 0.1 # same reason as above
  }
  if(fundability_high == 0){
    fundability_high <- 0.1 # same reason as above
  }
  
  dat_cost$fundability <- simulate_bounded_continuous_distribution(P5 = fundability_low, 
                                                                   P50 = fundability_base,
                                                                   P95 = fundability_high, 
                                                                   lower_bound = 0,
                                                                   upper_bound = 2*fundability_high,
                                                                   number_of_simulations = number_of_simulations)
  
  # Calculate the maximum amount of external funding that our
  # organization will accept (i.e. multiply the maximum funding proportion
  # of total cost with the total project cost).
  
  dat_cost$max_external_funding <- dat_cost$max_prop_total_external_funding*dat_cost$cost_total_project
  
  # Take the minimum of the fundability and the maximum amount of external
  # funding. The resulting vector is the external funding amounts.
  
  dat_cost$external_funding <- pmin(dat_cost$max_external_funding, dat_cost$fundability)
  
  # Calculate the organization project cost as the total project cost minus
  # external funding.
  
  dat_cost$cost_organization <- dat_cost$cost_total_project - dat_cost$external_funding
  
  
  
  
  ######  Fill in dat_benefit with the relevant parameter draws #####
  
  # Benefits
  
  row_GSGainWithDynamicBaseline <- which(inputs_parameter_prep$subcategory == "GSGainWithDynamicBaseline")
  benefit_low <- as.numeric(inputs_parameter_prep$lowP5[row_GSGainWithDynamicBaseline])
  benefit_base <- as.numeric(inputs_parameter_prep$baseP50[row_GSGainWithDynamicBaseline])
  benefit_high <- as.numeric(inputs_parameter_prep$highP95[row_GSGainWithDynamicBaseline])
  
  dat_benefit$GSGainWithDynamicBaseline <- metalogSelectBenefit(P5 = benefit_low,
                                                                P50 = benefit_base,
                                                                P95 = benefit_high,
                                                                number_of_simulations = number_of_simulations)
  
  #In rare scenario where benefit < 0, making it 0 
  dat_benefit$GSGainWithDynamicBaseline[which(dat_benefit$GSGainWithDynamicBaseline <0)] <- 0
  
  # Long-term aspiration
  row_longterm_aspiration <- which(inputs_parameter_prep$subcategory== "GSlongtermAspiration")
  longterm_aspiration_low <- as.numeric(inputs_parameter_prep$lowP5[row_longterm_aspiration])
  longterm_aspiration_base <- as.numeric(inputs_parameter_prep$baseP50[row_longterm_aspiration])
  longterm_aspiration_high <- as.numeric(inputs_parameter_prep$highP95[row_longterm_aspiration])
  
  
  dat_benefit$GSlongtermAspiration <- metalogSelectBenefit(P5 = longterm_aspiration_low,
                                                           P50 = longterm_aspiration_base, 
                                                           P95 = longterm_aspiration_high,
                                                           number_of_simulations = number_of_simulations)
  
  # current - national
  row_current_national <- which(inputs_parameter_prep$subcategory== "GScurrentNational")
  current_national_low <- as.numeric(inputs_parameter_prep$lowP5[row_current_national])
  current_national_base <- as.numeric(inputs_parameter_prep$baseP50[row_current_national])
  current_national_high <- as.numeric(inputs_parameter_prep$highP95[row_current_national])
  
  
  dat_benefit$GScurrentNational <- metalogSelectBenefit(P5 = current_national_low,
                                                        P50 = current_national_base, 
                                                        P95 = current_national_high,
                                                        number_of_simulations = number_of_simulations)
  
  # current - global
  row_current_global <- which(inputs_parameter_prep$subcategory== "GScurrentGlobal")
  current_global_low <- as.numeric(inputs_parameter_prep$lowP5[row_current_global])
  current_global_base <- as.numeric(inputs_parameter_prep$baseP50[row_current_global])
  current_global_high <- as.numeric(inputs_parameter_prep$highP95[row_current_global])
  
  
  dat_benefit$GScurrentGlobal <- metalogSelectBenefit(P5 = current_global_low,
                                                      P50 = current_global_base, 
                                                      P95 = current_global_high,
                                                      number_of_simulations = number_of_simulations)
  
  # Identify how many spatial units were used for the assessment
  dat_benefit$GSnSpatialUnits <- as.numeric(inputs_parameter_prep$baseP50[which(inputs_parameter_prep$subcategory == "GSnSpatialUnits")])
  
  #populate the organization portion of the benefit
  pct_benefit_organization_low <- as.numeric(gsub("%", "", inputs_parameter_prep$lowP5[which(inputs_parameter_prep$type == "pct_benefit_organization")]))
  pct_benefit_organization_base <- as.numeric(gsub("%", "", inputs_parameter_prep$baseP50[which(inputs_parameter_prep$type == "pct_benefit_organization")]))
  pct_benefit_organization_high <- as.numeric(gsub("%", "", inputs_parameter_prep$highP95[which(inputs_parameter_prep$type == "pct_benefit_organization")]))
  
  organization_portion_benefit_metalog <- NA # initalize
  
  
  dat_benefit$organization_portion_benefit <- simulate_bounded_continuous_distribution(P5 = pct_benefit_organization_low, 
                                                                                       P50 = pct_benefit_organization_base,
                                                                                       P95 =  pct_benefit_organization_high, 
                                                                                       lower_bound = 0, 
                                                                                       upper_bound = 100,
                                                                                       number_of_simulations = number_of_simulations)/100 # convert from pct to proportion
  
  # Populate the species range vector with the species range input, using a
  # probabilistic approach if more than one possible value has been specified.
  range_1_pct <- as.numeric(gsub("%", "", inputs_parameter_prep$lowGeneral[which(inputs_parameter_prep$type == "Area in Country")])) # unit is pct
  range_1_prob <- as.numeric(gsub("%", "", inputs_parameter_prep$lowGeneralprob[which(inputs_parameter_prep$type == "Area in Country")]))/100 # divide by 100 to change from pct to prob
  
  if(range_1_prob != 1){
    range_2_pct <- as.numeric(sub("%", "", inputs_parameter_prep$highGeneral[which(inputs_parameter_prep$type == "Area in Country")])) # unit is pct
    range_2_prob <- as.numeric(gsub("%", "", inputs_parameter_prep$highGeneralprob[which(inputs_parameter_prep$type == "Area in Country")]))/100 # divide by 100 to change from pct to prob
    dat_benefit$species_range_pct_in_nation <- sample(c(range_1_pct,range_2_pct), size=number_of_simulations, prob=c(range_1_prob, range_2_prob), replace=TRUE)
  }
  
  if(range_1_prob == 1){
    dat_benefit$species_range_pct_in_nation <- rep(range_1_pct, number_of_simulations)
  }
  
  ###### Combine dat_cost and dat_benefit #####
  
  dat <- cbind(dat_cost, dat_benefit)
  
  
  # Return the result
  return(dat)
}

