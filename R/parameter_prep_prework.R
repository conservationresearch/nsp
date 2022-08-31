
# Dylan Cole
# August 2021
# Primarily using code written by Laura Keating (July 2021) with some changes and cleanup


#' Prepares data from the inputs csv file and draws values for each parameter 
#' 
#' This function prepares the inputs.csv file, creates the metalog distributions for each parameter using the P5, P50, and P95, and 
#' lastly draws values from the distribution for each parameter. The number of values that are generated is dependent on the user-specified
#' "number_of_simulations" 
#' 
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param org_program A vector of the conservation program names
#' @param G_and_A_prop A value specifying the proportion of total cost that must be added on top of total cost
#' @param number_of_simulations A value specifying number of value draws for each parameter for each conservation program
#' @return A dataframe "dat" for a single conservation program which contains n number of values (determined by number_of_simulations argument) for the costs and benefit metrics
#' @examples
#' parameter_prep(inputs,org_program, G_and_A_prop, number_of_simulations)
#' @export

##### Main function #####
parameter_prep <- function(inputs, org_program, G_and_A_prop,
                           number_of_simulations){
  number_of_simulations_extra<-number_of_simulations*10
  # Call the appropriate species specific data from the inputs file
  inputs_parameter_prep <- inputs[which(inputs$species == org_program),]
  
  
  ##### Create a data frame to store the results #####
  
  dat_cost <- as.data.frame(matrix(nrow=number_of_simulations, ncol=11))
  colnames(dat_cost) <- c("cost_CB", "cost_RR",
                          "max_prop_total_external_funding", "G_and_A_prop_of_total",
                          "cost_total_project_without_G_and_A", "G_and_A",
                          "cost_total_project",  "fundability",
                          "max_external_funding", "external_funding", "cost_organization")
  
  dat_benefit <- as.data.frame(matrix(nrow=number_of_simulations, ncol=8))
  colnames(dat_benefit) <- c("GSfutureWConservation",
                             "GSfutureWOconservation",
                             "GSGainPlusDependence",
                             "GSlongtermAspiration", 
                             "GScurrentNational", "GScurrentGlobal",
                             "OrgBenefit", 
                             "species_range_pct_in_nation")
  
  #Create intermediate dataframe to store results of two Future parameters which will then be sampled from if Future with-future without is <0
  dat_benefit_int<-as.data.frame(matrix(nrow=number_of_simulations_extra, ncol=2))
  colnames(dat_benefit_int)<- c("GSFutureWCon", "GSFutureWOCon")
  
  ######  Write functions for different types of simulations we will do below #####
  #Metalog distribution bound on upper and lower ends
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
  #Metalog distribution bound only on lower end
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
      
      results_vector <- rmetalog::metalog(metalog_dist, number_of_simulations, term = 3)
    }
    return(results_vector)
  }
  
  # Write a function to apply a metalog distribution to the benefit inputs   
  metalogSelectBenefit <- function(P5, P50, P95, lower_bound, upper_bound, number_of_simulations){
    if(P5 != P50 && P50 != P95){
      metalog_dist <- rmetalog::metalog(c(P5,P50,P95), 
                                        term_limit = 3,
                                        bounds=c(lower_bound, upper_bound),
                                        boundedness = 'b',
                                        probs = c(0.05, 0.5, 0.95))
      results_vector <- rmetalog::rmetalog(metalog_dist, number_of_simulations, term = 3)    
    }

    if (P5 == P50 && P50 != P95){
      metalog_dist <- rmetalog::metalog(c(P5,P5,P5,P5,P5,P5,P50,P95),
                                         term_limit=8,
                                         bounds=c(lower_bound, upper_bound),
                                         boundedness = 'b',
                                         probs=c(0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.95))
      results_vector <- rmetalog::rmetalog(metalog_dist, number_of_simulations, term = 7)
    }
    
    if (P50 == P95 && P5 != P50){
      metalog_dist <- rmetalog::metalog(c(P5,P50,P50,P50,P50,P50,P50,P95),
                                         term_limit=8,
                                         bounds=c(lower_bound, upper_bound),
                                         boundedness = 'b',
                                         probs=c(0.05,0.5,0.55,0.6,0.65,0.7,0.8,0.95))
      results_vector <- rmetalog::rmetalog(metalog_dist, number_of_simulations, term = 7)
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
  
  # Maximum percent of total external funding allowed
  
  max_pct_total_external_funding_low <- 100 - as.numeric(as.character(inputs$highP95[which(inputs$type == "Mandatory Organization Contribution")]))
  max_pct_total_external_funding_base <- 100 - as.numeric(as.character(inputs$baseP50[which(inputs$type == "Mandatory Organization Contribution")]))
  max_pct_total_external_funding_high <- 100 - as.numeric(as.character(inputs$lowP5[which(inputs$type == "Mandatory Organization Contribution")]))
  
  dat_cost$max_prop_total_external_funding <- simulate_bounded_continuous_distribution(P5 = max_pct_total_external_funding_low,
                                                                                       P50 = max_pct_total_external_funding_base,
                                                                                       P95 = max_pct_total_external_funding_high,
                                                                                       lower_bound = 0,
                                                                                       upper_bound = 100,
                                                                                       number_of_simulations = number_of_simulations)/100 # convert the results from percent to proportion
  
  #General and Admin costs
  
  G_and_A_prop_total_low <- as.numeric(as.character(inputs$lowP5[which(inputs$type == "General and Administrative")]))/100 # note, our input here didn't have uncertainty but we built in the flexibility to change it later if we want to.
  G_and_A_prop_total_base <- as.numeric(as.character(inputs$baseP50[which(inputs$type == "General and Administrative")]))/100
  G_and_A_prop_total_high <- as.numeric(as.character(inputs$highP95[which(inputs$type == "General and Administrative")]))/100
  
  dat_cost$G_and_A_prop_of_total <- simulate_bounded_continuous_distribution(P5 = G_and_A_prop_total_low, 
                                                                            P50 = G_and_A_prop_total_base,
                                                                           P95 = G_and_A_prop_total_high, 
                                                                          lower_bound = 0,
                                                                         upper_bound = 1,
                                                                        number_of_simulations = number_of_simulations)
  
  # Add the cost inputs together to get a total project cost 
  #dat_cost$G_and_A_prop_of_total<-G_and_A_prop_of_total
  dat_cost$cost_total_project_without_G_and_A <-  dat_cost$cost_CB + dat_cost$cost_RR
  dat_cost$G_and_A <- dat_cost$cost_total_project_without_G_and_A*dat_cost$G_and_A_prop_of_total
  dat_cost$cost_total_project <- dat_cost$cost_total_project_without_G_and_A + dat_cost$G_and_A
  
  # Calculate fundability (i.e. external funding potential
  
  fundability_low <- as.numeric(inputs_parameter_prep$lowP5[which(inputs_parameter_prep$subcategory== "Fundability")])
  fundability_base <- as.numeric(inputs_parameter_prep$baseP50[which(inputs_parameter_prep$subcategory== "Fundability")])
  fundability_high <- as.numeric(inputs_parameter_prep$highP95[which(inputs_parameter_prep$subcategory== "Fundability")])
  if(fundability_low == 0 | fundability_low == "NA_real_"){
    fundability_low <- 0.1 # can't have it be 0 as lower bound is 0 so make it 10 cents
  } 
  if(fundability_base == 0 | fundability_base == "NA_real_"){
    fundability_base <- 0.1 # same reason as above
  }
  if(fundability_high == 0 | fundability_high == "NA_real_"){
    fundability_high <- 0.1 # same reason as above
  }
  
  dat_cost$fundability <- simulate_bounded_continuous_distribution(P5 = fundability_low, 
                                                                   P50 = fundability_base,
                                                                   P95 = fundability_high, 
                                                                   lower_bound = 0,
                                                                   upper_bound = 2*fundability_high,
                                                                   number_of_simulations = number_of_simulations)
  
  ######  Fill in dat_benefit with the relevant parameter draws #####
  
  ### OLD
  # Benefits
  #row_GSGainPlusDependence <- which(inputs_parameter_prep$subcategory == "GSGainPlusDependence")
  #benefit_low <- as.numeric(inputs_parameter_prep$lowP5[row_GSGainPlusDependence])
  #benefit_base <- as.numeric(inputs_parameter_prep$baseP50[row_GSGainPlusDependence])
  #benefit_high <- as.numeric(inputs_parameter_prep$highP95[row_GSGainPlusDependence])
  #Populate GainPlusDependence
  #dat_benefit$GSGainPlusDependence <- metalogSelectBenefit(P5 = benefit_low,
  #                                                        P50 = benefit_base,
  #                                                        P95 = benefit_high,
  #                                                        lower_bound = max(-100, benefit_low-abs((2*benefit_low)-0.001)),
  #                                                        upper_bound = min(100, 2*benefit_high),
  #                                                        number_of_simulations = number_of_simulations)
  #In rare scenario where current GSGainPlusDependence is < 0, setting it to 0
  #dat_benefit$GSGainPlusDependence[which(dat_benefit$GSGainPlusDependence < 0)] <- 0
  
  ### NEW
  #Future with conservation translocation
  row_GSFutureWConservation <- which(inputs_parameter_prep$subcategory == "GSfutureWConservation")
  future_with_low<-as.numeric(inputs_parameter_prep$lowP5[row_GSFutureWConservation])
  future_with_base<-as.numeric(inputs_parameter_prep$baseP50[row_GSFutureWConservation])
  future_with_high<-as.numeric(inputs_parameter_prep$highP95[row_GSFutureWConservation])
  
  dat_benefit_int$GSFutureWCon <- metalogSelectBenefit(P5 = future_with_low,
                                                       P50 = future_with_base,
                                                       P95 = future_with_high,
                                                       #lower_bound = max(-100, future_with_low-(abs((2*future_with_low)-0.001))),
                                                       lower_bound = if(future_with_low==0){-100}else{-0.001},
                                                       #upper_bound = min(100, 2*future_with_high),
                                                       upper_bound = if(future_with_high==100){200}else{100.001},
                                                       number_of_simulations = number_of_simulations_extra)
  #In rare scenario where longterm asp is < 0, setting it to 0
  dat_benefit_int$GSFutureWCon[which(dat_benefit_int$GSFutureWCon <0 )] <- 0
  
  #Future without conservation translocation
  row_GSfutureWOconservation <- which(inputs_parameter_prep$subcategory == "GSfutureWOconservation")
  future_without_low <- as.numeric(inputs_parameter_prep$lowP5[row_GSfutureWOconservation])
  future_without_base <- as.numeric(inputs_parameter_prep$baseP50[row_GSfutureWOconservation])
  future_without_high <- as.numeric(inputs_parameter_prep$highP95[row_GSfutureWOconservation])
  
  dat_benefit_int$GSFutureWOCon <- metalogSelectBenefit(P5 = future_without_low,
                                                        P50 = future_without_base,
                                                        P95 = future_without_high,
                                                        #lower_bound = max(-100, future_without_low-(abs((2*future_without_low)-0.001))),
                                                        lower_bound = if(future_without_low==0){-100}else{-0.001},
                                                        #upper_bound = min(100, 2*future_without_high),
                                                        upper_bound = if(future_without_high==100){200}else{100.001},
                                                        number_of_simulations = number_of_simulations_extra)
  
  #In rare scenario where longterm asp is < 0, setting it to 0
  dat_benefit_int$GSFutureWOCon[which(dat_benefit_int$GSFutureWOCon <0 )] <- 0
  
  #Subtract two distributions to obtain our Gain Plus Dependence metric, populate dat_benefit only possible values for two futures and G+D
  GplusD_int <- dat_benefit_int$GSFutureWCon - dat_benefit_int$GSFutureWOCon #Subtract two future
  possible_GplusD <- which(GplusD_int >= 0)[1:number_of_simulations] #select possible rows
  #Populate relevant dat_benefit rows
  dat_benefit$GSfutureWConservation <- dat_benefit_int$GSFutureWCon[possible_GplusD]
  dat_benefit$GSfutureWOconservation <- dat_benefit_int$GSFutureWOCon[possible_GplusD]
  dat_benefit$GSGainPlusDependence <- GplusD_int[possible_GplusD]
  
  # Long-term aspiration
  row_longterm_aspiration <- which(inputs_parameter_prep$subcategory== "GSlongtermAspiration")
  longterm_aspiration_low <- as.numeric(inputs_parameter_prep$lowP5[row_longterm_aspiration])
  longterm_aspiration_base <- as.numeric(inputs_parameter_prep$baseP50[row_longterm_aspiration])
  longterm_aspiration_high <- as.numeric(inputs_parameter_prep$highP95[row_longterm_aspiration])
  
  #Added lower/upper bounds
  dat_benefit$GSlongtermAspiration <- metalogSelectBenefit(P5 = longterm_aspiration_low,
                                                           P50 = longterm_aspiration_base, 
                                                           P95 = longterm_aspiration_high,
                                                           #lower_bound = max(-100, longterm_aspiration_low-abs((2*longterm_aspiration_low)-0.001)),
                                                           lower_bound = if(longterm_aspiration_low==0){-100}else{-0.001},
                                                           #upper_bound = min(100.001, 2*longterm_aspiration_high),
                                                           upper_bound = if(future_without_high==100){200}else{100.001},
                                                           number_of_simulations = number_of_simulations)
  
  
  #In rare scenario where longterm asp is < 0, setting it to 0
  dat_benefit$GSlongtermAspiration[which(dat_benefit$GSlongtermAspiration <= 0)] <- epsilon
  
  #Adding the below condition to deal with scenarios where GainsPlusDependence are larger than the Long Term Aspirations 
  for (i in 1:length(dat_benefit$GSGainPlusDependence)){
    if (dat_benefit[i,"GSGainPlusDependence"] > dat_benefit[i,"GSlongtermAspiration"])
    { dat_benefit[i,"GSGainPlusDependence"] <- dat_benefit[i,"GSlongtermAspiration"]
    }}
  
  
  # current - national
  row_current_national <- which(inputs_parameter_prep$subcategory== "GScurrentNational")
  current_national_low <- as.numeric(inputs_parameter_prep$lowP5[row_current_national])
  current_national_base <- as.numeric(inputs_parameter_prep$baseP50[row_current_national])
  current_national_high <- as.numeric(inputs_parameter_prep$highP95[row_current_national])
  
  #added lower/upper bounds
  dat_benefit$GScurrentNational <- metalogSelectBenefit(P5 = current_national_low,
                                                        P50 = current_national_base, 
                                                        P95 = current_national_high,
                                                        #lower_bound = max(-100, current_national_high-((2*current_national_high)-0.001)),
                                                        lower_bound= if(current_national_low==0){-100}else{-0.001},
                                                        #upper_bound = min(100, 2*current_national_high),
                                                        upper_bound = if(current_national_high==0){200}else{100.001},
                                                        number_of_simulations = number_of_simulations)
  #In rare scenario where current GS is < 0, setting it to 0 
  dat_benefit$GScurrentNational[which(dat_benefit$GScurrentNational <= 0)] <- epsilon
  
  # current - global
  row_current_global <- which(inputs_parameter_prep$subcategory== "GScurrentGlobal")
  current_global_low <- as.numeric(inputs_parameter_prep$lowP5[row_current_global])
  current_global_base <- as.numeric(inputs_parameter_prep$baseP50[row_current_global])
  current_global_high <- as.numeric(inputs_parameter_prep$highP95[row_current_global])
  
  #Populate GScurrentGlobal
  dat_benefit$GScurrentGlobal <- metalogSelectBenefit(P5 = current_global_low,
                                                      P50 = current_global_base, 
                                                      P95 = current_global_high,
                                                      #lower_bound = max(-100, current_global_low-((2*current_global_low)-0.001)),
                                                      lower_bound= if(current_global_low==0){-100}else{-0.001},
                                                      #upper_bound = min(100.001, 2*current_global_high),
                                                      upper_bound = if(current_global_high==0){200}else{100.001},
                                                      number_of_simulations = number_of_simulations)
  #In rare scenario where current GS is < 0, setting it to 0 
  dat_benefit$GScurrentGlobal[which(dat_benefit$GScurrentGlobal <= 0)] <- epsilon
  dat_benefit$GScurrentGlobal[which(dat_benefit$GScurrentGlobal > 100)] <- 100
  
  # Identify how many spatial units were used for the assessment
  dat_benefit$GSnSpatialUnits <- as.numeric(inputs_parameter_prep$baseP50[which(inputs_parameter_prep$subcategory == "GSnSpatialUnits")])
  
  #populate the organization portion of the benefit
  pct_benefit_organization_low <- as.numeric(gsub("%", "", inputs_parameter_prep$lowP5[which(inputs_parameter_prep$type == "OrganizationBenefit")]))
  pct_benefit_organization_base <- as.numeric(gsub("%", "", inputs_parameter_prep$baseP50[which(inputs_parameter_prep$type == "OrganizationBenefit")]))
  pct_benefit_organization_high <- as.numeric(gsub("%", "", inputs_parameter_prep$highP95[which(inputs_parameter_prep$type == "OrganizationBenefit")]))
  
  #Draw values for the benefit 
  dat_benefit$OrgBenefit <- simulate_bounded_continuous_distribution(P5 = pct_benefit_organization_low, 
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
}#end of parameter_prep function

