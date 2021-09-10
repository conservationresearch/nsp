if (sensitivity="yes"){
  newSpeciesPrioritization::sensitivity(org_programs = org_programs, inputs=inputs, results_full_analysis=results_full_analysis, rank_cutoff = rank_cutoff)
}


#Doing sensitivity analysis
sensitivity <- function(org_programs, inputs, results_full_analysis, rank_cutoff){
  
  # Update the inputs dataframe with columns to store the sensitivity analysis results.
  inputs_sens <- inputs # initialize
  
  inputs_sens$BCR_national_EV_high <- 'NA' # initialize
  inputs_sens$BCR_national_EV_low <- 'NA' # initialize
  inputs_sens$BCR_national_EV_overall <- 'NA' # initialize
  
  inputs_sens$BCR_global_EV_high <- 'NA' # initialize
  inputs_sens$BCR_global_EV_low <- 'NA' # initialize
  inputs_sens$BCR_global_EV_overall <- 'NA' # initialize

  inputs_sens$tornado_label <- "N/A" # initialize
  
  # For each input, hold at the low or high value and run the analysis. Record the results.
  
  for(j in 1:length(org_programs)){
    # Can now update using the dat file from each species simulation
    dat <- results_full_analysis[[8]][[j]]  
    
    # identify the inputs for this org program as well as the NA inputs
    relevant_input_rows <- which(inputs$species == org_programs[j]| inputs$species == 'N/A')
    n_inputs <- length(relevant_input_rows)
    inputs_int <- inputs[relevant_input_rows,]
    
    # Do the sensitivity analysis for all of the inputs on the main input sheet.
    for(k in 1:n_inputs){
      
      print(paste("Sensitivity:", inputs_int$name[k]))
      
      #Collect the data
      row_in_inputs <- which(inputs$name == inputs_int$name[k])
      col_in_dat <- which(grepl(sub(".*_", "", inputs_int$name[k]),colnames(dat), fixed = TRUE) == TRUE)
      
      # What is the function of this section?
      # Matching any occurence of "_" and replacing with a "" in pct_range_
      if(sub(".*_", "", inputs_int$name[k]) == "pctRangeCountry"){
        col_in_dat <- which(colnames(dat) == "species_range_pct_in_nation")
      }
      
      if(sub(".*_", "", inputs_int$name[k]) == "score"){
        col_in_dat <- which(colnames(dat) == "fundability")
      }
      
      ##### Analysis at low value ##### 
      
      # Hold the value at the low and recalculate results
      input_low <- as.numeric(quantile(dat[,col_in_dat], 0.1))
      rows_with_low <- which(dat[,col_in_dat] <=  input_low)
      inputs_sens$BCR_national_EV_low[row_in_inputs] <- mean(dat$BCR_national[rows_with_low])
      inputs_sens$BCR_global_EV_low[row_in_inputs] <- mean(dat$BCR_global[rows_with_low])
      
      # Organize the results
      results_overall  <- results_full_analysis[[1]]
      results_overall_int <- results_overall # initalize
      results_overall_int$org_program <- factor(results_overall_int$org_program, levels=unique(results_overall_int$org_program), ordered=FALSE) # unorder these factor levels
      results_overall_int$BCR_national_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_national_EV_low[row_in_inputs])
      results_overall_int$BCR_global_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_global_EV_low[row_in_inputs])
      
      #Ranking of projects after recalculating BCR at low value
      results_overall_int$BCR_national_rank_low<-rank(-results_overall_int$BCR_national_EV)
      inputs_sens$BCR_national_EV_rank_low[row_in_inputs]<-results_overall_int$BCR_national_rank_low[which(results_overall_int$org_program == as.character(org_programs[j]))]
      
      results_overall_int$BCR_global_rank_low<-rank(-results_overall_int$BCR_global_EV)
      inputs_sens$BCR_global_EV_rank_low[row_in_inputs]<-results_overall_int$BCR_global_rank_low[which(results_overall_int$org_program == as.character(org_programs[j]))]
      
      
      
      ##### Analysis at high value ##### 
      
      # Hold the value at the high and recalculate results
      input_high <- as.numeric(quantile(dat[,col_in_dat], 0.9))
      rows_with_high <- which(dat[,col_in_dat] >=  input_high)
      inputs_sens$BCR_national_EV_high[row_in_inputs] <- mean(dat$BCR_national[rows_with_high])
      inputs_sens$BCR_global_EV_high[row_in_inputs] <- mean(dat$BCR_global[rows_with_high])
      
      # Organize the results 
      results_overall  <- results_full_analysis[[1]]
      results_overall_int <- results_overall # initalize
      results_overall_int$BCR_national_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_national_EV_high[row_in_inputs])
      results_overall_int$BCR_global_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_global_EV_high[row_in_inputs])
      
      #Ranking of projects after recalculating BCR at high value
      results_overall_int$BCR_national_rank_high<-rank(-results_overall_int$BCR_national_EV)
      inputs_sens$BCR_national_EV_rank_high[row_in_inputs]<-results_overall_int$BCR_national_rank_high[which(results_overall_int$org_program == as.character(org_programs[j]))]
      
      results_overall_int$BCR_global_rank_high<-rank(-results_overall_int$BCR_global_EV)
      inputs_sens$BCR_global_EV_rank_high[row_in_inputs]<-results_overall_int$BCR_global_rank_high[which(results_overall_int$org_program == as.character(org_programs[j]))]
      
      } #end input loop
  } #end organization program loop
  
  # For each program, fill in the full probabilistic results
  for(j in 1:length(org_programs)){
    
    # identify the inputs for this org program
    relevant_program_rows <- which(inputs_sens$species == as.character(org_programs[j]))
    
    # Fill in the overall result for this program
    inputs_sens$BCR_national_EV_overall[relevant_program_rows] <- results_overall$BCR_national_EV[which(results_overall$org_program == as.character(org_programs[j]))]
    inputs_sens$BCR_global_EV_overall[relevant_program_rows] <- results_overall$BCR_global_EV[which(results_overall$org_program == as.character(org_programs[j]))]
  }
    inputs_sens$BCR_national_EV_rank_overall[relevant_program_rows] <- results_overall$BCR_national_EV_rank[which(results_overall$org_program == as.character(org_programs[j]))]
    inputs_sens$BCR_global_EV_rank_overall[relevant_program_rows] <- results_overall$BCR_global_EV_rank[which(results_overall$org_program == as.character(org_programs[j]))]
    }
  
  # Calculate the difference in ranks in national and global
  inputs_sens$difference_in_ranks_overall_minus_low_national <- as.numeric(inputs_sens$BCR_national_EV_rank_overall) - as.numeric(inputs_sens$BCR_national_EV_rank_low)
  inputs_sens$difference_in_ranks_overall_minus_high_national <- as.numeric(inputs_sens$BCR_national_EV_rank_overall) - as.numeric(inputs_sens$BCR_national_EV_rank_high)
  
  
  ####Issue with the next 2 lines
  inputs_sens$difference_in_ranks_overall_minus_low_global <- as.numeric(inputs_sens$BCR_global_EV_overall) - as.numeric(inputs_sens$BCR_global_EV_low)
  inputs_sens$difference_in_ranks_overall_minus_high_global <- as.numeric(inputs_sens$BCR_global_EV_overall) - as.numeric(inputs_sens$BCR_global_EV_high)
  
  
  
  
  
  
  
  
  #Calculate the swing of national and global ranks 
  inputs_sens$swing_ranks_national <- abs(inputs_sens$difference_in_ranks_overall_minus_low_national) + abs(inputs_sens$difference_in_ranks_overall_minus_high_national)
  inputs_sens$swing_ranks_global <- abs(inputs_sens$difference_in_ranks_overall_minus_low_global) + abs(inputs_sens$difference_in_ranks_overall_minus_high_global)
   
  #Determine current ranks and potential ranks
  #National
  inputs_sens$decision_sensitive_national <- "No" # initialize
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_low) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_high) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) > rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_low) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) > rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_high) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  #Global
  inputs_sens$decision_sensitive_global <- "No" # initialize
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$BCR_global_EV_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$weighted_BCR_rank_low) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$weighted_BCR_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$weighted_BCR_rank_high) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$weighted_BCR_rank_overall) > rank_cutoff & as.numeric(inputs_sens$weighted_BCR_rank_low) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$weighted_BCR_rank_overall) > rank_cutoff & as.numeric(inputs_sens$weighted_BCR_rank_high) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  
  
  return(inputs_sens)
}
  
  
  
  
  
   