#This file contains the sensitivity analysis
#Dylan Cole
#September 2021
#Incorporated code written by Laura Keating (July 2021) 


##### Sensitivity Analysis #####
#' INSERT
#' @param org_programs a vector of the conservation program names
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95)
#' @param results_full_analysis a list of the organization programs and their associated results generated from cost_benefit_analysis
#' @param rank_cutoff a value set by user to highlight their organization's number of conservation programs of interest (ex. top 10, top 5, top 3)
#' @examples
#' # STILL TO ADD
#'
#' @export
sensitivity <- function(org_programs, inputs, results_full_analysis, rank_cutoff){
  
  # Update the inputs dataframe with columns to store the sensitivity analysis results.
  inputs_sens <- inputs # initialize
  
  inputs_sens$BCR_national_EV_low <- 'NA' # initialize
  inputs_sens$BCR_national_EV_overall <- 'NA' # initialize
  inputs_sens$BCR_national_EV_high <- 'NA' # initialize
  inputs_sens$BCR_national_EV_rank_overall <- 'NA' #initialize
  
  inputs_sens$BCR_global_EV_low <- 'NA' # initialize
  inputs_sens$BCR_global_EV_overall <- 'NA' # initialize
  inputs_sens$BCR_global_EV_high <- 'NA' # initialize
  inputs_sens$BCR_global_EV_rank_overall <- 'NA'# initialize

  inputs_sens$tornado_label <- "N/A" # initialize
  
  # For each input, hold at the low or high value and run the analysis. Record the results.
  
  for(j in 1:length(org_programs)){
    # Can now update using the dat file from each species simulation
    dat <- results_full_analysis[[8]][[j]]  
    
    # Identify the inputs for this org program as well as the NA inputs
    relevant_input_rows <- which(inputs$species == org_programs[j]| inputs$species == 'N/A')
    n_inputs <- length(relevant_input_rows)
    inputs_int <- inputs[relevant_input_rows,]

    # Perform sensitivity analysis for each parameter for in each species
    for(k in 1:n_inputs){
      
      print(paste("Sensitivity:", inputs_int$name[k]))
      
      #Collect the data
      row_in_inputs <- which(inputs$name == inputs_int$name[k])
      col_in_dat <- which(grepl(sub(".*_", "", inputs_int$name[k]),colnames(dat), fixed = TRUE) == TRUE)
      
      # Matching any occurance of "_" and replacing with a "" in pct_range_
      if(sub(".*_", "", inputs_int$name[k]) == "pctRangeCountry"){
        col_in_dat <- which(colnames(dat) == "species_range_pct_in_nation")
      }
      
      if(sub(".*_", "", inputs_int$name[k]) == "score"){
        col_in_dat <- which(colnames(dat) == "fundability")
      }
      
      ##### Analysis at low value ##### 
      
      # Hold the value at the low and recalculate results
      # Calculate the 0.05 quantile from each column in dat 
      input_low <- as.numeric(quantile(dat[,col_in_dat], 0.10))
      
      # Extract rows from dat which are equal/less than the low quantile just calculated
      rows_with_low <- which(dat[,col_in_dat] <=  input_low)
      
      # Calculate mean of the BCR_national and BCR_global from the subsetted rows and enter into inputs_sens df
      inputs_sens$BCR_national_EV_low[row_in_inputs] <- mean(dat$BCR_national[rows_with_low])
      inputs_sens$BCR_global_EV_low[row_in_inputs] <- mean(dat$BCR_global[rows_with_low])
      
      # Organize the results
      # Populate with results_overall with data
      results_overall  <- results_full_analysis[[1]]
      #Create intermediate dataframe
      results_overall_int <- results_overall # initialize
      # Unorder these factor levels
      results_overall_int$org_program <- factor(results_overall_int$org_program, levels=unique(results_overall_int$org_program), ordered=FALSE) 
      # Input BCR_national_EV_low into corresponding program in intermediate df
      results_overall_int$BCR_national_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_national_EV_low[row_in_inputs])
      # Input BCR_global_EV_low into corresponding program in intermediate df
      results_overall_int$BCR_global_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_global_EV_low[row_in_inputs])
      
      #Ranking of projects after recalculating BCR at high value in national and global
      results_overall_int$BCR_national_rank_low<-rank(-results_overall_int$BCR_national_EV)
      results_overall_int$BCR_global_rank_low<-rank(-results_overall_int$BCR_global_EV)
      
      inputs_sens$BCR_national_EV_rank_low[row_in_inputs]<-results_overall_int$BCR_national_rank_low[which(results_overall_int$org_program == as.character(org_programs[j]))]
      inputs_sens$BCR_global_EV_rank_low[row_in_inputs]<-results_overall_int$BCR_global_rank_low[which(results_overall_int$org_program == as.character(org_programs[j]))]

      ##### Analysis at high value ##### 
      
      # Hold the value at the high and recalculate results
      input_high <- as.numeric(quantile(dat[,col_in_dat], 0.90))
      rows_with_high <- which(dat[,col_in_dat] >=  input_high)
      inputs_sens$BCR_national_EV_high[row_in_inputs] <- mean(dat$BCR_national[rows_with_high])
      inputs_sens$BCR_global_EV_high[row_in_inputs] <- mean(dat$BCR_global[rows_with_high])
      
      # Organize the results 
      results_overall  <- results_full_analysis[[1]]
      results_overall_int <- results_overall # initalize
      results_overall_int$BCR_national_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_national_EV_high[row_in_inputs])
      results_overall_int$BCR_global_EV[which(results_overall_int$org_program == as.character(org_programs[j]))] <- as.numeric(inputs_sens$BCR_global_EV_high[row_in_inputs])
      
      #Ranking of projects after recalculating BCR at high value in national and global
      results_overall_int$BCR_national_rank_high<-rank(-results_overall_int$BCR_national_EV)
      results_overall_int$BCR_global_rank_high<-rank(-results_overall_int$BCR_global_EV)
      
      inputs_sens$BCR_national_EV_rank_high[row_in_inputs]<-results_overall_int$BCR_national_rank_high[which(results_overall_int$org_program == as.character(org_programs[j]))]
      inputs_sens$BCR_global_EV_rank_high[row_in_inputs]<-results_overall_int$BCR_global_rank_high[which(results_overall_int$org_program == as.character(org_programs[j]))]
      
      } #end input loop
  } #end organization program loop
  
  # For each program, fill in the full probabilistic results
  for(j in 1:length(org_programs)){
    
    # identify the inputs for this org program
    relevant_program_rows <- which(inputs_sens$species == as.character(org_programs[j]))
    
    # Fill in the overall program BCR results for this program for national and global
    inputs_sens$BCR_national_EV_overall[relevant_program_rows] <- results_overall$BCR_national_EV[which(results_overall$org_program == as.character(org_programs[j]))]
    inputs_sens$BCR_global_EV_overall[relevant_program_rows] <- results_overall$BCR_global_EV[which(results_overall$org_program == as.character(org_programs[j]))]
  
    # Fill in the overall program rank results for this program for national and global
    inputs_sens$BCR_national_EV_rank_overall[relevant_program_rows] <- results_overall$BCR_national_EV_rank[which(results_overall$org_program == as.character(org_programs[j]))]
    inputs_sens$BCR_global_EV_rank_overall[relevant_program_rows] <- results_overall$BCR_global_EV_rank[which(results_overall$org_program == as.character(org_programs[j]))]
  
    # Calculate the difference in ranks in national and global from the overall program rank
    #National
    inputs_sens$difference_in_ranks_overall_minus_low_national <- as.numeric(inputs_sens$BCR_national_EV_rank_overall) - as.numeric(inputs_sens$BCR_national_EV_rank_low)
    inputs_sens$difference_in_ranks_overall_minus_high_national <- as.numeric(inputs_sens$BCR_national_EV_rank_overall) - as.numeric(inputs_sens$BCR_national_EV_rank_high)
    #Global
    inputs_sens$difference_in_ranks_overall_minus_low_global <- as.numeric(inputs_sens$BCR_global_EV_rank_overall) - as.numeric(inputs_sens$BCR_global_EV_rank_low)
    inputs_sens$difference_in_ranks_overall_minus_high_global <- as.numeric(inputs_sens$BCR_global_EV_rank_overall) - as.numeric(inputs_sens$BCR_global_EV_rank_high)
  } 
  
  #Calculate the swing of national and national ranks 
  inputs_sens$swing_ranks_national <- abs(inputs_sens$difference_in_ranks_overall_minus_low_national) + abs(inputs_sens$difference_in_ranks_overall_minus_high_national)
  inputs_sens$swing_ranks_global <- abs(inputs_sens$difference_in_ranks_overall_minus_low_global) + abs(inputs_sens$difference_in_ranks_overall_minus_high_global)
   
  #Determine current ranks and potential ranks
  #national
  inputs_sens$decision_sensitive_national <- "No" # initialize
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_low) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_high) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) > rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_low) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  inputs_sens$decision_sensitive_national[which(as.numeric(inputs_sens$BCR_national_EV_rank_overall) > rank_cutoff & as.numeric(inputs_sens$BCR_national_EV_rank_high) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  #national
  inputs_sens$decision_sensitive_global <- "No" # initialize
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$BCR_global_EV_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$BCR_global_EV_rank_low) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$BCR_global_EV_rank_overall) <= rank_cutoff & as.numeric(inputs_sens$BCR_global_EV_rank_high) >rank_cutoff)] <- paste("Yes - in top", rank_cutoff, "overall but could be out")
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$BCR_global_EV_rank_overall) > rank_cutoff & as.numeric(inputs_sens$BCR_global_EV_rank_low) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  inputs_sens$decision_sensitive_global[which(as.numeric(inputs_sens$BCR_global_EV_rank_overall) > rank_cutoff & as.numeric(inputs_sens$BCR_global_EV_rank_high) <=rank_cutoff)] <- paste("Yes - out of top", rank_cutoff, "overall but could be in")
  
  return(inputs_sens)
}#end of sensitivity function
  
  
##### Drawing tornado plots of the sensitivty analysis results at national level #####
#' INSERT
#'
#' @param inputs_sens a dataframe generated from the sensitivity function
#' @examples
#' # STILL TO ADD
#'
#' @export
draw_tornados_national <- function(inputs_sens){
  # This section could use some tidying.
  
  #Needs documentation
  ### Draw tornados for national level
  
  #Populate with data from sensitivity analysis columns
  int_low_national <- inputs_sens[,c("species", "category",
                            "subcategory", "type", 
                            "unit", "name","lowP5", "swing_ranks_national",
                            "BCR_national_EV_rank_overall",
                            "BCR_national_EV_rank_low",
                            "tornado_label")]
  #Populate column headers
  colnames(int_low_national) <- c("species", "category",
                         "subcategory", "type", 
                         "unit", "name","input_value", "swing_ranks_national",# rename lowP5 to 'value' so can rbind with high
                         "BCR_national_EV_rank_overall", 
                         "BCR_national_EV_rank_lowORhigh",
                         "tornado_label")
  #Specify data is from low analysis
  int_low_national$lowORhigh <- "low"
  
  #Populate with data from sensitivity analysis columns
  int_high_national <- inputs_sens[,c("species", "category",
                             "subcategory", "type", 
                             "unit", "name","highP95", "swing_ranks_national",
                             "BCR_national_EV_rank_overall",
                             "BCR_national_EV_rank_high",
                             "tornado_label")]
  #Populate column headers
  colnames(int_high_national) <- c("species", "category",
                          "subcategory", "type", 
                          "unit", "name","input_value", "swing_ranks_national", # rename highP95 to 'value' so can rbind with low
                          "BCR_national_EV_rank_overall", 
                          "BCR_national_EV_rank_lowORhigh",
                          "tornado_label")
  #Specify data is from high analysis
  int_high_national$lowORhigh <- "high"
  
  #combine high and low analysis into single data frame
  inputs_spSpecific_Tornado_national <- rbind(int_low_national, int_high_national)
  
  #Create a tornado label
  inputs_spSpecific_Tornado_national$tornado_label <- inputs_spSpecific_Tornado_national$name 
  
  #Turn into character
  inputs_spSpecific_Tornado_national$input_value <- as.character(inputs_spSpecific_Tornado_national$input_value)
  
  #Center the data
  inputs_spSpecific_Tornado_national$ranklowOrhigh_minus_rankEV <- as.numeric(inputs_spSpecific_Tornado_national$BCR_national_EV_rank_lowORhigh) -
    as.numeric(inputs_spSpecific_Tornado_national$BCR_national_EV_rank_overall)
  
  tornado_spSpecific_national_list <- list() # initialize
  
  # Make plots
  for(i in 1:length(org_programs)){
    
    species <- org_programs[i]
    
    print(paste0("Drawing National Tornado for species #", i, ":", species))
    
    # Calling the inputs related only to the org_programs[i]
    inputs_spSpecific_Tornado_species_national <- inputs_spSpecific_Tornado_national[which(inputs_spSpecific_Tornado_national$species == species),]
    
    # Include each tornado label only once so that it shows up with the same transparency in the tornado
    inputs_spSpecific_Tornado_species_national <- inputs_spSpecific_Tornado_species_national[which(duplicated(paste(inputs_spSpecific_Tornado_species_national$tornado_label, inputs_spSpecific_Tornado_species_national$lowORhigh)) == FALSE),]
    
    # Rank the order in which they the parameters appear based on swing (largest is at top)
    inputs_spSpecific_Tornado_species_national$tornado_order <- rank(-inputs_spSpecific_Tornado_species_national$swing_ranks_national, ties.method = "random")
    
    # Extract only top 20 parameters from ranked list
    inputs_spSpecific_Tornado_species_top20_national <- inputs_spSpecific_Tornado_species_national[which(inputs_spSpecific_Tornado_species_national$tornado_order <=20),]
    
    # Add in hjust so that we can put the labels on the tornado
    inputs_spSpecific_Tornado_species_top20_national$hjust <- NA # initalize
    
    # Setting hjust to 1.5 (This is outside range of 0 to 1 - need to revisit) for inputs where overall rank is greater than high/low rank
    inputs_spSpecific_Tornado_species_top20_national$hjust[which(inputs_spSpecific_Tornado_species_top20_national$BCR_national_EV_rank_overall > inputs_spSpecific_Tornado_species_top20_national$BCR_national_EV_rank_lowORhigh)] <- 1
    
    #Setting hjust to -0.5 for inputs where rank_overall is less than or equal to high/low rank
    inputs_spSpecific_Tornado_species_top20_national$hjust[which(inputs_spSpecific_Tornado_species_top20_national$BCR_national_EV_rank_overall <= inputs_spSpecific_Tornado_species_top20_national$BCR_national_EV_rank_lowORhigh)] <- -0.5
    
    # Make the graph
    
    tornado_ranks_top20_national <- ggplot2::ggplot(inputs_spSpecific_Tornado_species_top20_national, ggplot2::aes(reorder(tornado_label,swing_ranks_national),
                                                                                                                   y =  ranklowOrhigh_minus_rankEV, fill=lowORhigh)) +
      ggplot2::geom_bar(position="identity", stat="identity") +
      ggplot2::geom_text(ggplot2::aes(y = ranklowOrhigh_minus_rankEV),
                         label = inputs_spSpecific_Tornado_species_top20_national$input_value, 
                         hjust = inputs_spSpecific_Tornado_species_top20_national$hjust, size = 3) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      ggplot2::scale_fill_manual(values = ggplot2::alpha(c("red","blue"), 0.5), name = "") +
      ggplot2::scale_y_continuous(breaks=seq(-max(inputs_spSpecific_Tornado_species_top20_national$swing_ranks_national, na.rm = TRUE),max(inputs_spSpecific_Tornado_species_top20_national$swing_ranks_national, na.rm = TRUE), by = 1),
                                  labels=seq(-max(inputs_spSpecific_Tornado_species_top20_national$swing_ranks_national, na.rm = TRUE),max(inputs_spSpecific_Tornado_species_top20_national$swing_ranks_national, na.rm = TRUE), by = 1) + 
                                    round(as.numeric(inputs_spSpecific_Tornado_species_top20_national$BCR_national_EV_rank_overall)[1], 4)) +  # make the axis read the actual values centered around EV instead of zero (which is what needs to be plotted), from https://stackoverflow.com/questions/35324892/ggplot2-setting-geom-bar-baseline-to-1-instead-of-zero
      ggplot2::xlab("Variable") +
      ggplot2::ylab("National Species Rank") +
      ggplot2::coord_flip()
    
    print(tornado_ranks_top20_national)
    
    tornado_spSpecific_national_list[[i]] <- tornado_ranks_top20_national # save it to the list
  }
  return(tornado_spSpecific_national_list)
}#end of the draw_tornados_national function


##### Drawing tornado plots of the sensitivty analysis results at national level #####
#' INSERT
#'
#' @param inputs_sens a dataframe generated from the sensitivity function 
#' @examples
#' # STILL TO ADD
#'
#' @export
draw_tornados_global <- function(inputs_sens){ 
 
  
  int_low_global <- inputs_sens[,c("species", "category",
                                     "subcategory", "type", 
                                     "unit", "name","lowP5", "swing_ranks_global",
                                     "BCR_global_EV_rank_overall",
                                     "BCR_global_EV_rank_low",
                                     "tornado_label")]
  
  colnames(int_low_global) <- c("species", "category",
                                  "subcategory", "type", 
                                  "unit", "name","input_value", "swing_ranks_global",# rename lowP5 to 'value' so can rbind with high
                                  "BCR_global_EV_rank_overall", 
                                  "BCR_global_EV_rank_lowORhigh",
                                  "tornado_label")
  
  int_low_global$lowORhigh <- "low"
  
  int_high_global <- inputs_sens[,c("species", "category",
                                      "subcategory", "type", 
                                      "unit", "name","highP95", "swing_ranks_global",
                                      "BCR_global_EV_rank_overall",
                                      "BCR_global_EV_rank_high",
                                      "tornado_label")]
  
  colnames(int_high_global) <- c("species", "category",
                                   "subcategory", "type", 
                                   "unit", "name","input_value", "swing_ranks_global", # rename highP95 to 'value' so can rbind with high
                                   "BCR_global_EV_rank_overall", 
                                   "BCR_global_EV_rank_lowORhigh",
                                   "tornado_label")
  
  int_high_global$lowORhigh <- "high"
  
  #combine high and low analysis into single data frame
  inputs_spSpecific_Tornado_global <- rbind(int_low_global, int_high_global)
  
  #Create a tornado label
  inputs_spSpecific_Tornado_global$tornado_label <- inputs_spSpecific_Tornado_global$name # different then before, trying this
  
  #Turn into character
  inputs_spSpecific_Tornado_global$input_value <- as.character(inputs_spSpecific_Tornado_global$input_value)
  
  #Center data
  inputs_spSpecific_Tornado_global$ranklowOrhigh_minus_rankEV <- as.numeric(inputs_spSpecific_Tornado_global$BCR_global_EV_rank_lowORhigh) -
    as.numeric(inputs_spSpecific_Tornado_global$BCR_global_EV_rank_overall)
  
  tornado_spSpecific_global_list<- list() # initialize
  
  # Make plots
  for(i in 1:length(org_programs)){
    
    species <- org_programs[i]
    
    print(paste0("Drawing Global Tornado for species #", i, ":", species))
    
    # Calling the inputs related only to the org_programs[i]
    inputs_spSpecific_Tornado_species_global <- inputs_spSpecific_Tornado_global[which(inputs_spSpecific_Tornado_global$species == species),]
    
    # Include each tornado label only once so that it shows up with the same transparency in the tornado
    inputs_spSpecific_Tornado_species_global <- inputs_spSpecific_Tornado_species_global[which(duplicated(paste(inputs_spSpecific_Tornado_species_global$tornado_label, inputs_spSpecific_Tornado_species_global$lowORhigh)) == FALSE),]
    
    # Rank the order in which they the parameters appear based on swing (largest is at top)
    inputs_spSpecific_Tornado_species_global$tornado_order <- rank(-inputs_spSpecific_Tornado_species_global$swing_ranks_global, ties.method = "random")
    
    # Extract only top 20 parameters from ranked list
    inputs_spSpecific_Tornado_species_top20_global <- inputs_spSpecific_Tornado_species_global[which(inputs_spSpecific_Tornado_species_global$tornado_order <=20),]
    
    # Add in hjust so that we can put the labels on the tornado
    inputs_spSpecific_Tornado_species_top20_global$hjust <- NA # initalize
    
    # Setting hjust to 1.5 (This is outside range of 0 to 1 - need to revisit) for inputs where overall rank is greater than high/low rank
    inputs_spSpecific_Tornado_species_top20_global$hjust[which(inputs_spSpecific_Tornado_species_top20_global$BCR_global_EV_rank_overall > inputs_spSpecific_Tornado_species_top20_global$BCR_global_EV_rank_lowORhigh)] <- -1
    
    #Setting hjust to -0.5 for inputs where rank_overall is less than or equal to high/low rank
    inputs_spSpecific_Tornado_species_top20_global$hjust[which(inputs_spSpecific_Tornado_species_top20_global$BCR_global_EV_rank_overall <= inputs_spSpecific_Tornado_species_top20_global$BCR_global_EV_rank_lowORhigh)] <- -0.5
    
    # Make the graph
    
    tornado_ranks_top20_global <- ggplot2::ggplot(inputs_spSpecific_Tornado_species_top20_global, ggplot2::aes(reorder(tornado_label,swing_ranks_global),
                                                                                                                   y =  ranklowOrhigh_minus_rankEV, fill=lowORhigh)) +
      ggplot2::geom_bar(position="identity", stat="identity") +
      ggplot2::geom_text(ggplot2::aes(y = ranklowOrhigh_minus_rankEV),
                         label = inputs_spSpecific_Tornado_species_top20_global$input_value, 
                         hjust = inputs_spSpecific_Tornado_species_top20_global$hjust, size = 3) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      ggplot2::scale_fill_manual(values = ggplot2::alpha(c("red","blue"), 0.5), name = "") +
      ggplot2::scale_y_continuous(breaks=seq(-max(inputs_spSpecific_Tornado_species_top20_global$swing_ranks_global, na.rm = TRUE),max(inputs_spSpecific_Tornado_species_top20_global$swing_ranks_global, na.rm = TRUE), by = 1),
                                  labels=seq(-max(inputs_spSpecific_Tornado_species_top20_global$swing_ranks_global, na.rm = TRUE),max(inputs_spSpecific_Tornado_species_top20_global$swing_ranks_global, na.rm = TRUE), by = 1) + 
                                    round(as.numeric(inputs_spSpecific_Tornado_species_top20_global$BCR_global_EV_rank_overall)[1], 4)) +  # make the axis read the actual values centered around EV instead of zero (which is what needs to be plotted), from https://stackoverflow.com/questions/35324892/ggplot2-setting-geom-bar-baseline-to-1-instead-of-zero
      ggplot2::xlab("Variable") +
      ggplot2::ylab("Global Species Rank") +
      ggplot2::coord_flip()
    
    print(tornado_ranks_top20_global)
    
    tornado_spSpecific_global_list[[i]] <- tornado_ranks_top20_global # save it to the list
  }
return(tornado_spSpecific_global_list)
}#end of draw_tornados_global function
