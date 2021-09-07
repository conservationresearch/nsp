# This code runs a cost benefit analysis to prioritize new potential species for
# an organization's Conservation Research Conservation Translocation program.

# Laura Keating and Alyssa Friesen
# Last update: July 27, 2021

#Dylan Cole
#September 7, 2021
#Incorporating code written by Laura Keating and Alyssa Friesen 


### Clear the workspace.  
rm(list = ls())

### Load libraries
# library(newSpeciesPrioritization) # uncomment out when done in development mode

### Specify number of simulations and set the random seed.
number_of_simulations <- 10000
set.seed(123) # to ensure the same set of random numbers each time

### Specify project G&A percent (general and admin cost %)
G_and_A_prop_of_total <- 0.15

### Load the inputs
inputs <- newSpeciesPrioritization::inputs # lazy loaded with package

### Specify what the max score is for functional
functional_score_max <- 10 # 10 if using the fine-scale weights

### Identify the program names
org_programs <- unique(inputs$species)[which(unique(inputs$species) != "N/A")]

### Run the simulation using cost_benefit function
results_full_analysis <- cba_cgain_binnedbycgain(org_programs = org_programs, 
                                                 inputs = inputs,
                                                 #proxy = TRUE, # have true when running the full cost-benefit if we stick with this approach
                                                 # proxy = FALSE, # need false to make some of the triage graphs to show Jana/Resit
                                                 functional_score_max = functional_score_max,
                                                 sensitivity = "no")

### Store the results for easy use later
results_overall <- results_full_analysis[[1]]
results_cost_total <- results_full_analysis[[2]]
results_cost_organization <- results_full_analysis[[3]]
results_benefit_national <- results_full_analysis[[4]]
results_benefit_global <- results_full_analysis[[5]]
results_BCR_national <- results_full_analysis[[6]]
results_BCR_global <- results_full_analysis[[7]]

### Draw figures from figures function to show the results
p_benefit <- newSpeciesPrioritization::graph_benefit(results_benefit_national, results_benefit_global, inputs)
p_cost <- newSpeciesPrioritization::graph_cost(results_cost_organization, results_cost_total, inputs)
p_BCR <- newSpeciesPrioritization::graph_BCR(results_overall, inputs)

### Pick top 5 from national and global respectively, check for overlap
results_ranking <- as.data.frame(matrix(nrow = 10, ncol = 5))
colnames(results_ranking) <- c("rank","national_level", "global_level",
                               "BCR_national", "BCR_global")
results_ranking$rank<-c(1:10)

#Arrange data by national rank, extract top 10 national species and associated BCR into new dataframe
results_overall<-results_overall%>%arrange(BCR_national_EV_rank)
results_ranking$national_level<-results_overall$org_program[which(results_overall$BCR_national_EV_rank<=10)]
results_ranking$BCR_national<-results_overall$BCR_national_EV[which(results_overall$BCR_national_EV_rank<=10)]

#Arrange data by global rank, extract top 10 global species and associated BCR into new dataframe
results_overall<-results_overall%>%arrange(BCR_global_EV_rank)
results_ranking$global_level<-results_overall$org_program[which(results_overall$BCR_global_EV_rank<=10)]
results_ranking$BCR_global<-results_overall$BCR_global_EV[which(results_overall$BCR_global_EV_rank<=10)]

#Determine overlapping species and append list onto the results_ranking dataframe, incorporating NAs into unfilled entries
overlapping_species<-results_ranking$national_level[results_ranking$national_level %in% results_ranking$global_level]
clength<-max(length(results_ranking$rank), length(overlapping_species))
length(overlapping_species)<-clength
results_ranking<-cbind(results_ranking, overlapping_species)


