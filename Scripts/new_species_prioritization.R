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

##### Analysis setup ##### 

### Specify number of simulations and set the random seed.
number_of_simulations <- 10000
set.seed(123) # to ensure the same set of random numbers each time

### Specify project G&A percent (general and admin cost %)
G_and_A_prop_of_total <- 0.15

### Specify what the max score is for functional
functional_score_max <- 10 # 10 if using the fine-scale weights

### Specify number of species of interest
top_species <- 10

### Specify endemic species
endemic_species <- c("Banff Springs Snail","Maritime Ringlet","Atlantic whitefish")

### Load the inputs
inputs <- newSpeciesPrioritization::inputs # lazy loaded with package
### Identify the program names
org_programs <- unique(inputs$species)[which(unique(inputs$species) != "N/A")]

##### Main analysis function #####

### Run the simulation using cost_benefit function
results_full_analysis <- newSpeciesPrioritization::cba_cgain_binnedbycgain(org_programs = org_programs, 
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



##### Figures #####

### Draw figures from figures function to show the results
p_benefit <- newSpeciesPrioritization::graph_benefit(results_benefit_national, results_benefit_global, inputs)
p_cost <- newSpeciesPrioritization::graph_cost(results_cost_organization, results_cost_total, inputs)
p_BCR <- newSpeciesPrioritization::graph_BCR(results_overall, inputs)

##### Identifying and extracting top # of species #####

### Pick top 5 from national and global respectively, check for overlap
results_ranking <- as.data.frame(matrix(nrow = top_species, ncol = 5))
colnames(results_ranking) <- c("rank","national_level", "global_level",
                               "BCR_national", "BCR_global")
results_ranking$rank<-c(1:top_species)

#Arrange data by national rank, extract specified # of top national species and associated BCR into new dataframe
results_overall<-results_overall%>%arrange(BCR_national_EV_rank)
results_ranking$national_level<-results_overall$org_program[which(results_overall$BCR_national_EV_rank<=top_species)]
results_ranking$BCR_national<-results_overall$BCR_national_EV[which(results_overall$BCR_national_EV_rank<=top_species)]

#Arrange data by global rank, extract specified # of top global species and associated BCR into new dataframe
results_overall<-results_overall%>%arrange(BCR_global_EV_rank)
results_ranking$global_level<-results_overall$org_program[which(results_overall$BCR_global_EV_rank<=top_species)]
results_ranking$BCR_global<-results_overall$BCR_global_EV[which(results_overall$BCR_global_EV_rank<=top_species)]

#Determine overlapping species and append list onto the results_ranking dataframe, incorporating NAs into unfilled entries
overlapping_species<-results_ranking$national_level[results_ranking$national_level %in% results_ranking$global_level]
clength<-max(length(results_ranking$rank), length(overlapping_species))
length(overlapping_species)<-clength
results_ranking<-cbind(results_ranking, overlapping_species)

##### Export tables and figures #####

# Export panel of figures
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/panel_benefit_cost_BCR.tiff"
tiff(filename, width=12, height=18, units="in",
     pointsize=8, compression="lzw", bg="white", res=600,
     restoreConsole=TRUE)
gridExtra::grid.arrange(p_benefit, p_cost, p_BCR, ncol = 1, nrow = 3)
dev.off()

# Export table of ranks
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/results_ranks.csv"
write.csv(results_overall,filename, row.names = FALSE)

# Export a table of the ranking results

filename <- paste0(folder_name, "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/BCR_summary", version, ".csv", sep="")
write.csv(BCR_summary,filename, row.names = FALSE)

##### Descriptive stats of data #####
#Highest change in persistence (What is this compared to GS benefit?)
#Lowest change in persistence
#Highest impact in both national and global
#Highest change in only endemic species
#
#Highest total cost
#Highest organization cost
#lowest total cost
#lowest organization cost
