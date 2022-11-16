# This code runs a cost benefit analysis to prioritize new potential species for
# an organization's Conservation Translocation program.

# Started by Laura Keating and Alyssa Friesen
# Last update: July 27, 2021

# Continued by Dylan Cole
# Started September 7, 2021

# Users will run the code contained within this wrapped script

### Clear the workspace  
rm(list = ls())

### Load libraries
library(newSpeciesPrioritization)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(rmetalog)
library(data.table)
library(gtable)
library(gridExtra)
library(grid)

########## Analysis setup ##########

### Specify the input csv spreadsheet 
# inputs<-read.csv(file="C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Inputs/inputs_manuscript_v4.csv")
inputs<-read.csv(file="C:/Users/LauraK/OneDrive - The Calgary Zoological Society/New Species Prioritization/Manuscript/inputs_manuscript_v4.csv")

### Adjust any names/labels within inputs
inputs$species<-gsub(" (caurina subspecies)","", inputs$species, fixed=TRUE)
inputs$name<-gsub(" (caurina subspecies)_","_", inputs$name, fixed=TRUE)

### Specify number of simulations and set the random seed.
number_of_simulations <- 10000
set.seed(123) # to ensure the same set of random numbers each time

### Specify what the max score is for functional
functional_score_max <- 10 # 10 if using the fine-scale weights, 9 otherwise

### Specify number of species of interest
top_species <- 10 #For table of ranked species, limits to a smaller pool of highly ranked projects
rank_cutoff <- 6 #For sensitivity analysis, number of projects that the organization is wanting to examine

### Specify endemic species, needed for descriptive stats
endemic_species <- c("Banff Springs Snail","Maritime Ringlet","Atlantic whitefish")

### Specify species with conservation breeding potential, needed for figures
conservation_breeding <-c("Banff Springs Snail", "Woodland Caribou","Boreal Felt Lichen", 
                          "Contorted-pod Evening-Primrose", "Dakota Skipper", "Dense-flowered Lupine",
                          "Half Moon Hairstreak", "Oregon Spotted Frog", "Pacific Pond Turtle", 
                          "Spotted Owl", "Taylor's Checkerspot", 
                          "Whitebark pine")

### Specify epsilon value if using functions 5 or 6 (see below)
epsilon <- 0.0375 

### Identify the program names from imported CSV
# No longer need this as moved into the function calls for the cba
# org_programs <- unique(inputs$species)[which(unique(inputs$species) != "N/A")]

########## Main analysis function ##########

### Run the simulation using cost_benefit function

#IMPORTANT - User specifies which function to run
  # 1) cba_GplusD
  # 2) cba_GplusD_LongTermPot
  # 3) cba_GplusD_BinnedByBenefit
  # 4) cba_GplusD_LongTermPot_BinnedByGS
  # 5) cba_GplusD_CurrentGS_epsilon
  # 6) cba_GplusD_LongTermPot_CurrentGS_epsilon

results_full_analysis <-  newSpeciesPrioritization::cba_GplusD(org_programs = org_programs, 
                                                 inputs = inputs,
                                                 functional_score_max = functional_score_max, 
                                                 epsilon = epsilon)
                                        
### Store the results
results_overall <- results_full_analysis[[1]]
results_cost_total <- results_full_analysis[[2]]
results_cost_organization <- results_full_analysis[[3]]
results_benefit_national <- results_full_analysis[[4]]
results_benefit_global <- results_full_analysis[[5]]
results_BCR_national <- results_full_analysis[[6]]
results_BCR_global <- results_full_analysis[[7]]

########## Identifying and extracting list of top species ##########

### Pick top 5 from national and global respectively, check for overlap
results_ranking <- as.data.frame(matrix(nrow = top_species, ncol = 5))
colnames(results_ranking) <- c("rank","national_level", "global_level",
                               "BCR_national", "BCR_global")
results_ranking$rank<-c(1:top_species)

#Arrange data by national rank, extract specified # of top national species and associated BCR into new dataframe
results_overall<-dplyr::arrange(results_overall, BCR_national_EV_rank)
results_ranking$national_level<-results_overall$org_program[which(results_overall$BCR_national_EV_rank<=top_species)]
results_ranking$BCR_national<-results_overall$BCR_national_EV[which(results_overall$BCR_national_EV_rank<=top_species)]

#Arrange data by global rank, extract specified # of top global species and associated BCR into new dataframe
results_overall<-dplyr::arrange(results_overall, BCR_global_EV_rank)
results_ranking$global_level<-results_overall$org_program[which(results_overall$BCR_global_EV_rank<=top_species)]
results_ranking$BCR_global<-results_overall$BCR_global_EV[which(results_overall$BCR_global_EV_rank<=top_species)]

#Determine overlapping species and append list onto the results_ranking dataframe, incorporating NAs into 
#unpopulated cells only in overlapping species column
common_programs<-results_ranking$national_level[results_ranking$national_level %in% results_ranking$global_level]
clength<-max(length(results_ranking$rank), length(common_programs))
length(common_programs)<-clength
results_ranking<-cbind(results_ranking, common_programs)

########## Figures ##########

### Draw figures from figures function to show the results
figure_benefit <- newSpeciesPrioritization::graph_benefit(results_benefit_national, results_benefit_global, inputs, endemic_species)
figure_cost <- newSpeciesPrioritization::graph_cost(results_cost_organization, results_cost_total, inputs)
figure_BCR <- newSpeciesPrioritization::graph_BCR(results_BCR_national, results_BCR_global, inputs, endemic_species)
figure_BCR_uncertainty <- newSpeciesPrioritization::graph_BCR_uncertainty(results_BCR_national,results_BCR_global, inputs)
figure_direction_BCR_uncertainty <- newSpeciesPrioritization::graph_BCR_directional_uncertainty(results_BCR_national, results_BCR_global, inputs)

### Only use if performing binned analysis
#for option 3
figure_bincgain<-newSpeciesPrioritization::bargraph_BCR_binnedby_benefits(results_BCR_national, results_BCR_global, inputs, results_overall, endemic_species)
figure_bincgain_scatternational<-newSpeciesPrioritization::scatter_bin_benefits_national(results_benefit_national, inputs, results_overall)
figure_bincgain_scatterglobal<-newSpeciesPrioritization::scatter_bin_benefits_global(results_benefit_global, inputs, results_overall)
#for option 4
figure_bincurrentgs<-newSpeciesPrioritization::bargraph_binnedby_currentgs(results_BCR_global, results_BCR_national, inputs, results_overall)
figure_bincurrentgs_scatternational<-newSpeciesPrioritization::scatter_bin_currentgs_national(results_overall,results_benefit_national)
figure_bincurrentgs_scatterglobal<-newSpeciesPrioritization::scatter_bin_currentgs_global(results_overall,results_benefit_global)

########## Export tables and figures ##########

# Export panel of figures
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Manuscript Figures Final/Final Figures v4 - Revision Stage/Option6_BCR_Uncertainty_v3.tiff"
grid::grid.newpage()

tiff(filename, width=18, height=10, units="in",
     pointsize=14, compression="lzw", bg="white",res=1080,
     restoreConsole=TRUE)
grid::grid.draw(cbind(figure_BCR, figure_direction_BCR_uncertainty))
gridExtra::grid.arrange(figure_BCR,figure_direction_BCR_uncertainty, ncol = 2, nrow = 1)
dev.off()

#Doing fig 1 A+B, and fig 2 A+B to get them sized appropriately
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Manuscript Figures Final/Final Figures v4 - Revision Stage/option6_panel_benefit_costv2.tiff"
tiff(filename, width=12, height=18, units="in",
     pointsize=14, compression="lzw", bg="white", res=600,
     restoreConsole=TRUE)
gridExtra::grid.arrange(figure_benefit, figure_cost, ncol=1, nrow=2)
dev.off()

filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Manuscript Figures Final/BCR_Uncertainty_option6.tiff"
tiff(filename, width=18, height=10, units="in",
     pointsize=14, compression="lzw", bg="white", res=1080,
     restoreConsole=TRUE)
gridExtra::grid.arrange(BCR_graph_final, figure_direction_BCR_uncertainty, ncol=2, nrow=1)
dev.off()

#Export Option 3 / 4 figures
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Manuscript Figures Final/Final Figures v3/Option4_Scatter.tiff"
tiff(filename, width=18, height=10, units="in",
     pointsize=14, compression="lzw", bg="white",res=1080,
     restoreConsole=TRUE)
gridExtra::grid.arrange(figure_bincurrentgs_scatternational,figure_bincurrentgs_scatterglobal, ncol = 2, nrow = 1)
dev.off()


# Export table of ranks
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/overall_results.csv"
write.csv(results_overall, filename, row.names = FALSE)

# Export a table of the ranking results
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/top_10_ranked.csv"
write.csv(results_ranking, filename, row.names = FALSE)


########## Sensitivity Analysis ##########

#do sensitivity analysis
inputs_sens <- newSpeciesPrioritization::sensitivity(inputs, results_full_analysis, rank_cutoff = 6)

#Export the sensitivity table

#Draw tornado plots using the data from sensitivity analysis

tornado_spSpecific_national_list<-newSpeciesPrioritization::draw_tornados_national(inputs_sens = inputs_sens)
tornado_spSpecific_global_list<-newSpeciesPrioritization::draw_tornados_global(inputs_sens = inputs_sens)

## Export figures
for(i in 1:length(org_programs)){
  species<-org_programs[i]
  filename <- paste0("C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Manuscript Figures Final/Final Figures v2/TornadoPlots/National/tornado_ranks_top20_national_", species,".tiff", sep="")
  tiff(filename, width=12, height=12, units="in",
       pointsize=8, compression="lzw", bg="white", res=600,
       restoreConsole=TRUE)
  print(tornado_spSpecific_national_list[[i]])
  dev.off()
}


for(i in 1:length(org_programs)){
  species<-org_programs[i]
  filename <- paste0("C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Manuscript Figures Final/Final Figures v2/TornadoPlots/Global/tornado_ranks_top20_global_", species,".tiff", sep="")
  tiff(filename, width=12, height=12, units="in",
       pointsize=8, compression="lzw", bg="white", res=600,
       restoreConsole=TRUE)
  print(tornado_spSpecific_global_list[[i]])
  dev.off()
}

########## Descriptive stats of data ##########

##### Highest benefit
#National
benefit_max_national<-max(results_benefit_national$mean)
round(benefit_max_national, digits = 1)
benefits_max_national_program <- results_benefit_national$org_program[which(results_benefit_national$mean == benefit_max_national)]
benefits_max_national_program
#Global
benefit_max_global<-max(results_benefit_global$mean)
round(benefit_max_global, digits = 1)
benefits_max_global_program <- results_benefit_global$org_program[which(results_benefit_global$mean == benefit_max_global)]
benefits_max_global_program
##### Lowest benefit
#National 
benefit_min_national<-min(results_benefit_national$mean)
round(benefit_min_national, digits = 1)
benefit_min_national_program <- results_benefit_national$org_program[which(results_benefit_national$mean == benefit_min_national)]
benefit_min_national_program
#Global
benefit_min_global<-min(results_benefit_global$mean)
round(benefit_min_global, digits = 1)
benefit_min_global_program <- results_benefit_global$org_program[which(results_benefit_global$mean == benefit_min_global)]
benefit_min_global_program

#####Highest total cost
cost_total_max <- max(results_cost_total$mean)
round(cost_total_max, digits = 1)
cost_total_max_program <- results_cost_total$org_program[which(results_cost_total$mean==cost_total_max)]
cost_total_max_program
#####Highest organization cost
cost_organization_max <-max(results_cost_organization$mean)
round(cost_organization_max, digits = 1)
cost_organization_max_program <- results_cost_organization$org_program[which(results_cost_organization$mean==cost_organization_max)]
cost_organization_max_program
#####lowest total cost
cost_total_min <- min(results_cost_total$mean)
round(cost_total_min, digits = 1)
cost_total_min_program <- results_cost_total$org_program[which(results_cost_total$mean==cost_total_min)]
cost_total_min_program
#####lowest organization cost
cost_organization_min <-min(results_cost_organization$mean)
round(cost_organization_min, digits = 1)
cost_organization_min_program <- results_cost_organization$org_program[which(results_cost_organization$mean==cost_organization_min)]
cost_organization_min_program

