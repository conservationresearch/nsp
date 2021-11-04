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
# library(dplyr)


########## Analysis setup ##########

### Specify the input csv spreadsheet 
inputs<-read.csv(file="C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Inputs/inputs_manuscript_v2.csv")

### Specify number of simulations and set the random seed.
number_of_simulations <- 10000
set.seed(123) # to ensure the same set of random numbers each time

### Specify project G&A proportion (general and admin cost)
G_and_A_prop_of_total <- 0.15

### Specify what the max score is for functional
functional_score_max <- 10 # 10 if using the fine-scale weights

### Specify number of species of interest
top_species <- 10 #For table of ranked species
rank_cutoff <- 6 #For sensitivity analysis

### Specify endemic species, needed for descriptive stats
endemic_species <- c("Banff Springs Snail","Maritime Ringlet","Atlantic whitefish")

### Specify species with conservation breeding potential, needed for figures
conservation_breeding <-c("Banff Springs Snail", "Woodland Caribou","Boreal Felt Lichen", 
                          "Contorted-pod Evening-Primrose", "Dakota Skipper", "Dense-flowered Lupine",
                          "Half Moon Hairstreak", "Oregon Spotted Frog", "Pacific Pond Turtle", 
                          "Spotted Owl (caurina subspecies)", "Taylor's Checkerspot", 
                          "Whitebark pine")

### Specify epsilon value if using functions 7 or 8 (see below)
epsilon <- 0.0375

### Identify the program names
org_programs <- unique(inputs$species)[which(unique(inputs$species) != "N/A")]

########## Main analysis function ##########

### Run the simulation using cost_benefit function

#IMPORTANT - User specifies which function to run
  # 1) cba_cgain
  # 2) cba_cgain_longtermasp
  # 3) cba_cgain_binnedbycgain
  # 4) cba_cgain_longtermasp_binnedbyGS
  # 5) cba_cgain_currentGS
  # 6) cba_cgain_longtermasp_currentGS
  # 7) cba_cgain_currentGS_epsilon
  # 8) cba_cgain_longtermasp_currentGS_epsilon

results_full_analysis <-  newSpeciesPrioritization::cba_cgain_longtermasp_currentGS_epsilon(org_programs = org_programs, 
                                                 inputs = inputs,
                                                 functional_score_max = functional_score_max)
                                        
### Store the results for easy use later
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
results_overall<-results_overall %>% dplyr::arrange(BCR_national_EV_rank)
results_ranking$national_level<-results_overall$org_program[which(results_overall$BCR_national_EV_rank<=top_species)]
results_ranking$BCR_national<-results_overall$BCR_national_EV[which(results_overall$BCR_national_EV_rank<=top_species)]

#Arrange data by global rank, extract specified # of top global species and associated BCR into new dataframe
results_overall<-results_overall %>% dplyr::arrange(BCR_global_EV_rank)
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
p_benefit <- newSpeciesPrioritization::graph_benefit(results_benefit_national, results_benefit_global, inputs)
p_cost <- newSpeciesPrioritization::graph_cost(results_cost_organization, results_cost_total, inputs)
p_BCR <- newSpeciesPrioritization::graph_BCR(results_overall, inputs)

# Only use if performing binned analysis
p_bincgain<-newSpeciesPrioritization::graph_bin_cgains_national
p_bincurrentgs<-newSpeciesPrioritization::graph_binnedby_currentgs


########## Export tables and figures ##########

# Export panel of figures
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/panel_benefit_cost_BCR.tiff"
tiff(filename, width=12, height=18, units="in",
     pointsize=8, compression="lzw", bg="white", res=600,
     restoreConsole=TRUE)
gridExtra::grid.arrange(p_benefit, p_cost, p_BCR, ncol = 1, nrow = 3)
dev.off()

# Export table of ranks
filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Oct 28/overall_results.csv"
write.csv(results_overall, filename, row.names = FALSE)

# Export a table of the ranking results

filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Oct 28/top_10_ranked.csv"
write.csv(results_ranking, filename, row.names = FALSE)


########## Sensitivity Analysis ##########

#do sensitivity analysis
inputs_sens <- newSpeciesPrioritization::sensitivity(org_programs, inputs, results_full_analysis, rank_cutoff = 6)

#Export the sensitivity table

#Draw tornado plots using the data from sensitivity analysis

tornado_spSpecific_national_list<-newSpeciesPrioritization::draw_tornados_national(inputs_sens = inputs_sens)
tornado_spSpecific_global_list<-newSpeciesPrioritization::draw_tornados_global(inputs_sens = inputs_sens)

## Export panel of figures
for(i in 1:length(org_programs)){
  species<-org_programs[i]
  filename <- paste0("C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Oct 28/National Sensitivity/tornado_ranks_top20_national_", species,".tiff", sep="")
  tiff(filename, width=12, height=12, units="in",
       pointsize=8, compression="lzw", bg="white", res=600,
       restoreConsole=TRUE)
  print(tornado_spSpecific_national_list[[i]])
  dev.off()
}


for(i in 1:length(org_programs)){
  species<-org_programs[i]
  filename <- paste0("C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Oct 28/Global Sensitivity/tornado_ranks_top20_global_", species,".tiff", sep="")
  tiff(filename, width=12, height=12, units="in",
       pointsize=8, compression="lzw", bg="white", res=600,
       restoreConsole=TRUE)
  print(tornado_spSpecific_global_list[[i]])
  dev.off()
}

########## Descriptive stats of data ##########

##### Highest probability-weighted average change in persistence
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
##### Lowest change in persistence
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
#####Greatest change in endemic programs 

# Still need to fix
benefit_endemic<-results_benefit_national[which(results_benefit_national$org_program==endemic_species)]
benefit_endemic_national<-max(results_benefit_national$mean[which(results_benefit_national$org_program==endemic_species)])

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
##### Cost to top 6?


## Gather into DF for summary table
descriptive_stats<-as.data.frame(matrix(nrow = 9, ncol = 3))
colnames(descriptive_stats)<-c("Stat", "Program", "Value")
descriptive_stats$Stat<-c("Highest National Benefit", "Highest Global Benefit",
                               "Lowest National Benefit", "Lowest Global Benefit",
                               "Highest Cost to Organization","Highest Total Cost",
                               "Lowest Cost to Organization", "Lowest Total Cost",
                               "Highest Benefit in Endemic Species")
#Need to fix - currently if there is a tie (such as min national benefit) then it cant add to DF because its a vector
descriptive_stats$Value[which(descriptive_stats$Stat=="Highest National Benefit")]<-benefit_max_national
descriptive_stats$Program[which(descriptive_stats$Stat=="Highest National Benefit")]<-benefit_max_national_program
descriptive_stats$Value[which(descriptive_stats$Stat=="Highest Global Benefit")]<-benefit_max_global
descriptive_stats$Program[which(descriptive_stats$Stat=="Highest Global Benefit")]<-benefit_max_global_program
descriptive_stats$Value[which(descriptive_stats$Stat=="Lowest National Benefit")]<-benefit_min_national
descriptive_stats$Program[which(descriptive_stats$Stat=="Lowest National Benefit")]<-benefit_min_national_program
descriptive_stats$Value[which(descriptive_stats$Stat=="Lowest Global Benefit")]<-benefit_min_global
descriptive_stats$Program[which(descriptive_stats$Stat=="Lowest Global Benefit")]<-benefit_min_national_program
descriptive_stats$Value[which(descriptive_stats$Stat=="Highest Cost to Organization")]<-cost_organization_max
descriptive_stats$Program[which(descriptive_stats$Stat=="Highest Cost to Organization")]<-cost_organization_max_program
descriptive_stats$Value[which(descriptive_stats$Stat=="Lowest Cost to Organization")]<-cost_organization_min
descriptive_stats$Program[which(descriptive_stats$Stat=="Lowest Cost to Organization")]<-cost_organization_min_program
descriptive_stats$Value[which(descriptive_stats$Stat=="Highest Total Cost")]<-cost_total_max
descriptive_stats$Program[which(descriptive_stats$Stat=="Highest Total Cost")]<-cost_total_max_program
descriptive_stats$Value[which(descriptive_stats$Stat=="Lowest Total Cost")]<-cost_total_min
descriptive_stats$Program[which(descriptive_stats$Stat=="Lowest Total Cost")]<-cost_total_min_program
#Add in once finished
descriptive_stats$value[which(descriptive_stats$Stat=="Highest Benefit in Endemic Species")]<-benefit_endemic_national
descriptive_stats$Program[which(descriptive_stats$Stat=="Highest Benefit in Endemic Species")]<-benefit_endemic_national_program


filename <- "C:/Users/Dylanc/OneDrive - The Calgary Zoological Society/Documents/NewSpeciesPrioritization/Results/Oct 28/descriptive_stats.csv"
write.csv(descriptive_stats, filename, row.names = FALSE)
