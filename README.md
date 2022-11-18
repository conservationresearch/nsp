# nsp
New Species Prioritization

This package was developed by the Wilder Institute/Calgary Zoo as a method to rank and prioritize new conservation translocation projects through a
structured decision making process combined with a cost benefit analysis. This package is designed to perform the latter. Prior to using this package,
a user must prepare a list of candidate projects (or species of interest). For each project, the user must also calculate Green Status scores and the four
associated metrics and the potential costs for each project. Due to the inherent uncertainty in both quantifying species status and project costs, users must 
calculate the worst case (P5), best guess (P50), best case (P95) scenarios for each score, metric, and cost. Furthermore, because
the Wilder Institute/Calgary Zoo was interested in results at both the national and global scale, this package is able to extend the national benefits and
costs and extend these to a global scale. Therefore, users must also supply a low and high range of their global distribution attributed to the country of 
interest, and the probabilities of those ranges (probabilities must add to 100). A user must also supply the P5, P50, and P95 for two additional metrics not 
associated with any specific project, the "General and Administrative" and a "Mandatory Organization Contribution", both of which are percents of total costs.

This cost benefit analysis uses the input csv file and builds a metalog distribution using the P5, P50, and P95 for each benefit and cost metric. A Monte Carlo
simulation then samples a user-specified number of values from that distribution into a single vector. The mean of these values for each metric can then be used
in the cost-benefit analysis. The benefits are then divided by the cost to give the benefit cost ratio (BCRs) for each project, and these ratios are what the package then ranks. Higher ranking projects would maximize the benefit and minimize cost. 

Each organization has different perspectives and can choose to quantify the benefits differently. Six options are provided in hopes that organizations will be
able to reflect their beliefs in how they perform the analysis. Option 1, the simplest, quantifies the benefit as the Conservation Gain plus Conservation Dependence
metric. Option 2 builds upon Option 1 quantifies benefits as Conservation Gain Plus Dependence, relative to Long Term Aspiration metric. Option 3 performs the same
calculations as Option 1, however, before ranking the BCRs it bins the conservation projects based on the total benefits. Projects with higher total gain, regardless
of cost, are given priority. Option 4 performs the same calculations as Option 2, however, before ranking the BCRs it bins the conservation projects based on their
Current Green Score. Projects that have a lower Current Green Score are given priority giving priority to species that are more at risk. Option 5 calculates the benfit as the Conservation Gain plus Conservation Dependence, relative to Current Green Score plus epsilon. Option 6 quantifies the benefit as the Conservation Gain Plus Conservation Dependence, relative to Long Term Aspiration, and relative to the Current Green Score plus epsilon. Epsilon is a subjective term that is included to resolve the problem of dividing by zero in Species that have a Current Green Score of zero (extirpated). 

To use this package, users will need to edit the wrapper script new_species_prioritization.R file within the "Scripts" folder. After editing the input and output locations, specifiying variables, etc, this wrapper script can then call the other scripts contained within the "R" folder to perform the value draws, analysis, figure generation, and sensitivty analysis. 

Here is some example code showing how to use this package:
#---- Clear the workspace. ----
rm(list = ls())

#---- Install the library if you haven't already. -------
remotes::install_github("conservationresearch/newSpeciesPrioritization")

#---- Load the newSpeciesPrioritization package. ----
library(newSpeciesPrioritization)

#---- Load other packages used here for figures. ----
library(gridExtra)
library(grid)

#---- Load example data ----
dat_example <- newSpeciesPrioritization::ExampleDataset
# dat_example <- read.csv(file="C:/Users/LauraK/OneDrive - The Calgary Zoological Society/New Species Prioritization/Manuscript/inputs_manuscript_v4.csv") # for my practicing

#---- Specify the number of iterations and set the random seed ----
number_of_simulations <- 10000
set.seed(123) # to ensure the same set of random numbers each time

#---- Specify some parameters. ----
functional_score_max <- 10 # 10 if using the fine-scale weights, 9 otherwise
epsilon <- 0.0375
endemic_species <- c("Species A", "Species D") # Specify endemic species, needed to label the benefit and BCR figures with a * 
conservation_breeding <-c("Species A", "Species B", "Species C", "Species E") # Specify species with conservation breeding potential, needed to label the cost figure with a 'B'

#---- Run the simulation using cost_benefit function ----

######IMPORTANT - User specifies which function to run
###### 1) cba_GplusD
###### 2) cba_GplusD_LongTermPot
###### 3) cba_GplusD_BinnedByBenefit
###### 4) cba_GplusD_LongTermPot_BinnedByGS
###### 5) cba_GplusD_CurrentGS_epsilon
###### 6) cba_GplusD_LongTermPot_CurrentGS_epsilon

results_full_analysis <-  newSpeciesPrioritization::cba_GplusD_LongTermPot_CurrentGS_epsilon(org_programs = org_programs, 
                                                               inputs = dat_example,
                                                               functional_score_max = functional_score_max, 
                                                               epsilon = epsilon)

#---- Extract/store the individual components of the results  ----

results_overall <- results_full_analysis[[1]] # this is a summary table of the results
results_cost_total <- results_full_analysis[[2]]
results_cost_organization <- results_full_analysis[[3]]
results_benefit_national <- results_full_analysis[[4]]
results_benefit_global <- results_full_analysis[[5]]
results_BCR_national <- results_full_analysis[[6]]
results_BCR_global <- results_full_analysis[[7]]

#---- Graph the benefit and cost separately and show in a panel graph  ----

figure_benefit <- newSpeciesPrioritization::graph_benefit(results_benefit_national, results_benefit_global, 
                                                          inputs = dat_example,
                                                          endemic_species = endemic_species)
figure_cost <- newSpeciesPrioritization::graph_cost(results_cost_organization, results_cost_total, inputs = dat_example)
gridExtra::grid.arrange(figure_benefit, figure_cost, ncol=2, nrow=1)

#---- Graph the BCR and associated uncertainty and show in a panel graph   ----

figure_BCR <- newSpeciesPrioritization::graph_BCR(results_BCR_national, results_BCR_global, 
                                                  inputs = dat_example, 
                                                  endemic_species = endemic_species)
figure_BCR_uncertainty <- newSpeciesPrioritization::graph_BCR_uncertainty(results_BCR_national,results_BCR_global, inputs = dat_example)
figure_direction_BCR_uncertainty <- newSpeciesPrioritization::graph_BCR_directional_uncertainty(results_BCR_national, results_BCR_global, inputs = dat_example)
grid::grid.draw(cbind(figure_BCR, figure_direction_BCR_uncertainty))

#---- Run the sensitivity analysis ----

inputs_sens <- newSpeciesPrioritization::sensitivity(inputs = dat_example, results_full_analysis, rank_cutoff = 3)

#---- Draw tornado plots from sensitivity analysis using the national ranking ----
tornado_spSpecific_national_list<-newSpeciesPrioritization::draw_tornados_national(inputs_sens = inputs_sens)
tornado_spSpecific_national_list[[1]] # look at the first one, change to a 2 to see the second, etc.

#---- Draw tornado plots from sensitivity analysis using the global ranking ----
tornado_spSpecific_global_list<-newSpeciesPrioritization::draw_tornados_global(inputs_sens = inputs_sens)
tornado_spSpecific_global_list[[1]] # look at the first one, change to a 2 to see the second, etc.
