# This file contains the main cost-benefit function.
# Laura Keating
# July 2021

#####  graph_benefit  #####

#' STILL TO CLEAN UP CODE

#' INSERT
#'
#' @param results_benefit_national TO DO: ADD DESCRIPTION
#' @param results_benefit_global TO DO: ADD DESCRIPTION
#' @param inputs TO DO: ADD DESCRIPTION


#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
graph_benefit <- function(results_benefit_national, results_benefit_global, inputs){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_benefit_global$category <- "Global"
  results_benefit_national$category <- "National"
  sim_results_summary_benefits <- rbind(results_benefit_global, results_benefit_national)
  
  ## Need to add in symbol for endemic and conservation breeding ##
  # Make the graph
  
  ## Create a data frame to identify species that are endemic
  sim_results_summary_benefits <- sim_results_summary_benefits %>%
    dplyr::mutate(lab = dplyr::if_else(CZCT_program %in% c("Banff Springs Snail","Maritime Ringlet","Atlantic whitefish"), "*", ""))
  
  # Change the Boreal Caribou label to Woodland caribou
  sim_results_summary_benefits$CZCT_program <- as.character(sim_results_summary_benefits$CZCT_program) # remove the factor levels
  sim_results_summary_benefits$CZCT_program[which(sim_results_summary_benefits$CZCT_program == "Boreal Caribou")] <- "Woodland Caribou"
  
  # # Reorder the factor levels for the CZCT program so that they are ordered by global benefits.
  # results_benefit_global$CZCT_program <- as.character(results_benefit_global$CZCT_program) # remove the factor levels
  # results_benefit_global$CZCT_program[which(results_benefit_global$CZCT_program == "Boreal Caribou")] <- "Woodland Caribou"
  # sim_results_summary_benefits$CZCT_program <- factor(sim_results_summary_benefits$CZCT_program, 
  #                                                     levels=unique(results_benefit_global$CZCT_program[order(results_benefit_global$mean, decreasing = TRUE)]), ordered=TRUE)
  
  
  
  # Add triage cateogry
  # CZCT_program_triage$triage_category
  # sim_results_summary_benefits$triage_category <- NA # initalize
  # sim_results_summary_benefits$triage_category <- CZCT_program_triage$triage_category[match(sim_results_summary_benefits$CZCT_program, CZCT_program_triage$CZCT_program)]
  
  # Reorder the factor levels for the CZCT program so that they are ordered by triage category first and then global benefits.
  # results_benefit_global$CZCT_program <- as.character(results_benefit_global$CZCT_program) # remove the factor levels
  # sim_results_summary_benefits$CZCT_program <- factor(sim_results_summary_benefits$CZCT_program, 
  #                                                     levels=unique(results_benefit_global$CZCT_program[order(sim_results_summary_benefits$triage_category,
  #                                                                                                             -sim_results_summary_benefits$mean, decreasing = FALSE)]), ordered=TRUE)
  
  results_benefit_global$CZCT_program <- as.character(results_benefit_global$CZCT_program) # remove the factor levels
  sim_results_summary_benefits$CZCT_program <- factor(sim_results_summary_benefits$CZCT_program, 
                                                      levels=unique(results_benefit_global$CZCT_program[order(-sim_results_summary_benefits$mean, decreasing = FALSE)]), ordered=TRUE)
  
  # Make the graph
  # benefits_graph <- ggplot(sim_results_summary_benefits, aes(x = CZCT_program, y = mean, fill = category, color = triage_category)) +
  benefits_graph <- ggplot2::ggplot(sim_results_summary_benefits, ggplot2::aes(x = CZCT_program, y = mean, fill = category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::geom_text(ggplot2::aes(label = lab), vjust=-0.25, size = 10)+
    #scale_colour_manual(values=c("#000000")) +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    #scale_alpha_manual(values=c(1,0.3)) +
    ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5, ymax=P95), width=.1, color = "black") +
    # labs(x = "Species Program",
    ggplot2::labs(x = "",
                  # y = "\nProbability-weighted Average Benefit - \n Green Status Gain plus Dependence (%)",
                  y = "\nProbability-weighted Average Benefit - \n Green Score Benefit Proxy",
                  title = "A") +
    ggplot2::labs(alpha = "Category", fill = "Category")+
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.95,0.95),
                   # theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = "bottom",
                   # theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = c(0.05,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) 
  benefits_graph
  
  
  ## Try making the graph with two facets, one for the small values and one for the large (pond turtle) value
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentNational")])) == 0)])
  species_extinct_global <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentGlobal")])) == 0)])
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  sim_results_summary_benefits$extirpated_or_extinct <- "Extant" # initialize
  sim_results_summary_benefits$extirpated_or_extinct[is.na(match(sim_results_summary_benefits$CZCT_program, extirpated_or_extinct)) == FALSE] <- "EX/EW" # initialize
  
  sim_results_summary_benefits$extirpated_or_extinct <- factor(sim_results_summary_benefits$extirpated_or_extinct, 
                                                               levels = c("Extant", "EX/EW"))
  
  benefits_graph2 <- ggplot2::ggplot(sim_results_summary_benefits, ggplot2::aes(x = CZCT_program, y = mean, fill = category)) +
    ggplot2::geom_bar(stat="identity",position =ggplot2::position_dodge()) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free") + 
    ggplot2::geom_text(ggplot2::aes(label = lab), vjust=-0.25, size = 10)+
    #scale_colour_manual(values=c("#000000")) +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    #scale_alpha_manual(values=c(1,0.3)) +
    ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5, ymax=P95), width=.1, color = "black") +
    # labs(x = "Species Program",
    ggplot2::labs(x = "",
                  # y = "\nProbability-weighted Average Benefit - \n Green Status Gain plus Dependence (%)",
                  y = "\nProbability-weighted Average Benefit - \n Green Score Benefit Proxy",
                  title = "A") +
    ggplot2::labs(alpha = "Category", fill = "Category")+
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.85,0.95),
                   # theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = "bottom",
                   # theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = c(0.05,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) 
  
  # Now get the facets spaced more appropriately
  # following https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap/52422707
  # convert ggplot object to grob object
  gp <- ggplot2::ggplotGrob(benefits_graph2)
  # optional: take a look at the grob object's layout
  # gtable::gtable_show_layout(gp)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  x.var <- sapply(ggplot2::ggplot_build(benefits_graph2 )$layout$panel_scales_x,
                  function(l) length(l$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$widths[facet.columns] <- gp$widths[facet.columns] * x.var
  
  # plot result
  grid::grid.draw(gp)
  benefits_graph2_final <- gp
  
  return(benefits_graph2_final)
  
  
} 



#####  graph_cost  #####

#' STILL TO CLEAN UP CODE

#' INSERT
#'
#' @param results_cost_organization TO DO: ADD DESCRIPTION
#' @param results_cost_total TO DO: ADD DESCRIPTION
#' @param inputs TO DO: ADD DESCRIPTION


#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
graph_cost <- function(results_cost_organization, results_cost_total, inputs){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_cost_organization$category <- "Cost to the Calgary Zoo"
  results_cost_total$category <- "Costs Offset by Funders" # "Total Project Cost"
  sim_results_summary_costs <- rbind(results_cost_organization, results_cost_total)
  
  ## Create a data frame to identify species that are conservation breeding relevant
  sim_results_summary_costs <- sim_results_summary_costs %>%
    dplyr::mutate(lab = dplyr::if_else(category == "Costs Offset by Funders" & CZCT_program %in% c("Half Moon Hairstreak","Taylor's Checkerspot","Banff Springs Snail",
                                                                                                   "Whitebark pine","Spotted Owl (caurina subspecies)","Boreal Felt Lichen","Dense-flowered Lupine","Contorted-pod Evening-Primrose",
                                                                                                   "Dakota Skipper","Oregon Spotted Frog","Caribou",
                                                                                                   # "Southern Mountain Caribou","Pacific Pond Turtle"), "   B", ""))
                                                                                                   "Pacific Pond Turtle"), "   B", ""))
  
  # # Change the Boreal Caribou label to Woodland caribou
  # results_cost_organization$CZCT_program <- as.character(results_cost_organization$CZCT_program) # remove the factor levels
  # results_cost_organization$CZCT_program[which(results_cost_organization$CZCT_program == "Boreal Caribou")] <- "Woodland Caribou"
  # sim_results_summary_costs$CZCT_program <- as.character(sim_results_summary_costs$CZCT_program) # remove the factor levels
  # sim_results_summary_costs$CZCT_program[which(sim_results_summary_costs$CZCT_program == "Boreal Caribou")] <- "Woodland Caribou"
  
  # # Reorder the factor levels for the CZCT program so that they are ordered by global benefits
  # sim_results_summary_costs$CZCT_program <- factor(sim_results_summary_costs$CZCT_program, 
  #                                                  levels=unique(results_cost_organization$CZCT_program[order(results_cost_organization$mean)]), ordered=TRUE)
  # 
  
  
  # Add triage cateogry
  # CZCT_program_triage$triage_category
  # sim_results_summary_costs$triage_category <- NA # initalize
  # sim_results_summary_costs$triage_category <- CZCT_program_triage$triage_category[match(sim_results_summary_costs$CZCT_program, CZCT_program_triage$CZCT_program)]
  
  # Reorder the factor levels for the CZCT program so that they are ordered by triage category first and then global benefits.
  results_benefit_global$CZCT_program <- as.character(results_benefit_global$CZCT_program) # remove the factor levels
  # sim_results_summary_costs$CZCT_program <- factor(sim_results_summary_costs$CZCT_program, 
  #                                                     levels=unique(results_benefit_global$CZCT_program[order(sim_results_summary_costs$triage_category,
  #                                                                                                             -sim_results_summary_costs$mean, decreasing = FALSE)]), ordered=TRUE)
  
  sim_results_summary_costs$CZCT_program <- factor(sim_results_summary_costs$CZCT_program, 
                                                   levels=unique(results_benefit_global$CZCT_program[order(-sim_results_summary_costs$mean, decreasing = FALSE)]), ordered=TRUE)
  
  # Make the graph
  costs_graph <- ggplot2::ggplot(sim_results_summary_costs, ggplot2::aes(x = CZCT_program, y = mean/1000, alpha = category)) +
    # costs_graph <- ggplot(sim_results_summary_costs, aes(x = CZCT_program, y = mean/1000, alpha = category, color = triage_category)) +
    ggplot2::geom_bar(stat="identity",position ="identity") +
    ggplot2::scale_alpha_manual(values=c(1, .3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=P5/1000, ymax=P95/1000), width=.1, color = "black") +
    #geom_hline(yintercept = 1000, linetype = "dashed", color = "red") +
    #geom_hline(yintercept = 3000, linetype = "dashed", color = "red") +
    # labs(x = "Species Program",
    ggplot2::labs(x = "",
                  y = "Probability-weighted Average Cost\n over 10 Years (CAD Thousands)",
                  title = "B") +
    ggplot2::labs(alpha = "Category")+
    ggplot2::scale_y_continuous(breaks = seq(0, 12000, by = 1000)) + # Adjusted according to Jana's comments
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.95,0.95),
                   # theme(axis.text.x=element_text(angle=45, hjust=1),legend.justification = c(0,1),legend.position = "bottom",
                   # theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = c(0.05,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::geom_text(ggplot2::aes(label = lab), show.legend = FALSE, vjust=-0.25, size = 4, color = "black") #aes(label = lab, y=P95/1000)
  
  costs_graph
  
  ## Try making the graph with two facets, one for the small values and one for the large (pond turtle) value
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentNational")])) == 0)])
  species_extinct_global <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentGlobal")])) == 0)])
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  sim_results_summary_costs$extirpated_or_extinct <- "Extant" # initialize
  sim_results_summary_costs$extirpated_or_extinct[is.na(match(sim_results_summary_costs$CZCT_program, extirpated_or_extinct)) == FALSE] <- "EX/EW" # initialize
  
  sim_results_summary_costs$extirpated_or_extinct <- factor(sim_results_summary_costs$extirpated_or_extinct, 
                                                            levels = c("Extant", "EX/EW"))
  
  
  costs_graph2 <- ggplot2::ggplot(sim_results_summary_costs, ggplot2::aes(x = CZCT_program, y = mean/1000, alpha = category)) +
    # costs_graph <- ggplot(sim_results_summary_costs, aes(x = CZCT_program, y = mean/1000, alpha = category, color = triage_category)) +
    ggplot2::geom_bar(stat="identity",position ="identity") +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free") + 
    ggplot2::scale_alpha_manual(values=c(1, .3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=P5/1000, ymax=P95/1000), width=.1, color = "black") +
    #geom_hline(yintercept = 1000, linetype = "dashed", color = "red") +
    #geom_hline(yintercept = 3000, linetype = "dashed", color = "red") +
    # labs(x = "Species Program",
    ggplot2::labs(x = "",
                  y = "Probability-weighted Average Cost\n over 10 Years (CAD Thousands)",
                  title = "B") +
    ggplot2::labs(alpha = "Category")+
    ggplot2::scale_y_continuous(breaks = seq(0, 12000, by = 1000)) + # Adjusted according to Jana's comments
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.85,0.95),
                   # theme(axis.text.x=element_text(angle=45, hjust=1),legend.justification = c(0,1),legend.position = "bottom",
                   # theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = c(0.05,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::geom_text(ggplot2::aes(label = lab), show.legend = FALSE, vjust=-0.25, size = 4, color = "black") #aes(label = lab, y=P95/1000)
  
  
  # Now get the facets spaced more appropriately
  # following https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap/52422707
  # convert ggplot object to grob object
  gp <- ggplot2::ggplotGrob(costs_graph2)
  # optional: take a look at the grob object's layout
  # gtable::gtable_show_layout(gp)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  x.var <- sapply(ggplot2::ggplot_build(costs_graph2)$layout$panel_scales_x,
                  function(l) length(l$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$widths[facet.columns] <- gp$widths[facet.columns] * x.var
  
  # plot result
  grid::grid.draw(gp)
  costs_graph2_final <- gp
  
  return(costs_graph2_final)
  
  
} 



#####  graph_BCR  #####

#' STILL TO CLEAN UP CODE

#' INSERT
#'
#' @param results_overall TO DO: ADD DESCRIPTION
#' @param inputs TO DO: ADD DESCRIPTION


#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
graph_BCR <- function(results_overall, inputs){
  
  ## Make a panel figure with non-standardized BCR (change in persistence per dollar)
  
  # # Change the Boreal Caribou label to Woodland caribou
  # results_overall$CZCT_program <- as.character(results_overall$CZCT_program) # remove the factor levels
  # results_overall$CZCT_program[which(results_overall$CZCT_program == "Boreal Caribou")] <- "Woodland Caribou"
  
  # Order the species by rank
  # results_overall$CZCT_program <- factor(results_overall$CZCT_program, 
  #                                                   levels=unique(results_overall$CZCT_program[order(results_overall$BCR_national_EV_rank)]), ordered=TRUE)
  
  # Add triage cateogry
  # CZCT_program_triage$triage_category
  # results_overall$triage_category <- NA # initalize
  # results_overall$triage_category <- CZCT_program_triage$triage_category[match(results_overall$CZCT_program, CZCT_program_triage$CZCT_program)]
  
  # Reorder the factor levels for the CZCT program so that they are ordered by triage category first and then BCR national rank
  results_benefit_global$CZCT_program <- as.character(results_benefit_global$CZCT_program) # remove the factor levels
  # results_overall$CZCT_program <- factor(results_overall$CZCT_program, 
  #                                                  levels=unique(results_benefit_global$CZCT_program[order(results_overall$triage_category,
  #                                                                                                          -results_overall$weighted_BCRs, decreasing = FALSE)]), ordered=TRUE)
  results_overall$CZCT_program <- factor(results_overall$CZCT_program, 
                                         levels=unique(results_benefit_global$CZCT_program[order(-results_overall$weighted_BCRs, decreasing = FALSE)]), ordered=TRUE)
  
  
  
  ## Gather dataframe to create figure with both global and national BCR
  BCR_summary <- tidyr::gather(results_overall, category, value, c("BCR_national_EV", "BCR_global_EV"))
  BCR_summary$category <- dplyr::recode(BCR_summary$category,"BCR_global_EV" = "Global", "BCR_national_EV" = "National")
  colnames(BCR_summary)[which(colnames(BCR_summary) == "category")] <- "Category"
  
  # Create figure
  # BCR_per_cad <- ggplot(BCR_summary, aes(x = CZCT_program, y = value*100000, alpha = Category,  color = triage_category)) +
  BCR_per_cad <- ggplot2::ggplot(BCR_summary, ggplot2::aes(x = CZCT_program, y = value*100000, alpha = Category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::scale_alpha_manual(values=c(1,.3)) +
    ggplot2::labs(x = "Species Program",
                  # y = "Average BCR \n (% Change in 50yr Species Persistence per 100 000 CAD)",
                  # y = "Average BCR \n (Green Status Gain plus Dependence (%) per 100 000 CAD)",
                  y = "Average BCR \n (Green Score Benefit Proxy per 100 000 CAD)",
                  title = "C") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
  BCR_per_cad
  
  # Make the same graph as above but add the credible intervals
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_BCR_global$category <- "Global"
  results_BCR_national$category <- "National"
  results_BCR_all <- rbind(results_BCR_global, results_BCR_national)
  
  # Add triage cateogry
  # results_BCR_all$triage_category <- NA # initalize
  # results_BCR_all$triage_category <- CZCT_program_triage$triage_category[match(results_BCR_all$CZCT_program, CZCT_program_triage$CZCT_program)]
  # 
  # Reorder the factor levels for the CZCT program so that they are ordered by triage category first and then BCR national rank
  # results_BCR_all$CZCT_program <- factor(results_BCR_all$CZCT_program, 
  #                                        levels=unique(results_BCR_all$CZCT_program[order(results_overall$triage_category,
  #                                                                                         -results_overall$weighted_BCRs, decreasing = FALSE)]), ordered=TRUE)
  # 
  # 
  # # Order the species by rank
  # results_BCR_all$CZCT_program <- factor(results_BCR_all$CZCT_program, 
  #                                        levels=unique(results_overall$CZCT_program[order(results_overall$BCR_national_EV_rank)]), ordered=TRUE)
  
  
  BCR_per_cad2 <- ggplot2::ggplot(results_BCR_all, ggplot2::aes(x = CZCT_program, y = mean, fill = category, color = triage_category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    # geom_text(aes(label = lab), vjust=-0.25, size = 10)+
    #scale_colour_manual(values=c("#000000")) +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    #scale_alpha_manual(values=c(1,0.3)) +
    ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5, ymax=P95), width=.1, color = "black") +
    # labs(x = "Species Program",
    ggplot2::labs(x = "",
                  # y = "Average BCR \n (% Change in 50yr Species Persistence per CAD)",
                  y = "Average BCR \n (Green Status Gain plus Dependence (%) per 100 000 CAD)",
                  title = "A") +
    ggplot2::labs(alpha = "Category", fill = "Category")+
    ggplot2::theme_bw() + 
    # theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = c(0.05,0.95),
    ggplot2::theme(axis.text.x= ggplot2::element_text(angle=45, hjust=1), legend.position = "bottom",
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) 
  BCR_per_cad2 
  
  
  
  
  ## Try making the graph with two facets, one for the small values and one for the large (pond turtle) value
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentNational")])) == 0)])
  species_extinct_global <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentGlobal")])) == 0)])
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  BCR_summary$extirpated_or_extinct <- "Extant" # initialize
  BCR_summary$extirpated_or_extinct[is.na(match(BCR_summary$CZCT_program, extirpated_or_extinct)) == FALSE] <- "EX/EW" # initialize
  
  BCR_summary$extirpated_or_extinct <- factor(BCR_summary$extirpated_or_extinct, 
                                              levels = c("Extant", "EX/EW"))
  
  BCR_per_cad3 <- ggplot2::ggplot(BCR_summary, ggplot2::aes(x = CZCT_program, y = value*100000, alpha = Category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free") + 
    ggplot2::scale_alpha_manual(values=c(1,.3)) +
    ggplot2::labs(x = "Species Program",
                  # y = "Average BCR \n (% Change in 50yr Species Persistence per 100 000 CAD)",
                  # y = "Average BCR \n (Green Status Gain plus Dependence (%) per 100 000 CAD)",
                  y = "Average BCR \n (Green Score Benefit Proxy per 100 000 CAD)",
                  title = "C") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.85,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
  
  # Now get the facets spaced more appropriately
  # following https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap/52422707
  # convert ggplot object to grob object
  gp <- ggplot2::ggplotGrob(BCR_per_cad3)
  # optional: take a look at the grob object's layout
  # gtable::gtable_show_layout(gp)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  x.var <- sapply(ggplot2::ggplot_build(BCR_per_cad3)$layout$panel_scales_x,
                  function(l) length(l$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$widths[facet.columns] <- gp$widths[facet.columns] * x.var
  
  # plot result
  grid::grid.draw(gp)
  BCR_per_cad3_final <- gp
  
  
  return(BCR_per_cad3_final)
  
  
} 
