# Contains the code for drawing figures - includes
#     - Benefits
#     - Cost
#     - BCR
#     - Benefits binned by conservation gains
#     - Benefits binned by current GS
#Includes code originally written by Laura Keating (Last updated July 2021) and continued by Dylan Cole


##### Graphing the benefits #####
#' INSERT
#'
#' @param results_benefit_national a dataframe from cost_benefit_analysis that provides the national benefit each program
#' @param results_benefit_global a dataframe from cost_benefit_analysis that provides the global benefit each program
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95)
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
graph_benefit <- function(results_benefit_national, results_benefit_global, inputs){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_benefit_global$category <- "Global"
  results_benefit_national$category <- "National"
  sim_results_summary_benefits <- rbind(results_benefit_national,results_benefit_global)
  
  # Make the graph
  
  ## Create a data frame to identify species that are endemic
  sim_results_summary_benefits <- sim_results_summary_benefits %>%
    dplyr::mutate(lab = dplyr::if_else(org_program %in% endemic_species, "*", ""))
  # remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program) 
  # order by decreasing global benefit
  sim_results_summary_benefits$org_program <- factor(sim_results_summary_benefits$org_program, 
                                                     levels=unique(results_benefit_national$org_program[order(-sim_results_summary_benefits$mean, decreasing = FALSE)]), ordered=TRUE)
  
  # Makefigure
  benefits_graph <- ggplot2::ggplot(sim_results_summary_benefits, ggplot2::aes(x = org_program, y = mean, fill = category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::geom_text(ggplot2::aes(label = lab), vjust=-0.25, size = 10) +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5, ymax=P95), width=.1, color = "black") +
    ggplot2::labs(x = "",
                  #User will need to change ylab below according to analysis method 
                  y = "Probability-weighted Average Green Score Benefit (%)") +
    #title = "A") +
    ggplot2::labs(alpha = "Category", fill = "Category") +
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.85,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
  
  print(benefits_graph)
  
  
  ## Optional figure to seperate out Extinct/Extinct in the wild species if differences are extreme 
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentNational")])) == 0)])
  species_extinct_global <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentGlobal")])) == 0)])
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  sim_results_summary_benefits$extirpated_or_extinct <- "Extant" # initialize
  sim_results_summary_benefits$extirpated_or_extinct[is.na(match(sim_results_summary_benefits$org_program, extirpated_or_extinct)) == FALSE] <- "EX/EW" # initialize
  
  sim_results_summary_benefits$extirpated_or_extinct <- factor(sim_results_summary_benefits$extirpated_or_extinct, 
                                                               levels = c("EX/EW","Extant"))
  # Make figure
  benefits_graph2 <- ggplot2::ggplot(sim_results_summary_benefits, ggplot2::aes(x = org_program, y = mean, fill = category)) +
    ggplot2::geom_bar(stat="identity",position =ggplot2::position_dodge()) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free") + 
    ggplot2::geom_text(ggplot2::aes(label = lab), vjust=-0.25, size = 10)+
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5, ymax=P95), width=.1, color = "black") +
    ggplot2::labs(x = "",
                  #User will need to change ylab below according to analysis method
                  y = "Probability-weighted Average Green Score Benefit (%)") +
    #title = "A") +
    ggplot2::labs(alpha = "Category", fill = "Category")+
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) 
  
  # Spacing facets
  # following https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap/52422707
  # convert ggplot object to grob object
  gp <- ggplot2::ggplotGrob(benefits_graph2)
  # optional: take a look at the grob object's layout
  #gtable::gtable_show_layout(gp)
  
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
  
  
}#end of the graph_benefit function


#####  Graphing the costs  #####
#' INSERT
#'
#' @param results_cost_organization a dataframe from cost_benefit_analysis that provides the cost of each program to the organization
#' @param results_cost_total a dataframe from cost_benefit_analysis that provides the total cost of each program (including to organization and the amount offset by funders)
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95) 
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
graph_cost <- function(results_cost_organization, results_cost_total, inputs){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_cost_organization$category <- "Cost to our Organization"
  results_cost_total$category <- "Costs Offset by Funders" # "Total Project Cost"
  sim_results_summary_costs <- rbind(results_cost_organization, results_cost_total)
  
  ## Create a data frame to identify species that are conservation breeding relevant
  sim_results_summary_costs <- sim_results_summary_costs %>%
    dplyr::mutate(lab = dplyr::if_else(category == "Costs Offset by Funders" & org_program %in% conservation_breeding, "    B", ""))
  # remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program)
  
  # Reorder based on decreasing cost to our organization
  sim_results_summary_costs$org_program <- factor(sim_results_summary_costs$org_program, 
                                                  levels=unique(results_benefit_global$org_program[order(-sim_results_summary_costs$mean, decreasing = FALSE)]), ordered=TRUE)
  
  # Make the graph
  costs_graph <- ggplot2::ggplot(sim_results_summary_costs, ggplot2::aes(x = org_program, y = mean/1000, alpha = category)) +
    ggplot2::geom_bar(stat="identity",position ="identity") +
    ggplot2::scale_alpha_manual(values=c(1, .3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=P5/1000, ymax=P95/1000), width=.1, color = "black") +
    ggplot2::labs(x = "",
                  #User may need to change ylab below according to analysis method
                  y = "Probability-weighted Average Cost\n over 10 Years (CAD Thousands)") +
    #title = "B") +
    ggplot2::labs(alpha = "Category")+
    ggplot2::scale_y_continuous(breaks = seq(0, 12000, by = 1000)) + 
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::geom_text(ggplot2::aes(label = lab), show.legend = FALSE, vjust=-0.25, size = 4, color = "black") 
  
  costs_graph
  
  
  ## Optional figure to seperate out Extinct/Extinct in the wild species if differences are extreme 
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentNational")])) == 0)])
  species_extinct_global <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentGlobal")])) == 0)])
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  sim_results_summary_costs$extirpated_or_extinct <- "Extant" # initialize
  sim_results_summary_costs$extirpated_or_extinct[is.na(match(sim_results_summary_costs$org_program, extirpated_or_extinct)) == FALSE] <- "EX/EW" # initialize
  
  sim_results_summary_costs$extirpated_or_extinct <- factor(sim_results_summary_costs$extirpated_or_extinct, 
                                                            levels = c("EX/EW","Extant"))
  
  #Make figure 
  costs_graph2 <- ggplot2::ggplot(sim_results_summary_costs, ggplot2::aes(x = org_program, y = mean/1000, alpha = category)) +
    ggplot2::geom_bar(stat="identity",position ="identity") +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free") + 
    ggplot2::scale_alpha_manual(values=c(1, .3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=P5/1000, ymax=P95/1000), width=.1, color = "black") +
    ggplot2::labs(x = "",
                  #User may need to change y lab below to fit appropriate analysis method
                  y = "Probability-weighted Average Cost\n over 10 Years (CAD Thousands)") +
    #title = "B") +
    ggplot2::labs(alpha = "Category")+
    ggplot2::scale_y_continuous(breaks = seq(0, 12000, by = 1000)) + 
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.8,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::geom_text(ggplot2::aes(label = lab), show.legend = FALSE, vjust=-0.25, size = 4, color = "black")
  
  
  # Spacing facets
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
  
  
}#end of the graph_cost function


#####  Bar graph of the benefits binned by conservation gains - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' INSERT
#'
#' @param results_benefit_national a dataframe from cost_benefit_analysis that provides the national benefit each program
#' @param results_benefit_global a dataframe from cost_benefit_analysis that provides the global benefit each program
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95)
#' @param results_overall a dataframe from the cost_benefit_analysis that outlines BCR and ranks for each conservation program
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
bargraph_binnedby_cgains<-function(results_benefit_national, results_benefit_global, inputs, results_overall){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_benefit_global$category<-"Global"
  results_benefit_national$category<-"National"
  #Add the bin for triaging
  results_benefit_national$bin[match(results_benefit_national$org_program, results_overall$org_program)]<-results_overall$bin_cgainNational
  results_benefit_global$bin[match(results_benefit_global$org_program, results_overall$org_program)]<-results_overall$bin_cgainGlobal
  sim_results_summary_bincgain<-rbind(results_benefit_global, results_benefit_national)
  #Add label for endemic species
  sim_results_summary_bincgain <- sim_results_summary_bincgain %>%
    dplyr::mutate(lab = dplyr::if_else(org_program %in% endemic_species, "*", ""))
  # remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program) 
  #Reorder based on bin then by decreasing benefit
  sim_results_summary_bincgain$org_program <- factor(sim_results_summary_bincgain$org_program, 
                                                     levels=unique(results_benefit_global$org_program[
                                                       order(-sim_results_summary_bincgain$mean,decreasing=FALSE
                                                       )]), ordered=TRUE)
  
  #Make the graph
  ## Color code the bars - Red, green, blue
  bin_cgains_bargraph <- ggplot2::ggplot(sim_results_summary_bincgain, ggplot2::aes(x = org_program, y=mean, fill = category, color = bin)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_text(ggplot2::aes(label = lab, color="black"), color="black", show.legend = FALSE, vjust = -0.25, size = 10) +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    ggplot2::scale_color_manual(values=c("High" = "red",
                                         "Medium" = "darkorange",
                                         "Low" = "blue",
                                         "Zero" = "green"))+
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggplot2::labs(x = "",
                  y = "Probability-weighted Average \n Green Score Benefit (%)") +
    #title="C")                         
    #ggplot2::labs(alpha = "category", fill = "category", ggplot2::aes(color="black")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(0.85,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::guides(override.aes=ggplot2::aes(label=""))+
    ggplot2::theme(legend.key.size=unit(0.5, "cm"))
  
  
  bin_cgains_bargraph
  
  
}#end of the bargraph_binned_by_cgains function


##### Scatter plot of the national benefits binned by conservation gains - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' INSERT
#'
#' @param results_benefit_national a dataframe from cost_benefit_analysis that provides the national benefit each program
#' @param results_benefit_global a dataframe from cost_benefit_analysis that provides the global benefit each program
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95)
#' @param results_overall a dataframe from the cost_benefit_analysis that outlines BCR and ranks for each conservation program
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
scatter_bin_cgains_national<-function(results_benefit_national, inputs, results_overall){
  
  #Collate the data
  results_benefit_national$mean_GScurrentNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentNational
  results_benefit_national$mean_cgain_national_org[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_cgain_national_org
  results_benefit_national$bin_cgainNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$bin_cgainNational
  
  #Make the figure for national level, showing names of only those in within the "high" bin
  bin_cgains_scatter_national<-ggplot2::ggplot(results_benefit_national,aes(x=mean_GScurrentNational, y=mean_cgain_national_org,label=org_program))+
    ggplot2::aes(ymin=0, ymax=100, xmin=0, xmax=100)+
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_national$bin_cgainNational=="High"~"red",
                                               results_benefit_national$bin_cgainNational=="Medium"~"darkorange",
                                               results_benefit_national$bin_cgainNational=="Low"~"green",
                                               results_benefit_national$bin_cgainNational=="Zero"~"black"))+
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggrepel::geom_text_repel(data=subset(results_benefit_national,bin_cgainNational=="High")) +
    ggplot2::geom_segment(aes(x=0, y=0, xend=40, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=10, y=10, xend=90, yend=10), color="black",linetype=2) +
    ggplot2::geom_segment(aes(x=40, y=40, xend=60, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=0, y=100, xend=100, yend=0), color="black",linetype=2) +
    ggplot2::geom_segment(aes(x=0, y=0, xend=100, yend=0), color="black",linetype=2)+
    ggplot2::geom_text(aes(x=5, y=90, label="High"), color="red") +
    ggplot2::geom_text(aes(x=50, y=38, label="Medium"), color="darkorange") +
    ggplot2::geom_text(aes(x=50,y=8, label="Low"), color="green") +
    ggplot2::geom_text(aes(x=85,y=2, label="Zero"), color="black") +
    ggplot2::theme(legend.justification = c(0.85,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::xlab(label="Current Green Score (%)")+
    ggplot2::ylab(label="Probability-weighted Average Green Score Benefit (%)")
  
  bin_cgains_scatter_national
}#end of the scatter_bin_cgains_national function


##### Scatter plot of the global benefits binned by conservation gains - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' INSERT
#'
#' @param results_benefit_national a dataframe from cost_benefit_analysis that provides the national benefit each program
#' @param results_benefit_global ad ataframe from cost_benefit_analysis that provides the global benefit each program
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95)
#' @param results_overall a dataframe from the cost_benefit_analysis that outlines BCR and ranks for each conservation program
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
scatter_bin_cgains_global<-function(results_benefit_global, inputs, results_overall){
  
  #Collate the data
  results_benefit_global$mean_GScurrentGlobal[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentGlobal
  results_benefit_global$mean_cgain_global_org[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_cgain_global_org
  results_benefit_global$bin_cgainGlobal[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$bin_cgainGlobal
  
  #Make the graph for global level, labelling only those with high benefit
  bin_cgains_scatter_global<-ggplot2::ggplot(results_benefit_global,aes(x=mean_GScurrentGlobal, y=mean_cgain_global_org, label=org_program))+
    ggplot2::aes(ymin=0, ymax=100, xmin=0, xmax=100)+
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_global$bin_cgainGlobal=="High"~"red",
                                               results_benefit_global$bin_cgainGlobal=="Medium"~"darkorange",
                                               results_benefit_global$bin_cgainGlobal=="Low"~"green",
                                               results_benefit_global$bin_cgainGlobal=="Zero"~"black"))+
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    #ggrepel::geom_text_repel(data=subset(results_benefit_global,bin_cgainGlobal=="High"),position="") +
    ggplot2::geom_segment(aes(x=0, y=0, xend=40, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=10, y=10, xend=90, yend=10), color="black",linetype=2) +
    ggplot2::geom_segment(aes(x=40, y=40, xend=60, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=0, y=100, xend=100, yend=0), color="black",linetype=2) +
    ggplot2::geom_segment(aes(x=0, y=0, xend=100, yend=0), color="black",linetype=2)+
    ggplot2::geom_text(aes(x=5, y=90, label="High"), color="red") +
    ggplot2::geom_text(aes(x=50, y=38, label="Medium"), color="darkorange") +
    ggplot2::geom_text(aes(x=50,y=8, label="Low"), color="green") +
    ggplot2::geom_text(aes(x=85,y=2, label="Zero"), color="black") +
    ggplot2::theme(legend.justification = c(0.85,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::xlab(label="Current Green Score (%)")+
    ggplot2::ylab(label="Probability-weighted Average Green Score Benefit (%)")
  
  
  bin_cgains_scatter_global
  
}#end of the scatter_bin_cgains_global function


##### Bar graph of the benefits binned by current Green Score - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD ##### 
#' INSERT
#'
#' @param results_benefit_national a dataframe from cost_benefit_analysis that provides the national benefit each program
#' @param results_benefit_global  dataframe from cost_benefit_analysis that provides the global benefit each program
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95)
#' @param results_overall a dataframe from the cost_benefit_analysis that outlines BCR and ranks for each conservation program
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
bargraph_bin_currentgs<-function(results_benefit_global, results_benefit_national, inputs, results_overall){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_benefit_global$category<-"Global"
  results_benefit_national$category<-"National"
  #Add the bin for triaging
  results_benefit_national$bin[match(results_benefit_national$org_program, results_overall$org_program)]<-results_overall$bin_GScurrentNational
  results_benefit_global$bin[match(results_benefit_global$org_program, results_overall$org_program)]<-results_overall$bin_GScurrentGlobal
  sim_results_summary_bincurrentgs<-rbind(results_benefit_global, results_benefit_national)
  #Add label for endemic species
  sim_results_summary_bincurrentgs <- sim_results_summary_bincurrentgs %>%
    dplyr::mutate(lab = dplyr::if_else(org_program %in% endemic_species, "*", ""))
  #Remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program) 
  #Reorder based on bin then by decreasing benefit
  sim_results_summary_bincurrentgs$org_program <- factor(sim_results_summary_bincurrentgs$org_program, 
                                                         levels=unique(results_benefit_global$org_program[
                                                           order(-sim_results_summary_bincurrentgs$mean,decreasing=FALSE
                                                           )]),ordered=TRUE)
  
  
  #### Make the bar graph
  bin_currentgs_graph <- ggplot2::ggplot(sim_results_summary_bincurrentgs, ggplot2::aes(x = org_program, y=mean, fill = category, color = bin)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_text(ggplot2::aes(label = lab), color="black", show.legend=FALSE, vjust = -0.25, size = 10) +
    ggplot2::scale_fill_manual(values=c("Global"="grey30",
                                        "National"="grey70")) +
    ggplot2::scale_color_manual(values=c("Extinct in the Wild" = "red",
                                         "Critically Depleted" = "orange",
                                         "Largely Depleted" = "yellow",
                                         "Moderately Depleted" = "pink",
                                         "Slightly Depleted" = "green",
                                         "Fully Recovered"= "black")) +
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggplot2::labs(x = "",
                  y = "Probability-weighted Average Green Score Benefit (%)") +
    #title="C")                         
    ggplot2::labs(alpha = "category", fill = "category") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(0.7,1),legend.position = c(0.85,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::guides(override.aes=aes(label=""))+
    ggplot2::theme(legend.key.size=unit(0.5, "cm"))
  
  
  
  bin_currentgs_graph
  
  
}#end of the bargraph_bin_gurrentgs function


##### Scatter plot of the national benefits binned by current Green Score - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' INSERT
#'
#' @param results_benefit_national a dataframe from cost_benefit_analysis that provides the national benefit each program
#' @param results_overall a dataframe from the cost_benefit_analysis that outlines BCR and ranks for each conservation program
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
scatter_bin_currentgs_national<-function(results_overall,results_benefit_national){
  
  #Collate the data
  results_benefit_national$mean_GScurrentNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentNational
  results_benefit_national$mean_cgain_national_org[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_cgain_national_org
  results_benefit_national$bin_GScurrentNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$bin_GScurrentNational
  
  #Make the graph for national level, labeling only those extinct or critically depleted
  bin_currentgs_scatter_national<-ggplot2::ggplot(results_benefit_national,aes(x=mean_GScurrentNational, y=mean_cgain_national_org, label=org_program))+
    ggplot2::scale_y_continuous(limits=c(0,100))+
    ggplot2::scale_x_continuous(limits=c(0,100))+                              
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_national$bin_GScurrentNational=="Extinct in the Wild"~"red",
                                               results_benefit_national$bin_GScurrentNational=="Critically Depleted"~"darkorange",
                                               results_benefit_national$bin_GScurrentNational=="Largely Depleted"~"blue",
                                               results_benefit_national$bin_GScurrentNational=="Moderately Depleted"~"pink",
                                               results_benefit_national$bin_GScurrentNational=="Slightly Depleted"~"green",
                                               results_benefit_national$bin_GScurrentNational=="Fully Recovered"~"black"))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggrepel::geom_text_repel(data=subset(results_benefit_national, mean_GScurrentNational<= 15 ),
                             box.padding = 0.2, point.padding = 1, min.segment.length = 0, force=2)+
    ggplot2::geom_segment(aes(x=0, y=0, xend=0, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=20, y=0, xend=20, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=50, y=0, xend=50, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=80, y=0, xend=80, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=100, y=0, xend=100, yend=100), color="black",linetype=2)+
    ggplot2::geom_text(aes(x=0, y=90, label="Extinct in\nthe Wild"), color="red") +
    ggplot2::geom_text(aes(x=10,y=90, label="Critically \nDepleted"), color="darkorange") +
    ggplot2::geom_text(aes(x=35,y=90, label="Largely \nDepleted"), color="blue") +
    ggplot2::geom_text(aes(x=65,y=90, label="Moderately \nDepleted"), color="pink") +
    ggplot2::geom_text(aes(x=90,y=90, label="Slightly \nDepleted"), color="green") +
    ggplot2::geom_text(aes(x=100,y=90, label="Fully \nRecovered"), color="black") +
    ggplot2::theme(legend.justification = c(0.85,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::xlab(label="Current Green Score (%)")+
    ggplot2::ylab(label="Probability-weighted Average National\nGreen Score Benefit (%)")
  
  bin_currentgs_scatter_national
  
}#end of the scatter_bin_currentgs_national function


##### Scatter plot of the global benefits binned by current Green Score - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' INSERT
#'
#' @param results_benefit_global a dataframe from cost_benefit_analysis that provides the global benefit each program
#' @param results_overall a dataframe from the cost_benefit_analysis that outlines BCR and ranks for each conservation program
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
scatter_bin_currentgs_global<-function(results_overall,results_benefit_global){
  
  #Collect data from results overall
  results_benefit_global$mean_GScurrentGlobal[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentGlobal
  results_benefit_global$mean_cgain_global_org[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_cgain_global_org
  results_benefit_global$bin_GScurrentGlobal[match(results_benefit_global$org_program, results_overall$org_program)]<-results_overall$bin_GScurrentGlobal
  
  #Make the figure for global level, labeling only those binned as Extinct or Critically Depleted
  bin_currentgs_scatter_global<-ggplot2::ggplot(results_benefit_global,aes(x=mean_GScurrentGlobal, y=mean_cgain_global_org, label=org_program))+
    ggplot2::scale_y_continuous(limits=c(0,100))+
    ggplot2::scale_x_continuous(limits=c(0,100))+                              
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_global$bin_GScurrentGlobal=="Extinct in the Wild"~"red",
                                               results_benefit_global$bin_GScurrentGlobal=="Critically Depleted"~"darkorange",
                                               results_benefit_global$bin_GScurrentGlobal=="Largely Depleted"~"blue",
                                               results_benefit_global$bin_GScurrentGlobal=="Moderately Depleted"~"pink",
                                               results_benefit_global$bin_GScurrentGlobal=="Slightly Depleted"~"green",
                                               results_benefit_global$bin_GScurrentGlobal=="Fully Recovered"~"black"))+
    #ggrepel::geom_text_repel(aes(label=org_program),point.padding=0.5, box.padding=0.25)+
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggrepel::geom_text_repel(data=subset(results_benefit_global, bin_GScurrentGlobal=="Extinct in the Wild" | bin_GScurrentGlobal=="Critically Depleted"))+
    #aes(color=dplyr::case_when(results_benefit_global$bin_GScurrentGlobal=="Extinct in the Wild"~"red",
    #                        results_benefit_global$bin_GScurrentGlobal=="Critically Depleted"~"darkorange",
    #                        results_benefit_global$bin_GScurrentGlobal=="Largely Depleted"~"blue",
    #                        results_benefit_global$bin_GScurrentGlobal=="Moderately Depleted"~"pink",
    #                        results_benefit_global$bin_GScurrentGlobal=="Slightly Depleted"~"green",
    #                        results_benefit_global$bin_GScurrentGlobal=="Fully Recovered"~"black"))) +
    ggplot2::geom_segment(aes(x=0, y=0, xend=0, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=20, y=0, xend=20, yend=100), color="black",linetype=2) +
    ggplot2::geom_segment(aes(x=50, y=0, xend=50, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(aes(x=80, y=0, xend=80, yend=100), color="black",linetype=2) +
    ggplot2::geom_segment(aes(x=100, y=0, xend=100, yend=100), color="black",linetype=2)+
    ggplot2::geom_text(aes(x=0, y=90, label="Extinct in \nthe Wild"), color="red") +
    ggplot2::geom_text(aes(x=10,y=90, label="Critically \nDepleted"), color="darkorange") +
    ggplot2::geom_text(aes(x=35,y=90, label="Largely \nDepleted"), color="blue") +
    ggplot2::geom_text(aes(x=65,y=90, label="Moderately \nDepleted"), color="pink") +
    ggplot2::geom_text(aes(x=90,y=90, label="Slightly \nDepleted"), color="green") +
    ggplot2::geom_text(aes(x=100,y=90, label="Fully \nRecovered"), color="black") +
    ggplot2::theme(legend.justification = c(0.85,1),legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())+
    ggplot2::xlab(label="Current Green Score (%)")+
    ggplot2::ylab(label="Probability-weighted Average Global\n Green Score Benefit (%)")
  
  bin_currentgs_scatter_global
  
  
}#end of the scatter_bin_currentgs_global function


##### Graphing the cost-benefit ratios ##### 
#' INSERT
#'
#' @param inputs a CSV containing the conservation programs and the associated benefits and costs (P5, P50, P95)
#' @param results_overall a dataframe from the cost_benefit_analysis that outlines BCR and ranks for each conservation program
#' @examples
#' # STILL TO ADD
#'
#' @export
# Still needs unit testing
graph_BCR <- function(results_overall, inputs){
  ## Try making the graph with two facets, one for the small values and one for the large value from extirpated species
  ## Gather dataframe to create figure with both global and national BCR
  results_BCR_global$category <- "Global"
  results_BCR_national$category <- "National"
  results_BCR_all <- rbind(results_BCR_global, results_BCR_national)
  results_BCR_all <- results_BCR_all %>%
    dplyr::mutate(lab = dplyr::if_else(org_program %in% endemic_species, "*", ""))
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentNational")])) == 0)])
  species_extinct_global <- as.character(inputs$species[which(as.numeric(as.character(inputs$baseP50[which(inputs$subcategory == "GScurrentGlobal")])) == 0)])
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  results_BCR_all$extirpated_or_extinct <- "Extant" # initialize
  results_BCR_all$extirpated_or_extinct[is.na(match(results_BCR_all$org_program, extirpated_or_extinct)) == FALSE] <- "EX/EW" # initialize
  #Factor based on extirpated or extinct
  results_BCR_all$extirpated_or_extinct <- factor(results_BCR_all$extirpated_or_extinct, 
                                              levels = c("EX/EW", "Extant"))
  
  #Reorder based on national level
  results_BCR_all$org_program <- factor(results_BCR_all$org_program, 
                                                     levels=unique(results_BCR_national$org_program[order(-results_BCR_national$mean, decreasing = FALSE)]), ordered=TRUE)
  
  ##Make the figure
  BCR_per_cad3 <- ggplot2::ggplot(results_BCR_all, ggplot2::aes(x = org_program, y = mean*100000, alpha = category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free") + 
    ggplot2::scale_alpha_manual(values=c(1,.3)) +
    #ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5*100000, ymax=P95*100000), width=.1, color = "black") +
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
  gtable::gtable_show_layout(gp)
  
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
  
  
  
  ####Plotting the credible intervals
  results_BCR_global$category <- "Global"
  results_BCR_national$category <- "National"
  cred_intervals <- rbind(results_BCR_global, results_BCR_national)
  cred_intervals$ci90<-as.numeric(cred_intervals$P95)-as.numeric(cred_intervals$P5)
  cred_intervals <- cred_intervals %>%
    dplyr::mutate(lab = dplyr::if_else(org_program %in% endemic_species, "*", ""))
  ##Testing
  
  results_BCR_global$ci90<-results_BCR_global$P95-results_BCR_global$P5
  results_BCR_national$ci90<-results_BCR_national$P95-results_BCR_national$P5
  cred_intervals <- rbind(results_BCR_global, results_BCR_national)
  
  results_BCR_all$ci90<-results_BCR_all$P95-results_BCR_all$P5

  #Prepping the basis of the Facet panelling
  cred_intervals$extirpated_or_extinct <- "Extant" # initialize
  cred_intervals$extirpated_or_extinct[is.na(match(cred_intervals$org_program, extirpated_or_extinct)) == FALSE] <- "EX/EW" # initialize
  cred_intervals$extirpated_or_extinct <- factor(cred_intervals$extirpated_or_extinct, 
                                                  levels = c("EX/EW", "Extant"))
 
  
  #Reorder based on national level
  results_BCR_all$org_program <- factor(results_BCR_all$org_program, 
                                        levels=unique(results_BCR_all$org_program[order(-results_BCR_all$ci90, decreasing = FALSE)]), ordered=TRUE)
  #### Make figure 
  BCR_credible_intervals<-  ggplot2::ggplot(results_BCR_all, ggplot2::aes(x = org_program, y = ci90, alpha = category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free") + 
    ggplot2::scale_alpha_manual(values=c(1,.3)) +
    #ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5*100000, ymax=P95*100000), width=.1, color = "black") +
    ggplot2::labs(x = "Species Program",
                  # y = "Average BCR \n (% Change in 50yr Species Persistence per 100 000 CAD)",
                  # y = "Average BCR \n (Green Status Gain plus Dependence (%) per 100 000 CAD)",
                  y = "Uncertainty around BCR Mean\n(90% Credible Interval)")+
                  #title = "C") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.85,0.95),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
  
  # Now get the facets spaced more appropriately
  # following https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap/52422707
  # convert ggplot object to grob object
  gp2 <- ggplot2::ggplotGrob(BCR_credible_intervals)
  # optional: take a look at the grob object's layout
  gtable::gtable_show_layout(gp2)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns2 <- gp2$layout$l[grepl("panel", gp2$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  x.var2 <- sapply(ggplot2::ggplot_build(BCR_credible_intervals)$layout$panel_scales_x,
                  function(l) length(l$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp2$widths[facet.columns] <- gp2$widths[facet.columns2] * x.var2
  
  # plot result
  grid::grid.draw(gp2)
  BCR_credi_final <- gp2
  
  
  return(BCR_credi_final)
  
}#end of the graph_BCR function







#' STILL TO CLEAN UP CODE
#graph_BCR <- function(results_overall, inputs){
  
  ## Make a panel figure with non-standardized BCR (change in persistence per dollar)
  
  # # Change the Boreal Caribou label to Woodland caribou
  # results_overall$org_program <- as.character(results_overall$org_program) # remove the factor levels
  # results_overall$org_program[which(results_overall$org_program == "Boreal Caribou")] <- "Woodland Caribou"
  
  # Order the species by rank
  # results_overall$org_program <- factor(results_overall$org_program, 
  #                                                   levels=unique(results_overall$org_program[order(results_overall$BCR_national_EV_rank)]), ordered=TRUE)
  
  # Add triage cateogry
  # org_program_triage$triage_category
  # results_overall$triage_category <- NA # initalize
  # results_overall$triage_category <- org_program_triage$triage_category[match(results_overall$org_program, org_program_triage$org_program)]
  
  # Reorder the factor levels for the org program so that they are ordered by triage category first and then BCR national rank
  #results_benefit_global$org_program <- as.character(results_benefit_global$org_program) # remove the factor levels
  # results_overall$org_program <- factor(results_overall$org_program, 
  #                                                  levels=unique(results_benefit_global$org_program[order(results_overall$triage_category,
  #                                                                                                          -results_overall$weighted_BCRs, decreasing = FALSE)]), ordered=TRUE)
  #results_overall$org_program <- factor(results_overall$org_program, 
  #                                       levels=unique(results_benefit_global$org_program[order(-results_overall$BCR_national_EV, decreasing = FALSE)]), ordered=TRUE)
  
  
  

  
  # Create figure
  # BCR_per_cad <- ggplot(BCR_summary, aes(x = org_program, y = value*100000, alpha = Category,  color = triage_category)) +
  #BCR_per_cad <- ggplot2::ggplot(BCR_summary, ggplot2::aes(x = org_program, y = value*100000, alpha = Category)) +
  #  ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
  #  ggplot2::scale_alpha_manual(values=c(1,.3)) +
  #  ggplot2::labs(x = "Species Program",
  #                # y = "Average BCR \n (% Change in 50yr Species Persistence per 100 000 CAD)",
                  # y = "Average BCR \n (Green Status Gain plus Dependence (%) per 100 000 CAD)",
  #                y = "Average BCR \n (Green Score Benefit Proxy per 100 000 CAD)",
  #                title = "C") +
  #  ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  #  ggplot2::theme_bw() +
  #  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1), legend.justification = c(1,1),legend.position = c(0.95,0.95),
  #                 panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
  #BCR_per_cad
  
  # Make the same graph as above but add the credible intervals
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  #results_BCR_global$category <- "Global"
  #results_BCR_national$category <- "National"
  #results_BCR_all <- rbind(results_BCR_global, results_BCR_national)
  #results_BCR_all <- results_BCR_all %>%
  #  dplyr::mutate(lab = dplyr::if_else(org_program %in% endemic_species, "*", ""))
                  
  # Add triage cateogry
  # results_BCR_all$triage_category <- NA # initalize
  # results_BCR_all$triage_category <- org_program_triage$triage_category[match(results_BCR_all$org_program, org_program_triage$org_program)]
  # 
  # Reorder the factor levels for the org program so that they are ordered by triage category first and then BCR national rank
  # results_BCR_all$org_program <- factor(results_BCR_all$org_program, 
  #                                        levels=unique(results_BCR_all$org_program[order(results_overall$triage_category,
  #                                                                                         -results_overall$weighted_BCRs, decreasing = FALSE)]), ordered=TRUE)
  # 
  # 
  # # Order the species by rank
  # results_BCR_all$org_program <- factor(results_BCR_all$org_program, 
  #                                        levels=unique(results_overall$org_program[order(results_overall$BCR_national_EV_rank)]), ordered=TRUE)
  #
  #
  #BCR_per_cad2 <- ggplot2::ggplot(results_BCR_all, ggplot2::aes(x = org_program, y = mean, fill = category)) +  
  #  ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
  #            geom_text(aes(label = lab), vjust=-0.25, size = 10)+
  #             scale_colour_manual(values=c("#000000")) +
  #  ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
  #  #scale_alpha_manual(values=c(1,0.3)) +
  #  ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(ymin=P5, ymax=P95), width=.1, color = "black") +
  #  # labs(x = "Species Program",
  #  ggplot2::labs(x = "",
                  # y = "Average BCR \n (% Change in 50yr Species Persistence per CAD)",
  #                y = "Average BCR \n (Green Status Gain plus Dependence (%) per 100 000 CAD)",
  #                title = "A") +
  #  ggplot2::labs(alpha = "Category", fill = "Category")+
  #  ggplot2::theme_bw() + 
    #theme(axis.text.x=element_text(angle=45, hjust=1), legend.justification = c(0,1),legend.position = c(0.05,0.95),
  #  ggplot2::theme(axis.text.x= ggplot2::element_text(angle=45, hjust=1), legend.position = "bottom",
  #                 panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) 
  #BCR_per_cad2 
