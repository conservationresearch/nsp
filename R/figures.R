# This script contains the functions for figure production
# Dylan Cole
# Started August 2021
# Primarily using code written by Laura Keating (July 2021) with some changes and cleanup

##### Graphing the benefits #####
#' Figure of national and global benefits
#' 
#' This function creates a bar graph of the national and global benefits with programs that have a current Green Score of zero on a separate facet. 
#'
#' @param results_benefit_national A dataframe produced from the cost benefit analysis that outlines the national benefit each program
#' @param results_benefit_global A dataframe produced from the cost benefit analysis that outlines the global benefit each program
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param endemic_species A vector specifying which species are endemic (e.g., c("Species 2","Species 4")
#' @return Returns the figure produced
#' @examples
#' #graph_benefit(results_benefit_national, results_benefit_global, inputs)
#' @export

graph_benefit <- function(results_benefit_national, results_benefit_global, inputs, endemic_species){
  
  #Make Global vs National distinction and collect data for figure
  results_benefit_global$Category <- "Global"
  results_benefit_national$Category <- "National"
  sim_results_summary_benefits <- rbind(results_benefit_national,results_benefit_global)
  
  ## Create a data frame to identify species that are endemic
  sim_results_summary_benefits$lab <- ifelse(sim_results_summary_benefits$org_program %in% endemic_species, "*","")
  
  # remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program) 
  # order by decreasing national benefit
  sim_results_summary_benefits$org_program <- factor(sim_results_summary_benefits$org_program, 
                                                     levels=unique(results_benefit_national$org_program[order(-sim_results_summary_benefits$mean, decreasing = TRUE)]), ordered=TRUE)
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- inputs$species[which(inputs$subcategory=="GScurrentNational" & inputs$baseP50==0)]
  species_extinct_global <- inputs$species[which(inputs$subcategory=="GScurrentGlobal" & inputs$baseP50==0)]
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  #populate all with extant, then replace entries with "extirpated" for those that match in previous vector
  sim_results_summary_benefits$extirpated_or_extinct <- "Extant" # initialize
  sim_results_summary_benefits$extirpated_or_extinct[is.na(match(sim_results_summary_benefits$org_program, extirpated_or_extinct)) == FALSE] <- "Extirpated" # initialize
  #Factor on extirpated vs extant
  sim_results_summary_benefits$extirpated_or_extinct <- factor(sim_results_summary_benefits$extirpated_or_extinct, 
                                                               levels = c("Extirpated","Extant"))
  sim_results_summary_benefits<<-sim_results_summary_benefits
  if(analysisoption==1|analysisoption==3){ #Need to change xlab depending on analysis option
    xlabel = "Mean species program value (%)"}
  if(analysisoption==2|analysisoption==4){
    xlabel = "Mean conservation value (%)"}
  if(analysisoption==5){
    xlabel = "Mean species program value relative to SRS"}
  if(analysisoption==6){
    xlabel = "Mean program benefit "}
  # Make figure
  benefits_graph2 <- ggplot2::ggplot(sim_results_summary_benefits, ggplot2::aes(y = org_program, x = P50, fill = Category)) +
    ggplot2::geom_bar(stat="identity",position =ggplot2::position_dodge()) +
    ggplot2::facet_wrap(~sim_results_summary_benefits$extirpated_or_extinct,nrow=2, ncol=1, scales="free") + 
    ggplot2::geom_text(ggplot2::aes(label = lab), hjust = 0.1, vjust = 0.75 , size = 10) +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes(xmin=P5, xmax=P95), width=.1, color = "black") +
    ggplot2::labs(x = xlabel,
                  #User will need to change ylab below according to analysis method
                  y = "Species program",
                  title = "a") +
    ggplot2::labs(alpha = "Category", fill = "Category")+
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.title = ggplot2::element_text(size=16),
                   axis.text = ggplot2::element_text(size=16),
                   strip.text.x = ggplot2::element_text(size=16),
                   legend.text = ggplot2::element_text(size=16),
                   legend.title = ggplot2::element_text(size=16),
                   legend.justification = c(1,1), 
                   legend.position = c(0.95,0.35),
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank()) 
  
  # Spacing facets
  # convert ggplot object to grob object
  gp <- ggplot2::ggplotGrob(benefits_graph2)
  # optional: take a look at the grob object's layout
  gtable::gtable_show_layout(gp)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$t[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  y.var <- sapply(ggplot2::ggplot_build(benefits_graph2)$layout$panel_scales_y,
                  function(t) length(t$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$heights[facet.columns] <- gp$heights[facet.columns] * y.var
  
  # plot result
  grid::grid.draw(gp)
  benefits_graph2_final <- gp
  
  
  return(benefits_graph2_final)
  
  
}#end of the graph_benefit function


##### Graphing the costs  #####
#' Figure of costs to organization and financial leverage
#' 
#' This function creates a figure depicting the costs to the organization and the financial leverage for each conservation program. Programs
#' with a current Green Score of zero are presented on a separate facet. 
#' 
#' @param results_cost_organization A dataframe produced from the cost benefit analysis that outlines the cost of each program to the organization
#' @param results_cost_total A dataframe produced from the cost benefit analysis that outlines the total cost of each program (including to organization and the amount offset by funders)
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @return Returns the figure produced
#' @examples
#' #graph_cost(results_cost_organization, results_cost_total, inputs)
#' @export

graph_cost <- function(results_cost_organization, results_cost_total, inputs){
  
  #Make distinction between costs and funding and collect data
  results_cost_organization$Category <- "Cost to our Organization"
  results_cost_total$Category <- "Cost-sharing Potential" # "Total Project Cost"
  sim_results_summary_costs <- rbind(results_cost_organization, results_cost_total)
  
  ## Create a data frame to identify species that are conservation breeding relevant
  sim_results_summary_costs$lab <- ifelse(sim_results_summary_costs$Category == "Cost-sharing Potential" &
                                            sim_results_summary_costs$org_program %in% conservation_breeding,
                                          "    B", "")
  # remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program)
  
  # Reorder based on decreasing cost to our organization
  #sim_results_summary_costs$org_program <- factor(sim_results_summary_costs$org_program, 
  #                                                levels=unique(results_benefit_global$org_program[order(-sim_results_summary_costs$mean, decreasing = TRUE)]), ordered=TRUE)
  
  #Reodering based on decreasing benefits
  sim_results_summary_costs$org_program<-factor(sim_results_summary_benefits$org_program, 
                                                levels=unique(results_benefit_national$org_program[order(-sim_results_summary_benefits$mean, decreasing = TRUE)]), ordered=TRUE)
  
  #Reordering based on decreasing binned benefits
  #sim_results_summary_costs$org_program<-factor(sim_results_summary_bincgain$org_program, 
  #                                              levels=unique(results_benefit_national$org_program[
  #                                                order(-sim_results_summary_bincgain$bin_numeric, -sim_results_summary_bincgain$mean, decreasing=TRUE
  #                                                )]), ordered=TRUE)
  
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- inputs$species[which(inputs$subcategory=="GScurrentNational" & inputs$baseP50==0)]
  species_extinct_global <- inputs$species[which(inputs$subcategory=="GScurrentGlobal" & inputs$baseP50==0)]
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  sim_results_summary_costs$extirpated_or_extinct <- "Extant" # initialize
  sim_results_summary_costs$extirpated_or_extinct[is.na(match(sim_results_summary_costs$org_program, extirpated_or_extinct)) == FALSE] <- "Extirpated" # initialize
  
  sim_results_summary_costs$extirpated_or_extinct <- factor(sim_results_summary_costs$extirpated_or_extinct, 
                                                            levels = c("Extirpated","Extant"))
  
  #Make figure 
  costs_graph2 <- ggplot2::ggplot(sim_results_summary_costs, ggplot2::aes(x = mean/1000, y = org_program, alpha = Category)) +
    ggplot2::geom_bar(stat="identity",position ="identity") +
    ggplot2::facet_wrap(~ extirpated_or_extinct, ncol=1, nrow=2, scales="free") + 
    ggplot2::scale_alpha_manual(values=c(0.3, 1)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin=P5/1000, xmax=P95/1000), width=.1, color = "black") +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    ggplot2::labs(x = "Mean cost over 10 years (CAD thousands)",
                  y = "", 
                  title = "b") +
    ggplot2::labs(alpha = "Category")+
    ggplot2::geom_text(ggplot2::aes(label = lab), show.legend = FALSE, vjust=-0.25, size = 4.5, color="black", alpha=1) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16),
                   strip.text = ggplot2::element_text(size=16),
                   axis.title = ggplot2:: element_text(size = 16),
                   axis.text.y=ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size=16),
                   legend.text = ggplot2::element_text(size = 16),
                   legend.justification = c(1,1),
                   legend.position = c(0.95,0.35),
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank())
  
  # Spacing facets
  # convert ggplot object to grob object
  gp <- ggplot2::ggplotGrob(costs_graph2)
  # gtable::gtable_show_layout(gp)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$t[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  y.var <- sapply(ggplot2::ggplot_build(costs_graph2)$layout$panel_scales_y,
                  function(t) length(t$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$heights[facet.columns] <- gp$heights[facet.columns] * y.var
  
  # plot result
  grid::grid.draw(gp)
  costs_graph2_final <- gp
  
  return(costs_graph2_final)
  
}#end of the graph_cost function


##### Bar graph of the BCR binned by conservation benefits - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' Bar graph of the BCR binned based on benefits
#'
#' This function can be used when the cost benefit analysis function cba_GplusD_BinnedByBenefit is used. This function creates a figure depicting the
#' national and global benefits, giving priority to those that have the highest benefit. 
#'
#' @param results_BCR_national A dataframe produced from the cost benefit analysis that outlines the national benefit each program
#' @param results_BCR_global A dataframe produced from the cost benefit analysis that outlines the global benefit each program
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param results_overall A dataframe produced from the cost benefit analysis that outlines the BCR and ranks for each conservation program
#' @param endemic_species A vector specifying which species are endemic (e.g. c("Species 1","Species 2"))
#' @return Returns the figure produced
#' @export

bargraph_BCR_binnedby_benefits<-function(results_BCR_national, results_BCR_global,
                                         inputs, results_overall, endemic_species){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_BCR_global$Category<-"Global"
  results_BCR_national$Category<-"National"
  #Add the bin for triaging
  results_BCR_national$Bin[match(results_BCR_national$org_program, results_overall$org_program)]<-results_overall$bin_cgainNational
  results_BCR_global$Bin[match(results_BCR_global$org_program, results_overall$org_program)]<-results_overall$bin_cgainGlobal
  sim_results_summary_bincgain<-rbind(results_BCR_national, results_BCR_global)
  #Add label for endemic species
  sim_results_summary_bincgain$lab <- ifelse(sim_results_summary_bincgain$org_program %in% endemic_species, "*","")
  #Remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program) 
  #Add numeric bin for reorder
  sim_results_summary_bincgain$bin_numeric<-NA
  sim_results_summary_bincgain$bin_numeric[which(sim_results_summary_bincgain$Bin=="High")]<-4
  sim_results_summary_bincgain$bin_numeric[which(sim_results_summary_bincgain$Bin=="Medium")]<-3
  sim_results_summary_bincgain$bin_numeric[which(sim_results_summary_bincgain$Bin=="Low")]<-2
  sim_results_summary_bincgain$bin_numeric[which(sim_results_summary_bincgain$Bin=="Zero")]<-1
  
  #Reorder based on bin then by decreasing benefits
  sim_results_summary_bincgain$org_program <- factor(sim_results_summary_bincgain$org_program, 
                                                     levels=unique(sim_results_summary_bincgain$org_program[
                                                       order(-sim_results_summary_bincgain$bin_numeric, -sim_results_summary_bincgain$mean, decreasing=FALSE
                                                       )]), ordered=TRUE)
  sim_results_summary_bincgain$org_program <- factor(sim_results_summary_bincgain$org_program, 
                                                     levels=unique(results_benefit_national$org_program[
                                                       order(-sim_results_summary_bincgain$bin_numeric, -sim_results_summary_bincgain$mean, decreasing=TRUE
                                                       )]), ordered=TRUE)

  #Make the graph
  bin_cgains_bargraph <- ggplot2::ggplot(sim_results_summary_bincgain, ggplot2::aes(y = org_program, x=mean*100000, fill = Category, color = Bin)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_text(ggplot2::aes(label = lab, color="black"), color="black", show.legend = FALSE,vjust=0.70, hjust=0.1,size = 10) +
    ggplot2::scale_fill_manual(values=c("gray30", "gray70")) +
    ggplot2::scale_color_manual(values=c("High" = "red",
                                         "Medium" = "darkorange",
                                         "Low" = "blue",
                                         "Zero" = "green"))+
    ggplot2::labs(x = "Mean BCR (species program value per 100 000 CAD)",
                  y = "Species program",
                  title="a")+
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(size=20),
                  axis.text = ggplot2::element_text(size=16),
                   axis.title = ggplot2::element_text(size=16),
                   legend.title = ggplot2::element_text(size=16),
                   legend.text = ggplot2::element_text(size=16),
                   legend.justification = c(0.85,1),
                   legend.position = c(0.95,0.50),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())+
    ggplot2::theme(legend.key.size=unit(0.5, "cm"))
  
  print(bin_cgains_bargraph)
  return(bin_cgains_bargraph)
  
  
}#end of the bargraph_binned_by_cgains function


##### Scatter plot of the national benefits binned by conservation gains - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' Scatter plot of national benefits binned by benefits
#'
#' This function can be used when the cost benefit analysis function cba_GplusD_BinnedByBenefit is used. This function creates a scatter plot of the
#' national benefits of each  program, binned into groups by the benefits they provide (high, medium, low, zero)
#'
#' @param results_benefit_national A dataframe produced from the cost benefit analysis that outlines the national benefit each program
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param results_overall A dataframe produced from the cost benefit analysis that outlines the BCR and ranks for each conservation program
#' @return Returns the figure produced
#' @examples
#' #scatter_bin_benefits_national(results_benefit_national, inputs, results_overall)
#' @export

scatter_bin_benefits_national<-function(results_benefit_national, inputs, results_overall){
  
  #Collate the data
  results_benefit_national$mean_GScurrentNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentNational
  results_benefit_national$mean_cgain_national_org[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_cgain_national_org
  results_benefit_national$bin_cgainNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$bin_cgainNational
  
  #Make the figure for national level, labeling only those with high benefit
  bin_cgains_scatter_national<-ggplot2::ggplot(results_benefit_national,ggplot2::aes(x=mean_GScurrentNational, y=mean_cgain_national_org,label=org_program))+
    ggplot2::aes(ymin=0, ymax=100, xmin=0, xmax=100)+
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_national$bin_cgainNational=="High"~"red",
                                               results_benefit_national$bin_cgainNational=="Medium"~"darkorange",
                                               results_benefit_national$bin_cgainNational=="Low"~"green",
                                               results_benefit_national$bin_cgainNational=="Zero"~"black"))+
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggrepel::geom_text_repel(data=subset(results_benefit_national,bin_cgainNational=="High"), size=5) +
    ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=40, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=10, y=10, xend=90, yend=10), color="black",linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=40, y=40, xend=60, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=0, y=100, xend=100, yend=0), color="black",linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=100, yend=0), color="black",linetype=2)+
    ggplot2::geom_text(ggplot2::aes(x=5, y=90, label="High"), color="red", size=6) +
    ggplot2::geom_text(ggplot2::aes(x=50, y=38, label="Medium"), color="darkorange", size=6) +
    ggplot2::geom_text(ggplot2::aes(x=50,y=8, label="Low"), color="green", size=6) +
    ggplot2::geom_text(ggplot2::aes(x=85,y=2, label="Zero"), color="black",size=6) +
    ggplot2::theme(plot.title = ggplot2::element_text(size=20),
                  axis.title=ggplot2::element_text(size=16),
                   axis.text=ggplot2::element_text(size=16),
                   legend.title=ggplot2::element_text(size=16),
                   legend.text=ggplot2::element_text(size=16),
                   legend.justification = c(0.85,1),
                   legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())+
    ggplot2::labs(x="Current green score (%)",
                  y="Mean species program value (%)",
                  title="b")
    
  print(bin_cgains_scatter_national)
  return(bin_cgains_scatter_national)
  
}#end of the scatter_bin_cgains_national function


##### Scatter plot of the global benefits binned by conservation gains - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' Scatter plot of global benefits binned by benefits
#'
#' This function can be used when the cost benefit analysis function cba_GplusD_BinnedByBenefit is used. This function creates a scatter plot of the
#' global benefits of each  program, binned into groups by the benefits they provide (high, medium, low, zero)
#'
#' @param results_benefit_global A dataframe produced from the cost benefit analysis that outlines the global benefit each program
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param results_overall A dataframe produced from the cost benefit analysis that outlines the BCR and ranks for each conservation program
#' @return Returns the figure produced
#' @examples
#' #scatter_bin_benefits_national(results_benefit_national, inputs, results_overall)
#' @export

scatter_bin_benefits_global<-function(results_benefit_global, inputs, results_overall){
  
  #Collate the data
  results_benefit_global$mean_GScurrentGlobal[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentGlobal
  results_benefit_global$mean_cgain_global_org[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_cgain_global_org
  results_benefit_global$bin_cgainGlobal[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$bin_cgainGlobal
  
  #Make the graph for global level, labeling only those with high benefit
  bin_cgains_scatter_global<-ggplot2::ggplot(results_benefit_global,ggplot2::aes(x=mean_GScurrentGlobal, y=mean_cgain_global_org, label=org_program))+
    ggplot2::aes(ymin=0, ymax=100, xmin=0, xmax=100)+
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_global$bin_cgainGlobal=="High"~"red",
                                               results_benefit_global$bin_cgainGlobal=="Medium"~"darkorange",
                                               results_benefit_global$bin_cgainGlobal=="Low"~"green",
                                               results_benefit_global$bin_cgainGlobal=="Zero"~"black"))+
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +    ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=40, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=10, y=10, xend=90, yend=10), color="black",linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=40, y=40, xend=60, yend=40), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=0, y=100, xend=100, yend=0), color="black",linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=100, yend=0), color="black",linetype=2)+
    ggplot2::geom_text(ggplot2::aes(x=5, y=90, label="High"), color="red", size=6) +
    ggplot2::geom_text(ggplot2::aes(x=50, y=38, label="Medium"), color="darkorange", size=6) +
    ggplot2::geom_text(ggplot2::aes(x=50, y=8, label="Low"), color="green", size=6) +
    ggplot2::geom_text(ggplot2::aes(x=85, y=2, label="Zero"), color="black", size=6) +
    ggplot2::theme(plot.title = ggplot2::element_text(size=20),
                  axis.title=ggplot2::element_text(size=16),
                   axis.text=ggplot2::element_text(size=16),
                   legend.title=ggplot2::element_text(size=16),
                   legend.text=ggplot2::element_text(size=16),
                   legend.justification = c(0.85,1),
                   legend.position = c(0.95,0.50),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())+
    ggplot2::labs(x="Current green score (%)",
                  y="Mean species program value (%)",
                  title="c")
  
  print(bin_cgains_scatter_global)
  return(bin_cgains_scatter_global)
  
}#end of the scatter_bin_cgains_global function


##### Bar graph of the BCR binned by current Green Score - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD ##### 
#' Bar graph of the BCR binned by current Green Score
#' 
#' This function can be used when the cost benefit analysis function cba_GplusD_LongTermPot_BinnedByGS is used. This function creates a bargraph of
#' the national and global BCRs, binned into groups based on the current Green Score of each program at the national and global levels
#'
#' @param results_BCR_global  A dataframe produced from the cost benefit analysis that outlines the global benefit each program
#' @param results_BCR_national A dataframe produced from the cost benefit analysis that outlines the national benefit each program
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @param results_overall A dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program
#' @return Returns the figure produced
#' @examples
#' #bargraph_binnedby_currentgs(results_BCR_global, results_BCR_national, inputs, results_overall)
#' @export

bargraph_binnedby_currentgs<-function(results_BCR_global, results_BCR_national, inputs, results_overall){
  
  # Add the category type to the results and then merge them so we can plot global and national together.
  results_BCR_global$Category<-"Global"
  results_BCR_national$Category<-"National"
  #Add the bin for triaging
  results_BCR_national$Bin[match(results_BCR_national$org_program, results_overall$org_program)]<-results_overall$bin_GScurrentNational
  results_BCR_global$Bin[match(results_BCR_global$org_program, results_overall$org_program)]<-results_overall$bin_GScurrentGlobal
  sim_results_summary_bincurrentgs<-rbind(results_BCR_national,results_BCR_global)
  #Add label for endemic species
  sim_results_summary_bincurrentgs$lab <- ifelse(sim_results_summary_bincurrentgs$org_program %in% endemic_species, "*","")
  #Remove the factor levels
  results_benefit_global$org_program <- as.character(results_benefit_global$org_program) 
  #Add group numbers based on Current Green Score bin
  sim_results_summary_bincurrentgs$bin_numeric<-NA
  sim_results_summary_bincurrentgs$bin_numeric[which(sim_results_summary_bincurrentgs$Bin=="Extinct in the Wild")]<-6
  sim_results_summary_bincurrentgs$bin_numeric[which(sim_results_summary_bincurrentgs$Bin=="Critically Depleted")]<-5
  sim_results_summary_bincurrentgs$bin_numeric[which(sim_results_summary_bincurrentgs$Bin=="Largely Depleted")]<-4
  sim_results_summary_bincurrentgs$bin_numeric[which(sim_results_summary_bincurrentgs$Bin=="Moderately Depleted")]<-3
  sim_results_summary_bincurrentgs$bin_numeric[which(sim_results_summary_bincurrentgs$Bin=="Slightly Depleted")]<-2
  sim_results_summary_bincurrentgs$bin_numeric[which(sim_results_summary_bincurrentgs$Bin=="Fully Recovered")]<-1
  
  #Reorder based on bin then by decreasing benefit
  sim_results_summary_bincurrentgs$org_program <- factor(sim_results_summary_bincurrentgs$org_program, 
                                                         levels=unique(results_BCR_national$org_program[
                                                           order(-sim_results_summary_bincurrentgs$bin_numeric, -sim_results_summary_bincurrentgs$mean, decreasing=TRUE
                                                           )]),ordered=TRUE)
  
  #### Make the bar graph
  bin_currentgs_graph <- ggplot2::ggplot(sim_results_summary_bincurrentgs, ggplot2::aes(y = org_program, x=mean*100000, fill = Category, color = Bin)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_text(ggplot2::aes(label = lab), color="black", show.legend=FALSE, vjust=0.70, hjust=0.1, size = 10) +
    ggplot2::scale_fill_manual(values=c("Global"="grey30",
                                        "National"="grey70")) +
    ggplot2::scale_color_manual(values=c("Extinct in the Wild" = "red",
                                         "Critically Depleted" = "orange",
                                         "Largely Depleted" = "purple",
                                         "Moderately Depleted" = "green",
                                         "Slightly Depleted" = "blue",
                                         "Fully Recovered"= "black")) +
    ggplot2::labs(x = "Mean BCR (conservation value per 100 000 CAD)",
                  y = "Species",
                  title="a") +
    ggplot2::labs(alpha = "Category", fill = "Category") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(size=20),
                  axis.text = ggplot2::element_text(size=16),
                   axis.title = ggplot2::element_text(size=16),
                   legend.title = ggplot2::element_text(size=16),
                   legend.text = ggplot2::element_text(size=16),
                   legend.justification = c(0.85,1),
                   legend.position = c(0.85,0.65),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())+
    ggplot2::theme(legend.key.size=unit(0.5, "cm"))
  
  
  print(bin_currentgs_graph)
 return(bin_currentgs_graph)
  
  
}#end of the bargraph_bin_gurrentgs function


##### Scatter plot of the national benefits binned by current Green Score - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' Scatter plot of the national benefits binned by current Green Score
#' 
#' This function can be used when the cost benefit analysis function cba_GplusD_LongTermPot_BinnedByGS is used. This function creates a scatter plot
#' of the national benefits of each conservation program, binned by the current Green Score. 
#'
#' @param results_overall a dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program
#' @param results_benefit_national a dataframe produced from the cost benefit analysis that outlines the national benefit each program
#' @return Returns the figure produced
#' @examples
#' #scatter_bin_currentgs_national(results_overall, results_benefit_national)
#' @export

scatter_bin_currentgs_national<-function(results_overall,results_benefit_national){
  
  #Collate the data
  results_benefit_national$mean_GScurrentNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentNational
  results_benefit_national$mean_cgain_national_org[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$mean_cgain_national_org
  results_benefit_national$bin_GScurrentNational[match(results_benefit_national$org_program,results_overall$org_program)]<-results_overall$bin_GScurrentNational
  
  #Make the graph for national level, labeling only those extinct or critically depleted
  bin_currentgs_scatter_national<-ggplot2::ggplot(results_benefit_national,ggplot2::aes(x=mean_GScurrentNational, y=mean_cgain_national_org, label=org_program))+
    ggplot2::scale_y_continuous(limits=c(0,100))+
    ggplot2::scale_x_continuous(limits=c(0,100))+                              
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_national$bin_GScurrentNational=="Extinct in the Wild"~"red",
                                               results_benefit_national$bin_GScurrentNational=="Critically Depleted"~"orange",
                                               results_benefit_national$bin_GScurrentNational=="Largely Depleted"~"purple",
                                               results_benefit_national$bin_GScurrentNational=="Moderately Depleted"~"green",
                                               results_benefit_national$bin_GScurrentNational=="Slightly Depleted"~"blue",
                                               results_benefit_national$bin_GScurrentNational=="Fully Recovered"~"black"))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggrepel::geom_text_repel(data=subset(results_benefit_national, mean_GScurrentNational<= 15 ),
                            box.padding=0.5, point.padding = 0.75, min.segment.length = 1, force=2, direction="both")+
    ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=0, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=20, y=0, xend=20, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=50, y=0, xend=50, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=80, y=0, xend=80, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=100, y=0, xend=100, yend=100), color="black",linetype=2)+
    ggplot2::geom_text(ggplot2::aes(x=0, y=90, label="Extinct in\nthe Wild"), color="red",size=4) +
    ggplot2::geom_text(ggplot2::aes(x=10,y=90, label="Critically \nDepleted"), color="orange", size=4) +
    ggplot2::geom_text(ggplot2::aes(x=35,y=90, label="Largely \nDepleted"), color="purple", size=4) +
    ggplot2::geom_text(ggplot2::aes(x=65,y=90, label="Moderately \nDepleted"), color="green",size=4) +
    ggplot2::geom_text(ggplot2::aes(x=90,y=90, label="Slightly \nDepleted"), color="blue",size=4) +
    ggplot2::geom_text(ggplot2::aes(x=100,y=90, label="Fully \nRecovered"), color="black",size=4) +
    ggplot2::theme(plot.title = ggplot2::element_text(size=20),
                  axis.title=ggplot2::element_text(size=16),
                   axis.text=ggplot2::element_text(size=16),
                   legend.title=ggplot2::element_text(size=16),
                   legend.text=ggplot2::element_text(size=16),
                   legend.justification = c(0.85,1),
                   legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())+
    ggplot2::labs(x="Current green score (%)",
                  y="Mean conservation value (%)",
                  title="b")
  
  print(bin_currentgs_scatter_national)
  return(bin_currentgs_scatter_national)
  
}#end of the scatter_bin_currentgs_national function


##### Scatter plot of the global benefits binned by current Green Score - ONLY APPLICABLE IF USING BINNED ANALYSIS METHOD #####
#' Scatter plot of the global benefits binned by current Green Score
#' 
#' This function can be used when the cost benefit analysis function cba_GplusD_LongTermPot_BinnedByGS is used. This function creates a scatter plot
#' of the global benefits of each conservation program, binned by the current Green Score. 
#' 
#' @param results_overall A dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program
#' @param results_benefit_global A dataframe produced from the cost benefit analysis that outlines the global benefit of each program
#' @return Returns the figure produced
#' @examples
#' #scatter_bin_currentgs_global(results_overall, results_benefit_global)
#' @export

scatter_bin_currentgs_global<-function(results_overall,results_benefit_global){
  
  #Collect data from results overall
  results_benefit_global$mean_GScurrentGlobal[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_GScurrentGlobal
  results_benefit_global$mean_cgain_global_org[match(results_benefit_global$org_program,results_overall$org_program)]<-results_overall$mean_cgain_global_org
  results_benefit_global$bin_GScurrentGlobal[match(results_benefit_global$org_program, results_overall$org_program)]<-results_overall$bin_GScurrentGlobal
  
  #Make the figure for global level, labeling only those binned as Extinct or Critically Depleted
  bin_currentgs_scatter_global<-ggplot2::ggplot(results_benefit_global,ggplot2::aes(x=mean_GScurrentGlobal, y=mean_cgain_global_org, label=org_program))+
    ggplot2::scale_y_continuous(limits=c(0,100))+
    ggplot2::scale_x_continuous(limits=c(0,100))+                              
    ggplot2::theme_bw()+
    ggplot2::geom_point(color=dplyr::case_when(results_benefit_global$bin_GScurrentGlobal=="Extinct in the Wild"~"red",
                                               results_benefit_global$bin_GScurrentGlobal=="Critically Depleted"~"orange",
                                               results_benefit_global$bin_GScurrentGlobal=="Largely Depleted"~"purple",
                                               results_benefit_global$bin_GScurrentGlobal=="Moderately Depleted"~"green",
                                               results_benefit_global$bin_GScurrentGlobal=="Slightly Depleted"~"blue",
                                               results_benefit_global$bin_GScurrentGlobal=="Fully Recovered"~"black"))+
    #ggrepel::geom_text_repel(ggplot2::aes(label=org_program),point.padding=0.5, box.padding=0.25)+
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.9), ggplot2::aes(ymin = P5, ymax = P95), width = 0.1, color = "black") +
    ggrepel::geom_text_repel(data=subset(results_benefit_global, bin_GScurrentGlobal=="Extinct in the Wild" | bin_GScurrentGlobal=="Critically Depleted"), 
                             force=2)+
    ggplot2::geom_segment(ggplot2::aes(x=0, y=0, xend=0, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=20, y=0, xend=20, yend=100), color="black",linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=50, y=0, xend=50, yend=100), color="black", linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=80, y=0, xend=80, yend=100), color="black",linetype=2) +
    ggplot2::geom_segment(ggplot2::aes(x=100, y=0, xend=100, yend=100), color="black",linetype=2)+
    ggplot2::geom_text(ggplot2::aes(x=0, y=90, label="Extinct in \nthe Wild"), color="red", size=4) +
    ggplot2::geom_text(ggplot2::aes(x=10,y=90, label="Critically \nDepleted"), color="orange", size=4) +
    ggplot2::geom_text(ggplot2::aes(x=35,y=90, label="Largely \nDepleted"), color="purple", size=4) +
    ggplot2::geom_text(ggplot2::aes(x=65,y=90, label="Moderately \nDepleted"), color="green", size=4) +
    ggplot2::geom_text(ggplot2::aes(x=90,y=90, label="Slightly \nDepleted"), color="blue", size=4) +
    ggplot2::geom_text(ggplot2::aes(x=100,y=90, label="Fully \nRecovered"), color="black", size=4) +
    ggplot2::theme(plot.title = ggplot2::element_text(size=20),
                  axis.title=ggplot2::element_text(size=16),
                   axis.text=ggplot2::element_text(size=16),
                   legend.title=ggplot2::element_text(size=16),
                   legend.text=ggplot2::element_text(size=16),
                   legend.justification = c(0.85,1),
                   legend.position = c(0.95,0.95),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())+
    ggplot2::labs(x="Current green score (%)",
                  y="Mean conservation value (%)",
                  title="c")
  
  print(bin_currentgs_scatter_global)
  return(bin_currentgs_scatter_global)
  
  
}#end of the scatter_bin_currentgs_global function


##### Graphing the cost-benefit ratios ##### 
#' Figure of the benefit-cost ratios
#' 
#' This function produces a bargraph of the national and global benefit-cost ratios calculated in the analysis functions. 
#'
#' @param results_BCR_national a dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program at the national level
#' @param results_BCR_global a dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program at the global level
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @return Returns the figure produced
#' @examples
#' #graph_BCR(results_BCR_national, results_BCR_global)
#' @export

graph_BCR <- function(results_BCR_national,results_BCR_global, inputs){
  results_BCR_global$Category <- "Global"
  results_BCR_national$Category <- "National"
  results_BCR_all <- rbind(results_BCR_national, results_BCR_global)
  #Label endemics
  results_BCR_all$lab <- ifelse(results_BCR_all$org_program %in% endemic_species, " *", "")
  # Identify extirpated or extinct in the wild species
  species_extirpated_national <- inputs$species[which(inputs$subcategory=="GScurrentNational" & inputs$baseP50==0)]
  species_extinct_global <- inputs$species[which(inputs$subcategory=="GScurrentGlobal" & inputs$baseP50==0)]
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  results_BCR_all$extirpated_or_extinct <- "Extant" # initialize
  results_BCR_all$extirpated_or_extinct[is.na(match(results_BCR_all$org_program, extirpated_or_extinct)) == FALSE] <- "Extirpated" # initialize
  #Factor based on extirpated or extinct
  results_BCR_all$extirpated_or_extinct <- factor(results_BCR_all$extirpated_or_extinct, 
                                                  levels = c("Extirpated", "Extant"))
  
  #Reorder based on national benefits level
  results_BCR_all$org_program <- factor(results_BCR_all$org_program, 
                                        levels=unique(results_BCR_national$org_program[order(-results_BCR_all$mean, decreasing = TRUE)]), ordered=TRUE)
  
  #Change xaxis label depending on analysis option
  if(analysisoption==1|analysisoption==3){ #Need to change xlab depending on analysis option
    xbcrlabel = "Mean BCR\n(species program value per 100 000 CAD)"}
  if(analysisoption==2|analysisoption==4){
    xbcrlabel = "Mean BCR\n(conservation value per 100 000 CAD)"}
  if(analysisoption==5){
    xbcrlabel = "Mean BCR\n(species program value relative to SRS per 100 000 CAD)    "}
  if(analysisoption==6){
    xbcrlabel = "Mean BCR\n(program benefit per 100 000 CAD)"}
  
  ##Make the figure
  BCR_graph <- ggplot2::ggplot(results_BCR_all, ggplot2::aes(y = org_program, x = mean*100000, alpha = Category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct,nrow=2, ncol=1, scales="free") + 
    ggplot2::scale_alpha_manual(values=c(1,.3)) +
    ggplot2::geom_text(ggplot2::aes(label = lab), show.legend = FALSE, vjust=0.5, size = 10, color = "black")+
    ggplot2::labs( x = xbcrlabel,
                   # y = "Average BCR \n (% Change in 50yr Species Persistence per 100 000 CAD)",
                   # y = "Average BCR \n (Green Status Gain plus Dependence (%) per 100 000 CAD)",
                   y = "Species program",
                   title = "a") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_text(size=16),
                   legend.text = ggplot2:: element_text(size=16),
                   axis.title = ggplot2::element_text(size=16),
                   axis.text = ggplot2::element_text(size=16),
                   strip.text = ggplot2::element_text(size=16),
                   legend.justification = c(1,1),
                   legend.position = c(0.95,0.35),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
     
  # convert ggplot object to grob object
  gp <- ggplot2::ggplotGrob(BCR_graph)
  # optional: take a look at the grob object's layout
  gtable::gtable_show_layout(gp)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$t[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  y.var <- sapply(ggplot2::ggplot_build(BCR_graph)$layout$panel_scales_y,
                  function(t) length(t$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$heights[facet.columns] <- gp$heights[facet.columns] * y.var
  
  # plot result
  grid::grid.draw(gp)
  BCR_graph_final <- gp
  
  return(BCR_graph_final)
}


##### Graphing the uncertainty of the BCR results ##### 
#' Figure of the uncertainty of the BCR results
#'
#' This function produces a figure of the uncertainty around the means benefit-cost ratios at the national and global ratios for each program. 
#'
#' @param results_BCR_national A dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program at the national level
#' @param results_BCR_global A dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program at the global level
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @return Returns the figure produced
#' @examples
#' #graph_BCR_uncertainty(results_BCR_national, results_BCR_global, inputs)
#' @export

graph_BCR_uncertainty <- function(results_BCR_national,results_BCR_global, inputs){

  #### Plotting the credible intervals #####
  results_BCR_global$Category <- "Global"
  results_BCR_national$Category <- "National"
  #calculate the CI90 for each mean at national and global levels
  cred_intervals <- rbind(results_BCR_national,results_BCR_global)
  cred_intervals$ci90<-as.numeric(cred_intervals$P95)-as.numeric(cred_intervals$P5)
  #label endemic
  cred_intervals$lab <- ifelse(cred_intervals$org_program %in% endemic_species, "*","")
  
  #Prepping the extinct vs extant for the facet panelling 
  species_extirpated_national <- inputs$species[which(inputs$subcategory=="GScurrentNational" & inputs$baseP50==0)]
  species_extinct_global <- inputs$species[which(inputs$subcategory=="GScurrentGlobal" & inputs$baseP50==0)]
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  
  cred_intervals$extirpated_or_extinct <- "Extant" # initialize
  cred_intervals$extirpated_or_extinct[is.na(match(cred_intervals$org_program, extirpated_or_extinct)) == FALSE] <- "Extirpated" # initialize
  cred_intervals$extirpated_or_extinct <- factor(cred_intervals$extirpated_or_extinct, 
                                                 levels = c("Extirpated", "Extant"))
  
  #Reorder based on national level
  cred_intervals$org_program <- factor(cred_intervals$org_program, 
                                       levels=unique(cred_intervals$org_program[order(-results_BCR_national$mean, decreasing = TRUE)]), ordered=TRUE)
  #### Make figure 
  BCR_credible_intervals<-  ggplot2::ggplot(cred_intervals, ggplot2::aes(y = org_program, x = ci90*100000, alpha = Category)) +
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free", nrow=2, ncol=1) + 
    ggplot2::scale_alpha_manual(values=c(1,.3)) +
    ggplot2::labs(x = "Uncertainty around mean BCR\n(90% credible interval)",
                  y = "", 
                  title = "b") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text=ggplot2::element_text(size=16),
                   axis.text.y=ggplot2::element_blank(),
                   axis.title=ggplot2::element_text(size=16),
                   strip.text=ggplot2::element_text(size=16),
                   legend.text = ggplot2::element_text(size=16),
                   legend.title = ggplot2::element_text(size=16),
                   legend.justification = c(1,1), 
                   legend.position = c(0.95,0.35),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
  
   # convert ggplot object to grob object
  gp2 <- ggplot2::ggplotGrob(BCR_credible_intervals)
  # optional: take a look at the grob object's layout
  gtable::gtable_show_layout(gp2)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns2 <- gp2$layout$t[grepl("panel", gp2$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  y.var2 <- sapply(ggplot2::ggplot_build(BCR_credible_intervals)$layout$panel_scales_y,
                   function(t) length(t$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp2$heights[facet.columns2] <- gp2$heights[facet.columns2] * y.var2
  
  # plot result
  grid::grid.draw(gp2)
  BCR_credi_final <- gp2
  
  
  return(BCR_credi_final)
  
}#end of the graph_BCR function

##### Graphing the directional uncertainty of the BCR results ##### 
#' Figure of the directional uncertainty of the mean BCR results
#'
#' This function produces a figure of the directional uncertainty around the zero-centered means benefit-cost ratios at the national and global ratios for each program. 
#'
#' @param results_BCR_national A dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program at the national level
#' @param results_BCR_global A dataframe produced from the cost benefit analysis that outlines BCR and ranks for each conservation program at the global level
#' @param inputs A CSV containing the conservation programs and the P5, P50, and P95 of associated benefits and costs
#' @return Returns the figure produced
#' @examples
#' #graph_BCR_uncertainty(results_BCR_national, results_BCR_global, inputs)
#' @export

graph_BCR_directional_uncertainty <- function(results_BCR_national,results_BCR_global, inputs){
  
  #### Plotting directional credible intervals rather than total amount #####
  results_BCR_global$Category <- "Global"
  results_BCR_national$Category <- "National"
  #Center data
  temp_bcr_national<-results_BCR_national
  temp_bcr_global<-results_BCR_global
  #recenter data
  temp_bcr_national$centered_BCR_mean<-temp_bcr_national$mean-temp_bcr_national$mean
  temp_bcr_national$centered_BCR_UCL<-temp_bcr_national$P5-temp_bcr_national$mean
  temp_bcr_national$centered_BCR_LCL<-temp_bcr_national$P95-temp_bcr_national$mean
  
  temp_bcr_global$centered_BCR_mean<-temp_bcr_global$mean-temp_bcr_global$mean
  temp_bcr_global$centered_BCR_UCL<-temp_bcr_global$P5-temp_bcr_global$mean
  temp_bcr_global$centered_BCR_LCL<-temp_bcr_global$P95-temp_bcr_global$mean
  
  temp_bcr<-rbind(temp_bcr_national, temp_bcr_global)
  
  #Prepping the extinct vs extant for the facet panelling 
  
  species_extirpated_national <- inputs$species[which(inputs$subcategory=="GScurrentNational" & inputs$baseP50==0)]
  species_extinct_global <- inputs$species[which(inputs$subcategory=="GScurrentGlobal" & inputs$baseP50==0)]
  extirpated_or_extinct <- c(species_extirpated_national, species_extinct_global)
  temp_bcr$extirpated_or_extinct <- "Extant" #initialize
  temp_bcr$extirpated_or_extinct[is.na(match(temp_bcr$org_program, extirpated_or_extinct)) == FALSE] <- "Extirpated"
  temp_bcr$extirpated_or_extinct <- factor(temp_bcr$extirpated_or_extinct, levels = c("Extirpated", "Extant"))
  
  #Reorder based on national level
  temp_bcr$org_program <- factor(temp_bcr$org_program, 
                                       levels=unique(temp_bcr$org_program[order(-results_BCR_national$mean, decreasing = TRUE)]), ordered=TRUE)
  #### Make figure 
  BCR_directional_credible_intervals<-  ggplot2::ggplot(temp_bcr, ggplot2::aes(y = org_program, x = centered_BCR_mean, alpha = Category)) + 
    ggplot2::geom_bar(stat="identity",position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin=centered_BCR_LCL*100000, xmax=centered_BCR_UCL*100000), width = 0.75, position = ggplot2::position_dodge2(width=0.9)) +
    ggplot2::facet_wrap(. ~ extirpated_or_extinct, scales="free", nrow=2, ncol=1) + 
    ggplot2::scale_alpha_manual(values=c(1,.3)) +
    ggplot2::geom_vline(xintercept = 0, lty="dashed") + 
    ggplot2::labs(x = "90% confidence interval of zero-centered Mean BCR",
                  y = "", 
                  title = "b") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text=ggplot2::element_text(size=16),
                   axis.text.y=ggplot2::element_blank(),
                   axis.title=ggplot2::element_text(size=16),
                   strip.text=ggplot2::element_text(size=16),
                   legend.text = ggplot2::element_text(size=16),
                   legend.title = ggplot2::element_text(size=16),
                   legend.justification = c(1,1), 
                   legend.position = c(0.95,0.35),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
  
  # convert ggplot object to grob object
  gp2 <- ggplot2::ggplotGrob(BCR_directional_credible_intervals)
  # optional: take a look at the grob object's layout
  gtable::gtable_show_layout(gp2)
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns2 <- gp2$layout$t[grepl("panel", gp2$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  y.var2 <- sapply(ggplot2::ggplot_build(BCR_directional_credible_intervals)$layout$panel_scales_y,
                   function(t) length(t$range$range))
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp2$heights[facet.columns2] <- gp2$heights[facet.columns2] * y.var2
  
  # plot result
  grid::grid.draw(gp2)
  BCR_credi_direction_final <- gp2
  
  
  return(BCR_credi_direction_final)
  
}#end of the graph_BCR function