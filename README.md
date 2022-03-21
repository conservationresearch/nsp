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

This cost benefit analysis then takes the input csv and builds a metalog distribution using the P5, P50, and P95 for each benefit and cost metric. A Monte Carlo
simulation then samples a user-specified number of values from that distribution into a single vector. The mean of these values for each metric can then be used
in the cost-benefit analysis. The benefits are then divided by the cost to give the benefit cost ratio (BCRs) for each project, and these ratios are what the package then ranks. Higher ranking projects would maximize the benefit and minimize cost. 

Each organization has different perspectives and can choose to quantify the benefits differently. Six options are provided in hopes that organizations will be
able to reflect their beliefs in how they perform the analysis. Option 1, the simplest, quantifies the benefit as the Conservation Gain plus Conservation Dependence
metric. Option 2 builds upon Option 1 quantifies benefits as Conservation Gain Plus Dependence, relative to Long Term Aspiration metric. Option 3 performs the same
calculations as Option 1, however, before ranking the BCRs it bins the conservation projects based on the total benefits. Projects with higher total gain, regardless
of cost, are given priority. Option 4 performs the same calculations as Option 2, however, before ranking the BCRs it bins the conservation projects based on their
Current Green Score. Projects that have a lower Current Green Score are given priority. Option 5 quantifies the benfit as the Conservation Gain plus Conservation
Dependence, relative to Current Green Score plus Epsilon. Option 6 quantifies the benefit as the Conservation Gain Plus Conservation Dependence, relative to 
Long Term Aspiration, and relative to the Current Green Score plus Epsilon. Epsilon is a subjective term that is added to deal with Species that have a Current
Green Score of zero (extirpated), thus avoiding dividing by zero. 

To use this package, users will need to edit the wrapper script new_species_prioritization.R file within the "Scripts" folder. After editing the input and output locations, specifiying variables, etc, this wrapper script can then call the other scripts contained within the "R" folder to perform the value draws, analysis, figure generation,
and sensitivty analysis. 
