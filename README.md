This is the working project repository for DW Impacts Group Project at the Bren School of Environmental Science & Management 

The goal of this project is to conduct a Cost-Benefit Analysis for the state of Liberia to provide information as well as potential outcomes of modifying their fisheries management regime. 

Data References: 

**FAO Data from FishstatJ**
FAO. 2023. Fishery and Aquaculture Statistics. CECAF (Eastern Central Atlantic) capture production 1970-2021 (FishStatJ). In: FAO Fisheries and Aquaculture Division [online]. Rome. Updated 2023. www.fao.org/fishery/en/statistics/software/fishstatj

General Layout/Workflow:

Folders starting with "data_" hold either raw data or intermediate data. The primary difference between the two are that the raw data are unclean or from the original sources, whereas intermediate data are cleaned and are what is used for biological parameter calculations as well as the main analysis. 

Folders ending with "_data_prep" are either data tidying processes (raw_data_prep) or analysis/biological parameter calculation scripts (int_data_prep). Raw data prep is delineated by data source, whereas Intermediate data prep contains both "simple_bio_model" which contains the main work for the analysis, and "bio_experiment" which is not used in the final analysis but contains experimentation with creating a complex age-structured bioeconomic model. Unfortunately this process was very messy, although still provides useful insight for how to effectively deploy an age structured population model in R, which is why it is being maintained. 

The "plots" folder contains all plots written out from the project, and are created within "int_data_prep". These plots contain information from both raw data as well as the final analysis, and are titled with an abbrevation based off of the data being plotted (Effort, catch, catch per unit effort, net present value of benefits stream, present value of benefits stream), any associated years or timelines, and by species if applicable. 

The "utils" folder contains custom functions written by Dustin Duncan to ease workflows within scripts. These contain the primary age structured bioeconomic model used in the analysis, as well as a calibration function to calculate negative log-likelihood from the bioeconomic function of fit to raw catch data. In addition, there is a constraint function which was used with nloptr to constrain the parameter values acceptable to the optimization algorithm. Finally, there is a plot function which creates various plots from the final output of the bioeconomic model. 