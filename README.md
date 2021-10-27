# PCA_Collider
Scripts associated with the manuscript "Principal Component Analysis Corrects Collider Bias in Polygenic Risk Score Effect Size Estimation"

0 Simulation 

-ColliderBiasPhRM_simulation_fenn.R
*Runs simulation on remote server as 50 jobs 

-Concat_SimulationData.R
*Concatenates the 50 output files from ColliderBiasPhRM_simulation_fenn.R

Simulation output data presented in the manuscript is available here:
https://drive.google.com/file/d/1XBvQnTMp3rtUR4ozTyn6EAUgk85Ozlm9/view?usp=sharing

1 Real Data Example

-ColliderPCA_AUDtarget_AnyEnv_COGA.R
*Runs real data PCA and prediction analyses for ALC PRS, EXT PRS, TOB environment, and EDU environment

- PCA_Select.R

* Function to run identify confounder variables for PCA, implement imputation, run PCA, and run parallel analysis to identify the number of PCs to use as covariates. Dependency for ColliderPCA_AUDtarget_AnyEnv_COGA.R


2 misc

-PlotsAndTables

-0MakePlotsForManuscript.R
*Plots for real data example with ALC PRS

-0MakeTablesForManuscript.R
*Tables for real data example with ALC PRS

-MakePlotsForManuscript_EXT.R
*Plots for real data example with EXT PRS

-MakeTablesForManuscript_EXT.R
*Tables for real data example with EXT PRS

-PlotPhRM_simulation.R
*Plots for simulation 

