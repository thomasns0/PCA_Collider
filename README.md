# PCA_Collider
Scripts associated with the manuscript "Principal Component Analysis Reduces Collider Bias in Polygenic Risk Score Effect Size Estimation"

<b>**0 Simulation**</b>

-ColliderBiasPhRM_simulation_fenn.R
  *Runs simulation with normally distributed continuous outcome variable on remote server as 50 jobs 

-Concat_SimulationData.R
  *Concatenates the 50 effect size files from ColliderBiasPhRM_simulation_fenn.R

--Concat_SimulationData_R2raw.R
  *Concatenates the 50 R-squared output files from ColliderBiasPhRM_simulation_fenn.R

-Logit and Probit Simulations

--ColliderBiasPhRM_simulation_fenn_logit.R
  *Runs simulation with logistic binary outcome variable on remote server as 50 jobs 

--ColliderBiasPhRM_simulation_fenn_probit.R
  *Runs simulation with probit binary outcome variable on remote server as 50 jobs 

--Concat_SimulationData_logit.R
  *Concatenates the 50 effect size files from ColliderBiasPhRM_simulation_fenn_logit.R

--Concat_SimulationData_probit.R
  *Concatenates the 50 effect size files from ColliderBiasPhRM_simulation_fenn_probit.R

Simulation output data is available here:
https://drive.google.com/drive/folders/1Wpd9WoFL4C1Xt_xHX7VNDJGvP3mNwSzY?usp=sharing


<b>**1 Real Data Example**</b>

-ColliderPCA_AUDtarget_AnyEnv_COGA.R
  *Runs real data PCA and prediction analyses for ALC PRS, EXT PRS, TOB environment, and EDU environment

- PCA_Select.R
  * Function to run identify confounder variables for PCA, implement imputation, run PCA, and run parallel analysis to identify the number of PCs to use as covariates. Dependency for ColliderPCA_AUDtarget_AnyEnv_COGA.R


<b>**2 misc**</b>

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
  *Plots effect sizes from simulation with normally distributed continuous outcome variable

-PlotPhRM_simulation_R2raw.R
  *Plots R-squared from simulation with normally distributed continuous outcome variable

-Plot Logit and Probit Simulations

--PlotPhRM_simulation_logit.R
  *Plots effect sizes from simulation with logistic binary outcome variable
 
--PlotPhRM_simulation_probit.R
  *Plots effect sizes from simulation with probit binary outcome variable
