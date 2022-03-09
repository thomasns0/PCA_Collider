# PCA_Collider
Scripts associated with the manuscript "Principal Component Analysis Reduces Collider Bias in Polygenic Risk Score Effect Size Estimation"

<b>**0 Simulation**</b>

- ColliderBiasPhRM_simulation_fenn.R
  <i> : Runs simulation with normally distributed continuous outcome variable on remote server as 50 jobs</i>

- Concat_SimulationData.R
    <i> : Concatenates the 50 effect size files from ColliderBiasPhRM_simulation_fenn.R</i>

- Concat_SimulationData_R2raw.R
    <i> : Concatenates the 50 R-squared output files from ColliderBiasPhRM_simulation_fenn.R</i>

Logit and Probit Simulations

-   ColliderBiasPhRM_simulation_fenn_logit.R
      <i> : Runs simulation with logistic binary outcome variable on remote server as 50 jobs </i>

-   ColliderBiasPhRM_simulation_fenn_probit.R
      <i> : Runs simulation with probit binary outcome variable on remote server as 50 jobs </i>

-   Concat_SimulationData_logit.R
      <i> : Concatenates the 50 effect size files from ColliderBiasPhRM_simulation_fenn_logit.R</i>

-   Concat_SimulationData_probit.R
      <i> : Concatenates the 50 effect size files from ColliderBiasPhRM_simulation_fenn_probit.R</i>

Simulation output data is available here:
https://drive.google.com/drive/folders/1Wpd9WoFL4C1Xt_xHX7VNDJGvP3mNwSzY?usp=sharing


<b>**1 Real Data Example**</b>

-ColliderPCA_AUDtarget_AnyEnv_COGA.R
    <i> : Runs real data PCA and prediction analyses for ALC PRS, EXT PRS, TOB environment, and EDU environment</i>

- PCA_Select.R
    <i> :  Function to run identify confounder variables for PCA, implement imputation, run PCA, and run parallel analysis to identify the number of PCs to use as covariates. Dependency for ColliderPCA_AUDtarget_AnyEnv_COGA.R</i>


<b>**2 misc**</b>

PlotsAndTables

- 0MakePlotsForManuscript.R
    <i> : Plots for real data example with ALC PRS</i>

- 0MakeTablesForManuscript.R
    <i> : Tables for real data example with ALC PRS</i>

- MakePlotsForManuscript_EXT.R
    <i> : Plots for real data example with EXT PRS</i>

- MakeTablesForManuscript_EXT.R
    <i> : Tables for real data example with EXT PRS</i>

- PlotPhRM_simulation.R
    <i> : Plots effect sizes from simulation with normally distributed continuous outcome variable</i>

- PlotPhRM_simulation_R2raw.R
    <i> : Plots R-squared from simulation with normally distributed continuous outcome variable</i>

Plot Logit and Probit Simulations

-   PlotPhRM_simulation_logit.R
      <i> : Plots effect sizes from simulation with logistic binary outcome variable</i>
 
-   PlotPhRM_simulation_probit.R
      <i> : Plots effect sizes from simulation with probit binary outcome variable</i>
