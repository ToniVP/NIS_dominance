This README file was generated on 2024-01-29 by Antoni Vivó Pons.

GENERAL INFORMATION

1. Title of Dataset: Disentangling the effects of abiotic and biotic processes on non-indigenous species dominance

2. Author Information
	A. Principal Investigator Contact Information
  	 Name: Antoni Vivó-Pons	
  	 Institution: National Insitute for Aquatic resources - Denmark Technical University
  	 Address:  Kgs. Lyngby, Denmark
  	 Email: avipo@aqua.dtu.dk

3. Date of data collection (single date, range, approximate date): 1993-2020 & 2009 - 2021

4. Geographic location of data collection: Baltic Sea, Swedish coast

SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: CC0 1.0 Universal (CC0 1.0) Public Domain

2. Links to publications that cite or use the data:

	Vivó-Pons, A., van Denderen, D., Flensborg, L., Jaspers, C., Lindegren, M. (2024). Disentangling the effects of abiotic and biotic processes on non-indigenous species dominance. Ecology Letters.

3. Links to other publicly accessible locations of the data: [https://sharkweb.smhi.se](https://sharkweb.smhi.se) & [https://www.slu.se/en/departments/aquatic-resources1/databases/database-for-coastal-fish-kul/](https://www.slu.se/en/departments/aquatic-resources1/databases/database-for-coastal-fish-kul/)

4. Links/relationships to ancillary data sets: None

5. Was data derived from another source? No

6. Recommended citation for this dataset: 

Vivó-Pons, A., van Denderen, D., Flensborg, L., Jaspers, C., Lindegren, M. (2024). Data from: Disentangling the effects of abiotic and biotic processes on non-indigenous species dominance. Dryad Digital Repository. 

DATA & FILE OVERVIEW

1. File List:

In *Data* folder
A) Marenzelleria_data_metrics.txt
B) round_goby_data_metrics.txt
C) partial_plot_Marenzelleria.txt
D) partial_plot_RG.txt

2. Relationship between files, if important: -

3. Additional related data collected that was not included in the current data package: None

4. Are there multiple versions of the dataset? No

#########################################################################

DATA-SPECIFIC INFORMATION FOR: Marenzelleria_data_metrics.txt

1. Number of variables: 34

2. Number of cases/rows: 3534

3. Variable List:

    - station: numerical identifier for each unique sampling station
    * year:  sampling year (1993-2020)
    - lon: longitude value (decimal degrees)
    * lat: latitude value (decimal degrees)
    * non_NIS_Di: mean functional distinctiveness weighted by AFDW for all native species
    * overall_Di: mean functional distinctiveness weighted by AFDW for all species
    * ratio_NISvsNAt: relative NIS biomass (%) in the community
    * ratio_sp: percentage of NIS in the total number of recorded species within the community
    * non_NIS_Di_nw: mean functional distinctiveness not weighted by AFDW for all native species
    * overall_Di_nw: mean functional distinctiveness not weighted by AFDW for all species
    * nbsp/richness: total species number in the community
    * sing.sp: number of functionally singular species in each community
    * FRic: Functional Richness of each community
    * FEve: Functional Evenness of each community
    * FDiv: Functional Divergence of each community
    * FDis: Functional Dispersion of each community
    * RaoQ: Rao's quadratic entropy (Q) of each community
    * Shannon: Shannon's diversity index (H') of each community
    * Simpson: Simpson's diversity index (D) of each community
    * Evenness: Pielou's species evenness index (J) of each community
    * mean_NIS_Di: mean functional distinctiveness weighted by AFDW for all NIS
    * NIS_Di: *Marenzelleria*’s values of distinctiveness weighted by AFDW
    * NIS_relab: relative biomass (AFDW) of *Marenzelleria*
    * NIS_Di_nw: *Marenzelleria*’s values of distinctiveness not weighted by AFDW
    * species: species name
    * depth: depth (m)
	- bot_sal: sea bottom salinity (PSU)
	- bot_sal_var: yearly standard deviation for the bottom salinity (PSU)
	- bot_T: sea bottom temperature (Celsius)
	- bot_T_var: yearly standard deviation for the bottom temperature (Celsius)
	- bot_oxy: sea bottom oxygen concentration (mmol/m3)
	- bot_oxy_var: yearly standard deviation for the bottom oxygen concentration (mmol/m3)

#########################################################################

DATA-SPECIFIC INFORMATION FOR: partial_plot_Marenzelleria.txt

1. Number of variables: 2100

2. Number of cases/rows: 6

3. Variable List:

    * rel_abund_NIS: prediction for the relative biomass of *Marenzelleria* based on the effects of single predictors (*variable*)
    * variable: name of the predictor used to obtain the partial effects
    * CI.lower: predicted values for relative biomass corresponding to the lower part of the standard error // error interval given by the model
    * CI.upper: predicted values for relative biomass corresponding to the upper part of the standard error // error interval given by the model
    * species: species name
    * x_axis: values used to predict the relative biomass


#########################################################################

DATA-SPECIFIC INFORMATION FOR: round_goby_data_metrics.txt

1. Number of variables: 45

2. Number of cases/rows: 735

3. Variable List:

    - fiske_id: numerical identifier for each sampling event
    - StationsNr: numerical identifier for each station within each location (*Sample*)
    - Sample: name of each location
    - catch_area: name of the specific area within each location (*Sample*) where samples were taken
    * year:  sampling year (1993-2020)
    * tool: gear used for sampling
    - lon: longitude value (decimal degrees)
    * lat: latitude value (decimal degrees)
    * non_NIS_Di: mean functional distinctiveness weighted by WPUE for all native species
    * overall_Di: mean functional distinctiveness weighted by WPUE for all species
    * ratio_NISvsNAt: relative NIS biomass (%) in the community
    * ratio_sp: percentage of NIS in the total number of recorded species within the community
    * non_NIS_Di_nw: mean functional distinctiveness not weighted by WPUE for all native species
    * overall_Di_nw: mean functional distinctiveness not weighted by WPUE for all species
    * nbsp/richness: total species number in the community
    * sing.sp: number of functionally singular species in each community
    * FRic: Functional Richness of each community
    * FEve: Functional Evenness of each community
    * Shannon: Shannon's diversity index (H') of each community
    * Simpson: Simpson's diversity index (D) of each community
    * Evenness: Pielou's species evenness index (J) of each community
    * NIS_Di: Round goby’s values of distinctiveness weighted by WPUE
    * NIS_relab: relative biomass (WPUE) of round goby
    * mean_NIS_Di: mean functional distinctiveness weighted by WPUE for all NIS
    * NIS_Di_nw: *Marenzelleria*’s values of distinctiveness not weighted by WPUE
    * depth: depth (m)
    * wind_speed: wind speed (m/s)
    * wind_direction: wind direction (degrees (o))
	- bot_sal: sea bottom salinity (PSU)
	- bot_sal_var: monthly standard deviation for the bottom salinity (PSU)
	- surf_sal: sea surface salinity (PSU)
	- surf_sal_var: monthly standard deviation for the surface salinity (PSU)
	- bot_t: sea bottom temperature (Celsius)
	- bot_t_var: monthly standard deviation for the bottom temperature (Celsius)
	- surf_t: sea surface temperature (Celsius)
	- surf_t_var: monthly standard deviation for the surface temperature (Celsius)
	- bot_oxy: sea bottom oxygen concentration (mmol/m3)
	- bot_oxy_var: monthly standard deviation for the bottom oxygen concentration (mmol/m3)
	- secchi: secchi depth (m)
	- chl: chlorophyll a concentration (mg/m3)
	- chl_var: monthly standard deviation for the chlorophyll a concentration (mg/m3)
	- dist_offshore: distance to the offshore, proxy for coastal exposure (m)
	- samp_st: unique identifyer for each station within location
	- time_step: time steps for the invasion process (3 time steps)

#########################################################################

DATA-SPECIFIC INFORMATION FOR: partial_plot_RG.txt

1. Number of variables: 1500

2. Number of cases/rows: 6

3. Variable List:

    * rel_abund_NIS: prediction for the relative biomass of round goby based on the effects of single predictors (*variable*)
    * variable: name of the predictor used to obtain the partial effects
    * CI.lower: predicted values for relative biomass corresponding to the lower part of the standard error // error interval given by the model
    * CI.upper: predicted values for relative biomass corresponding to the upper part of the standard error // error interval given by the model
    * species: species name
    * x_axis: values used to predict the relative biomass

*******************************************************************************
CODE INFORMATION

**Libraries_Functions**. List of required libraries.

**01_Descriptive_figures**. This script contains the code related to the **descriptive figures of the data** used in the study (Fig. 2).

**02_SEM Marenzelleria**. This script corresponds to the exploratory and final structural equations model (SEM) for *Marenzelleria*, as well as their corresponding figures (Fig. 3A, Fig. S1A, Fig. S2)

**03_SEM Round goby**. This script corresponds to the exploratory and final structural equations model (SEM) for round goby, as well as their corresponding figures (Fig. 3B, Fig. S1B, Fig. S2)

**04_Variance partitioning (Fig. 4)**. This script contains all the code related with the variance partitioning plots (Fig. 4A, B), all the effects from environmental variables (Fig. 4C, D) and the combination of all abiotic vs biotic effects (Fig. 4E).

**05_Partial effects plot (Fig. 5)**. This script contains all the code related with the partial effects plots for all variables on NIS dominance (Fig. 5).