# Covid Surgical Modelling paper

This is the code for our paper:

**Resource requirements for reintroducing elective surgery in England during the COVID-19 pandemic: a modelling study**

It includes:
* main_run.R - core analysis, table generation and plots

* opcs_grouping.csv includes the definition of surgery and associated procedure categorisations
* R folder:
  * library.R - dependencies
  * MAR_data_load_clean.R - code to load [Monthly activity returns](https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/monthly-hospital-activity/mar-data/) data from NHS England
  * model_funs.R - functions for modelling, automated HES download, cleaning, assorted functions
  * proportions_table.R - generate table to weight resources by OPCS codes
  * resource_model.R -  functions to calculate resources
  
  *Please note that you need to manually run the first few lines of main_run.R as this has some manual inputs required during data cleaning steps*
