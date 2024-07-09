## Fed-global-spillovers
Replication files for COMPREHENSIVE LOOK ON THE GLOBAL SPILLOVERS OF US MONETARY POLICY: THE ROLE OF THE “FED INFORMATION EFFECT” AND UNCERTAINTY


##Replication of Table 3-9:

Panel data available on this [link](https://docs.google.com/spreadsheets/d/1UQwIR2QPpHSAuk50hbdnqgOv16F_f55a/edit?usp=drive_link&ouid=101240359591743494198&rtpof=true&sd=true) 

Source of data: [Lakdawala et al. (2021)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/O2DABL) and Bloomberg

Instruction:
Follow the _Table 3-9.r_ code for more information on variables and panel data analysis

##Replication of Table 10:

_data_for_table.csv_ is the data for Table 10. These releant variables were selected from the large panel dataset. 
_Table 10 stata code.do_.  is based on Lakdawala et al.(2021) and Iacoviello & Navarro (2019).

Instruction:
Read _data_for_table.csv_ into Stata and run _Table 10 stata code.do_. 

##Replication of the decomposition of monetary policy shocks into central bank information effect and true monetary policy shocks
_decompdata.xlsx_ contains the relevant data for the decomposition. Monetary policy shocks obtained from Lakdawala et al. (2021) and two-day changes in S&P500 obtained from Bloomberg
_signrestr_median.m_ was obtained from [Marek Jarocinski's website](https://marekjarocinski.github.io/) and implements the median rotation described in Jarocinki(2022)
_decomposition_main_code.m_ contains the code to carry out the decomposition

Instruction:
Run _decomposition_main_code.m_.
