# Philly Campaign Finance Data
Repo for aggregating, cleaning, and visualizing Philadelphia Campaign Finance Data

Data source is ftp://ftp.phila-records.com/

This repo includes three R scripts:
1. dataPrep.R
	- For aggregating and cleaning all of the .txt files included in the repo, which are Philadelphia campaign finance records from the source indicated above
	- The output of this script is a cleaned dataset "CleanedContributions.csv"
	- Currently this script is set up to filter for only contributions to campaigns of candidates who are current Philadelphia City Council members or candidates, but this could be changed by editing the filtering in the script and adding any additional candidates of interest to the "CandidateStatus.csv" file. Note that you must make sure that the campaign committee names in the "CandidateStatus.csv" file match those in the raw data exactly.
	- The dataPrep.R script uses Jaro-Winkler distance to find approximate matches of Donor Name, Employer and Address, in order to standardize donor names in cases of typos or alternative spellings across contribution records. This is an imperfect process and any output data should be checked by a human. The "maxDist" threshold can be altered if the user wants to widen or narrow the range of acceptable string match distance.

2. Server.R
	- This script contains the R Shiny Server scripts for a web application that allows for dynamic generation of a social network graph using the cleaned campaign finance data
	
3. ui.R
	- This script contains the R Shiny user interface script that goes along with Server.R script

These scripts were developed using:
- R version 3.5.1
- RStudio version 1.1.456
- Packages: shiny, rsconnect, igraph, plyr, dplyr, readr, data.table, stringdist

 Note that the Server.R script is using the FinalContributions.csv file, rather than the CleanedContributions.csv file (which is output by the dataPrep.R script). Substantial further manual data cleaning of donor names was completed on the CleanedContributions.csv file to produce the FinalContributions.csv file.