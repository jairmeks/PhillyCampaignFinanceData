# Philly Campaign Finance Data

Repo for aggregating, cleaning, and visualizing Philadelphia Campaign Finance Data

Raw data source is City of Philadelphia Department of Records: ftp://ftp.phila-records.com/

To use the Campaign Finance Network web application, you can navigate to the following URL: https://jairmeks.shinyapps.io/PhillyCampaignFinanceData/

To run the R scripts locally, open the .R files in RStudio. The scripts use the packages listed below, and these packages will need to be installed before running the scripts, if not already installed on the machine. You can use the “install.packages()” lines which are commented out near the top of the scripts to install the necessary packages, by running these lines in the R Studio Console (without the #).
	- shiny, rsconnect, igraph, plyr, dplyr, readr, data.table, stringdist
	
Before running the R scripts, make sure that the working directory is the source file location (can use setwd(“path/to/directory”) or choose this directory using the RStudio GUI). Then highlight the full R script code and click “Run” for the dataPrep.R script, or simply click “Run App” for either the ui.R or Server.R scripts.

For the dataPrep.R script, if it is run as is, it may take a few hours to complete. You may notice one or two error messages saying “Error in `$<-.data.frame`(`*tmp*`, id, value = ";e") : replacement has 1 row, data has 0” but these are expected, as they occur if the algorithm that checks for cases of the same donor donating the same amount to the same candidate on the same day (with the same filing submission date) does not find any more of these cases after iterating a few times searching for matches. The series of numbers that are printed are the numbers of rows that have been processed by the Jaro-Winkler approximate match function. There are over 26,000 rows to be processed in total with the filter settings included in the code.

To see what the output of this file looks like without having to wait for the script to complete, you can view the “CleanedContributions.csv” and “matches.csv” files. The former is the cleaned set of contributions data, while the latter shows all cases of near-matching entity names that were identified by the “amatch” algorithm and then given standardized “Donor.Name.Cleaned” values.

To run the web application locally, you can open either the Server.R or ui.R file in RStudio. Then choose “Run App” within the RStudio interface and the web application will launch (you can optionally choose whether you want it to run in window, in viewer pane, or externally in a browser if you click the dropdown arrow to the right of “Run App”).

The network graph may take a couple seconds to load as the R script initially prepares the data. After the initial load, the graph generally refreshes fairly quickly after the parameters are adjusted, although if both incumbents and challengers are selected and the “Min. Amount of Total Contributions…” slider (the first slider shown) is moved to a low value, then you may notice a little bit more lag time (due to larger data volumes in these cases). Additionally, moving the “Min. Amount of Total Contributions…” slider to the far left may in some cases result in non-useful graph visualizations due to the existence of some far-removed outliers who have not received very many contributions.

	
This repo includes three R scripts:
1. dataPrep.R
	- For aggregating and cleaning all of the .txt files included in the repo, which are Philadelphia campaign finance records from the Department of Records website
	- The output of this script is a cleaned dataset "CleanedContributions.csv"
	- Currently this script is set up to filter for only contributions to campaigns of candidates who are current Philadelphia City Council members or candidates, but this could be changed by editing the filtering in the script and adding any additional candidates of interest to the "Campaigns.csv" and “CommitteeNames.csv” files. Note that you must make sure that the “Campaign.Commitee.Name.Original” names in the "CommitteeNames.csv" file match those from the “FilerName” column of the raw data files exactly, and that the “Campaign.Committee.Name.Cleaned” names match between the “Campaigns.csv” file and “CommiteeNames.csv” files, since these names are used for joining in the R scripts.
	- The dataPrep.R script uses Jaro-Winkler distance to find approximate matches of Donor Name, Employer and Address, in order to standardize donor names in cases of typos or alternative spellings across contribution records. This is an imperfect process and any output data should be checked by a human. The "maxDist" threshold can be altered if the user wants to widen or narrow the range of acceptable string match distance.


2. Server.R
	- This script contains the R Shiny Server scripts for a web application that allows for dynamic generation of a social network graph using the cleaned campaign finance data.
	
3. ui.R
	- This script contains the R Shiny user interface script that goes along with Server.R script.

These scripts were developed using:
- R version 3.5.1
- RStudio version 1.1.456
- Packages: shiny, rsconnect, igraph, plyr, dplyr, readr, data.table, stringdist

Note that the Server.R script is using the “FinalContributions.csv” file, rather than the “CleanedContributions.csv” file (which is output by the dataPrep.R script). Substantial further manual data cleaning of donor names was completed on the “CleanedContributions.csv” file to produce the “FinalContributions.csv” file.