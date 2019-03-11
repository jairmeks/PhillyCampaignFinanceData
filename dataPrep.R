# install.packages("plyr")
# install.packages("dplyr")
# nstall.packages("readr")
# install.packages("data.table")
# install.packages("stringdist")


require(plyr)
require(dplyr)
require(readr)
require(data.table)
require(stringdist)

# change to TRUE if you are adding new data to an existing dataset
ADDITION <- FALSE
# change to filepath of existing dataset if needed
DATAFILE = "CleanedContributions.csv"

# initialize data frames for each year as NA
data2018 <- NA
data2017 <- NA
data2016 <- NA
data2015 <- NA
data2014 <- NA
data2013 <- NA
data2012 <- NA
data2011 <- NA
data2010 <- NA
data2009 <- NA
data2008 <- NA
data2007 <- NA

# data import and aggregation (currently only importing 2014-2018)
data2018 <- read.delim('Explorer.Transactions.2018.YTD.txt', header=TRUE, sep="\t", dec=".")
colnames(data2018)[which(names(data2018) == "ï..FilerName")] <- "FilerName"
data2017 <- read.delim('Explorer.Transactions.2017.YTD.txt', header=TRUE, sep="\t", dec=".")
data2016 <- read.delim('Explorer.Transactions.2016.YTD.txt', header=TRUE, sep="\t", dec=".")
data2015 <- read.delim('Explorer.Transactions.2015.YTD.txt', header=TRUE, sep="\t", dec=".")
data2014 <- read.delim('Explorer.Transactions.2014.YTD.txt', header=TRUE, sep="\t", dec=".")
#data2013 <- read.delim('Explorer.Transactions.2013.YTD.txt', header=TRUE, sep="\t", dec=".")
#data2012 <- read.delim('Explorer.Transactions.2012.YTD.txt', header=TRUE, sep="\t", dec=".")
#data2011 <- read.delim('Explorer.Transactions.2011.txt', header=TRUE, sep="\t", dec=".")
#data2010 <- read.delim('Explorer.Transactions.2010.txt', header=TRUE, sep="\t", dec=".")
#data2009 <- read.delim('Explorer.Transactions.2009.YTD.txt', header=TRUE, sep="\t", dec=".")
#data2008 <- read.delim('Explorer.Transactions.2008ytd.txt', header=TRUE, sep="\t", dec=".")
#data2007 <- read.delim('Explorer.Transactions.2007ytd.txt', header=TRUE, sep="\t", dec=".")
alldata <- rbind(data2018, data2017, data2016, data2015, data2014, data2013, data2012, data2011, data2010, data2009, data2008, data2007)
rm(data2018, data2017, data2016, data2015, data2014, data2013, data2012, data2011, data2010, data2009, data2008, data2007)

# format date fields as dates
alldata$Date <- as.Date(alldata$Date, "%m/%d/%Y")
alldata$SubDate <- as.Date(alldata$SubDate, "%m/%d/%Y")

# filter full data for contribution records
contrib <- alldata[alldata$DocType %in% c("CFR - Schedule I - Part A - Contributions Received From Political Committees ($50.01 to $250.00)", 
                                          "CFR - Schedule I - Part B - All Other Contributions ($50.01 - $250.00)", 
                                          "CFR - Schedule I - Part C - Contributions Received From Political Committees (Over $250.00)", 
                                          "CFR - Schedule I - Part D - All Other Contributions (Over $250.00)", 
                                          "CFR - Schedule II - Part F - In-Kind Contributions Received (Value of $50.01 to $250.00)", 
                                          "CFR - Schedule II - Part G - In-Kind Contributions Received (Value Over $250.00)"),]

# filter full data for expenditure records
expend <- alldata[alldata$DocType=="CFR - Schedule III - Statement of Expenditures",]

rm(alldata)

# filter out contributions with NULL amounts
contrib <- contrib[!is.na(contrib$Amount),]
# filter out reimbursements
contrib <- contrib[!(contrib$Description %in% c("REIMBURSEMENT")),]

# convert attributes to factors
contrib$DocType = as.factor(contrib$DocType)
contrib$Cycle = as.factor(contrib$Cycle)

# read in candidate status data
candidates <- read.csv("CandidateStatus.csv")

# join candidate names and attributes to contrib table
colnames(candidates)[which(names(candidates) == "Campaign.Committee.Name.Original")] <- "FilerName"
contrib <- join(contrib, candidates, type="left", match="first")

# filter for just candidates currently on City Council or running for City Council (both District and At-Large seats)
contrib <- contrib[contrib$City.Council %in% c("District", "At-Large"),]

# create surrogate key for contribution records
contrib$id = paste(contrib$FilerName, contrib$Date, contrib$EntityName, contrib$Amount, sep = ";")

# prepare cleaned donor name and ID columns
contrib$Donor.Name.Cleaned = contrib$EntityName

# sort contribution records
contrib <- contrib[order(contrib$EntityName, contrib$Date, contrib$FilerName, contrib$Amount),]

# check for matching ids with same SubDate, and append letters to id's for these records to distinguish them
# iterate five times to check for up to 4 donations of same amount on same day by same donor to same candidate
contrib[duplicated(contrib[,c('SubDate','id')]),]$id <- paste(contrib[duplicated(contrib[,c('SubDate','id')]),]$id,"b", sep = ";")
contrib[duplicated(contrib[,c('SubDate','id')]),]$id <- paste(contrib[duplicated(contrib[,c('SubDate','id')]),]$id,"c", sep = ";")
contrib[duplicated(contrib[,c('SubDate','id')]),]$id <- paste(contrib[duplicated(contrib[,c('SubDate','id')]),]$id,"d", sep = ";")
contrib[duplicated(contrib[,c('SubDate','id')]),]$id <- paste(contrib[duplicated(contrib[,c('SubDate','id')]),]$id,"e", sep = ";")
contrib[duplicated(contrib[,c('SubDate','id')]),]$id <- paste(contrib[duplicated(contrib[,c('SubDate','id')]),]$id,"f", sep = ";")

# deduplicate contribution records
contrib <- contrib %>% distinct(id, .keep_all = TRUE)

# add columns for sector, In-Kind, PAC, and id2 (id2 used for second round of deduplication after completing fuzzy matching process)
contrib$Sector <- NA
contrib$In.Kind <- "N"
contrib$PAC <- "N"
contrib$id2 <- contrib$id

# add column with today's date for DataAddedDate
contrib$DataAddedDate <- as.Date(Sys.Date(), "%m/%d/%Y")

# mark in-kind contributions
contrib$In.Kind[contrib$DocType %in% c("CFR - Schedule II - Part F - In-Kind Contributions Received (Value of $50.01 to $250.00)", 
                                       "CFR - Schedule II - Part G - In-Kind Contributions Received (Value Over $250.00)")] = "Y"

# mark contributions by PACs
contrib$PAC[contrib$DocType %in% c("CFR - Schedule I - Part C - Contributions Received From Political Committees (Over $250.00)",
                                  "CFR - Schedule I - Part A - Contributions Received From Political Committees ($50.01 to $250.00)")] = "Y"

# clean/normalize addresses
contrib$EntityZip <- substr(contrib$EntityZip, 1, 5)
contrib$EntityAddressLine1 <- gsub(" Street$", " St", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine1 <- gsub(" Avenue$", " Ave", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine1 <- gsub(" Road$", " Rd", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine1 <- gsub(" Boulevard$", " Blvd", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine1 <- gsub(" Lane$", " Ln", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine1 <- gsub(" Court$", " Ct", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine1 <- gsub(" Highway$", " Hwy", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine1 <- gsub(" Suite$", " Ste", contrib$EntityAddressLine1, ignore.case=T)
contrib$EntityAddressLine2 <- gsub(" Suite$", " Ste", contrib$EntityAddressLine2, ignore.case=T)
contrib$EntityCity <- gsub("Phila.$", "Philadelphia", contrib$EntityCity, ignore.case=T)
contrib$EntityCity <- gsub("phila$", "Philadelphia", contrib$EntityCity, ignore.case=T)
contrib$Address <- paste(contrib$EntityAddressLine1,contrib$EntityAddressLine2,contrib$EntityCity,contrib$EntityState,contrib$EntityZip, sep=", ")
for (j in 1:6){
  contrib$Address <- gsub(", ,", ",", contrib$Address)
}
contrib$Address <- gsub('\\.', "", contrib$Address)

# create column with concatenated donor name, address, occupation, and employer
contrib$matchcriteria <- paste(contrib$EntityName,contrib$Address,contrib$EmployerName)

# convert to characters
contrib$Donor.Name.Cleaned <- lapply(contrib$Donor.Name.Cleaned, as.character)
contrib$Sector <- lapply(contrib$Sector, as.character)

# reduce dataframe to necessary columns
contrib <- contrib[c("EntityName", "Donor.Name.Cleaned", "Address", "EntityAddressLine1", "EntityAddressLine2", "EntityCity", "EntityState", "EntityZip",
                     "EmployerName", "Occupation", "Sector", "EmployerAddressLine1", "EmployerAddressLine2", "EmployerCity", "EmployerState", "EmployerZip",
                      "Amount", "Date", "Description", "FilerName", "In.Kind", "PAC", "id", "DataAddedDate", "Year", "Cycle", "DocType", "SubDate", "matchcriteria")]

# load already cleaned data if you are adding new data to an already prepared dataset
if (ADDITION) {
  cleanedcontrib <- read.csv(DATAFILE, header=TRUE)
  
  # convert currencies to numbers
  cleanedcontrib$Amount <- as.numeric(gsub('[$,]', '', cleanedcontrib$Amount))
  
  # convert factors to characters
  cleanedcontrib %>% mutate_if(is.factor, as.character) -> cleanedcontrib
  
  # convert to dates
  cleanedcontrib$Date <- as.Date(cleanedcontrib$Date, "%m/%d/%Y")
  cleanedcontrib$SubDate <- as.Date(cleanedcontrib$SubDate, "%m/%d/%Y")
  cleanedcontrib$DataAddedDate <- as.Date(cleanedcontrib$DataAddedDate, "%m/%d/%Y")  
} else {
  # or start from scratch with first row of new data
  cleanedcontrib <- contrib[1,]
}

# initialize result dataframe for potential donor closestmatch
matches <- matrix(c("value","match"),nrow=1,ncol=2)

# loop through each row in original data frame
for (i in 1:nrow(contrib)){
  print(i)
  # if name or employer are NULL, then use Jaro-Winkler distance to match based on just name, with lower max distance threshold than in following case
  if (is.na(contrib$EntityAddressLine1) || is.na(contrib$EmployerName)) {
    closestmatch <- amatch(contrib$Donor.Name.Cleaned[i], cleanedcontrib$Donor.Name.Cleaned, method="jw", p=0.1, maxDist=0.03)
  } else {
    # otherwise find closest match in other rows using Jaro-Winkler distance on matchcriteria field, with slightly higher max distance threshold
    closestmatch <- amatch(contrib$matchcriteria[i], cleanedcontrib$matchcriteria, method="jw", p=0.1, maxDist=0.09)
  }
  # if at least one near match was found
  if (!is.na(closestmatch)){
    # if the donor name for the match is not an exact match with the donor name being compared
    if (cleanedcontrib$EntityName[closestmatch] != contrib$EntityName[i]){
      # store match details in result data frame
      matches <- rbind(matches, c(contrib$matchcriteria[i], cleanedcontrib$matchcriteria[closestmatch]))
    }
    # update the cleaned donor name for the new record and add sectors from matching record
    contrib$Donor.Name.Cleaned[i] = toString(cleanedcontrib$Donor.Name.Cleaned[closestmatch])
    contrib$Sector[i] = toString(cleanedcontrib$Sector[closestmatch])
  }
  cleanedcontrib <- rbind(cleanedcontrib, contrib[i,])
}

# create new, updated surrogate key for contribution records
cleanedcontrib$id2 = paste(cleanedcontrib$FilerName, cleanedcontrib$Date, cleanedcontrib$Donor.Name.Cleaned, cleanedcontrib$Amount, sep = ";")

# check for matching new ids with same SubDate, and append letters to new id's for these records to distinguish them
# iterate 5 times to check for up to 5 donations of same amount on same day by same donor to same candidate
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2,"b", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2,"c", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2,"d", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2,"e", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','id2')]),]$id2,"f", sep = ";")

# deduplicate contribution records
cleanedcontrib <- cleanedcontrib %>% distinct(id2, .keep_all = TRUE)

# write cleaned contributions data file
fwrite(cleanedcontrib, "CleanedContributions.csv")
# write matches file for verification
fwrite(as.data.frame(matches), "matches.csv")
