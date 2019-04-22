require(plyr)
require(dplyr)
require(readr)
require(data.table)
require(stringdist)

# data import and aggregation
alldata <- read.csv('NewData.csv', header = TRUE)

# filter for current cycle
# alldata <- alldata[alldata$Cycle==1,]

# add any other missing data if needed
# data2018 <- read.delim('Explorer.Transactions.2018.YTD.txt', header=TRUE, sep="\t", dec=".")
# colnames(data2018)[which(names(data2018) == "ï..FilerName")] <- "FilerName"
# data2017 <- read.delim('Explorer.Transactions.2017.YTD.txt', header=TRUE, sep="\t", dec=".")
# colnames(data2017)[which(names(data2017) == "ï..FilerName")] <- "FilerName"
# data2016 <- read.delim('Explorer.Transactions.2016.YTD.txt', header=TRUE, sep="\t", dec=".")
# colnames(data2016)[which(names(data2016) == "ï..FilerName")] <- "FilerName"
# data2015 <- read.delim('Explorer.Transactions.2015.YTD.txt', header=TRUE, sep="\t", dec=".")
# data2014 <- read.delim('Explorer.Transactions.2014.YTD.txt', header=TRUE, sep="\t", dec=".")
# otherdata <- rbind(data2018, data2017, data2016, data2015, data2014)

# filter for specific candidates that need to be added if needed
# otherdata <- otherdata[otherdata$FilerName %in% c(
# ),]

# rm(data2014, data2015, data2016, data2017, data2018)

# combine data
# alldata <- rbind(alldata, otherdata)

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

# filter out contributions with NULL amounts
contrib <- contrib[!is.na(contrib$Amount),]
# filter out reimbursements
contrib <- contrib[!(contrib$Description %in% c("REIMBURSEMENT")),]

# format DocType and Cycle as factors
contrib$DocType = as.factor(contrib$DocType)
contrib$Cycle = as.factor(contrib$Cycle)

# read in candidate status and committee names data
candidates <- read.csv("Campaigns.csv", fileEncoding = "UTF-8-BOM")
committees <- read.csv("CommitteeNames.csv", fileEncoding = "UTF-8-BOM")

# add cleaned committee names to contrib data frame
colnames(committees)[which(names(committees) == "Campaign.Committee.Name.Original")] <- "FilerName"
contrib <- join(contrib, committees, type="left", match="first")

# sort candidates file before joining with contributions
candidates <- candidates[order(-candidates$Election.Year),]

# add column for election year to contrib data frame
contrib[contrib$Year %in% c(2014,2015),c("Election.Year")] = 2015
contrib[contrib$Year %in% c(2016,2017,2018,2019),c("Election.Year")] = 2019
contrib[contrib$Year %in% c(2020,2021,2022,2023),c("Election.Year")] = 2023
contrib <- join(contrib, candidates, match="first")

# filter to only include filers that are included in candidates file
contrib <- contrib[!is.na(contrib$Candidate),]

# add needed columns to dataframe
contrib$Sector.of.Employment..grouped. <- NA
contrib$Sector.of.Employment <- NA
contrib$BuildingRealEstateFlag <- NA
contrib$CharterPrivateSchoolFlag <- NA
contrib$FoodBeverageTobaccoFlag <- NA
contrib$LawFlag <- NA
contrib$CorporateFlag <- NA
contrib$LobbyingFlag <- NA
contrib$Notes <- NA
contrib$In.Kind. <- NA
contrib$Notes.2 <- NA
contrib$Notes.3 <- NA
contrib$Notes.4 <- NA
contrib$Notes.5 <- NA
contrib$StateLevelRace <- "Not State-Level"
contrib$PAC <= "N"
# add column with today's date for DataAddedDate
contrib$DataAddedDate <- as.Date(Sys.Date(), "%m/%d/%Y")

# create surrogate key for contribution records
contrib$ID = paste(contrib$Candidate, contrib$Date, contrib$EntityName, contrib$Amount, sep = ";")
# ID2 used later for second round of deduplication after completing fuzzy matching process
contrib$ID2 <- contrib$ID

# prepare cleaned donor name column
contrib$Donor.Name.Cleaned = contrib$EntityName

# mark in-kind contributions
contrib$In.Kind.[contrib$DocType %in% c("CFR - Schedule II - Part F - In-Kind Contributions Received (Value of $50.01 to $250.00)", 
                                       "CFR - Schedule II - Part G - In-Kind Contributions Received (Value Over $250.00)")] = "Y"

# mark contributions by PACs
contrib$PAC[contrib$DocType %in% c("CFR - Schedule I - Part C - Contributions Received From Political Committees (Over $250.00)",
                                   "CFR - Schedule I - Part A - Contributions Received From Political Committees ($50.01 to $250.00)")] = "Y"

# sort contribution records
contrib <- contrib[order(contrib$EntityName, contrib$Date, contrib$FilerName, contrib$Amount),]

# check for matching IDs with same SubDate, and append letters to IDs for these records to distinguish them
# iterate five times to check for up to 4 donations of same amount on same day by same donor to same candidate
contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID <- paste(contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID,"b", sep = ";")
contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID <- paste(contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID,"c", sep = ";")
contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID <- paste(contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID,"d", sep = ";")
contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID <- paste(contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID,"e", sep = ";")
contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID <- paste(contrib[duplicated(contrib[,c('SubDate','ID')]),]$ID,"f", sep = ";")

# deduplicate contribution records
contrib <- contrib %>% distinct(ID, .keep_all = TRUE)

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

# convert to characters
contrib$Donor.Name.Cleaned <- lapply(contrib$Donor.Name.Cleaned, as.character)
contrib$Sector.of.Employment..grouped. <- lapply(contrib$Sector.of.Employment..grouped., as.character)
contrib$Sector.of.Employment <- lapply(contrib$Sector.of.Employment, as.character)

# reduce dataframe to necessary columns
contrib <- contrib[c("EntityName", "Donor.Name.Cleaned", "Sector.of.Employment..grouped.", "Sector.of.Employment",
                     "BuildingRealEstateFlag", "CharterPrivateSchoolFlag", "FoodBeverageTobaccoFlag", "LawFlag","CorporateFlag",
                     "Amount", "Date", "EmployerName", "Occupation", "Notes", "Address", "Candidate", "In.Kind.",
                     "ID", "Notes.2", "Notes.3", "Notes.4", "Notes.5", "StateLevelRace", "DataAddedDate", "Year", "Cycle", "DocType", "SubDate", "ID2", "LobbyingFlag","PAC")]


# create column with concatenated donor name, address, occupation, and employer to use as matching criteria
contrib$matchcriteria <- paste(contrib$EntityName,contrib$Address,contrib$Occupation,contrib$EmployerName)

# update column names to match existing dataset
colnames(contrib)[colnames(contrib)=="Candidate"] <- "Contribution.To"
colnames(contrib)[colnames(contrib)=="EntityName"] <- "Donor.Name.Original"
colnames(contrib)[colnames(contrib)=="EmployerName"] <- "Employer"

# load already cleaned data
cleanedcontrib <- read.csv("CleanedContributions.csv", header=TRUE)

# convert currencies to numbers
cleanedcontrib$Amount <- as.numeric(gsub('[$,]', '', cleanedcontrib$Amount))

# convert factors to characters
cleanedcontrib %>% mutate_if(is.factor, as.character) -> cleanedcontrib

# convert to dates
cleanedcontrib$Date <- as.Date(cleanedcontrib$Date, "%m/%d/%Y")
cleanedcontrib$SubDate <- as.Date(cleanedcontrib$SubDate, "%m/%d/%Y")
cleanedcontrib$DataAddedDate <- as.Date(cleanedcontrib$DataAddedDate, "%m/%d/%Y")  

# initialize matches dataframe for potential donor matches
matches <- matrix(c("value","match"),nrow=1,ncol=2)

# loop through each row in original data frame
for (i in 1:nrow(contrib)){
  print(i)
  # if name or employer are NULL, then use Jaro-Winkler distance to match based on just name, with lower max distance threshold than in following case
  if (is.na(contrib$Address) || is.na(contrib$Employer)) {
    closestmatch <- amatch(contrib$Donor.Name.Cleaned[i], cleanedcontrib$Donor.Name.Cleaned, method="jw", p=0.1, maxDist=0.04)
  } else {
    # otherwise find closest match in other rows using Jaro-Winkler distance on MatchCriteria field (name+employer+address), with slightly higher max distance threshold
    closestmatch <- amatch(contrib$matchcriteria[i], cleanedcontrib$matchcriteria, method="jw", p=0.1, maxDist=0.12)
  }
  # if at least one near match was found
  if (!is.na(closestmatch)){
    # if the donor name for the match is not an exact match with the donor name being compared
    if (cleanedcontrib$Donor.Name.Original[closestmatch] != contrib$Donor.Name.Original[i]){
      # store match details in result data frame
      matches <- rbind(matches, c(contrib$matchcriteria[i], cleanedcontrib$matchcriteria[closestmatch]))
    }
    # update the cleaned donor name for the new record and add sectors and flags from matching record
    contrib$Donor.Name.Cleaned[i] = toString(cleanedcontrib$Donor.Name.Cleaned[closestmatch])
    contrib$Sector.of.Employment..grouped.[i] = toString(cleanedcontrib$Sector.of.Employment..grouped.[closestmatch])
    contrib$Sector.of.Employment[i] = toString(cleanedcontrib$Sector.of.Employment[closestmatch])
    contrib$BuildingRealEstateFlag[i] = cleanedcontrib$BuildingRealEstateFlag[closestmatch]
    contrib$CharterPrivateSchoolFlag[i] = cleanedcontrib$CharterPrivateSchoolFlag[closestmatch]
    contrib$FoodBeverageTobaccoFlag[i] = cleanedcontrib$FoodBeverageTobaccoFlag[closestmatch]
    contrib$LawFlag[i] = cleanedcontrib$LawFlag[closestmatch]
    contrib$CorporateFlag[i] = cleanedcontrib$CorporateFlag[closestmatch]
    contrib$LobbyingFlag[i] = cleanedcontrib$LobbyingFlag[closestmatch]
  }
  cleanedcontrib <- rbind(cleanedcontrib, contrib[i,])
}

# create new, updated surrogate key for contribution records
cleanedcontrib$ID2 = paste(cleanedcontrib$Contribution.To, cleanedcontrib$Date, cleanedcontrib$Donor.Name.Cleaned, cleanedcontrib$Amount, sep = ";")

# check for matching new IDs (col 30) with same SubDate (col28), and append letters to new id's for these records to distinguish them
# iterate 4 times to check for up to 5 donations of same amount on same day by same donor to same candidate
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2,"b", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2,"c", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2,"d", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2,"e", sep = ";")
cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2 <- paste(cleanedcontrib[duplicated(cleanedcontrib[,c('SubDate','ID2')]),]$ID2,"f", sep = ";")

# deduplicate contribution records
cleanedcontrib <- cleanedcontrib %>% distinct(ID2, .keep_all = TRUE)

# remove 1/1/1900 subDates
cleanedcontrib$SubDate[cleanedcontrib$SubDate==as.Date("1/1/1900", "%m/%d/%Y")] <- NA

# write full cleaned contributions to disk
fwrite(cleanedcontrib, "CleanedContributions.csv")
# write matches file for verification
fwrite(as.data.frame(matches), "matches.csv")
