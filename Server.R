# install.packages("shiny")
# install.packages("rsconnect")
# install.packages("plyr")
# install.packages("igraphy")

library(shiny)
library(rsconnect)
library(plyr)
library(igraph)

# load data
originalcontrib <- read.csv("FinalContributions.csv", fileEncoding = "UTF-8-BOM")
originalcandidates <- read.csv("Campaigns.csv", fileEncoding = "UTF-8-BOM")
committeenames <- read.csv("CommitteeNames.csv", fileEncoding = "UTF-8-BOM")
displaynames <- read.csv("DisplayNames.csv", fileEncoding = "UTF-8-BOM")

# filter out blank donor names
originalcontrib <- originalcontrib[originalcontrib$Donor.Name.Cleaned!='',]

# rename column in candidates data frame
colnames(committeenames)[which(names(committeenames) == "Campaign.Committee.Name.Original")] <- "FilerName"

# add cleaned committee names to contrib data frame
originalcontrib <- join(originalcontrib, committeenames, match="first")

# sort candidates file before joining with contributions
originalcandidates <- originalcandidates[order(-originalcandidates$Election.Year),]

# add column for election year to contrib data frame
originalcontrib[originalcontrib$Year %in% c(2014,2015),c("Election.Year")] = 2015
originalcontrib[originalcontrib$Year %in% c(2016,2017,2018,2019),c("Election.Year")] = 2019
originalcontrib[originalcontrib$Year %in% c(2020,2021,2022,2023),c("Election.Year")] = 2023
originalcontrib <- join(originalcontrib, originalcandidates, match="first")

# convert to appropriate data types
originalcontrib$Amount <- as.numeric(gsub('[$,]', '', originalcontrib$Amount))
originalcontrib$Sector <- as.character(originalcontrib$Sector)

# pull out donor attributes
donorattributes <- unique(originalcontrib[,c("Donor.Name.Cleaned","Sector","PAC")])
# donorattributes = aggregate(originalcontrib$Amount, by=list(Donor.Name.Cleaned = originalcontrib$Donor.Name.Cleaned, Sector = originalcontrib$Sector, PAC = originalcontrib$PAC), FUN=sum)

# pull list of donors and append campaign committees, deduplicate
originalentities <- data.frame(unique(originalcontrib$Donor.Name.Cleaned))
colnames(originalentities) <- "EntityName"
committeelist <- data.frame(EntityName = unique(committeenames$Campaign.Committee.Name.Cleaned))
originalentities <- unique(rbind(originalentities, committeelist, stringsAsFactors = FALSE))

# update column names in donor attributes and candidates tables
colnames(donorattributes)[colnames(donorattributes)=="Donor.Name.Cleaned"] <- "EntityName"
colnames(originalcandidates)[colnames(originalcandidates)=="Campaign.Committee.Name.Cleaned"] <- "EntityName"

# sort donor attributes file so that PAC records come first
donorattributes <- donorattributes[order(donorattributes$EntityName, -xtfrm(donorattributes$PAC)),]

# join donor and candidate attributes
originalentities <- join(originalentities, donorattributes, match="first")
rm(donorattributes)
originalentities <- join(originalentities, originalcandidates, match="first")

# add display names
originalentities <- join(originalentities, displaynames)

# update sectors for campaign committees
originalentities[!is.na(originalentities$Elected.Office),c("Sector")] <- "Politics / Campaign Committees"

# launch server
shinyServer(function(input, output) {
  
  # re-scale scale factors for node size (larger scales->larger nodes)  
  scalefactor = reactive({ input$scalefactor / 100000 })
  candidatescale = reactive({ input$candidatescale / 10})
  zoomfactor = reactive({ -input$zoom / 25})
  
  # substitute Yes and No for Incumbents and Challengers respectively
  incumbentselection = reactive({ 
    gsub("Challengers", "No", gsub("Incumbents", "Yes", input$incumbent))
  })
  
  output$plot <- renderPlot({
    # reload original data
    contrib <- originalcontrib
    entities <- originalentities
    
    # FILTERS
    # only include city council members and candidates
    contrib <- contrib[contrib$City.Council %in% c("District","At-Large"),]
    # filter for incumbenets based on user selection
    contrib <- contrib[contrib$Incumbent %in% incumbentselection(),]
    # filter for date range
    contrib <- contrib[contrib$Year %in% input$years,]

    # aggregate donor totals
    donorsums = aggregate(contrib$Amount, by=list(Donor = contrib$Donor.Name.Cleaned), FUN=sum)
    colnames(donorsums)[colnames(donorsums)=="x"] <- "DonorTotal"
    
    # FILTER by donor total
    # donorsums <- donorsums[donorsums$DonorTotal>30000,]
    
    # aggregate by each donor-recipient pair
    sums = aggregate(contrib$Amount, by=list(Donor=contrib$Donor.Name.Cleaned, Recipient=contrib$Campaign.Committee.Name.Cleaned), FUN=sum)
    
    # aggregate recipient totals
    recipsums = aggregate(contrib$Amount, by=list(EntityName = contrib$Campaign.Committee.Name.Cleaned), FUN=sum)
    
    # merge donor sums back to sums data frame
    sums <- join(sums, donorsums, match="first")
    sums <- sums[!is.na(sums$DonorTotal),]
    
    # filter donor-recipient sums to only include those larger than cutoff
    filteredsums <- sums[sums$x >= input$cutoff,]
    
    # filter entities based on filtered sums
    finalentities <- data.frame(EntityName = unique(filteredsums$Donor))
    recipientsfinal <- data.frame(EntityName = unique(filteredsums$Recipient))
    finalentities <- unique(rbind(finalentities, recipientsfinal))
    
    # join attributes, including donor sums
    finalentities <- join(finalentities, entities, match="first")
    colnames(donorsums)[colnames(donorsums)=="Donor"] <- "EntityName"
    finalentities <- join(finalentities, donorsums, match="first")
    colnames(finalentities)[colnames(finalentities)=="x"] <- "DonorTotal"
    finalentities <- join(finalentities, recipsums, match="first")
    colnames(finalentities)[colnames(finalentities)=="x"] <- "RecipTotal"
    finalentities$DonorTotal[is.na(finalentities$DonorTotal)] <- 0
    finalentities$RecipTotal[is.na(finalentities$RecipTotal)] <- 0
    finalentities$MoneyTotal <- finalentities$DonorTotal + finalentities$RecipTotal
    
    # determine scale of graph nodes
    finalentities$Scale <- (finalentities$MoneyTotal * scalefactor())
    
    # join candidate info
    # colnames(candidates)[colnames(candidates)=="EntityName"] <- "Recipient"
    # filteredsums <- join(filteredsums, candidates, match="first")
    
    # make sure that donor name is coming before recipient name
    filteredsums <- filteredsums[,c("Donor","Recipient","x","DonorTotal")]
    
    # create modified scale attribute for candidates
    finalentities$Scale2 <- finalentities$Scale
    finalentities[!is.na(finalentities$Elected.Office),c("Scale2")] <- finalentities[!is.na(finalentities$Elected.Office),c("Scale2")] * candidatescale()
    
    # add labels for candidates only
    finalentities$Candidate.Label <- ""
    finalentities[!is.na(finalentities$Elected.Office),c("Candidate.Label")] <- as.character(finalentities[!is.na(finalentities$Elected.Office),c("EntityName")])
    
    # create graph and include entity attributes
    g1 <- graph.data.frame(d = filteredsums, vertices = finalentities)
    
    # set layout
    layoutdef <- layout.fruchterman.reingold(g1)
    
    # color code by sector
    colors = c('SkyBlue2','Orange','Light Green')
    pac_vertex_colors = get.vertex.attribute(g1, "PAC")
    sector_vertex_colors = get.vertex.attribute(g1, "Sector")
    vertex_colors = pac_vertex_colors
    vertex_colors[pac_vertex_colors == "Y"] = colors[2]
    vertex_colors[sector_vertex_colors == "Politics / Campaign Committees"] = colors[1]
    vertex_colors[vertex_colors == "N"] = colors[3]
    
    # prepare labels
    labels <- get.vertex.attribute(g1, name="name")
    # substitute display names where they exist
    labels[!is.na(get.vertex.attribute(g1, name="DisplayName"))] <- get.vertex.attribute(g1, name="DisplayName")[!is.na(get.vertex.attribute(g1, name="DisplayName"))]
    # change labels to NA for donors who do not meet donation threshold provided by user
    labels[get.vertex.attribute(g1, name="DonorTotal") < input$labels & is.na(get.vertex.attribute(g1, name="City.Council"))] <- NA
    
    # scale edge widths
    E(g1)$width = get.edge.attribute(g1,name="x")/5000
    # set color scheme
    V(g1)$color = vertex_colors
    # choose font family
    V(g1)$label.family = "mono"
    # set arrow size
    E(g1)$arrow.size = 0.05
    # scale sizes of vertices
    V(g1)$size = get.vertex.attribute(g1,name="Scale2")
    # use bold format for candidate labels
    V(g1)$label.font <- ifelse(V(g1)$Candidate.Label == "", 2, 2)
    # set label sizes
    V(g1)$label.cex <- ifelse(V(g1)$Candidate.Label == "", 0.9, 1.4)
    V(g1)$label.color = "Dark Blue"
    
    plot(g1,
         # set layout def
         layout=layoutdef, 
         vertex.label = labels,
         # scale zoom factor
         margin = zoomfactor()
    )
    
    legend(1.1, 
           1.1,
           legend = c(paste('City Council Candidate'), 
                      paste('PAC Donor'),
                      paste('Individual Donor')
           ),
           fill = c('SkyBlue2','Orange','Light Green'),
           cex = 1)
  }, height=800)
}
)
#rsconnect::deployapp('path/to/your/app')