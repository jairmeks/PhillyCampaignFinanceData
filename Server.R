# install.packages("shiny")
# install.packages("rsconnect")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("igraph")

library(shiny)
library(rsconnect)
library(plyr)
library(dplyr)
library(igraph)

# load data
originalcontrib <- read.csv("FinalContributions.csv", fileEncoding = "UTF-8-BOM")
originalcandidates <- read.csv("Campaigns.csv", fileEncoding = "UTF-8-BOM")
committeenames <- read.csv("CommitteeNames.csv", fileEncoding = "UTF-8-BOM")
displaynames <- read.csv("DisplayNames.csv", fileEncoding = "UTF-8-BOM")

# filter out blank donor names
originalcontrib <- originalcontrib[originalcontrib$Donor.Name.Cleaned!='',]

# rename Campaign.Committee.Name.Original column in candidates data frame to FilerName
colnames(committeenames)[which(names(committeenames) == "Campaign.Committee.Name.Original")] <- "FilerName"

# join cleaned committee names to originalcontrib data frame (joining on FilerName)
originalcontrib <- join(originalcontrib, committeenames, match="first")

# add column for election year to contrib data frame
originalcontrib[originalcontrib$Year %in% c(2014,2015),c("Election.Year")] = 2015
originalcontrib[originalcontrib$Year %in% c(2016,2017,2018,2019),c("Election.Year")] = 2019
originalcontrib[originalcontrib$Year %in% c(2020,2021,2022,2023),c("Election.Year")] = 2023

# join candidate information to originalcontrib dataframe (joining on cleaned campaign committee name)
originalcontrib <- join(originalcontrib, originalcandidates, match="first")

# convert to appropriate data types
originalcontrib$Amount <- as.numeric(gsub('[$,]', '', originalcontrib$Amount))
originalcontrib$Sector <- as.character(originalcontrib$Sector)

# pull out donor attributes
donorattributes <- unique(originalcontrib[,c("Donor.Name.Cleaned","Sector","PAC")])

# standardize entity name column names
colnames(donorattributes)[colnames(donorattributes)=="Donor.Name.Cleaned"] <- "EntityName"
colnames(originalcandidates)[colnames(originalcandidates)=="Campaign.Committee.Name.Cleaned"] <- "EntityName"

# pull list of donors and append campaign committees to produce full list of entities
originalentities <- data.frame(EntityName = unique(donorattributes$EntityName))
committeelist <- data.frame(EntityName = unique(committeenames$Campaign.Committee.Name.Cleaned))
originalentities <- unique(rbind(originalentities, committeelist, stringsAsFactors = FALSE))
rm(committeelist)

# sort donor attributes file so that PAC records come first
donorattributes <- donorattributes[order(donorattributes$EntityName, -xtfrm(donorattributes$PAC)),]

# join donor and candidate attributes to entities dataframe
originalentities <- join(originalentities, donorattributes, match="first")
rm(donorattributes)
originalentities <- join(originalentities, originalcandidates, match="first")
rm(originalcandidates)

# join display names to entities dataframe
originalentities <- join(originalentities, displaynames)

# update sectors for campaign committees
originalentities[!is.na(originalentities$Elected.Office),c("Sector")] <- "Campaign Committees"

# launch server
shinyServer(function(input, output) {
  
  # re-scale scale factors supplied by end user for vertex sizes and zoom level (larger scales->larger nodes)  
  scalefactor = reactive({ input$scalefactor / 100000 })
  candidatescale = reactive({ input$candidatescale / 10})
  zoomfactor = reactive({ -input$zoom / 25})
  
  # substitute Yes and No for Incumbents and Challengers respectively
  incumbentselection = reactive({ 
    gsub("Challengers", "No", gsub("Incumbents", "Yes", input$incumbent))
  })
  
  # reactively render the network graph plot
  output$plot <- renderPlot({
    # reload original data
    contrib <- originalcontrib
    entities <- originalentities
    
    # apply filters
    # only include city council members and candidates (current dataset is already doing this, but in case we want to expand filtering options in future)
    contrib <- contrib[contrib$City.Council %in% c("District","At-Large"),]
    # filter for incumbents and/or challengers based on user selection
    contrib <- contrib[contrib$Incumbent %in% incumbentselection(),]
    # filter for year range selected by user
    contrib <- contrib[contrib$Year %in% input$years,]

    # aggregate donor contribution totals
    donorsums = aggregate(contrib$Amount, by=list(Donor = contrib$Donor.Name.Cleaned), FUN=sum)
    colnames(donorsums)[colnames(donorsums)=="x"] <- "DonorTotal"
    
    # optionally could implement filter by donor total
    # donorsums <- donorsums[donorsums$DonorTotal>30000,]
    
    # aggregate contribution totals for each donor-recipient pair
    sums = aggregate(contrib$Amount, by=list(Donor=contrib$Donor.Name.Cleaned, Recipient=contrib$Campaign.Committee.Name.Cleaned), FUN=sum)
    
    # aggregate recipient contribution totals
    recipsums = aggregate(contrib$Amount, by=list(EntityName = contrib$Campaign.Committee.Name.Cleaned), FUN=sum)
    
    # join donor totals into donor-recipient sums data frame (joining by Donor name)
    sums <- join(sums, donorsums, match="first")
    # filter out any rows with null donor totals
    sums <- sums[!is.na(sums$DonorTotal),]
    
    # filter donor-recipient sums to only include those larger than user-supplied cutoff
    filteredsums <- sums[sums$x >= input$cutoff,]
    
    # filter entities based on filtered sums
    finalentities <- data.frame(EntityName = unique(filteredsums$Donor))
    recipientsfinal <- data.frame(EntityName = unique(filteredsums$Recipient))
    finalentities <- unique(rbind(finalentities, recipientsfinal))
    
    # join entity attributes to final entities dataframe (joining by EntityName)
    finalentities <- join(finalentities, entities, match="first")
    
    # join donor sums and recipient sums to final entities dataframe (joining by EntityName)
    colnames(donorsums)[colnames(donorsums)=="Donor"] <- "EntityName"
    finalentities <- join(finalentities, donorsums, match="first")
    colnames(finalentities)[colnames(finalentities)=="x"] <- "DonorTotal"
    finalentities <- join(finalentities, recipsums, match="first")
    colnames(finalentities)[colnames(finalentities)=="x"] <- "RecipTotal"
    
    # replace null donor or recipient totals in final entities dataframe with 0s
    finalentities$DonorTotal[is.na(finalentities$DonorTotal)] <- 0
    finalentities$RecipTotal[is.na(finalentities$RecipTotal)] <- 0
    # sum donor and recipient totals for a "money total" calculation, used for entity vertex scaling
    finalentities$MoneyTotal <- finalentities$DonorTotal + finalentities$RecipTotal
    
    # determine scale of graph nodes, using user-supplied scalefactor
    finalentities$Scale <- (finalentities$MoneyTotal * scalefactor())
    
    # make sure that donor name comes before recipient name in final donor-recipient sums dataframe
    filteredsums <- filteredsums[,c("Donor","Recipient","x","DonorTotal")]
    
    # create modified scale attribute for candidates
    finalentities$Scale2 <- finalentities$Scale
    finalentities[!is.na(finalentities$Elected.Office),c("Scale2")] <- finalentities[!is.na(finalentities$Elected.Office),c("Scale2")] * candidatescale()
    
    # add candidate labels
    finalentities$Candidate.Label <- ""
    finalentities[!is.na(finalentities$Elected.Office),c("Candidate.Label")] <- as.character(finalentities[!is.na(finalentities$Elected.Office),c("EntityName")])
    
    # create vertex type attribute for color coding
    finalentities$color <- 'Light Green' # for individuals
    finalentities$color[finalentities$PAC=="Y"] <- "Orange" # for PACs
    finalentities$color[finalentities$Sector=="Campaign Committees"] <- "SkyBlue2" # for campaign committees
    
    # create graph using donor-recipient sums dataframe and include entity attributes
    g1 <- graph.data.frame(d = filteredsums, vertices = finalentities)
    
    # set layout
    layoutdef <- layout.fruchterman.reingold(g1)
    
    # prepare vertex labels
    labels <- get.vertex.attribute(g1, name="name")
    # substitute display names where they exist
    labels[!is.na(get.vertex.attribute(g1, name="DisplayName"))] <- get.vertex.attribute(g1, name="DisplayName")[!is.na(get.vertex.attribute(g1, name="DisplayName"))]
    # change labels to NA for donors who do not meet donation threshold provided by user
    labels[get.vertex.attribute(g1, name="DonorTotal") < input$labels & is.na(get.vertex.attribute(g1, name="City.Council"))] <- NA
    
    # scale edge widths
    E(g1)$width = get.edge.attribute(g1,name="x")/6000
    # set color scheme
    edge.start <- ends(g1, es=E(g1), names=F)[,1]
    edge.col <- V(g1)$color[edge.start]
    # choose font family
    V(g1)$label.family = "mono"
    # set arrow size
    E(g1)$arrow.size = 0.3
    # scale sizes of vertices
    V(g1)$size = get.vertex.attribute(g1,name="Scale2")
    # use bold format for candidate labels
    V(g1)$label.font <- ifelse(V(g1)$Candidate.Label == "", 2, 2)
    # set label sizes
    V(g1)$label.cex <- ifelse(V(g1)$Candidate.Label == "", 0.9, 1.4)
    # set label color
    V(g1)$label.color = "Dark Blue"
    
    # plot network graph
    plot(g1,
         # set edge colors and style
         edge.color = edge.col,
         # set layout def
         layout=layoutdef,
         # set labels
         vertex.label = labels,
         # scale zoom factor
         margin = zoomfactor()
    )

    # add legend
    legend(1.1, 
           1.1,
           legend = c(paste('City Council Candidate'), 
                      paste('PAC Donor'),
                      paste('Individual Donor')
           ),
           pch=21,
           col="#777777",
           pt.bg = c('SkyBlue2','Orange','Light Green'),
           cex = 1.1,
           bty="n",
           ncol=1)
  }, height=800)
}
)

