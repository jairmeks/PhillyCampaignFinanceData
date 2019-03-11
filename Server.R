# install.packages("shiny")
# install.packages("rsconnect")
# install.packages("plyr")
# install.packages("igraphy")


library(shiny)
library(rsconnect)
library(plyr)
library(igraph)

shinyServer(function(input, output) {
  # load data
  originalcontrib <- read.csv("FinalContributions.csv")
  originalcandidates <- read.csv("CandidateStatus.csv")
  displaynames <- read.csv("DisplayNames.csv")
  
  # filter out blank donor names
  originalcontrib <- originalcontrib[originalcontrib$Donor.Name.Cleaned!='',]

  # deduplicate committee names
  comm_dedup <- unique(originalcandidates[,c("Contribution.To","Campaign.Committee.Name.Cleaned")])
  
  # rename column in candidates data frame
  colnames(originalcandidates)[which(names(originalcandidates) == "Campaign.Committee.Name.Original")] <- "FilerName"
  
  # add candidate info to contrib data frame
  originalcontrib <- join(originalcontrib, originalcandidates, match="first")
        
  # pull out donor attributes
  donorattributes <- originalcontrib[,c("Donor.Name.Cleaned","Sector")]
  
  # narrow down columns in candidate attributes
  originalcandidates <- originalcandidates[,c("Contribution.To","ElectedOffice","City.Council","Incumbent","Political.Party","Campaign.Committee.Name.Cleaned")]
  
  # convert amounts to numeric
  originalcontrib$Amount <- as.numeric(gsub('[$,]', '', originalcontrib$Amount))
  
  # pull list of donors and append campaign committees, deduplicate
  originalentities <- data.frame(unique(originalcontrib$Donor.Name.Cleaned))
  colnames(originalentities) <- "EntityName"
  committeelist <- data.frame(EntityName = comm_dedup$Campaign.Committee.Name.Cleaned)
  originalentities <- unique(rbind(originalentities, committeelist, stringsAsFactors = FALSE))
  
  # update column names in donor attributes and candidates tables
  colnames(donorattributes)[colnames(donorattributes)=="Donor.Name.Cleaned"] <- "EntityName"
  colnames(originalcandidates)[colnames(originalcandidates)=="Campaign.Committee.Name.Cleaned"] <- "EntityName"
  
  # join donor and campaign attributes
  originalentities <- join(originalentities, donorattributes, match="first")
  rm(donorattributes)
  originalentities <- join(originalentities, originalcandidates, match="first")
  
  # add display names
  originalentities <- join(originalentities, displaynames)
  
  # update sectors for campaign committees
  originalentities[!is.na(originalentities$ElectedOffice),c("Sector")] <- "Politics / Campaign Committees"

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
    candidates <- originalcandidates
    entities <- originalentities
    
    # FILTERS
    #contrib <- contrib[!(contrib$Contribution.To %in% c("Joe Cox", "Larry King", "Bill Heeney","Luigi Borda","Irina Goldstein","Melissa Robbins","Beth Finn","Adrian Rivera Reyes","Erika Almiron","Matt Wolfe","Tonya Bah")),]
    contrib <- contrib[contrib$City.Council %in% c("District","At-Large"),]
    contrib <- contrib[contrib$Incumbent %in% incumbentselection(),]
    #contrib <- contrib[contrib$Donor.Name.Cleaned %in% c("Allan Domb","Joseph S Zuritsky", "Robert A Zuritsky"),]
    #contrib <- contrib[contrib$Contribution.To %in% c("Darrell Clarke"),]
    #contrib <- contrib[contrib$Sector %in% c("Real Estate & Building Industry"),]

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
    colnames(candidates)[colnames(candidates)=="EntityName"] <- "Recipient"
    filteredsums <- join(filteredsums, candidates, match="first")
    
    # make sure that donor name is coming before recipient name
    filteredsums <- filteredsums[,c("Donor","Recipient","x","DonorTotal","Contribution.To","ElectedOffice","City.Council","Incumbent","Political.Party")]
    
    # create modified scale attribute for candidates
    finalentities$Scale2 <- finalentities$Scale
    finalentities[!is.na(finalentities$ElectedOffice),c("Scale2")] <- finalentities[!is.na(finalentities$ElectedOffice),c("Scale2")] * candidatescale()
    
    # add labels for candidates only
    finalentities$Candidate.Label <- ""
    finalentities[!is.na(finalentities$ElectedOffice),c("Candidate.Label")] <- as.character(finalentities[!is.na(finalentities$ElectedOffice),c("EntityName")])
    
    # create graph and include entity attributes
    g1 <- graph.data.frame(d = filteredsums, vertices = finalentities)
    
    # set layout
    layoutdef <- layout.fruchterman.reingold(g1)
    
    # color code by sector
    sector_vertex_colors = get.vertex.attribute(g1,"Sector")
    colors = c('Purple', 'Light Green', 'Orange', 'SkyBlue2', 'Red', 'Blue', 'Yellow', 'Grey')
    sector_vertex_colors[sector_vertex_colors == "Charter / Education Privatization"] = colors[1]
    sector_vertex_colors[sector_vertex_colors == "Food, Beverage & Tobacco"] = colors[2]
    sector_vertex_colors[sector_vertex_colors == "Law"] = colors[3]
    sector_vertex_colors[sector_vertex_colors == "Politics / Campaign Committees"] = colors[4] 
    sector_vertex_colors[sector_vertex_colors == "Real Estate & Building Industry"] = colors[5]
    sector_vertex_colors[sector_vertex_colors == "Unions"] = colors[6]
    sector_vertex_colors[sector_vertex_colors == "Other"] = colors[7]
    sector_vertex_colors[sector_vertex_colors == "Unknown"] = colors[8]
    sector_vertex_colors[is.na(sector_vertex_colors)] = colors[8]
    sector_vertex_colors[sector_vertex_colors == ''] = colors[8]
    
    # prepare labels
    labels <- get.vertex.attribute(g1, name="name")
    # substitute display names where they exist
    labels[!is.na(get.vertex.attribute(g1, name="DisplayName"))] <- get.vertex.attribute(g1, name="DisplayName")[!is.na(get.vertex.attribute(g1, name="DisplayName"))]
    # change labels to NA for donors who do not meet donation threshold provided by user
    labels[get.vertex.attribute(g1, name="DonorTotal") < input$labels & is.na(get.vertex.attribute(g1, name="City.Council"))] <- NA
    
    # scale edge widths
    E(g1)$width = get.edge.attribute(g1,name="x")/5000
    # set color scheme
    V(g1)$color = sector_vertex_colors
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
    
    legend(1.3, 
           1.3,
           legend = c(paste('Charter / Education Privatization'), 
                      paste('Food / Beverage / Tobacco'),
                      paste('Law'),
                      paste('Politics / Campaign Committee'),
                      paste('Real Estate / Building Industry'),
                      paste('Union'),
                      paste('Other'),
                      paste('Unknown')
           ),
           fill = c('Purple', 'Light Green', 'Orange', 'SkyBlue2', 'Red', 'Blue', 'Yellow', 'Grey'),
           cex = .9)
  }, height=800)
}
)
#rsconnect::deployapp('path/to/your/app')