# install.packages('shiny')
# install.packages('rsconnect')
# install.packages('plyr')
# install.packages('igraph')

library(shiny)
library(rsconnect)
library(plyr)
library(igraph)

shinyUI(pageWithSidebar(
    # title
    headerPanel('Network Graph of Contributions to Phila. City Council Members and Candidates 2014-2018'),
    
    # siderbar panel containing sliders and filters for user input
    sidebarPanel(
      sliderInput('cutoff',
                  label='Min. Amount of Total Contributions for Each Donor-Recipient Connection Displayed',
                  min = 1000,
                  max = 100000,
                  value = 20000,
                  width = '100%'
      ),
      sliderInput('scalefactor',
                  label='Size Scale for All Nodes',
                  min = 1,
                  max = 10,
                  value = 4,
                  width = '100%'
      ),
      sliderInput('candidatescale',
                  label='Relative Size of Candidate Nodes to Donor Nodes',
                  min = 1,
                  max = 10,
                  value = 5,
                  width = '100%'
      ),
      checkboxGroupInput('incumbent',
                         label = 'Incumbent Filter',
                         choices = c('Incumbents','Challengers'),
                         selected = c('Incumbents'),
                         inline = TRUE,
                         width = '100%'
      ),
      sliderInput('zoom',
                  label='Zoom Level',
                  min = 1,
                  max = 30,
                  value = 2,
                  width = '100%'
      ),
      sliderInput('labels',
                   label='Include Labels Only for Donors Who Have Given Total of at Least This Amount',
                   min = 1000,
                   max = 250000,
                   value = 10000,
                   width = '100%'
      ),
      checkboxGroupInput('years',
                     label = 'Years',
                     choices = c(2014, 2015, 2016, 2017, 2018),
                     selected = c(2014, 2015, 2016, 2017, 2018),
                     inline = TRUE,
                     width = '100%'
                     )
    ),

    # main panel contains the contributions network graph plot    
    mainPanel(
      h4('Wider connections between nodes represent larger sums of contributions from each donor to each candidate.'),
      conditionalPanel(condition = 'input.OK = true',
                       plotOutput('plot')
      )
    )
  )
)