# install.packages("shiny")
# install.packages("rsconnect")
# install.packages("plyr")
# install.packages("igraphy")


library(shiny)
library(rsconnect)
library(plyr)
library(igraph)

shinyUI(pageWithSidebar(
    headerPanel("Network Graph of Contributions to Phila. City Council Members and Candidates 2014-2018"),
    
    sidebarPanel(
      sliderInput("cutoff",
                  label="Min. amount of total donations for each donor-recipient link displayed",
                  min = 1000,
                  max = 100000,
                  value = 20000,
                  width = '100%'
      ),
      sliderInput("scalefactor",
                  label="Size scale for all nodes",
                  min = 1,
                  max = 10,
                  value = 4,
                  width = '100%'
      ),
      sliderInput("candidatescale",
                  label="Relative size of candidate nodes to donor nodes",
                  min = 1,
                  max = 10,
                  value = 5,
                  width = '100%'
      ),
      checkboxGroupInput("incumbent",
                         label = "Incumbent Filter",
                         choices = c("Incumbents","Challengers"),
                         selected = c("Incumbents"),
                         inline = TRUE,
                         width = '100%'
      ),
      sliderInput("zoom",
                  label="Zoom Level",
                  min = 1,
                  max = 30,
                  value = 2,
                  width = '100%'
      ),
      sliderInput("labels",
                   label="Include labels only for donors who have given at least this amount",
                   min = 1000,
                   max = 250000,
                   value = 10000,
                   width = '100%'
      ),
      checkboxGroupInput("years",
                     label = "Years",
                     choices = c(2014, 2015, 2016, 2017, 2018),
                     selected = c(2014, 2015, 2016, 2017, 2018),
                     inline = TRUE,
                     width = '100%'
                     )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.OK = true",
                       plotOutput('plot')
      )
    )
  )
)