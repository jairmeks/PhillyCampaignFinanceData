# install.packages("shiny")
# install.packages("rsconnect")
# install.packages("plyr")
# install.packages("igraphy")


library(shiny)
library(rsconnect)
library(plyr)
library(igraph)

shinyUI(pageWithSidebar(
    headerPanel("Network Graph of Contributions to Philadelphia City Council Members and Candidates 2014-2018"),
    
    sidebarPanel(
      sliderInput("cutoff",
                  label="Min. amount of total donations for each donor - recipient link displayed",
                  min = 1000,
                  max = 100000,
                  value = 20000,
                  width = '80%'
      ),
      sliderInput("scalefactor",
                  label="Size scale for all nodes",
                  min = 1,
                  max = 10,
                  value = 4,
                  width = '80%'
      ),
      sliderInput("candidatescale",
                  label="Relative size of canidate nodes to donor nodes",
                  min = 1,
                  max = 10,
                  value = 5,
                  width = '80%'
      ),
      checkboxGroupInput("incumbent",
                         label = "Incumbent Filter",
                         choices = c("Incumbents","Challengers"),
                         selected = c("Incumbents"),
                         inline = TRUE,
                         width = '80%'
      ),
      sliderInput("zoom",
                  label="Zoom Level",
                  min = 1,
                  max = 30,
                  value = 2,
                  width = '80%'
      ),
      sliderInput("labels",
                   label="Include labels only for donors who have given this amount or more",
                   min = 1000,
                   max = 250000,
                   value = 10000,
                   width = '80%'
        
      )
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.OK = true",
                       plotOutput('plot')
      )
    )
  )
)