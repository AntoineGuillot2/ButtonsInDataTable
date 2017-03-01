
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

dashboardPage(dashboardHeader(disable = T),
              dashboardSidebar(disable = T),
              dashboardBody(uiOutput("MainBody")
                            
                            )
              
)
