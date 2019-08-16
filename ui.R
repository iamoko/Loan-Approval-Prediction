library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(skin = 'blue',
    dashboardHeader(titleWidth = 280,title='Loan Approval Prediction App',
                    dropdownMenu(
                      type = 'message',
                      messageItem(from='Amoko', 'Please Review the analysis below', time='18:08'),
                      messageItem(from='Ivan', 'Please Review the analysis below', time='18:08')
                    )),
    dashboardSidebar(width = 280,
                     uiOutput('sides') 
    ),
    dashboardBody(
      uiOutput('body')
    )
  )
) 