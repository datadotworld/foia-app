source("listAgencies.R")

shinyUI(fluidPage(
  theme = "stylesheet.css",
  
  tags$a(href='http://data.world',
              tags$img(src='poweredby_sparkle.png', class="header-logo")),
  
  titlePanel("Predicting Your FOIA Request Success"),
  
  headerPanel(tags$div(class="subheader",
                       "Try out our open source Freedom of Information Act request success predictor based on 9,000+ FOIA requests from 2016 and 2017.")),
  
  sidebarLayout(position = "left",
    sidebarPanel(
      helpText("Get our prediction by simply inputting details about your FOIA request in the form below."),
      
      tags$hr(class="sidebar-break"),
      
      textAreaInput("request_text", label = p("Your request text:"), height = '250px', 
                    placeholder = "Paste your request here (without greeting or salutation works best!)"),
      
      selectizeInput("agency", label = p("Please select your agency. If not listed, select 'Agency not listed'"),
                  choices = listAgencies(), selected = NULL,
                  options = list(
                    placeholder = 'Please select an agency'
                  )),

      actionButton(
        inputId = "submit_loc",
        label = "Get prediction"),
      
      tags$p(class="model-running", "Note: getting your prediction may take a minute or so.")
    ),
    
    mainPanel(
      h3(textOutput("textResult")),
      htmlOutput("cta")
      )
  )
))