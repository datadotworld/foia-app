source("listAgencies.R")

shinyUI(
  fluidPage(
    theme = "stylesheet.css",
    
    tags$a(
      href = 'http://data.world',
      tags$img(src = 'poweredby_sparkle.png', class = "header-logo")
    ),
    
    titlePanel("Predict Your FOIA Request Success"),
    
    headerPanel(
      tags$div(
        class = "subheader",
        "Does your FOIA have a shot? This model is trained on 9,000+ FOIA requests tracked by MuckRock."
      )
    ),
    
    sidebarLayout(
      position = "left",
      sidebarPanel(
        textAreaInput(
          "request_text",
          label = p('Your request text:'),
          height = '250px',
          placeholder = "Paste your request here (without greeting or salutation works best!)"
        ),
        
        selectizeInput(
          "agency",
          label = p("Select the agency, or 'Agency not listed'"),
          choices = listAgencies(),
          selected = NULL,
          options = list(placeholder = 'Please select an agency')
        ),
        
        actionButton(inputId = "submit_loc",
                     label = "Get prediction"),
        
        tags$p(class = "model-running", "Note: getting your prediction may take a minute or so.")
      ),
      
      mainPanel(
        h3(textOutput("textResult")),
        htmlOutput("cta"),
        tags$h3(class = "text", strong("About the model")),
        tags$p(class = "text",
          "Predictions made using a K nearest neighbors classification algorithm with a test classification accuracy rate of 80%.
          Factors include word count, average sentence length, specificity (presence of nouns), references to fees, references to
          FOIA, presence of hyperlinks, presence of email addresses, and success rate of agency."
        ),
        br(),
        tags$p(class = "text",
          "Improve this app and the predictive model! View the ",
          a(href = "https://data.world/rdowns26/foia-analysis", "data on data.world"),
          " and ",
          a(href = "https://github.com/datadotworld/foia-app", "the code on GitHub"),
          "."
        )
      )
    ),
    hr(),
    tags$h3(class = "text", strong("Shouts")),
    tags$p(class = "text",
           "Nicolas Dias, Rashida Kamal, and Laurent Bastien for their CJR article ",
           a(href = "https://www.cjr.org/analysis/foia-request-how-to-study.php", "\"What makes a good FOIA request?\""),
           br(),
           "Jeremy Singer-Vine for his ",
           a(href = "https://github.com/FOIA-data-hackathon/muckrock-file-formats", "analysis of FOIA filetypes"),
           br(),
           "Michael Morisey & ",
           a(href = "https://www.muckrock.com/", "MuckRock"),
           "for building a powerful FOIA platform and API",
           br(),
           a(href = "http://racheldowns.co/", "Rachel Downs "),
           "for the idea and initial build"
    ),
    br(),
    br()
  )
)