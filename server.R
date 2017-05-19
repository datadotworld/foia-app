# server.R file
source("listAgencies.R")

######### BUILDING THE MODEL (run once)
# Import statements
library(caret)

# Read data into app
recent_data <- read.csv("https://query.data.world/s/gnihglprzlg116azb6gjj3jd", header=T, stringsAsFactors = TRUE)
# Create target variable for successful or not
recent_data$successful_bool <- ifelse(recent_data$status == "done", 1, 0)
recent_data$ref_fees <- as.numeric(recent_data$ref_fees)
recent_data$hyperlink <- as.numeric(recent_data$hyperlink)
recent_data$specificity <- as.numeric(recent_data$specificity)
recent_data$ref_foia <- as.numeric(recent_data$ref_foia)
recent_data$email_address <- as.numeric(recent_data$email_address)
recent_data$word_count <- as.numeric(recent_data$word_count)
recent_data$high_success_rate_agency <- as.numeric(recent_data$high_success_rate_agency)

# Break into training and testing sets with 70% in train. We also use seed to make sure the same partitions are made
# each time the code in run.
set.seed(3033)
intrain <- createDataPartition(y = recent_data$successful_bool, p= 0.7, list = FALSE)
training <- recent_data[intrain,]
testing <- recent_data[-intrain,]

# Train model
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(successful_bool ~ ref_fees + hyperlink + specificity + ref_foia + avg_sen_len +
                   email_address + word_count + high_success_rate_agency,
                 data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

########## Text analytics of user input
library(stringr)
library(NLP, warn = FALSE)
library(openNLP, warn = FALSE)

InputTextMining <- function(request_text){
  # Remove non ascii characters
  Encoding(request_text) <- "latin1"
  request_text <- iconv(request_text, "latin1", "ASCII", sub="")

  # Lowercase text
  request_text <- tolower(request_text)
  # Get word count of lowercased text
  word_count <- sapply(gregexpr("[[:alpha:]]+", request_text), function(x) sum(x > 0))
  word_count <- as.numeric(word_count)
  # Sentence count = number of periods, question marks or exclamation points
  sen_count <- str_count(request_text, "\\.") + str_count(request_text,"\\?") + str_count(request_text,"!")
  # If the sentence count is 0, then count the whole phrase as a sentence
  if (sen_count != 0) {
    avg_sen_len <- word_count / sen_count
    avg_sen_len <- as.numeric(avg_sen_len)
  }
  else{
    avg_sen_len <- word_count
  }

  # 1 or 0 for boolean presence
  match_fees <- c("fees")
  ref_fees <- ifelse(grepl(paste(match_fees,collapse="|"), request_text),1,0)
  ref_fees <- as.numeric(ref_fees)
  match_foia <- c("foia","freedom of information")
  ref_foia <- ifelse(grepl(paste(match_foia,collapse="|"), request_text),1,0)
  ref_foia <- as.numeric(ref_foia)
  match_hyperlink <- c("((https?):((//)|(\\\\))+[\\w\\d:#@%/;$()~_?\\+-=\\\\.&]*)","(www.(?:[a-z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-f][0-9a-f]))+)")
  hyperlink <- ifelse(grepl(paste(match_hyperlink,collapse="|"), request_text),1,0)
  hyperlink <- as.numeric(hyperlink)
  match_email <- c("([\\w\\-\\.]+@(\\w[\\w\\-]+\\.)+[\\w\\-]+)")
  email_address <- ifelse(grepl(paste(match_email,collapse="|"), request_text),1,0)
  email_address <- as.numeric(email_address)

  tagPOS <-  function(x, ...) {
    s <- as.String(x)
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- NLP::Annotation(1L, "sentence", 1L, nchar(s))
    a2 <- NLP::annotate(s, word_token_annotator, a2)
    a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2)
    a3w <- a3[a3$type == "word"]
    POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
    return (POStags)
  }

  # Function to return specificity score
  SpecificityScore <- function(pos_tags){
    counter <- 0
    last_tag <- ""
    for (tag in pos_tags){
      if (tag=='NN' && last_tag!='NN'){
        counter <- counter + 1
      }
      if (tag!="." | tag!="," | tag!="?" | tag!="!"){
        last_tag <- tag
      }
    }
    return (counter)
  }

  pos_tags <-  tagPOS(request_text)
  specificity <- SpecificityScore(pos_tags)
  specificity <- as.numeric(specificity)

  return (c(word_count, avg_sen_len, ref_fees, ref_foia, hyperlink, email_address, specificity))
}




shinyServer(function(input, output) {

  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {

      high_success_rate_agency <- ifelse(input$agency == "Agency not listed", 0, 1)

      validate(
        need(input$request_text != "", "Please fill out the fields to the left.")
      )

      textmined <- InputTextMining(input$request_text)

      # If word count is smaller than 10, ask for longer request text.
      if (textmined[1] < 10){
        paste("ERROR: 90% of successful requests are longer than 10 words, please expand your request.")
      }
      else {

        output$textResult <- renderText({
            user_instance <- data.frame("ref_fees" = as.numeric(textmined[3]),
                                        "hyperlink" = as.numeric(textmined[5]),
                                        "specificity" = as.numeric(textmined[7]),
                                        "ref_foia" = as.numeric(textmined[4]),
                                        "avg_sen_len" = as.numeric(textmined[2]),
                                        "email_address" = as.numeric(textmined[6]),
                                        "word_count" = as.integer(textmined[1]),
                                        "high_success_rate_agency" = as.numeric(high_success_rate_agency))

            user_pred <- predict(knn_fit, newdata=user_instance)

            formated_prediction <- round(user_pred * 100, 0)

            paste("Your FOIA request has a", formated_prediction, "% chance of success")
          })


        output$cta <- renderUI({
          ctaText <- '</br>
          This prediction uses the following attributes of your request:
          <ul>'
          wordcountText <- paste('<li><b>Word Count:</b>',as.character(textmined[1]),"</li>")
          avgsenlenText <- paste('<li><b>Average Sentence Length:</b>', as.character(round(textmined[2],2)),"</li>")
          specificityText <- paste('<li><b>Specificity (measured by presence of nouns):</b>', as.character(textmined[7]),"</li>")
          reffeesText <- paste('<li><b>References Fees:</b>', as.character(ifelse(textmined[3] == 0, "False","True")),"</li>")
          reffoiaText <- paste('<li><b>References FOIA:</b>', as.character(ifelse(textmined[4] == 0, "False","True")),"</li>")
          hyperlinkText <- paste('<li><b>Includes Hyperlink:</b>', as.character(ifelse(textmined[5] == 0, "False","True")),"</li>")
          emailText <- paste('<li><b>Includes Email:</b>', as.character(ifelse(textmined[6] == 0, "False","True")),"</li>")
          successagencyText <- paste('<li><b>Agency Requested has > 50% success rate:</b>', as.character(ifelse(high_success_rate_agency == 0, "False","True")),"</li></ul>")

          HTML(paste(ctaText,wordcountText,avgsenlenText,specificityText,reffeesText,reffoiaText,hyperlinkText,emailText,successagencyText))
        })

      }

    }
  )

})
