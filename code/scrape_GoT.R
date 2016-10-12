#Source:
# http://datascienceplus.com/goodreads-webscraping-and-text-analysis-with-r-part-1/
library(data.table)
library(dplyr)
library(magrittr)
library(rvest)
library(RSelenium)

setwd("~/Documents/Workshops/hackathon-asoiaf/")

url <- "https://www.goodreads.com/book/show/13496.A_Game_of_Thrones#other_reviews"
book.title <- "A Game of Thrones"
output.filename <- "~/Downloads/GoT.csv"


selServ <- startServer(args = c("-port 5556"))
remDr <- remoteDriver(extraCapabilities = list(marionette = TRUE), port=5556)
remDr$open()
remDr$navigate(url) 

global.df <- data.frame(book=character(),
                        reviewer = character(),
                        rating = character(),
                        review = character(), 
                        stringsAsFactors = F)

reviews <- remDr$findElements("css selector", "#bookReviews .stacked")

reviews.html <- lapply(reviews, function(x){x$getElementAttribute("outerHTML")[[1]]})
reviews.list <- lapply(reviews.html, function(x){read_html(x) %>% html_text()} )
reviews.text <- unlist(reviews.list)

# Removing all characters that are not letters or dash
reviews.text2 <- gsub("[^A-Za-z\\-]|\\.+", " ", reviews.text)
# Removing the end of line characters and extra spaces
reviews.clean <- gsub("\n|[ \t]+", " ", reviews.text2)  

n <- floor(length(reviews)/2)
reviews.df <- data.frame(book = character(n),
                         reviewer = character(n),
                         rating = character(n),
                         review = character(n), 
                         stringsAsFactors = F)

for(j in 1:n){
  reviews.df$book[j] <- book.title
  
  #Isolating the name of the author of the review
  auth.rat.sep <- regexpr(" rated it | marked it | added it ", 
                          reviews.clean[2*j-1]) 
  reviews.df$reviewer[j] <- substr(reviews.clean[2*j-1], 5, auth.rat.sep-1)
  
  #Isolating the rating
  rat.end <- regexpr("· | Shelves| Recommend| review of another edition",
                     reviews.clean[2*j-1])
  if (rat.end==-1){rat.end <- nchar(reviews.clean[2*j-1])}
  reviews.df$rating[j] <- substr(reviews.clean[2*j-1], auth.rat.sep+10, rat.end-1)
  
  #Removing the beginning of each review that was repeated on the html file
  short.str <- substr(reviews.clean[2*j], 1, 50)
  rev.start <- unlist(gregexpr(short.str, reviews.clean[2*j]))[2]
  if (is.na(rev.start)){rev.start <- 1}
  rev.end <- regexpr("\\.+more|Blog", reviews.clean[2*j])
  if (rev.end==-1){rev.end <- nchar(reviews.clean[2*j])}
  reviews.df$review[j] <- substr(reviews.clean[2*j], rev.start, rev.end-1)
}

global.lst <- list(global.df, reviews.df)
global.df <- rbindlist(global.lst)

NextPageButton <- remDr$findElement("css selector", ".next_page")
NextPageButton$clickElement()
Sys.sleep(3)