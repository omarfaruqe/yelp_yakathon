#set working directory
setwd("/Users/faruqe/Desktop/yelp")

#Reading Review.json
library(rjson)
file <- 'review.json'
con = file(file, "r")
input <- readLines(con, -1L)
review <- lapply(X=input,fromJSON)

#Reading Business.json
file <- 'business.json'
con = file(file, "r")
input <- readLines(con, -1L)
business <- lapply(X=input,fromJSON)


cities <- c("Montreal", "Toronto", "New York", "Tokyo", "Sao Paulo", "Seoul","Mexico City","Osaka", "Manila", "Mumbai","Delhi","Jakarta","Lagos","Kolkata", "Cairo","Los Angeles","Buenos Aires","Rio de Janeiro","Moscow","Shanghai", "Karachi", "Paris","Dhaka")

#Extract id, date and review text
precise_review=""
i = 0
for (r in review) {
  precise_review[i] <- c(r$business_id, r$date, r$text)
  i = i+1
}

#Extract id, and city
precise_business=""
i = 0
for (r in business) {
  precise_business[i] <- c(r$business_id, r$city)
  i = i+1
}


for (city in cities){
  j=0
  for (b in precise_business[1:100]) {
    if(b[]$city == city[1:100]){
      for (c in precise_review[1:100]) {
        if(b$business_id == c$business_id){
          t[city][j] <- c(c$text)
          j = j+1
        }
      }
    }
  }
}


j=0
for (b in business) {
  if(b[]$city == "Montreal"){
    for (c in review) {
      if(b$business_id == c$business_id){
        t[j] <- c(c$text)
        j = j+1
      }
    }
  }
}

#mergin
montreal<-c(t[1:l])


