library(rjson)
file <- 'review.json'
con = file(file, "r")
input <- readLines(con, -1L)
review <- lapply(X=input,fromJSON)

i = 0
for (r in review) {
  rv[i] <- c(r$business_id, r$date, r$text)
  i = i+1
}

file <- 'business.json'
con = file(file, "r")
input <- readLines(con, -1L)
business <- lapply(X=input,fromJSON)

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


