#checking with cymon to see if website is human-made or robot generated.
library("httr")
checkip <- function()
{ 
  n <- readline(prompt="Enter a domain address to check (eg. xyzhfazc.us) : ")
  "Using Cymon to check whether this domain is not randomly generated"
  url = paste("https://cymon.io:443/api/nexus/v1/dga/",n,"/",sep="")
  url
  info <- GET(url)
  content(info)
  curlGetHeaders(url)

}

print(checkip())

  
  
#library(jsonlite)
#fromJSON
