
#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Portia_Healy_O_Connor",
                   key = "07be2ede7868dda505ac",
                   secret = "0feb2c26054026597fb7d7d9d0e3e7ba4c9f4728")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
1

# Use API
myToken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", myToken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]


# The code above was sourced from Michael Galarnyk's blog, found at:
# https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

findFollowers <- function(username)
{
  i = 1
  x = 1
  followersDataFrame <- data_frame()
  while(x!=0)
  {
    followers <- GET( paste0("https://api.github.com/users/", username, "/followers?per_page=100&page=", i),myToken)
    followersContent <- content(followers)
    currentFollowersDF <- lapply(followersContent, function(x) 
    {
      df <- data.frame(user = x$login, userID = x$id, followersURL = x$followers_url, followingURL = x$following_url)
    }) %>% bind_rows()
  
    x <- length(followersContent)
    followersDataFrame <- rbind(followersDataFrame, currentFollowersDF)
  }
  return (followersDataFrame)
}


currentUser <- "phadej"
x <- findFollowers(currentUser)