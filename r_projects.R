
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
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

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


#Learn about the GitHub API.  

#The information about my github profile is 
#stored in a data fram called data. 

data <- fromJSON("https://api.github.com/users/healyocp")
data$followers #number of followers i have
data$public_repos #number of public repositories i have
data$id
data$url
data$followers_url
data$following_url
data$name
data$company
data$following
data$name
data$name
data$created_at
data$updated_at
data$bio
data$email
data$type
data$site_admin
data$public_gists
data$public_repos
data$hireable
data$login
data$blog
data$location
data$repos_url



# specific details about followers must add to the url 
followers <- fromJSON("https://api.github.com/users/healyocp/followers")
followers$login #the usernames of all my followers
length <- length(followers$login) #the amount of people who follow me
length

#Repsoitory information 

repositories <- fromJSON("https://api.github.com/users/healyocp/repos")
repositories$name #the names ofnpublic repositories
repositories$created_at #when these  were created
lca <- fromJSON("https://api.github.com/repos/healyocp/cs3012Asig1/commits")
lca$commit$message #the message included in each commit to this  repository


#Convert data to jason format 
dataJason=toJSON(data, pretty = TRUE)

