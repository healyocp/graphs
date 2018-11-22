
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
    followers <- GET( paste0("https://api.github.com/users/", username, "/followers?per_page=100&page=", i),myToken) # ensures all pages of followers are used
    followersContent <- content(followers)
    currentFollowersDF <- lapply(followersContent, function(singleFollower) 
    {
      df <- data.frame(user = singleFollower$login, userID = singleFollower$id, followersURL = singleFollower$followers_url, followingURL = singleFollower$following_url)
    }) %>% bind_rows() #data frames do not have same number of rows
    print("hey")
    i = i+1
    x = length(followersContent)
    followersDataFrame <- rbind(followersDataFrame, currentFollowersDF)# equally sized 
  }
  print(i)
  return (followersDataFrame)
}

#Returns a dataframe with information on the Current Users Repositories
findRepository <- function(username)
{
  i = 1
  x = 1
  repositoryDF = data_frame()
  while(x!=0)
  {
    repository = GET( paste0("https://api.github.com/users/", username, "/repos?per_page=100&page=", i),myToken)
    repositoryContent = content(repository)
    currentRepositoryDF = lapply( repositoryContent, function(individualRepository) 
    {
      df <- data_frame(repo = individualRepository$name, id = individualRepository$id, commits = individualRepository$git_commits_url, language = individualRepository$languages) 
    }) %>% bind_rows()
    i = i+1
    x = length(repositoryContent) # number of repositories per page ( in total there are x )
    print(x)
    repositoryDF = rbind(repositoryDF, currentRepositoryDF)
  }
  return (repositoryDF)
}

findLanguages <- function(username)
{
  i=1
  x=1
  languageVector=c()
  RepoNameVector=c()
  languageDF = data_frame()
  while(x!=0)
  {
    
 
    repositoryDF = GET( paste0("https://api.github.com/users/", username, "/repos?per_page=100&page=", i),myToken)
    repoContent = content(repositoryDF)
    x = length(repoContent) 
    print(x)
    if (x==0)
    {
      break
    }
    for ( j in 1:length(repoContent))
    {
      repoLanguage=repoContent[[j]]$language
      if(is.null(repoLanguage))
      {
        RepoNameVector[j] = repoContent[[j]]$name
        languageVector[j] = ""
      }else
      {
        languageVector[j] =repoContent[[j]]$language
        RepoNameVector[j] = repoContent[[j]]$name
      }
    }
    currentLanguageDF <- data_frame(repo =  RepoNameVector, language = languageVector)
    languageDF <- rbind(languageDF, currentLanguageDF)
    
    i = i+1
    
  }

  return (languageDF)
}






currentUser <- "unicodeveloper"
#x <- findFollowers(currentUser)
#p= findRepository(currentUser)
z= findLanguages(currentUser)
