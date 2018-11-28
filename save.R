
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
    # print("hey")
    i = i+1
    x = length(followersContent)
    followersDataFrame <- rbind(followersDataFrame, currentFollowersDF)# equally sized 
  }
  #print(i)
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
    #print(x)
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
  userVector = c()
  while(x!=0)
  {
    repositoryDF = GET( paste0("https://api.github.com/users/", username, "/repos?per_page=100&page=", i),myToken)
    repoContent = content(repositoryDF)
    x = length(repoContent) 
    #print(x)
    if (x==0)
    {
      break
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    }
    for ( j in 1:length(repoContent))
    {
      print("hey")
      userVector[j]=username
      print(userVector[j])
      repoLanguage=repoContent[[j]]$language
      if(is.null(repoLanguage))
      {
        RepoNameVector[j] = repoContent[[j]]$name
        languageVector[j] = "Unstated"
      }else
      {
        languageVector[j] =repoContent[[j]]$language
        RepoNameVector[j] = repoContent[[j]]$name
      }
      
    }
    currentLanguageDF <- data_frame(repo =  RepoNameVector, language = languageVector, user = userVector)
    languageDF <- rbind(languageDF, currentLanguageDF)
    
    i = i+1
    
  }
  
  return (languageDF)
}


#Returns a dataframe giving the number of followers and number of repos a user has
getFollowersInformation <- function(username)
{
  
  followersDF <- findFollowers(username)
  numberOfFollowers <- length(followersDF$userID)
  followersUsernames <- followersDF$user
  data <- data.frame()
  
  #Iterating through the current users followers to extract number of followers 
  #and number of repos
  for(i in 1:numberOfFollowers)
  {
    userName <- followersUsernames[i]
    repos <- findRepository(userName)
    followers <- findFollowers(userName) 
    numberOfRepositories <- length(repos$repo)
    numberOfFollowers <- length(followers$user)
    
    
    newRow <- data.frame(userName, numberOfRepositories, numberOfFollowers)
    
    data <- rbind(data, newRow)
    
    i <- i+1;
  }
  return(data)
}

getFollowersLanguages<-function(username)
{
  followersDF <- findFollowers(username)
  numberOfFollowers <- length(followersDF$userID)
  followersUsernames <- followersDF$user
  #dataReLanguages = data.frame()
  dataReLanguages=matrix()
  for(i in 1:numberOfFollowers)
  {
    languageData =findLanguages(username)
    dataReLanguages=as.matrix(rbind( dataReLanguages,languageData)) #may not use as.matrix
    i <- i+1;
  }
  return(dataReLanguages)
  
}




#Returns a pie chart which depicts the languages information for the current user
languagesVisualization <- function(username)
{
  z = findLanguages(username)
  x =data.frame(table(z$language))
  
  
  pie =plot_ly(data =x, labels = ~Var1, values = ~Freq, type = 'pie') %>%
    layout(title = paste('Languages used by  User', username),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(pie)
  
}


checkDuplicate <- function(dataframe)
{
  noDuplicates <- distinct(dataframe)
  return(noDuplicates)
}


#x <- findFollowers(currentUser)
#p= findRepository(currentUser)
#z= findLanguages(currentUser)
#m= languagesVisualization(currentUser)
#a=getFollowersInformation(currentUser)
#d=getFollowersLanguages(currentUser)






#Generate data for followers and repos starting at user phadej



x = findFollowers(currentUser)
followersUsernames = x$user
numberOfFollowers = length(x$userID)
fullData = getFollowersInformation(currentUser)
i=1
#change 7 to correct number 
while(nrow(fullData)<7)
{
  current = followersUsernames[i]
  newData=getFollowersInformation(current)
  fullData=rbind(newData,fullData)
  i=i+1
  
}
fullData =checkDuplicate(fullData)
scatterplot = plot_ly(data = fullData, x = ~numberOfFollowers, y = ~numberOfRepositories,
                      text = ~paste("User: ", userName, '<br>Followers: ', numberOfFollowers, '<br>Repos:', numberOfRepositories),
                      marker = list(size = 10, color = 'rgba(255, 182, 193, .9)',
                                    line = list(color = 'rgba(152, 0, 0, .8)',width = 2))) %>%
  
  layout(title = 'Relationship Between Followers and Repositories',yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE),
         plot_bgcolor='rgba(63, 191, 165,0.2)')



mostFollowers = fullData[which(fullData$numberOfFollowers>=1000),]
mostFollowers$code = 1
mostRepos = fullData[which(fullData$numberOfRepositories>=1000),]
mostRepos$code = 0

combined = rbind(mostFollowers,mostRepos)
scatter2 = plot_ly(data = combined, x = ~numberOfFollowers, y = ~numberOfRepositories, color = ~code, colors = "Set1",
                   text = ~paste("User: ", userName, '<br>Followers: ', numberOfFollowers, '<br>Repos:', numberOfRepositories)) %>%
  layout(title = 'Most Followers and Repositories',yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE),
         plot_bgcolor='rgba(63, 191, 165,0.2)')
scatter2

#create a scatterplot with number of followers and number of languges and make radius of circle inteeractive, and hover to show languages 




x = findFollowers(currentUser)
followersUsernames = x$user
numberOfFollowers = length(x$userID)







currentUser <- "aoifetiernan"
m= getLanguages(currentUser)


languageData=getFollowersLanguages(currentUser)
j=1

while(nrow(fullData)<15000)
{
  current = followersUsernames[j]
  newLanguageData=getLanguages(current)
  fullData=rbind(newLanguageData,languageData)
  j=j+1
  
}