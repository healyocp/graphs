
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
  mainLanguageVector = c()
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
    #currentLanguageDF <- data_frame(repo =  RepoNameVector, language = languageVector, user = userVector)
    mainLanguageVector <- as.vector(rbind(languageVector,mainLanguageVector))
    
    i = i+1
    
  }
  distincTv= mainLanguageVector[!duplicated(mainLanguageVector)]
  return ( distincTv)
}


getFollowers <- function(username)
{
 
  URL <- paste("https://api.github.com/users/", username , "/followers", sep="")
  followers = fromJSON(URL)
  return (followers$login)
}
make_social_graph <- function(toPlot,labels)
{
  username <- 'phadej'
  myFollowers <- getFollowers('phadej')
  labels <- c(username)
  toPlot <- c()
  for(i in 1:length(myFollowers))
  {
    their_username <- myFollowers[i]
    labels = c(labels, their_username)
    toPlot = c(toPlot, username, their_username)
  }
  
  for(i in 1:length(myFollowers))
  {
    username <- myFollowers[i]
    theirFollowers <- getFollowers(username)
    for (j in 1:length(theirFollowers))
    {
      if (is.element(theirFollowers[j], myFollowers))
      {
        toPlot = c(toPlot, username, theirFollowers[j])
      }
      else
      {
        next
      }
    }
  }
  social_graph(toPlot, labels)
}

social_graph <- function(toPlot, labels)
{
  library(plotly)
  library(quantmod)
  library(igraph)
  
  g<-make_graph(edges=c(toPlot))
  G <- upgrade_graph(g)
  L <- layout.circle(G)
  vs <- V(G)
  es <- as.data.frame(get.edgelist(G))
  Nv <- length(vs)
  Ne <- length(es[1]$V1)
  Xn <- L[,1]
  Yn <- L[,2]
  
  network <- plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = labels, hoverinfo = "text", type="scatter")
  
  edge_shapes <- list()
  for(i in 1:Ne) 
  {
    v0 <- es[i,]$V1
    v1 <- es[i,]$V2
    
    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0.3),
      x0 = Xn[v0],
      y0 = Yn[v0],
      x1 = Xn[v1],
      y1 = Yn[v1]
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  p <- layout(
    network,
    title = 'Followers',
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis
  )
  #chart_link = api_create(p, filename="social_graph")
  return (p)
}



networkGraph = make_social_graph(toPlot,labels)




help("api_create")

























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
  dataReLanguages = data.frame()
  for(i in 1:numberOfFollowers)
  {
    languageData =findLanguages(username)
    dataReLanguages=rbind( dataReLanguages,languageData)
    i <- i+1;
  }
  return(dataReLanguages)
  
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
lengthF<-function(x)
{
  l=0
  for (value in x)
  {
    print(value)
    if (!strcmp("Unstated",value ))
    {
      l=l+1
    }
  }
  return(l)
  
}




#x <- findFollowers(currentUser)
#p= findRepository(currentUser)
#z= findLanguages(currentUser)
#m= languagesVisualization(currentUser)
#a=getFollowersInformation(currentUser)
#d=getFollowersLanguages(currentUser)






#Generate data for followers and repos starting at user phadej

currentUser="aoifetiernan"

#x = findFollowers(currentUser)
#followersUsernames = x$user
#numberOfFollowers = length(x$userID)
m=findLanguages(currentUser)

z=lengthF(m)


      
numberOfLanguagesPerUser <-function(matrixofLangages)
{
  noLanguagesPerUser =c()
  for (c in ncol(matrixofLangages))# if  c is pointer great, if not need to keep track of pointer
  {
    #do soemthing here that will count the number of rows per each col 
    
  }
}
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      





