library(netdiffuseR)

# reads in a csv file
# must  be formatted as 1st column: year, following columns of authors in that reference

  CSV = read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

#creates new dataframe, a list of authors and the year they appeared
  authorYear = data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE)
  
  #renames columns in authorYear
  colnames(authorYear) = c('year', 'author', 'id')

  #adds temp values to authorYear dataframe to prevent future errors
  newRow = data.frame(year = 0, author = 'temp', id = 0, stringsAsFactors = FALSE)
  authorYear = rbind(authorYear, newRow)
  
  #adds authors to authorYear
  #nested for loops search both rows and columns to ensure all authors are read into authorYear
  count = 0
  year = CSV[1,1]
  for(i in 1:nrow(CSV)){
    for(j in 2:ncol(CSV)){ 
      #checks if cell is a comparable string
      if(CSV[i,j] != ' ' && CSV[i,j] != '' &&  !is.null(CSV[i,j]) && !is.na(CSV[i,j])){
        if(year != CSV[i,1]){
          count = 0;
          year = CSV[i,1]
        }
        auth = CSV[i,j]
        auth = paste('^', auth, '$', sep = '') #raps auth name in regex
        
          if(length(grep(auth, authorYear$author, ignore.case = TRUE)) != 0){ #if exists in authyear look for id
            temp = grep(auth, authorYear$author, ignore.case = TRUE)
            min = which.min(temp)
            minIndex = temp[min]
            id = authorYear[minIndex, 3]
          } else { #if auth does not exist in authYear calc id
            id = CSV[i, 1] * 1000 + count
            count = count + 1
          }
        
        newRow = data.frame(author = CSV[i,j], year = CSV[i,1], id, stringsAsFactors = FALSE)
        authorYear = rbind(authorYear, newRow)
     
      } else {
        break
      }
    
    }
  }
  
  #removes temporary row added at the beginning
  authorYear <- authorYear[-c(1),]
  
  #now, a dataframe of authors, their year of publication, and their id has been created
  #the next step is to create an edgelist out of their id's
  
  #deletes variables that will no be used again
  rm(newRow, auth, count, i, id, j, min, minIndex, temp, year)

#sorts dataframe by year in ascending order
#authorYear = authorYear[order(authorYear$year),]

#creates edgelist from authorYear dataframe

  #initializes authEdgelist
  authEdgelist = data.frame(matrix(ncol = 2, nrow = 0))
  #sets column names
  colnames(authEdgelist) = c("alter", "ego")
  
  #populates authEdgelist
  #first for loop for parsing through authorYear, and adding every author to alter
  
  for(i in 1:nrow(authorYear)){
    authAlter = authorYear[i,3]
    year = authorYear[i,1]
  
    j = i+1
    #second for loop used to add ego authors
    for(j in 1:nrow(authorYear)){
      if(authorYear[j,1] == (year+1)){
        authEgo = authorYear[j,3]
        
        newRow = data.frame(alter = authAlter, ego = authEgo)
        authEdgelist = rbind(authEdgelist, newRow)
      } 
      
    }
  }
  
  #deletes variables that will no be used again
  rm(i, j, year, authAlter, authEgo, newRow)
  
  temp <- edgelist_to_adjmat(
    edgelist = authEdgelist[,1:2], 
    undirected = FALSE, 
    t=nrow(authEdgelist)
  )
  
  adjmat <- edgelist_to_adjmat(
    edgelist = authEdgelist[,1:2],
    undirected = FALSE,
    t=nrow(temp[[1]])
  )
  
  rm(temp)
  
  toa <- data.frame(matrix(ncol = 1, nrow = 0))
  
  colnames(toa) <- c("toa")
  
  for(i in 1:nrow(adjmat[[1]])){
    toa[i,1] = i
  }
  
  diffnet <- as_diffnet( graph=adjmat, toa=toa$toa)
  
  timePeriod <- 1
  timeRange <- nrow(adjmat[[1]])
  
  cat("Select a time period to plot between 1 and", timeRange, ". Input 0 to quit", "\n")
  while(timePeriod!=0){
    timePeriod <- as.integer(readline())
    
    
    if(timePeriod == 0) break
    
    #checks for valid time period
    if(timePeriod<1 || timePeriod>timeRange){
      cat("Invalid time period. Please enter a number between 1 and", timeRange, "or 0 to quit", "\n")
      next
    }
    
    plot(diffnet, t=timePeriod)
    
    cat("Select another time period to plot between 1 and", timeRange, "input 0 to quit", "\n")
  }
  