library(netdiffuseR)

createList <- function(CSV){
  authYr = data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE)
  
  #renames columns in authYr
  colnames(authYr) = c('year', 'author', 'id')
  
  #adds temp values to authYr dataframe to prevent future errors
  newRow = data.frame(year = 0, author = 'temp', id = 0, stringsAsFactors = FALSE)
  authYr = rbind(authYr, newRow)
  
  #adds authors to authYr
  #nested for loops search both rows and columns to ensure all authors are read into authYr
  count = 0 #counter for int id assignment of authors
  year = CSV[1,1] #keeps track of year to update/reset count
  for(i in 1:nrow(CSV)){
    
    for(j in 2:ncol(CSV)){ 
      #checks if cell is a comparable string
      if(CSV[i,j] != ' ' && CSV[i,j] != '' &&  !is.null(CSV[i,j]) && !is.na(CSV[i,j])){
        
        if(year != CSV[i,1]){ #checks if the same year as prev
          count = 0; #if a new year, resets count
          year = CSV[i,1] #updates year
        }
        
        auth = CSV[i,j] #records current author name
        auth = paste('^', auth, '$', sep = '') #raps auth name in regex so that it can be compared
        
        if(length(grep(auth, authYr$author, ignore.case = TRUE)) != 0){ #if auth exists in authyear look for id
          temp = grep(auth, authYr$author, ignore.case = TRUE) #returns the indices at which the name appear
          min = which.min(temp) #grabs the first index
          minIndex = temp[min] #grabs the row number
          id = authYr[minIndex, 3] #sets the id to exisiting id so that it can be assigned correctly
        } else { #if auth does not exist in authYear calc id
          id = CSV[i, 1] * 1000 + count #create a new id 
          count = count + 1 #update count to avoid assignment of same id to multiple authors
        }
        
        #creates and adds new row authorYear
        newRow = data.frame(author = CSV[i,j], year = CSV[i,1], id, stringsAsFactors = FALSE)
        authYr = rbind(authYr, newRow)
        
      } else { #if string at csv[i,j] is not a comparable string, the last author in this reference has already been read and it is safe to continue to the next row
        break
      } #end if/else
    } #end for loop column traversal
  } #end for loop row traversal
  
  #removes temporary row added at the beginning
  authYr <- authYr[-c(1),]
  
  #now, a dataframe of authors, their year of publication, and their id has been created
  
  #deletes variables that will no be used again
  rm(newRow, auth, count, i, id, j, min, minIndex, temp, year)
  
  return(authYr)
}

createEdgeList <- function(authorYear){
  #initializes authEdgelist
  authEdgelist = data.frame(matrix(ncol = 2, nrow = 0))
  
  #sets column names
  colnames(authEdgelist) = c("alter", "ego")
  
  #populates authEdgelist
  #first for loop for parsing through authorYear, and adding every author to alter
  
  for(i in 1:nrow(authorYear)){
    authAlter = authorYear[i,3] #stores current author and year
    year = authorYear[i,1]
    
    #j = i+1
    #second for loop used to add ego authors
    for(j in 1:nrow(authorYear)){ #second for loop used to add ego authors
      if(authorYear[j,1] == (year+1)){ #if current author published the year after saved author
        authEgo = authorYear[j,3] #then save author as ego
        
        newRow = data.frame(alter = authAlter, ego = authEgo) #create new row of saved author, current author
        authEdgelist = rbind(authEdgelist, newRow) #adds newrow to edgelist
      } #endif
    } #end inner loop
  } #end outer loop
  
  #deletes variables that will no be used again
  rm(i, j, year, authAlter, authEgo, newRow)
  
  #returns edgelist created
  return (authEdgelist)
}

createAdjmat <- function(authEdgelist){
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
  
  return(adjmat)
}

createDiffNet <- function(authEdgelist, adjmat){
  
  toa <- data.frame(matrix(ncol = 1, nrow = 0))
  
  colnames(toa) <- c("toa")
  
  for(i in 1:nrow(adjmat[[1]])){
    toa[i,1] = i
  }
  
  diffnet <- as_diffnet( graph=adjmat, toa=toa$toa)
  
  return(diffnet)
}

plotDiffNet <- function(diffnet, adjmat){
  timePeriod <- 1
  timeRange <- nrow(adjmat[[1]])
  
  cat("Select a time period to plot between 1 and", timeRange, ". Input 0 to quit", "\n")
  while(timePeriod!=0){
    timePeriod <- as.integer(readline())
    
    
    if(timePeriod == 0) return()
    
    #checks for valid time period
    if(timePeriod<1 || timePeriod>timeRange){
      cat("Invalid time period. Please enter a number between 1 and", timeRange, "or 0 to quit", "\n")
      next
    }
    
    plot(diffnet, t=timePeriod)
    
    cat("Select another time period to plot between 1 and", timeRange, "input 0 to quit", "\n")
  }
  return(0)
  
}

#############################################################################################################################
#############################################################################################################################

CSV = read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

authorYear = createList(CSV)

quant = floor(nrow(authorYear)/1000) + 1

count = 0

while(count < quant){
  authorYearName = paste("authorYear", count+1, sep = "")
  edgelistName = paste("edgelist", count+1, sep = "")
  adjmatName = paste("adjmat", count+1, sep = "")
  diffNetName = paste("diffnet", count+1, sep = "")
  
  assign(authorYearName, authorYear[(count*1000+1):min((count*1000)+1000, nrow(authorYear)), 1:3])
  assign(edgelistName, createEdgeList(get(authorYearName)))
  assign(adjmatName, createAdjmat(get(edgelistName)))
  assign(diffNetName, createDiffNet(get(diffNetName), get(adjmatName)))
  count = count + 1
}

cat("There are ", count, " plots available. Choose between 1 - ", count, ". Enter 0 to quit.")
choice = as.integer(readline())

while(choice!=0){

  diffNetName = paste("diffnet", choice, sep = "")
  adjmatName = paste("adjmat", choice, sep = "")
  
  plotDiffNet(get(diffNetName), get(adjmatName))
  
  cat("There are ", count, " plots available. Choose between 1 - ", count, ". Enter 0 to quit.")
  choice = as.integer(readline())
}


rm(authorYearName, choice, count, edgelistName, quant, authorYear, CSV, edgelist1)

