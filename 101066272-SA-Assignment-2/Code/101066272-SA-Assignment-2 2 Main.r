windows(record=TRUE)

# Installing required packages
#install.packages("arules")
#install.packages("rggobi")

# Calling necessary libraries
#library(arules)
#library(rggobi)

#---------------------------------------#
# Setting up the directory and database #
#---------------------------------------#

Student.Number <- "101066272"
Initials <- "SA"
ASorLAB <- "Assignment"
Assignment.Number <- "2"
Student.info <- paste(Student.Number, Initials, ASorLAB, Assignment.Number, sep="-")

# "drive", AND "path.up" SHOULD BE THE ONLY PARTS THAT REQUIRE YOUR PROFESSOR 
# OR TA TO BE ABLE TO RUN YOUR CODE

drive="S:"
path.upto <- paste("Grad 3 term", "Data Mining", sep="/" )
code.dir <- paste(drive, path.upto, Student.info, "Code", sep="/")
data.dir <- paste(drive, path.upto, Student.info, "Data", sep="/")
work.dir <- paste(drive, path.upto, Student.info, "Work", sep="/")
setwd(work.dir)


#~~~~~~~~~~~~~~~~REading Files~~~~~~~~~~~~~~~~~~~~~~~~#

#votes.data <- read.csv(file.choose(), header= True)
#We can use this method to read the files but this method opens up a new window where we can select the file and takes lot of time

#~~~~~~~~~~~~Reading Votes,county_facts,county_facts_dictionary~~~~~~~~~~~~~~~~~~~#

votes.data <- paste(data.dir,"votes.csv", sep="/")
votes <- read.csv(votes.data)
head(votes)
facts.data <- paste(data.dir,"county_facts.csv", sep="/")
facts <- read.csv(facts.data)
head(facts)
dict.data <-paste(data.dir,"county_facts_dictionary.csv", sep="/")
dict <- read.csv(dict.data)
head(dict)

#~~~~~~~importing library to join the data set~~~~~~~~~#

library(data.table)
library(dplyr)

#~~~~~~~~~~~~~Cleaning up data as there are some null values in facts and having a look at the data~~~~~~~~~~~~#
facts[1:10,1:5]
facts <-facts[facts[,3]!="",]
dim(facts)
facts[1:2,]
dict

#~~~~~~~~~~~~~~combining the 3 databses~~~~~~~~~~~~~~~~~#

all_dat<-inner_join(facts[,1:3],votes, by="fips")
dim(all_dat)
all_dat[104:115,1:30]


#~~~~~~Adding up data of Democratic party representatives and storing that into a new column~~~~~~~~~~#

Democratic <- all_dat$Clinton+all_dat$Obama
Democratic
all_dat$Democratic <- Democratic
all_dat


#~~~~~~Adding up data of Republic party representatives and storing that into a new column~~~~~~~~~~#

Republic <- (all_dat$Romney)+(all_dat$Trump)
all_dat$Republic <- Republic
dim(all_dat)
all_dat


#~~~~~~~~~~~Converting the whole file into a numeric file using As.numeric function and writing that to a CSV format~~~~~~~~~~~~#

all_dat_numeric = lapply(all_dat, as.numeric)
write.csv(all_dat_numeric,file="all_dat_numeric1.csv")


#~~~~~~~~~~~Reading the new CSV file into a variable~~~~~~~~~~~~~~~~~~~~~#

all_dat3<- paste(work.dir,"all_dat_numeric1.csv", sep="/")
all_dat1 <- read.csv(all_dat3)
head(all_dat1)
summary(all_dat1)


#~~~~~~~~~~~~~~Removing colums that are not needed for clustering~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~Reducing DATA~~~~~~~~~~~~#

dat_kmeans<-all_dat1[ ,c(2:3, 11:12, 15, 23:24, 80:87)]
head(dat_kmeans)


#~~~~~~~~~~~~~~Comparing the total votes of Demcratic and REpublic and giving them vale 1 or 2 according to who has more~~~~~~~~~~#

winners<-ifelse(dat_kmeans$Democratic > dat_kmeans$Republic,1,ifelse(dat_kmeans$Democratic < dat_kmeans$Republic,2,NA))
summary(winners)
head(winners)
dim(winners)
str(winners)
dat_kmeans$winner <- winners       #adding the winner to the data as winner column
dat_kmeans



#~~~~~~~~~~~~~Davis Bouldin functio to calculate DBI for finding best Center~~~~~~~~~~~~~~#

Davies.Bouldin <- function(A, SS, m) {
  # A - the centres of the clusters
  # SS - the within sum of squares
  # m - the sizes of the clusters
  N <- nrow(A) # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      R[i,j] <- (S[i] + S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}



#~~~~~~~~~Performing Kmeans Clustering with Center between 2 to 15 without using any SEED~~~~~~~~~~~~#

clustering1 <- function(data.set,data.set.orig)
{
  min_error<-100
  
  errs <- rep(0, 10)
  DBI <- rep(0, 10)
  size.cluster <- rep(0,10)
  max.cluster <- rep(0,15)
  library(cluster)
  library(fpc)
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  for (i in 2:15) {
    KM <- kmeans(data.set, i, 15)
    
 #~~~~~~~~~Performing plotting of cluster with many methods but got succeessful with one~~~~~~~~~~~~~~#
    
    #plot(data.set$state_abbr,type = "n",xlab = "Winner pArty",ylab = "sates")
    #text(x=data.set$winner,y=data.set$state_abbr,labels=data.set$winner,col=(KM$cluster+1))
    plotcluster(data.set,col=data.set.orig$state_abbr, KM$cluster, main=paste(i,"clusters") )
    
    
    #ggplot(data.set, aes(x=data.set.orig$Democratic, y=data.set.orig$state_abbr, color=KM$cluster, 
    #  )) + geom_point()
    
    
    errs[i-1] <- sum(KM$withinss)
    DBI[i-1] <- Davies.Bouldin(KM$centers, KM$withinss, KM$size)
    
    
    size <- "\n\nSize of all clusters"
    anly <- "\n\n~~~~~~~~~~~~~~~Analysis of Clusters having K ="
    en <-"~~~~~~~~~~~~~~"
    cat(paste(anly,i,en))
    cat(size)
    
    cat("\nDistribution of Winner:\n")
    con <- table(data.set.orig$winner, KM$cluster)
    rownames(con) <- c("Democratic","Republic")
    print(con)
    
    
    mis_class<-0
    for (n in 1:i)
    {
      mis_class=mis_class+KM$size[n]-max(con[,n])
      
    }  
    dec<-("\nTotal number of misclassifications =")
    cat(paste(dec,mis_class))
    if(mis_class<min_error)
    {
      min_error<-mis_class
      min_error.cluster<-i
    }
    
    ss <- "\nSSE ="
    db <- "\nDBI ="
    cat(paste(ss,errs[i-1]))
    cat(paste(db,DBI[i-1]))
  }
  
  plot(2:15, errs, main="SSE")
  lines(2:15, errs)
  plot(2:15, DBI, main="Davies-Bouldin")
  lines(2:15, DBI)
  par(oldpar)
  
  cat(paste("\n\n\nLeast DBI=",min(DBI)))
  
}

clustering_no_seed <- clustering1(dat_kmeans,dat_kmeans)
#as we are not using any seed so best cluster keeps on varying and is not fixed


#~~~~~~~~~~~~~~~PErforming Kmeans clustering With SEED~~~~~~~~~~~~~~~~~#

clustering <- function(data.set,data.set.orig)
{
  min_error<-100
  
  errs <- rep(0, 10)
  DBI <- rep(0, 10)
  size.cluster <- rep(0,10)
  max.cluster <- rep(0,15)
  library(cluster)
  library(fpc)
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  for (i in 2:15) {
    set.seed(654321)
    KM <- kmeans(data.set, i, 15)
  
    plotcluster(data.set,col=data.set.orig$state_abbr, KM$cluster, main=paste(i,"clusters"))
   
    errs[i-1] <- sum(KM$withinss)
    DBI[i-1] <- Davies.Bouldin(KM$centers, KM$withinss, KM$size)
    
    
    size <- "\n\nSize of all clusters"
    anly <- "\n\n~~~~~~~~~~~ Analysis of Clusters having K ="
    en <-"~~~~~~~~~~~~~~~"
    cat(paste(anly,i,en))
    cat(size)
    
    cat("\nDistribution of Winners:\n")
    con <- table(data.set.orig$winner, KM$cluster)
    rownames(con) <- c("Democratic  ", "Republic  ")
    print(con)
    
    
    mis_class<-0
    for (n in 1:i)
    {
      mis_class=mis_class+KM$size[n]-max(con[,n])
      
    }  
    dec<-("\nTotal number of misclassifications =")
    cat(paste(dec,mis_class))
    if(mis_class<min_error)
    {
      min_error<-mis_class
      min_error_cluster<-i
    }
    
    ss <- "\nSSE ="
    db <- "\nDBI ="
    cat(paste(ss,errs[i-1]))
    cat(paste(db,DBI[i-1]))
  }
  
  plot(2:15, errs, main="SSE")
  lines(2:15, errs)
  plot(2:15, DBI, main="Davies-Bouldin")
  lines(2:15, DBI)
  par(oldpar)
  
  cat(paste("\n\n\nLeast DBI=",min(DBI)))
  
  
}

Kmeans_clustering <- clustering(dat_kmeans,dat_kmeans)
#best cluster according to dbi is 5



#~~~~~~~~~~~~~function to get best seed~~~~~~~~~~~~~~~~#

min_error <- function(data.set,data.set.org,ke)
{
  min_error<-600
  for(j in 1:3200)
  {
    
    size.cluster <- rep(0,10)
    max.cluster <- rep(0,15)
    library(cluster)
    library(fpc)
    
    set.seed(j)
    KM <- kmeans(data.set, ke, 15)
    
    
    con <- table(data.set.org$winner, KM$cluster)
    print(con)
    mis_class<-0
    for (n in 1:ke)
    {
      mis_class=mis_class+KM$size[n]-max(con[,n])
      
    }  
    
    if(mis_class<min_error)
    {
      min_error<-mis_class
      min_error.seed<-j
    }
    
    
    
  } 
  
  cat(paste("\nMinimum misplacements=",min_error))
  cat(paste("\nSeed of minimum misplacements =",min_error.seed))
  
}  


clustering_error <- min_error(dat_kmeans,dat_kmeans,5)
# best seed is 1


#~~~~~~~~~~~~~~~~~~~~~setting the seed that we got from error function and checking the centres values~~~~~~~~~~~~~~~~~~~~~~#

set.seed(1)
centre_first <- kmeans(dat_kmeans, 5, 15)
print(centre_first$centers)


#~~~~~~~~~~~~~~~~finding optimal result for particular seed and the best value of centres~~~~~~~~~~~~~~~~~~~~#

optimal.results<-function(data.set,data.set.orig,k,seed)
{  
  set.seed(seed)
  clust <- kmeans(data.set, k, 15)
  
  plotcluster(data.set,col=data.set.orig$state_abbr, clust$cluster, main=paste(k,"clusters"))
  print(paste("Optimal Cluster: ",k," Analysis:"))
  
  wrong_data <- rep(0,k)
  con <- table(data.set.orig$winner, clust$cluster)
  rownames(con) <- c("Democratic  ", "Republic")
  print(con)
  cat("\n\n")
  
}

clustering_optimal <- optimal.results(dat_kmeans,dat_kmeans,5,1)





#~~~~~~~~~~~~~~~~~~~Doing clustering on basis of Distance~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~Performing MAnhattan Clustering~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~dist_manhattan(a,b) = sum(abs(a1-b1) + ... + abs(an-bn))~~~~~~~~~~~~~~~~~~#

manhattenclustering <- function(data.set,data.set.orig)
{
  min_error<-100
  
  errs <- rep(0, 10)
  avg.width <- rep(0, 10)
  size.cluster <- rep(0,10)
  max.cluster <- rep(0,15)
  library(cluster)
  library(fpc)
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  for (i in 2:15) {
    set.seed(654321)
    library(fpc)
    KM <- pam(data.set, i, metric = "manhattan")
    plotcluster(data.set,col=data.set.orig$state_abbr, KM$cluster, main=paste(i,"clusters"))
    avg.width[i-1] <- KM$silinfo$avg.width
    size <- "\n\nSize of all clusters"
    anly <- "\n\n~~~~~~~~~~~~Analysis of Clusters having K ="
    en <-"~~~~~~~~~~~~~~~"
    cat(paste(anly,i,en))
    cat(size)
    cat("\nDistribution of Winners:\n")
    con <- table(data.set.orig$winner, KM$cluster)
    rownames(con) <- c("Democratic  ", "Republic")
    print(con)
    mis_class<-0
    for (n in 1:i)
    {
      mis_class=mis_class+KM$clusinfo[n,1]-max(con[,n])
      
    }  
    dec<-("\nTotal number of misclassifications =")
    cat(paste(dec,mis_class))
    if(mis_class<min_error)
    {
      min_error<-mis_class
      
      min_error.cluster<-i
    }
    
    
    db <- "\nAverage Silhoutte Width ="
    cat(paste(db,avg.width[i-1]))
  }
  
  
  plot(2:15, avg.width, main="Silhouette width")
  lines(2:15, avg.width)
  par(oldpar)
  
  cat(paste("\n\n\nHighest average Silhoutte Width=",max(avg.width)))
  
}

clustering_manhatten <- manhattenclustering(dat_kmeans,dat_kmeans)
# best k is 2



#~~~~~~~~~~~~~~function to get best seed~~~~~~~~~~~~~~~~~~#

min_error_manhatten <- function(data.set,data.set.org,ke)
{
  min_error<-600
  
  #as it takes long time considering only 10 else you can consider until 3200
  for(j in 1:10)
  {
    
    size.cluster <- rep(0,10)
    max.cluster <- rep(0,15)
    library(cluster)
    library(fpc)
    
    set.seed(j)
    KM <- pam(data.set, ke, metric = "manhattan")
    
    
    con <- table(data.set.org$winner, KM$cluster)
    print(j)
    print(con)
    mis_class<-0
    for (n in 1:ke)
    {
      mis_class=mis_class+KM$clusinfo[n]-max(con[,n])
      
    }  
    
    if(mis_class<min_error)
    {
      min_error<-mis_class
      min_error.seed<-j
    }
    
    
    
  } 
  
  cat(paste("\nMinimum misplacements=",min_error))
  cat(paste("\nSeed of minimum misplacements =",min_error.seed))
  
}


clustering_manhatten_error <- min_error_manhatten(dat_kmeans,dat_kmeans,2)


#~~~~~~~~~~~~~~~~~~~~~setting the seed that we got from error function and checking the similar values to form a cluster(medoids)~~~~~~~~~~~~~~~~~~~~~~#

set.seed(1)
centre4.1 <- pam(dat_kmeans, 2, metric="manhattan")
print(centre4.1$medoids)



#~~~~~~~~~~~~~~~~~~~fucntion for optimal result~~~~~~~~~~~~~~~~~~~~~~~#

manhatten_optimal_results<-function(data.set,data.set.orig,k,seed)
{  
  set.seed(seed)
  clust <- pam(data.set, k, metric = "manhattan")
  
  plotcluster(data.set,col=data.set.orig$state_abbr, clust$cluster, main=paste(k,"clusters"))
  print(paste("Optimal Cluster: ",k," Analysis:"))
  
  wrong_data <- rep(0,k)
  con <- table(data.set.orig$winner, clust$clustering)
  rownames(con) <- c("Democratic", "Republic")
  print(con)
  cat("\n\n")
  
}

manhatten_clustering_result <- manhatten_optimal_results(dat_kmeans,dat_kmeans,2,1)




#~~~~~~~~~~~~~~~~~~~~~~Euclidean Clustering~~~~~~~~~~~~~~~~~~~~~~#

euclideanclustering <- function(data.set,data.set.orig)
{
  min_error<-100
  
  errs <- rep(0, 10)
  avg.width <- rep(0, 10)
  size.cluster <- rep(0,10)
  max.cluster <- rep(0,15)
  library(cluster)
  library(fpc)
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  for (i in 2:15) {
    set.seed(654321)
    library(fpc)
    KM <- pam(data.set, i, metric = "euclidean")
    plotcluster(data.set,col=data.set.orig$state_abbr, KM$cluster, main=paste(i,"clusters"))
    avg.width[i-1] <- KM$silinfo$avg.width
    size <- "\n\nSize of all clusters"
    anly <- "\n\n~~~~~~~~~~~~Analysis of Clusters having K ="
    en <-"~~~~~~~~~~~~~~~"
    cat(paste(anly,i,en))
    cat(size)
    cat("\nDistribution of Winners:\n")
    con <- table(data.set.orig$winner, KM$cluster)
    rownames(con) <- c("Democratic  ", "Republic")
    print(con)
    mis_class<-0
    for (n in 1:i)
    {
      mis_class=mis_class+KM$clusinfo[n,1]-max(con[,n])
      
    }  
    dec<-("\nTotal number of misclassifications =")
    cat(paste(dec,mis_class))
    if(mis_class<min_error)
    {
      min_error<-mis_class
      
      min_error.cluster<-i
    }
    
    
    db <- "\nAverage Silhoutte Width ="
    cat(paste(db,avg.width[i-1]))
  }
  
  
  plot(2:15, avg.width, main="Silhouette width")
  lines(2:15, avg.width)
  par(oldpar)
  
  cat(paste("\n\n\nHighest average Silhoutte Width=",max(avg.width)))
  
}

clustering_euclidean <- euclideanclustering(dat_kmeans,dat_kmeans)
# best k is 6




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~function to get best seed~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

min_error_euclidean <- function(data.set,data.set.org,ke)
{
  min_error<-600
  
  #as it takes long time
  for(j in 1:10)
  {
    
    size.cluster <- rep(0,10)
    max.cluster <- rep(0,15)
    library(cluster)
    library(fpc)
    
    set.seed(j)
    KM <- pam(data.set, ke, metric = "euclidean")
    
    
    con <- table(data.set.org$winner, KM$cluster)
    print(j)
    print(con)
    mis_class<-0
    for (n in 1:ke)
    {
      mis_class=mis_class+KM$clusinfo[n]-max(con[,n])
      
    }  
    
    if(mis_class<min_error)
    {
      min_error<-mis_class
      min_error.seed<-j
    }
    
    
    
  } 
  
  cat(paste("\nMinimum misplacements=",min_error))
  cat(paste("\nSeed of minimum misplacements =",min_error.seed))
  
}

clustering_euclidean_error <- min_error_euclidean(dat_kmeans,dat_kmeans,2)



#~~~~~~~~~~~~~~~~~~~~fucntion for optimal result~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

euclidean_optimal_results<-function(data.set,data.set.orig,k,seed)
{  
  set.seed(seed)
  clust <- pam(data.set, k, metric = "euclidean")
  
  plotcluster(data.set,col=data.set.orig$state_abbr, clust$cluster, main=paste(k,"clusters"))
  print(paste("Optimal Cluster: ",k," Analysis:"))
  
  wrong_data <- rep(0,k)
  con <- table(data.set.orig$winner, clust$clustering)
  rownames(con) <- c("Democratic", "Republic")
  print(con)
  cat("\n\n")
  
}

euclidean_clustering_result <- manhatten_optimal_results(dat_kmeans,dat_kmeans,6,1)


#~~~~~~~~~~~~~~~~~~~Hierarchical Clustering~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
clust_dist <- dist(dat_kmeans)
clust_hclust <-hclust(clust_dist, method="single")
clust_hclust$labels <- dat_kmeans$state_abbr
plot(clust_hclust, main="Single Linkage")
rect.hclust(clust_hclust, k=5, border="blue")


clust_hclust1 <-hclust(clust_dist, method="ave")
clust_hclust1$labels <- dat_kmeans$state_abbr
plot(clust_hclust1, main="Average Linkage")
rect.hclust(clust_hclust1, k=5, border="red")


clust_hclust2 <-hclust(clust_dist, method="complete")
clust_hclust2$labels <- dat_kmeans$state_abbr
plot(clust_hclust2, main="Complete Linkage")
rect.hclust(clust_hclust2, k=5, border="green")



