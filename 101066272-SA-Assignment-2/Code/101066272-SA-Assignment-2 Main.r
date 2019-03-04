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


# Reading data into datafile variable from the data folder from the directory
datafile <- read.csv(file.choose(), header = TRUE)
dim(datafile)


#REmoving the null values from the databse
datafile <- na.omit(datafile)

#checking for duplicate values and removing them
datafile <- datafile[row.names(datafile)!="13352",]
datafile = datafile[,-1]
head(datafile)
dim(datafile)
summary(datafile)
str(datafile)


#reducing data by just considering the nutrients and removing every other thing
datafile1 <-datafile[,c(8:44)]
summary(datafile1)


#other way to solve it for PCA is converting whole data into numeric and then using that to calculate PCA
#datafile2 = lapply(datafile1, as.numeric)
#write.csv(datafile2,file="datafile2.csv")

#creating subset of original data by checking where Protein nutrient is greater than 50
Protein_subset<-subset(datafile, Protein_g>50)
Protein_subset$Descrip
Protein_subset$Fat_g


#plotting the graph for protein and fat and visualizing accordingly
plot(datafile$Protein_g, datafile$Fat_g, xlab="Protein", ylab="Fat", main="Protein Vs Fat", col="Blue")

#plotting histogram for Vitc_mg to check the frequency of occurences 
hist(datafile$VitC_mg, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C levels", xlim = c(0,100), breaks = 2000)


#performing PCA on the data
usda <- prcomp(datafile1)
head(usda)
summary(usda)

