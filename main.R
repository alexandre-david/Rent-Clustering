####################
# Dacia Clustering #
####################

# Alexandre 
# 29/08/14


###############
## Variables
###############

# General
path_root = "/Users/Alex/Documents/55/Renault/clustering"
path_data = file.path(path_root, "_data",fsep=.Platform$file.sep)
path_script = file.path(path_root, "script",fsep=.Platform$file.sep)

# Load data
scriptname_loadData = "load_data.R"
filename_data = "dacia.csv"


###############
## Load data ##
###############

source(file=file.path(path_script,scriptname_loadData,fsep=.Platform$file.sep)) 
data <- readInput(file.path(path_data, filename_data,fsep=.Platform$file.sep))


###############
## General Statistics
###############

nbUsers <- dim(data)[1]
nbSessions <- sum(data$nbVisits)

# Visits
summary(data$nbVisits)
table(data$nbVisits)
hist(data$nbVisits, breaks = c(seq(0,320,1)), freq=TRUE, xlim = c(0,30), ylim= c(0,12000), main="", xlab = "nb of visits")

# Cars seen
sum(data$NbCarsSeen > 0) # nb of users that saw a car
table(data$NbCarsSeen) # distribution
hist(data$NbCarsSeen, breaks = c(0:7), main="", xlab="nb of cars seen")

# Time on site
data$TimeOnSite[is.na(data$TimeOnSite)] <- 0
summary(data$TimeOnSite/60) # in minutes
table(data$TimeOnSite)
hist(data$TimeOnSite/60, breaks=c(seq(0,round(max(data$TimeOnSite)/60)+1,1)), freq= TRUE, xlim=c(0,50), main="", xlab="time on site (min)")

# Number of events
summary(data$nbEvents)
sum(data$nbEvents) # nb of events generated
hist(data$nbEvents, breaks=c(seq(0,round(max(data$nbEvents))+1,1)), freq= TRUE, xlim=c(0,20), main="", xlab="nb of events")

# Number of pages
sum(data$nbPages) # nb of pages seen
hist(data$nbPages, breaks=c(seq(0,round(max(data$nbPages))+1,1)), freq= TRUE, xlim=c(0,60), main="", xlab = "nb of pages")

# Use of the configurator
sum(data$Configurator) # nb of times used
hist(data$Configurator, breaks=c(seq(0,round(max(data$Configurator))+1,1)), freq= TRUE, xlim=c(0,50), ylim=c(0,4000), main="", xlab = "use of configurator")

# Use of ProductPlan
sum(data$ProductPlan) # nb of times used
hist(data$ProductPlan, breaks=c(seq(0,round(max(data$ProductPlan))+1,1)), freq= TRUE, xlim=c(0,30), ylim=c(0,10000), main="", xlab = "Product plan")

# Use of Finance
summary(data$Financiaci)
hist(data$Financiaci, breaks=c(seq(0,round(max(data$Financiaci))+1,1)), freq= TRUE, xlim=c(0,15) , ylim=c(0,1500), main="", xlab="Financiaci")

# Dealer locator
summary(data$DealerLocator)
hist(data$DealerLocator, breaks=c(seq(0,round(max(data$DealerLocator))+1,1)), freq= TRUE, xlim=c(0,15) , ylim=c(0,1000), main="", xlab="Dealer locator")

par(mfrow= c(3,3)) 
hist(data$nbVisits, breaks = c(seq(0,320,1)), freq=TRUE, xlim = c(0,30), ylim= c(0,12000), main="", xlab = "nb of visits")
hist(data$NbCarsSeen, breaks = c(0:7), main="", xlab="nb of cars seen")
hist(data$TimeOnSite/60, breaks=c(seq(0,round(max(data$TimeOnSite)/60)+1,1)), freq= TRUE, xlim=c(0,50), main="", xlab="time on site (min)")
hist(data$nbEvents, breaks=c(seq(0,round(max(data$nbEvents))+1,1)), freq= TRUE, xlim=c(0,20), main="", xlab="nb of events")
hist(data$nbPages, breaks=c(seq(0,round(max(data$nbPages))+1,1)), freq= TRUE, xlim=c(0,60), main="", xlab = "nb of pages")
hist(data$Configurator, breaks=c(seq(0,round(max(data$Configurator))+1,1)), freq= TRUE, xlim=c(0,50), ylim=c(0,4000), main="", xlab = "use of configurator")
hist(data$ProductPlan, breaks=c(seq(0,round(max(data$ProductPlan))+1,1)), freq= TRUE, xlim=c(0,30), ylim=c(0,10000), main="", xlab = "Product plan")
hist(data$Financiaci, breaks=c(seq(0,round(max(data$Financiaci))+1,1)), freq= TRUE, xlim=c(0,15) , ylim=c(0,1500), main="", xlab="Financiaci")
hist(data$DealerLocator, breaks=c(seq(0,round(max(data$DealerLocator))+1,1)), freq= TRUE, xlim=c(0,15) , ylim=c(0,1000), main="", xlab="Dealer locator")
par(mfrow=c(1,1))

# Leads
sum(data$Lead_Ebrochure)
sum(data$Lead_New_Test_drive_VN)
sum(data$Lead_After_sales_enquiry) 
sum(data$Lead_General_enquiry) 
sum(data$Lead_Professional_Contact) 


###############
## Visualisation
###############

library(graphics)
data_ = data[,2:24]
cordata_ <- cor(data_)
cordata_[is.na(cordata_)] = 0
image(cordata_)

#install.packages("gclus")
library(gclus)
abscor = abs(cor(data[,2:19]))
colors = dmat.color(abscor)
order = order.single(cor(data[,2:19]))
cpairs(data[seq(1,70000,70),2:19], order, panel.color=colors, gap=.5)


###############
## PCA
###############

my.prc <- prcomp(data[,2:19], center=T, scale=T) # want to center and scale it to unit variance
#ls(my.prc) # to see information about the object. x : scores
summary(my.prc)

# How many components to keep ?
# 1) Kaiser criterion : 
# Look at eigenvalue associated with the component. If greater than 1 --> keep.If not reject.
my.prc$sdev^2 # get the eigenvalue back because the sum of the eigenvalues = the sum of the total variance in the dataset
# Keep 4 (or 5)
# 2) Screeplot
screeplot(my.prc, type="l")
# Keep 3

# Feature importances
my.prc$feature_imp = my.prc$sdev^2[1] / sum(my.prc$sdev^2)
plot(cumsum(my.prc$feature_imp)/sum(my.prc$feature_imp), type="ol", main="Cumulated variance explained", ylab = "variance explained", xlab = "nb of principal components", ylim=c(0,1))

# Look at the PC themselves
# Each component is a linear combination of the variables. The coefficients of that linear combination are the loadings or rotations and indicate to which extent the variable is correlated with the component.
# Look at the rotations
my.prc$rotation
# Dot Plot
load = my.prc$rotation
# ??

# Biplot
#LONG biplot(my.prc, cex=c(1,0.7))
# Recall : the cosine of the angle between these vectors is the correlation between the variables.

# Varimax rotation
# Change of coordinates that maximizes the sum of the variances of the squared loadings
# <=> Clean up the rotations that we found.
#my.var = varimax(my.prc$rotation)
# Get a rotation on top of the original rotation
#my.var$loadings


###############
# Clustering
###############
#data_cl <- data[,2:19]
mydata <- my.prc$x[,1:4]

nb_clusters = 10
wss <- numeric(nb_clusters)
swss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
wss[1] = swss
for (i in 2:nb_clusters) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:nb_clusters, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

km <- kmeans(mydata, centers=4)
km$cluster

# Visualisation of clusters 

# PC 1 and 2
plot(mydata[,1],mydata[,2], col=km$cluster, xlab = "PC1", ylab= "PC2")

# All possibilities
par(mfrow=c(3,2))
plot(mydata[,1],mydata[,2], col=km$cluster, xlab="PC1", ylab="PC2")
plot(mydata[,1],mydata[,3], col=km$cluster, xlab="PC1", ylab="PC3")
plot(mydata[,1],mydata[,4], col=km$cluster, xlab="PC1", ylab="PC4")
plot(mydata[,2],mydata[,3], col=km$cluster, xlab="PC2", ylab="PC3")
plot(mydata[,2],mydata[,4], col=km$cluster, xlab="PC2", ylab="PC4")
plot(mydata[,3],mydata[,4], col=km$cluster, xlab="PC3", ylab="PC4")
par(mfrow=c(1,1))

# 
par(mfrow=c(2,2))
plot(mydata[,1], km$cluster, xlab="PC1", ylab="")
plot(mydata[,2], km$cluster, xlab="PC2", ylab="")
plot(mydata[,3], km$cluster, xlab="PC3", ylab="")
plot(mydata[,4], km$cluster, xlab="PC4", ylab="")
par(mfrow=c(1,1))

# T-test
idx1 = (km$cluster==1)
idx2 = (km$cluster==2)
idx3 = (km$cluster==3)
idx4 = (km$cluster==4)

ttest <- function(pc){
  
  cl1 <- t.test(mydata[idx1,pc],mu = mean(mydata[,pc]), var.equal=FALSE)[[3]]
  cl2 <- t.test(mydata[idx2,pc],mu = mean(mydata[,pc]), var.equal=FALSE)[[3]]
  cl3 <- t.test(mydata[idx3,pc],mu = mean(mydata[,pc]), var.equal=FALSE)[[3]]
  cl4 <- t.test(mydata[idx4,pc],mu = mean(mydata[,pc]), var.equal=FALSE)[[3]]
  
  mean1 <- mean(mydata[idx1,pc])
  mean2 <- mean(mydata[idx2,pc])
  mean3 <- mean(mydata[idx3,pc])
  mean4 <- mean(mydata[idx4,pc])
  
  if (cl1 < 0.05 && cl2 < 0.05 && cl3 < 0.05 && cl4 < 0.05){
    means <- c(mean1,mean2,mean3,mean4)
  }
  
  return(means)
}

means1 = ttest(pc=1)
means2 = ttest(pc=2)
means3 = ttest(pc=3)
means4 = ttest(pc=4)




# 3D plot
#install.packages("rgl")
#library(rgl)
#plot3d(mydata[,1],mydata[,2],mydata[,3],col=km$cluster)

table(km$cluster[data$Lead_Ebrochure])
table(km$cluster[data$Lead_New_Test_drive_VN])
