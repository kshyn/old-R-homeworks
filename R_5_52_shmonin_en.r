# Report 5. Cluster analysis
# Student Kirill Shmonin
# Department of Soil Geography. Option 52
#count data
cludat=read.csv("data5_var52.csv",dec=",",sep = ";")
cludat
# check the dimension of the data
dim(cloudat)
# see the names
names(cluster)
# Let's look at the data structure
str(cluster)
# write only quantitative features to mydata
mydata=cloudat[ ,4:8]; mydata
# Data preparation
##Remove columns and rows where there are NA values
mydata = na.omit(mydata)
##Normalization of variables
mydata = scale(mydata); mydata
#check that the data is indeed standardized
summary(mydata)
#Construct a dendrogram using Euclid's distance and Ward's method as the union method.
##Hierarchical classification
###Build a matrix of Euclidean distances between objects
d = dist(mydata, method = "euclidean")
###Calculate the dendrogram using the Ward method
fit = hclust(d, method="ward.D2")
###Let's draw a dendrogram and sign the names
plot(fit, labels = cludat$horizon)
###Let's make a section of the dendrogram into 5 classes
groups = cutree(fit, k=5)
###Let's draw red boxes around 5 classes
rect.hclust(fit, k=5, border="red")
# Let's make a section of the dendrogram into 2 classes
plot(fit, labels = cludat$horizon)
groups = cutree(fit, k=2)
# Draw blue boxes around the 2 classes
rect.hclust(fit, k=2, border="blue")
