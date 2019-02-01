### Template for REU Lesson 7/10/2017 ###

## Intro ##

# 0.0 Look at our data #
# Can use Notepad, Notepad++, Excel, etc.

# 0.1 Look at R-studio #
# Open R-studio, familize with enviroment
# Do some basic command in the CONSOLE
expression_t0 = 2.5
expression_t1 = 3.3
fc = expression_t0/expression_t1
fc
expression_t1 = 1.8
fc
# Note: Naming convetions
# -Always start with a lowerscse letter
# -Sperate words by underscores
# -Use lower case letters, numbers, and underscore only

# Note: Start off in the console, use script laters


## 1: Working with dataframes in R ##

# 1.0 Set working directory #
# Note: Have them look at the contents of their directory

# Full Path
setwd("C:/Users/Nicholas/Desktop/athala_circaidan_rythm")
#setwd('/Users/cbazodi/Desktop/athala_circaidan_rythm/')  # For mac users
dir()

# Short cut 
setwd("~/../Desktop/athala_circaidan_rythm/")
#setwd('~/Desktop/athala_circaidan_rythm/')   # For mac usersdir()
dir()

# 1.1 Open our data file in R #
# Note: Show them how to do tab completion 

# Error 1 --> Incorrect # of element because we didn't specify a tab character
stress_data = read.table("Athal_heat_drought_rma.txt")

# Error 2 --> Didn't specify column names (make them look at their data)
stress_data = read.table("Athal_heat_drought_rma.txt",sep="\t")

# Correct
stress_data = read.table("Athal_heat_drought_rma.txt",sep="\t",header=TRUE)

# 1.2 Look at our data #

# Contents
head(stress_data)

# "Class" of data
class(stress_data)

# Dimensions of data
# Note: First rows then columns
dim(stress_data)

# 1.3 Slicing Data #
# General format var_name[row,column]

# Look at the first value
stress_data[1,1]

# Look at first numerical value
stress_data[1,2]

# Look at first row
stress_data[1,]

# Look at first column
stress_data[,1]

# Get a range of values
stress_data[1,1:7]

# Get a non-continious set of values
stress_data[1,c(1,2,8,11)]

# Combing different ways of slicing
# Note: Copy paste from above to to show how to take advantage of that
stress_data[1:10,c(1,2,8,11)]

# Slicing by column name
stress_data[1:10,c("Gene","heat.Rep1","heat.Rep2","heat.Rep3","heat.Rep4")]

# 1.4 Working with Data #

# First, lets make a new variable 
zt2_head <- stress_data[1:10,c("Gene","heat.Rep1","heat.Rep2","heat.Rep3","heat.Rep4")]
zt2_head

# What if we wanted to get rid of the last lines with 0 exprssion
zt2_head <- zt2_head[c(-2,-3,-8,-9,-10),]
zt2_head
# IMPORTANT: Note that negative indexing works different in R compared
# to python and some other languages

# Find max value
# Error 1 -- Only work for numeric values, and we have gene name
max(zt2_head)

# Fixed
zt2_head <- zt2_head[,-1]
zt2_head

# Max
max(zt2_head)

# Min
min(zt2_head)

# Median
median(zt2_head[,1])

# Mean
mean(zt2_head[,1])

# Sumamry
summary(zt2_head)

# What if we want mea for more than one row?
mean(zt2_head[,1:2])
# Doesn't work b/c diff data types
class(zt2_head[,1])
class(zt2_head[,1:2])

# Instead, yse apply
# Format: apply(data_frame,axis,function) where axis = 1 --> rows and axis = 2 --> columns
apply(zt2_head,1,mean)
zt2_head

# Means
apply(zt2_head,1,mean)
apply(zt2_head,2,mean)

# What if you want if the mean of the whole thing?
# Convert to matrix:
mean(as.matrix(zt2_head))

# 1.5 Plotting #

# Lets look at the average across our time points subset
x_values = seq(1,12,1)
plot(x_values,stress_data[1,-1])
barplot(stress_data[1,-1])

# This is kind of ulgy, so lets clean this up
plot(x_values,stress_data[1,-1],ylab="Avg expression",ylim=c(-2,2),col="red",pch=16)

# Lets plot the expression pattern of a single gene
par(new=T)
plot(x_values,stress_data[2,-1],ylab="Avg expression",ylim=c(-2,2),col="blue",pch=16)
# Here is where things go wrong...
par(new=T)
plot(x_values,stress_data[10,-1],ylab="Avg expression",ylim=c(-2,2),col="green",pch=16)

# Show that the data is off of the 0 to 20 scale, that why nothing shows up
stress_data[10,-1]

# Try plotting on new scale to show that it makes a mess
par(new=T)
plot(x_values,stress_data[10,-1],ylab="Avg expression",col="green",pch=16)

## 2: Functions and Loops ##

# 2.0 Basic Functions #
# To make nicer plots, let scale our data
# What does scaling look like?
# Want value between 0 and 1 --> (x-min)/(max-min)

test_v <- c(1,2,3)
min_v <- min(test_v)
max_v <- max(test_v)
scale_1 <- (1-min_v)/(max_v-min_v)
scale_1
scale_2 <- (2-min_v)/(max_v-min_v)
scale_2
scale_3 <- (3-min_v)/(max_v-min_v)
scale_3

# Lets use a function to make our lives easier
scale_01 = function(x,min_v,max_v){
  scaled_value = (x-min_v)/(max_v-min_v)
  return(scaled_value)
}
scale_01(2,1,3)
scale_01(1.77,1,3)
scale_01(4,1,3)

# 2.1 Basic Loops #
# Now that we have a function we can scale our vector
scale_01(stress_data[1,2],min(stress_data[1,-1]),max(stress_data[1,-1]))
scale_01(stress_data[1,3],min(stress_data[1,-1]),max(stress_data[1,-1]))

# This take awhile by hand.
# We can speed thigns up with a loop
v_min <- min(stress_data[1,-1])
v_max <- max(stress_data[1,-1])
for (v in stress_data[1,-1]){     # Explain the idea of a loop
  print(scale_01(v,v_min,v_max))
}   

# 2.2 Making variables with loops #
scaled_values <- matrix(,nrow=1,ncol=12) # Mention the warning
index = 1
for (v in stress_data[1,-1]){
  scaled_values[index] <- scale_01(v,v_min,v_max)
  index = index + 1
}
scaled_values
plot(x_values,scaled_values,col="red",pch=16)

# 2.3 Composing functions #
# Lets put our loops and function together to makes things even easier

scale_expression <- function(expr_vector){
  # Takes a vector of expression values and scales them so that the max is 1 and the min is 0
  v_max = max(expr_vector)
  v_min = min(expr_vector)
  v_len = dim(expr_vector)[2]
  scaled_values <- matrix(,nrow=1,ncol=v_len)
  index = 1
  for (value in expr_vector){
    scaled_values[index] <- scale_01(value,v_min,v_max)
    index = index + 1
  }
  return(scaled_values)
}

# And lets make a function to plot things more efficient
plot_expression <- function(x_values,expr_vector,color){
  scaled_values <- scale_expression(expr_vector)
  plot(x_values,scaled_values,ylab="Expresison value",pch=16,col=color)
}
plot_expression(x_values,stress_data[1,-1],"red")
par(new=T)
plot_expression(x_values,stress_data[2,-1],"blue")
par(new=T)
plot_expression(x_values,stress_data[3,-1],"green")
# Change pch=16 to type="l" and replot for example of function flexibility

## 3: Making dataframes and writing output ##

# In the next part of our tutorial, we want to identify genes which similar
# circadian rythm. To do this we want to compare the AVERAGE expression under
# each stress using a metric called fold change. To do this, we need to
# accomplish the following tasking:
#   1. Average replicates for each condition
#   2. Calculate fold-change for each stress relative to the control (ZT2)
#   3. Apply this to every gene in our data set


# First, write a function a average replicates

# Test function
average_reps = function(expression_vector){
  for (v in c(1,5,9)){
    print(expression_vector[c(v,v+1,v+2,v+3)])
  }
}
average_reps(stress_data[1,-1])

# Actual function
average_reps = function(expression_vector){
  replicate_averages <- matrix(,nrow=1,ncol=3)
  index = 1
  for (v in c(1,5,9)){
    replicate_averages[index] <- mean(as.matrix(expression_vector[c(v,v+1,v+2,v+3)]))
    index = index + 1
  }
  return(replicate_averages)
}
gene1_avg = average_reps(stress_data[1,-1])
gene1_avg

# Next, write a function to calculate fold-change
division = function(num,div){return(num/div)}

fold_change = function(expression_vector){
  fc_values <- apply(as.matrix(expression_vector),1,division,div=expression_vector[1])
  return(fc_values)
}
fold_change(gene1_avg)

# Finally, dp this for all changes
# This is the fast, complicated way

# Define average values
average_matrix <- apply(stress_data[,-1],1,average_reps)
average_df <- data.frame(t(average_matrix)) # For some reason, apply transponse the matrix, so...

# Define fc values
fc_matrix <- apply(average_df,1,fold_change)
fc_df <- data.frame(t(fc_matrix))
fc_df$Gene <- stress_data$Gene

# Finally, do this for all changes
# This is the slow, simple way
fc_matrix <- matrix(,nrow=dim(stress_data)[1],ncol=3)
for (index in seq(1,dim(stress_data)[1])){
  row_average <- average_reps(stress_data[index,-1])
  row_fc <- fold_change(row_average)
  fc_matrix[index,] <- t(row_fc)
}
fc_df <- data.frame(fc_matrix)
fc_df$Gene <- stress_data$Gene
rownames(average_df) <- stress_data$Gene


### 4: Group genes by expression pattern

# In this part of the tutorial we will use k-means clustering to 
# separate genes into co-expression groups. Co-expression groups
# can be used to discover novel genes involved in a pathway "guilt-
# by-association", to elucidate how the genes are regulated, or to
# assist in functional annotation predictions.

#install.packages("fpc")
#install.packages("ggplot2")
#install.packages("reshape")
library(fpc)
library(ggplot2)
library(reshape)

# Clean up the fold change data frame for clustering
fc_df_c <- fc_df[,-4]
rownames(fc_df_c) <- fc_df[,4] 
names(fc_df_c) <- c('control','heat','drought')
fc_df_c$control <- NULL

# Generate and inspect kmeans-clusters
c <- kmeans(fc_df_c, 5, iter.max = 100, nstart = 50)
c$size

# Merge cluster information with FC data
cluster <- as.matrix(c$cluster)
cluster <- merge(cluster, fc_df_c, by ='row.names')

# "Melt" data for plotting
cluster_m <- melt(cluster, id = c('Row.names','V1'))
g <- ggplot(cluster_m, aes(x=variable, y=value, fill=variable))
g + geom_boxplot(outlier.shape = NA)+
  geom_hline(yintercept=0, linetype="dashed")+ 
  facet_wrap(~V1)


# How can we transform to the data so that our clusters are based more on the response pattern
# and less on the scale of the log-fold change?

# Log transform the data:
log(1000)
log(3)
log(-1000)

# Packages that will calculate log-fold change for negative numbers, but today we will use a 
# log-like transformation called the inverse hyperbolic sin that is defined for all real numbers

asinh(1000)
asinh(3)
asinh(-1000)

# Apply to all the data
log_fc <- asinh(fc_df_c)  

# Generate and inspect kmeans-clusters
c_log <- kmeans(log_fc, 12, iter.max = 100, nstart = 50)
c_log$size


# Make a merged dataframe with the cluster information and the FC data
cluster <- as.matrix(c_log$cluster)
cluster <- merge(cluster, log_fc, by ='row.names')

# "Melt" data for plotting
cluster_m <- melt(cluster, id = c('Row.names','V1'))
g <- ggplot(cluster_m, aes(x=variable, y=asinh(value), fill=variable))
g + geom_boxplot(outlier.shape = NA)+
  geom_hline(yintercept=0, linetype="dashed") + 
  facet_wrap(~V1)

# Output cluster information to a file
write.table(cluster, 'cluster_output_asinh.csv',row.names=FALSE, sep=",")


### If we have time... ####

## Assess the "spread" of our clusters, how well separated is the data?
plotcluster(log_fc,c_log$cluster)
plotcluster(fc_df_c, c$cluster)



## How correlated are heat and drought responses?
cor <- ggplot(cluster, aes(x=heat, y=drought)) 
cor + geom_point()

# How are our clusters breaking down?
cluster$V1 <- as.factor(cluster$V1)
cor <- ggplot(cluster, aes(x=heat, y=drought, color=V1)) 
cor + geom_point()


# Venn Diagram of gene responses
install.packages('VennDiagram')
library(VennDiagram)

heat <- length(which(log_fc$heat >= 1 & log_fc$drought < 1))
drought <- length(which(log_fc$drought >=1 & log_fc$heat < 1))
both <- length(which(log_fc$heat >= 1 & log_fc$drought >=1))


grid.newpage()
draw.pairwise.venn(area1 = heat, area2=drought, cross.area=both,
                   category = c('Heat', "drought"),
                   fill = c('red', 'blue'), alpha = 0.4)



