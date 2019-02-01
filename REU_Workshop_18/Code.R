###################################
############ R Basics #############
###################################

### Assigning variables and doing math
t0 <- 2.5
t1 <- 3.3
fc <- t0/t1
fc
t1 <- 1.8
fc
fc <- t0/t1
fc

### Making and working with lists
l <- c(1, 2, 5, 5, 1, 7)
mean(l)
median(l)
l2 <- l*10
l2

### Set a working directory and load in some data
# PC users
setwd("C:/Users/Christina/Desktop/REU_Workshop")
# Mac users
setwd('/Users/cbazodi/Desktop/REU_Workshop/')
getwd()

d <- read.table('starch_data.txt', header=TRUE, sep='\t')

# Checking out your data
head(d) # look at the first 5 rows of your data
summary(d) # gives you a summary of each column in the dataset
d$starch

### Subset data
# Note: & = and, | = or
d_WT_MA <- subset(d, genotype == 'WT' | genotype == 'MutA') # Subset data by keeping rows where genotype is WT or gi-2.

### Basic plots
# plot(Y ~ X, dataset)
plot(starch ~ genotype, data=d)


###################################
###### Basic statistics in R ######
###################################

### T-test
# Determine if the mean starch values for WT and gi-2 are significantly different.
t.test(starch ~ genotype, data = d_WT_MA)

### ANOVA
# Determine if there are significant differences between the genotypes using a One-Way ANOVA 
fit <- aov(starch ~ genotype, data = d)
summary.aov(fit)
#Df= degrees of freedom (# of variables -1), residuals (# observations - # groups)
#Sum Sq= sum of squares (sum(yi-y)^2)
#Mean Sq= SS/df (sample variance)
#F value= observed(SS/df)/residual(SS/df)
#Pr(>F) - probability of observed F value give H0 is true


### Tukeys HSD
# Now you know that there is a significant difference, but which ones are different?
tuk <- TukeyHSD(fit)
tuk
plot(tuk)

## Now lets try building a linear model  are genotypes the same or different?
lin.fit = lm(starch ~ genotype, data = d)
summary(lin.fit)
#plot(lin.fit)
#Residuals are essentially the difference between the actual observed response values he response values that the model predicted
#residuals- look for symmetrical distribution across these points on the mean value zero (0)
#residuals
lin.fit.res = resid(lin.fit)
#plot against observed values
plot(d$starch, lin.fit.res, ylab="Residuals", xlab="Starch", main="Residuals vs. Observed values")
abline(0, 0)
#intercept- sort of the average of the model
#each variable is a slope.. are the slopes significantly different from the average?
#estimate- how different are you from the average (instercept)
#error- standard error (measure of variance- how much to coeffecients vary from the average)
#t-value- how many standard deviations our coefficient estimate is far away from 0- the further away,
##the more likely we reject the null
#Pr(>ltl) probability of observing any value equal or larger than t. (probability observed value falls within the H0)
#Df= degrees of freedom (# of variables -1), residuals (# observations - # groups)
#Sum Sq= sum of squares (sum(yi-y)^2)
#Mean Sq= SS/df (sample variance)
#F value= observed(SS/df)/residual(SS/df)


### Correlation
cor(l, l2)
# What if we just get a random list of 6 numbers between 1 and 10?
l3 <- sample(1:10, 6, replace=T)
cor(l, l3)

### Are the starch levels in Mutant B correlated with the starch levels in the WT plants?
wt <- subset(d, genotype == 'WT')
MB <- subset(d, genotype == 'MutB')
wt_values <- wt$starch
MB_values <- MB$starch

plot(wt_values, MB_values)
MB_cor <- cor(wt_values, MB_values)

# Is that higher or lower than you would expect by random chance?
random <- sample(1:10, 8, replace=T)
cor(wt_values, random)

# How could you answer that in a more statistically sound way?
cor_list <- c()
for (i in 1:100){
  random <- sample(1:10, 8, replace=T)
  correl <- cor(wt_values, random)
  cor_list <- c(cor_list, correl)
}
cor_95th <- quantile(cor_list, 0.95)

MB_cor > cor_95th


# Really, before doing any type of statistic, you need to make sure your data fulfills some assumptions
# 1. data is normally distributed
# 2. Equal variance around the means
# 3. Independence - your variables are independent of each other

plot(starch ~ genotype, data=d)



###################################
# Differential expression (edgeR) #
###################################

# Download and load the package and data

# ONLY RUN ONCE
# If asked to update packages select "all" (may take a few minutes)
#source("http://bioconductor.org/biocLite.R")
#biocLite("edgeR") #  to dowload the package
# Run every time to you start a new R session.
library(edgeR)

# Download the expression data  
counts <- read.table('counts_data.csv', header=T, row.names='gene', sep=',')
head(counts)
summary(counts$control_1) # Get a summary of the expression levels for the first replicate of the control treatment


# You first need to tell edgeR where to look for your data using DGEList
# DGEList is a data container used by edgeR to organize this information
# To define the treatment groups, you need to tell EdgeR what columns belong
# to the same treatment group (meaning they are replicates) 
treatments <- c("C","C","C","Cold","Cold","Cold")
dge <- DGEList(counts, group=treatments, genes=rownames(counts))

# You can visualize the frequency distribution of the counts using a histogram.
hist(dge$counts, breaks=50, xlab = "Counts")

# Wow, a few genes have very large counts, but most are small!
# Try log transforming the data to get a better look at the distribution.
hist(log2(dge$counts), xlim=c(0, 20), breaks=50, xlab = "LogCounts")


# Normalize between samples
# The goal is to normalize the counts across samples so that MOST genes have 
# the same abundance across the different samples. The idea is that while you
# expect some genes will be differentially expressed across the datasets, most
# genes will not be affected by your treatment. We can use those "unchanging" 
# genes to normalize between the samples. 
dge <- calcNormFactors(dge) #This calculates the effective library size and normalzization factors. 
dge$samples 



# Biological Coefficient of Variation (BCV)
# Because of the way variance is calculated, genes with higher expression have higher variance.
# The BCV corrects for this by taking mean expression level into consideration. 
# The BCV is determined by estimating the common (across all genes in all samples) and 
# tagwise (across all samples for an individual gene) dispersal. 
# A common BCV between 0.2-0.4 is consideredgood enough to be able to identify differentially expressed genes. 

# Estimate the BCV between biological replicates (also called dispersion).
dge <- estimateCommonDisp(dge)
dge <- estimateTagwiseDisp(dge)
summary(dge$counts[,'control_1'])
summary(dge$pseudo.counts[,'control_1'])



###### Use the Fisher's Exact test to look for differentially expressed genes. #######

# First we'll look for genes differentially expressed between control & cold treatment

# The Fisher's exact test takes into account the Tagwise Dispersian when calculating the p-value
c_vs_cold <- exactTest(dge, pair=c('C','Cold')) 

# Since we are calculating significance for thousands of genes, we need to use false
# discovery rate correction. We'll use the Benjamini-Hochberg (BH) method.
# We are saving the corrected p-values in a new column in the c_vs_cold$table called "FDR"
c_vs_cold$table$FDR <- p.adjust(c_vs_cold$table$PValue, method='BH') 

# Select the genes that are significantly differentially expressed (q-value <= 0.05) and have a logFC > 2 or < -2
c_vs_cold_genes <- subset(c_vs_cold$table, c_vs_cold$table$FDR <= 0.05 & abs(c_vs_cold$table$logFC) >=2)

# Check out the relationship between logFC and significance by making a "volcano" plot:
plot(-log(c_vs_cold_genes$FDR)~c_vs_cold_genes$logFC, 
     ylab= "logFDR", xlab = "logFC") 

# Save your results (you do not need to turn these in, but you may want to refer to these results later)
write.table(c_vs_cold$table,file="Control_vs_Cold.txt",quote=FALSE,sep="\t",row.names=TRUE) #quote=FALSE, to remove the "" that are automatically added on. 




