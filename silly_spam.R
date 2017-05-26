#-----------------------------------------------
# Ex.1: Build and apply logistic regression model
#-----------------------------------------------
getwd() # check you working directory
# place spamD.tsv file into working directory
spamD <- read.table('spamD.tsv', header=T, sep='\t')
names(spamD)
dim(spamD)

spamTrain <- subset(spamD, spamD$rgroup >= 10)
spamTest <- subset (spamD, spamD$rgroup<10)
dim(spamTrain)
dim(spamTest)

spamVars <- setdiff(colnames(spamD), list('rgroup','spam'))

spamFormula <- as.formula(paste('spam=="spam"',
	paste(spamVars, collapse=' + '), sep=' ~ '))
spamFormula

# build a logistic regression model
# that evaluates probability of belonging to each class
spamModel <- glm(spamFormula, family=binomial(link='logit'),
		data=spamTrain)

# generate predictions for training and test data
# using the spamModel
spamTrain$pred <- predict(spamModel, newdata=spamTrain,
	type='response')
spamTest$pred <- predict(spamModel, newdata=spamTest,
	type='response')

# note: columns are predicted class and rows are actual class
print(with(spamTest, table(y=spam, glmPred=pred>0.5)))

#-----------------------------------------------
# Ex.2: Create confusion matrix
#-----------------------------------------------
cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5)
print(cM)

#-----------------------------------------------
# Ex.3: Performance Measures derived from confusion matrix
#-----------------------------------------------
# enter confusion matrix data by hand
# for the Akismet filter that uses link destination clues
# and determination from other websites
# in addition to text features

ct <- as.table(matrix(data=c(288-1,17,1,13882-17), nrow=2, ncol=2))
rownames(ct) <- rownames(cM)
colnames(ct) <- colnames(cM)
print(ct)

#Write an R code to compute performance measures 
#(Accuracy, Precision, Recall, Sensitivity, Specifcity, F1-measure)
#for the Akismet filter for spam vs. non-spam emails .

#Report the values of each performance measure.

#How do these values compare with the measures derived
#from cM of the glmModel?

#-----------------------------------------------
# Ex.4: ROC Curve
#-----------------------------------------------
# install ROCR package and load it
library('ROCR')
eval <- prediction(spamTest$pred, spamTest$spam)

plot(performance(eval,"tpr","fpr"))

print(attributes(performance(eval,'auc'))$y.values[[1]])

# Given the confusion matrix for the Akismet spam filter,
# is the ROC curve what one would expect?

# What is the AUC value of this filter?
#  AC - 0.9660072
# Is it close to a perfect classifier? Why?

#-----------------------------------------------
# Ex.5: Log Likelihood estimation for the Null model
#-----------------------------------------------
# the number of known spam emails
pNull <- sum(ifelse(spamTest$spam=='spam',1,0))
pNull

# rescale pNull by the number of points
# to give a rough average surprize per point
dim(spamTest)[1]
pNull <- pNull/dim(spamTest)[1]
pNull


sum(ifelse(spamTest$spam=='spam',1,0))*log(pNull)+
  sum(ifelse(spamTest$spam=='spam',0,1))*log(1-pNull)
          
# What is the log likelihood estimation for the Null model?

#-----------------------------------------------
# Ex.6: Log Likelihood estimation for the glm spam model
#-----------------------------------------------
sum(ifelse(spamTest$spam=='spam',
           log(spamTest$pred),
           log(1-spamTest$pred)))

# What is the log likelihood estimation for the glm model?
# Is it better than the null model? How do you know that?

# scaled by the number of data points
sum(ifelse(spamTest$spam=='spam',
           log(spamTest$pred),
           log(1-spamTest$pred))) / dim(spamTest)[1]

