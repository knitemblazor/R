#install.packages('ISLR')
library(ISLR)

print(College[1:4,1:3])

# Create Vector of Column Max and Min Values
maxs <- apply(College[,2:8], 2, max)
mins <- apply(College[,2:8], 2, min)

#print(mins)
# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(College[,2:8],center = mins, scale = maxs - mins))
#print(scaled.data)
# Check out results
#print(head(scaled.data,2))


# Convert Private column from Yes/No to 1/0
Private = as.numeric(College$Private)-1
#print(Private)
data = cbind(Private,scaled.data)
#print(data)
library(caTools)
#set.seed(101)

# Create Split (any column is fine)
split = sample.split(data$Private, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('Private ~',f)
print(f)
# Convert to formula
f <- as.formula(f)
#install.packages('neuralnet')
library(neuralnet)
nn <- neuralnet(f,train,hidden=c(0),linear.output=FALSE)
# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[,2:8])

# Check out net.result
print(head(predicted.nn.values$net.result))

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
#confusion matrix
q<-table(test$Private,predicted.nn.values$net.result)
print(q)
plot(nn)
#install.packages('caret')

