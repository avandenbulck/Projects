ctrData<-read.csv("../Data/sample.csv",colClasses=c(rep("character",24)))

#The weird name gave problems
ctrData$id<-ctrData[,1]

ctrData$clickNum<-as.numeric(ctrData$click)
target<-"clickNum"
columnsToUse<-
  c(
    "C1",
    "banner_pos",
    "device_type",
    "device_conn_type",
    "C15",
    "C16",
    "C18"
    )

#assert
isColNotInNames<- !(columnsToUse %in% names(ctrData))
if(sum(isColNotInNames)) print("!!WRONG COLUMN NAME IN LIST!!")

#################
# Create formula
#################
ctrFormula<-paste0(target,"~",columnsToUse[1])
for(col in columnsToUse[2:length(columnsToUse)])
  ctrFormula<-paste0(ctrFormula,"+",col)

ctrFormula<-formula(ctrFormula)


###############
# Create train & test
###############
sizeTrain <-100000
sizeTest <- 1000000 
trainSet<-ctrData[1:sizeTrain,]
testSet<-ctrData[(sizeTrain+1):(sizeTrain+1+sizeTest),]

#assert
if(sum(trainSet$id %in% testSet$id) > 0) print(!!"SAME ID IN TRAIN AND TEST!!")

#################
# Prep trainset
#################
for(col in columnsToUse){
  sortedNames<-names(sort(table(trainSet[,col]),decreasing = T))
  top10SortedNames<-sortedNames[1:min(10,length(sortedNames))]
  valueInTop10 <- trainSet[,col] %in% top10SortedNames
  
  newColName<-paste0(col,"Prepped")
  trainSet[,newColName]<-trainSet[,col]
  trainSet[,!valueInTop10]<-"Other"
}

###########################
# TRAIN MODEL
###########################
model<-glm(formula=ctrFormula,family = binomial(link="logit"),data=trainSet,control = list(maxit = 100),x=F,y=F)
###########################
# TEST MODEL
###########################

for(col in columnsToUse) {
  isNewValue<-!(testSet[,col] %in% unique(trainSet[,col]))
  testSet[isNewValue,col]<-NA
  print(paste0(col," NA:", sum(isNewValue)))
}

#predictions<-predict(model,testSet,type="response")
predictions<-predict(model,testSet,type="response",na.action = na.pass)
predictions[is.na(predictions)]<-0.17

llfun(actual = testSet$clickNum,prediction = predictions)



