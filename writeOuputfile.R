testData<-read.csv("C:/Users/alvd/Documents/Ordina/Kaggle/test.csv",colClasses=c(rep("character",23)))

for(col in columnsToUse){  
  newColName<-paste0(col,"Prepped")
  testData[,newColName]<-testData[,col]
  
  isNewValue<-!(testData[,newColName] %in% unique(trainSet[,col]))
  testData[isNewValue,newColName]<-NA
  print(paste0(newColName," NA:", sum(isNewValue)))
}

predictions<-predict(model,testData,type="response")
predictions[is.na(predictions)]<-0.17

outputFrame <- data.frame(id=testData$id,click=predictions)
write.csv(outputFrame,"outputFile.csv",row.names=F,quote=F)
