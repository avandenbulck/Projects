for(col in names(ctrData)[-1]){
  sortedTable<-sort(prop.table(table(ctrData[,col])),decreasing = T)
  print(paste0(col,":",sum(sortedTable[1:10],na.rm = T)))
}