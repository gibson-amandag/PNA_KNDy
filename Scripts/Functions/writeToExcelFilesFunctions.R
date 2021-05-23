####Write to Excel Files 
#uses openxslx

writeToWorkbook = function(sheetName, df, wb){
  addWorksheet(wb, sheetName)
  writeDataTable(wb, sheetName, df, tableStyle = "TableStyleMedium2")
}