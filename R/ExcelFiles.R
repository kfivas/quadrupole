
# The excelFiles function takes all the results and calculations from the calc function and converts the tables to and Excel file with multiple tabs.
# This function also allows users to define the path they want the excel file to follow and also allows them to choose whether or not to allow the fnction to overwrite an already exsiting file.
# Each tab is a specific chemical calculation needed by the INL analytical team.



#' @title Create excel file for the quadrapole calculations and data
#' @param dat the calculations outputted from the `calc()` function
#' @param mypath will determine where we save the excel file. Entered as a string.
#' @param Rewrite lets the user decide if the function will be allowed to 
#' overwrite an already used file. Entered as a logical.
#' @example excelFiles(dat, mypath = "", rewrite = TRUE)
#' @export  

excelFiles <- function(dat = calculations, mypath = NULL, rewrite = TRUE){
  excelFile <- createWorkbook()
  
  addWorksheet(excelFile, "Raw Data")
  addWorksheet(excelFile, "Dilution Factor")
  addWorksheet(excelFile, "Solution Concentration")
  addWorksheet(excelFile, "Sample Concentration")
  addWorksheet(excelFile, "IQL")
  addWorksheet(excelFile, "MQL")
  addWorksheet(excelFile, "MQL vs SampleConc")
  addWorksheet(excelFile, "Averages")
  addWorksheet(excelFile, "Error %")
  addWorksheet(excelFile, "Results")
  addWorksheet(excelFile, "Blanks")
  addWorksheet(excelFile, "Internal Standards")
  
  writeData(excelFile, sheet = "Raw Data", x = dat$`Raw Data`)
  writeData(excelFile, sheet = "Dilution Factor", x = dat$`Dilution Factor`)
  writeData(excelFile, sheet = "Solution Concentration", x = dat$`Solution Concentration`)
  writeData(excelFile, sheet = "Sample Concentration", x = dat$`Sample Concentration`)
  writeData(excelFile, sheet = "IQL", x = dat$IQL)
  writeData(excelFile, sheet = "MQL", x = dat$MQL)
  writeData(excelFile, sheet = "MQL vs SampleConc", x = dat$`MQL vs SampleConc`)
  writeData(excelFile, sheet = "Averages", x = dat$Averages)
  writeData(excelFile, sheet = "Error %", x = dat$`Error %`)
  writeData(excelFile, sheet = "Results", x = dat$Results)
  writeData(excelFile, sheet = "Blanks", x = dat$Blanks)
  writeData(excelFile, sheet = "Internal Standards", x = dat$`Internal Standards`)
  
  
  saveWorkbook(excelFile, mypath, overwrite = rewrite)
  
}