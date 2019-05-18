
# The excelFiles function takes all the results and calculations from the calc function and converts the tables to and Excel file with multiple tabs.
# This function also allows users to define the path they want the excel file to follow and also allows them to choose whether or not to allow the fnction to overwrite an already exsiting file.
# Each tab is a specific chemical calculation needed by the INL analytical team.



#' @title Create excel file for the quadrapole calculations and data
#'
#' @param dat the calculations outputted from the `calc()` function
#' @param rewrite 
#' @param mypath will determine where we save the excel file. Entered as a string.
#'
#' @example excelFiles(dat, mypath = "", rewrite = TRUE)
#' @export  

excelFiles <- function(dat = calculations, mypath = NULL, rewrite = TRUE){
  excelFile <- createWorkbook()
  
  addWorksheet(excelFile, "Raw Data")
  addWorksheet(excelFile, "Dilution Factor")
  addWorksheet(excelFile, "IQL")
  addWorksheet(excelFile, "MQL")
  addWorksheet(excelFile, "MQL vs SampleConc")
  addWorksheet(excelFile, "Averages")
  addWorksheet(excelFile, "Error %")
  addWorksheet(excelFile, "Results")
  addWorksheet(excelFile, "Internal Standards")
  addWorksheet(excelFile, "Check Standards")
  
  
  writeData(excelFile, sheet = "Raw Data", x = dat$`Raw Data`)
  writeData(excelFile, sheet = "Dilution Factor", x = dat$`Dilution Factor`)
  writeData(excelFile, sheet = "IQL", x = dat$IQL)
  writeData(excelFile, sheet = "MQL", x = dat$MQL)
  writeData(excelFile, sheet = "MQL vs SampleConc", x = dat$`MQL vs SampleConc`)
  writeData(excelFile, sheet = "Averages", x = dat$Averages)
  writeData(excelFile, sheet = "Error %", x = dat$`Error %`)
  writeData(excelFile, sheet = "Results", x = dat$Results)
  writeData(excelFile, sheet = "Internal Standards", x = dat$`Internal Standards`)
  conditionalFormatting(excelFile,"Internal Standards",6,
                        rows = (2:length(dat$`Internal Standards`$`% Difference`)), 
                        rule = ">=10",style = createStyle(bgFill= "#fce50c"))
  conditionalFormatting(excelFile,"Internal Standards",6,
                        rows = (2:length(dat$`Internal Standards`$`% Difference`)), 
                        rule = ">=20",style = createStyle(bgFill= "#b20303"))
  conditionalFormatting(excelFile,"Internal Standards",6,
                        rows = (2:length(dat$`Internal Standards`$`% Difference`)), 
                        rule = "<=-20",style = createStyle(bgFill= "#b20303"))
  conditionalFormatting(excelFile,"Internal Standards",6,
                        rows = (2:length(dat$`Internal Standards`$`% Difference`)), 
                        rule = "<=-10",style = createStyle(bgFill= "#fce50c"))
  
  print(dat$Plots)
  insertPlot(excelFile, sheet = "Internal Standards", xy = c("L", 2))
  writeData(excelFile, sheet = "Check Standards", x = dat$`Check Standards`)
  
  
  
  saveWorkbook(excelFile, mypath, overwrite = rewrite)
  
}