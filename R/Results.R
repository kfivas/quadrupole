
#' @title Combines the read_rep, calc, and excelfiles Functions.
#' @param file_in = path to the .rep file you need. Entered as a string.
#' @param weights = a path to the .csv file with the needed weights. Entered as a string.
#' @param file_out = a string name of what you want the function to output the file as. Entered as a string.
#' @example results(file_in = "", weights = , file_out)
#' @export 

results <- function(file_in = NULL, weights = NULL, file_out = NULL){
  require(mosaic)
  require(tidyverse)
  require(readr)
  require(fs)
  require(openxlsx)
  require(stringr)
  require(quadrupole)
  
  dat <- quadrupole::read_rep(file_in)
  print("File digested")
  
  calculations <- quadrupole::calc(dat, weights)
  print("calculations done")
  
  excelFiles(calculations, mypath = file_out)
  print(paste0("Excel file created at ", file_out))
}