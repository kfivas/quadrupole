
# The read_rep function works with quadrapole data output by the ELAN DRC-2 machiene. 
# The ELAN DRC-2 outputs the chemical data in a .rep file. This .rep file is output in a table format that is pretty to look at, and not easy to work with when wanting to code and manipulate the data.
# So the read_rep function finds unique information and titles that is always output by the machiene in order for us to organize and set up a .csv file that is easy to work with when needing to manipulate data. 
# Once the data is beautifully organized in a .csv file, the columns are set to either numeric or character as needed. 

#' @title Read in rep files from quadrapole machine
#'
#' @param file = path to the .rep file you need. Entered as a string.
#'
#' @example read_rep(file = "")
#' @export  

read_rep <- function(file = NULL){
  dat <- read_lines(file = file)
  
  # find each sample id chunk
  endofeachsample <- dat %>% 
    str_which("Quantitative Analysis - Summary Report") %>%
    .[-1] - 1
  
  endofeachsample <- c(endofeachsample, length(dat))
  
  startofeachsample <- dat %>% 
    str_which("Quantitative Analysis - Summary Report") %>%
    .[] + 1
  
  
  topofeachsample <- dat %>% 
    str_which("Concentration Results") %>%
    .[] - 1
  
  
  
  bottomofeachsample <- dat %>% 
    str_which("Concentration Results") %>%
    .[] + 1
  
  datDF <- vector("list",length(startofeachsample))
  
  for (i in seq_along(startofeachsample)) {
    topHalf <- dat[startofeachsample[i]:topofeachsample[i]]
    bottomHalf <- dat[bottomofeachsample[i]:endofeachsample[i]]
    
    # Processing top half
    topHalfLines <- topHalf %>% str_c(collapse = "\n")
    
    topHalf_csv <- suppressWarnings(
      read_csv(topHalfLines, col_names = TRUE, skip = 4))
    addColumnName <- rename(topHalf_csv, "Symbol" = "X1" )
    
    value <- str_replace(topHalf[1], "\"Sample ID:\",\"", "") %>%
      str_replace("\\\"", "")
    
    sampleIDvalue <- data.frame(value) %>%
      rename("Sample ID" = "value")
    
    
    topFinal <- cbind(sampleIDvalue, addColumnName)
    
    # Processing bottom half
    
    bottomHalfLines <- bottomHalf %>% str_c(collapse = "\n")
    
    bottomHalf_csv <-  suppressWarnings(
      read_csv(bottomHalfLines, col_names = TRUE))
    addColumnName2 <- dplyr::rename(bottomHalf_csv, Symbol = X1)
    
    bottomFinal <- cbind(sampleIDvalue, addColumnName2)
    
    mergeData <- topFinal %>%
      left_join(bottomFinal, by = c("Sample ID", "Symbol", "Analyte", "Mass")) %>%
      mutate_all(as.character)
    
    datDF[[i]] <- mergeData
  }
  
  dat <- datDF %>%
    map_df(bind_rows)
  
  dat$`Meas. Intens. Mean` <- as.numeric(dat$`Meas. Intens. Mean`)
  dat$`Meas. Intens. RSD` <- as.numeric(dat$`Meas. Intens. RSD`)
  dat$`Blank Intensity` <- as.numeric(dat$`Blank Intensity`)
  dat$`Blank Intens. RSD` <- as.numeric(dat$`Blank Intens. RSD`)
  dat$`Net Intens. Mean` <- as.numeric(dat$`Net Intens. Mean`)
  dat$`Conc. Mean` <- as.numeric(dat$`Conc. Mean`)
  dat$`Conc. SD` <- as.numeric(dat$`Conc. SD`)
  dat$`Conc. RSD` <- as.numeric(dat$`Conc. RSD`)
  
  dat$`Sample ID` <- trimws(dat$`Sample ID`, which = "right")
  dat
}