
# The calc function works with the .csv file output from the read_rep function and combines that data with a .csv file with different chemical weights.
# The calc function runs through the chemical analysis of multiple calculations to find the results INL vendors are looking for.
# It starts by joining the two .csv files together. We use a left join with the weights file on the left because that returns only the Sample ID's we want and need for the chemical calculations and analysis.
# The function then goes through the chemical calculations the INL quadrapole team needs in order to run a chemical analysis and interpretation.
# After it runs through all the calculations, we end up with 10 different data frames displaying the needed data and calculations requested by the INL quadrapole team.


#' @title Calculate and go through the quadrapole data chemical analysis
#' @param dat is the .csv file made from the read_rep() 
#' @param weights is the .csv file with all the chemical weights
#' @example calc(dat, weights)
#' @export  

calc <- function(dat, weights){
  
  combineData <- weights %>%
    left_join(dat, by = "Sample ID")
  
  DF <- combineData %>%
    mutate(dfWeight = `Dissolver Wt` / `Sample Wt`, 
           dilution10 = `1:10 Dil + Samp Wt` / `1:10 SampleWt`, 
           dilution100 = `1:100 Dil + Samp Wt` / `1:100 SampleWt`, 
           dilution1000 = `1:1000 Dil + Samp Wt` / `1:1000 SampleWt`, 
           dilution5000 = `1:5000 Dil + Samp Wt` / `1:5000 SampleWt`, 
           dilution500k = `1:500k Dil + Samp Wt` / `1:500k SampleWt`, 
           DilutionFactor = (dilution10 * dilution100 * dilution1000 * dilution5000 * dilution500k)) %>%
    select("Sample ID", "Symbol", "Analyte", "Mass", "Conc. Mean", "dfWeight", "dilution10", "dilution100", "dilution1000", "dilution5000", "dilution500k", "DilutionFactor")
  
  
  
  solutionConc <- combineData %>%
    filter(!(Symbol %in% "|>")) %>%
    select("Sample ID", "Analyte", "Mass", "Conc. Mean")
  
  
  
  sampleConc <- DF%>%
    mutate(Conc.in.Samples = `Conc. Mean` * DilutionFactor * dfWeight / 1000) %>%
    mutate_if(is.numeric, signif, digits=3) %>%
    filter(!(Symbol %in% "|>")) %>%
    select("Sample ID", "Analyte", "Mass", "Conc.in.Samples")
  
  
  
  IQL <- dat %>%
    filter(!(Symbol %in% "|>")) %>%
    filter(`Sample ID` %in% "Rinse") %>%
    select("Sample ID", "Symbol", "Analyte", "Mass", "Conc. SD") %>%
    mutate(RinseSD3 = `Conc. SD` * 3, RinseSD15 = `Conc. SD` * 15) %>%
    mutate_if(is.numeric, signif, digits=3) %>%
    select("Analyte", "Symbol", "Mass", "Conc. SD", "RinseSD3", "RinseSD15")
  
  
  
  MQL <- DF %>%
    filter(!(Symbol %in% "|>")) %>%
    left_join(IQL, by = c("Analyte", "Symbol", "Mass")) %>%
    mutate(MQLinSamples = RinseSD15 * DilutionFactor * dfWeight / 1000) %>%
    mutate_if(is.numeric, signif, digits=1) %>%
    select("Sample ID", "Symbol", "Analyte", "Mass", "MQLinSamples")
  
  
  
  
  MQL.vs.SampleCon <- MQL %>%
    full_join(sampleConc) %>%
    mutate(MQL.vs.SampleConc = ifelse(Conc.in.Samples > MQLinSamples, Conc.in.Samples, paste("<", MQLinSamples, sep = "")))
  
  
  
  numbers <- MQL %>%
    full_join(sampleConc) %>%
    mutate(MQL.vs.SampleConc = ifelse(Conc.in.Samples > MQLinSamples, Conc.in.Samples, MQLinSamples))
  
  
  
  
  #Ask what columns they want to see. Can use mutate() if needed.
  #Possibly might need to use the "numbers" data for data with MQL.vs.Sample Concentration with the less than sign. We'll test it out with another datset later.
  #Because we don't want the averages of the samples with the < sign. 
  averages <- numbers %>%
    separate(`Sample ID`, c("Sample", "Rep", "Dilution"), sep = " ") %>%
    group_by(Sample, Analyte, Mass) %>%
    summarise(Average = mean(MQL.vs.SampleConc), SD = sd(MQL.vs.SampleConc), 
              RSD = SD/Average * 100) %>%
    mutate_if(is.numeric, signif, digits=3) %>%
    filter(!(Sample %in% "Diss"))
  
  
  
  
  errors <- combineData %>%
    filter(!(Symbol %in% "|>")) %>%
    mutate(Error = `Conc. RSD` * 2 / 100) %>%
    select("Sample ID", "Analyte", "Mass", "Conc. RSD", "Error") 
  
  
  
  #We don't divide "conc.RSD" by 100 because when we want to do the ceiling() we need it not in percentages.
  errorOnlyData <- combineData %>%
    filter(!(Symbol %in% "|>")) %>%
    select("Sample ID", "Symbol", "Analyte", "Mass", "Conc. RSD") %>%
    mutate(Error = `Conc. RSD` * 2) %>%
    select("Error")
  
  
  
  #This is for the ceiling round by 5's
  mround <- function(x,base = 5){ 
    base*ceiling(x/base) 
  } 
  
  errorOnly <- errorOnlyData%>%
    mutate(Error = mround(Error)) %>%
    signif(digits = 3) %>%
    mutate(Error = ifelse(Error > 35, "N/A", paste("\u00b1", Error, "%")))
  
  #Do we want Conc.RSD and Actual Error? Or just Error%?
  error <- cbind(errors, errorOnly)
  
  colnames(error) [5] <- "Actual Error"
  
  
  
  #max() is not working correctly
  #We want the highest error% per Sample. 
  #If there's an NA, NA is the highest
  highestError <- error %>%
    separate(`Sample ID`, c("Sample", "Rep", "Dilution"), sep = " ") %>%
    group_by(Sample, Analyte, Mass) %>%
    arrange(desc(`Actual Error`)) %>%
    summarise(`Highest Error` =  Error[1]) %>%
    filter(!(Sample %in% "Diss"))
  
  
  #We want the highest average and highest error% per Sample.
  #Not sure if the ifelse statement will work in data that actually has NA's. This needs furthur testing
  results <- averages %>%
    right_join(highestError) %>%
    select("Sample", "Analyte", "Mass", "Average", "Highest Error")
  
  files <- list("Raw Data" = dat, "Dilution Factor" = DF, "Solution Concentration" = solutionConc, "Sample Concentration" = sampleConc, "IQL" = IQL, 
                "MQL" = MQL, "MQL vs SampleConc" = MQL.vs.SampleCon, "Averages" = averages, "Error %" = error, "Results" = results)
  files
}