
# The calc function works with the .csv file output from the read_rep function and combines that data with a .csv file with different chemical weights.
# The calc function runs through the chemical analysis of multiple calculations to find the results INL vendors are looking for.
# It starts by joining the two .csv files together. We use a left join with the weights file on the left because that returns only the Sample ID's we want and need for the chemical calculations and analysis.
# The function then goes through the chemical calculations the INL quadrapole team needs in order to run a chemical analysis and interpretation.
# After it runs through all the calculations, we end up with 10 different data frames displaying the needed data and calculations requested by the INL quadrapole team.


#' @title Calculate and go through the quadrapole data chemical analysis
#'
#' @param dat is the .csv file made from the read_rep() 
#' @param weights is the .csv file with all the chemical weights
#'
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
  
  
  
  
  sampleConc <- DF%>%
    mutate(Conc.in.Samples = `Conc. Mean` * DilutionFactor * dfWeight / 1000) %>%
    mutate_if(is.numeric, signif, digits = 3) %>%
    filter(!(Symbol %in% "|>")) %>%
    select("Sample ID", "Analyte", "Mass", "Conc.in.Samples")
  
  #I took out all dissolver blanks. Are there ever any different names needed to be taken out?
  #Should I leave the column names as rinse?
  IQL <- dat %>%
    filter(!(Symbol %in% "|>")) %>%
    filter(str_detect(str_to_lower(`Sample ID`), "blk|blank|rinse")) %>%
    filter(!(str_detect(str_to_lower(`Sample ID`), "diss|dis|dissolver"))) %>%
    mutate(RinseSD3 = `Conc. SD` * 3, RinseSD15 = `Conc. SD` * 15) %>%
    group_by(Analyte, Mass) %>%
    mutate(AverageSD = mean(RinseSD15, na.rm = TRUE)) %>%
    mutate_if(is.numeric, signif, digits=3) %>%
    select("Sample ID", "Analyte", "Mass", "Conc. SD", "RinseSD3", "RinseSD15",
           "AverageSD") %>%
    arrange((as.numeric(Mass)), Analyte)
  
  
  
  MQL <- DF %>%
    filter(!(Symbol %in% "|>")) %>%
    left_join(IQL, by = c("Analyte", "Mass")) %>%
    mutate(MQLinSamples = AverageSD * DilutionFactor * dfWeight / 1000) %>%
    mutate_if(is.numeric, signif, digits=1) %>%
    rename(`Sample ID` = `Sample ID.x`) %>%
    select("Sample ID", "Symbol", "Analyte", "Mass", "MQLinSamples") %>%
    mutate(Unit = "ug/g")%>%
    distinct()
  
  
  
  #MQL.vs.SampleCon <- MQL %>%
  # full_join(sampleConc, by = c("Sample ID", "Analyte", "Mass")) %>%
  #mutate(MQL.vs.SampleConc = ifelse(Conc.in.Samples > MQLinSamples, Conc.in.Samples, 
  #                                 paste("<", MQLinSamples, sep = "")), 
  #     Unit = "ug/g")%>%
  #    {.[,c(1:5,7:length(.),6)]}
  
  
  MQL.vs.SampleCon <- MQL %>%
    full_join(sampleConc, by = c("Sample ID", "Analyte", "Mass")) %>%
    separate(`Sample ID`, c("Sample", "Rep", "Dilution"), sep = " ") %>%
    mutate(MQL.vs.SampleConc = ifelse(Conc.in.Samples > MQLinSamples, Conc.in.Samples, 
                                      paste("<", MQLinSamples, sep = "")), 
           Unit = "ug/g")%>%
    select(-Dilution, -Symbol) %>%
    {.[,c(1:5,7:length(.),6)]} %>%
    arrange(Sample, (as.numeric(Mass)), Rep)
  
  
  
  numbers <- MQL %>%
    full_join(sampleConc, by = c("Sample ID", "Analyte", "Mass")) %>%
    mutate(MQL.vs.SampleConc = ifelse(Conc.in.Samples > MQLinSamples, Conc.in.Samples, MQLinSamples))
  
  
  
  
  
  
  #Ask what columns they want to see. Can use mutate() if needed.
  #Possibly might need to use the "numbers" data for data with MQL.vs.Sample Concentration with the less than sign. We'll test it out with another datset later.
  #Because we don't want the averages of the samples with the < sign. 
  averages <- numbers %>%
    separate(`Sample ID`, c("Sample", "Rep", "Dilution"), sep = " ") %>%
    group_by(Sample, Analyte, Mass) %>%
    summarise(Average = mean(MQL.vs.SampleConc), Unit = "ug/g", SD = sd(MQL.vs.SampleConc), 
              RSD = SD/Average * 100) %>%
    mutate_if(is.numeric, signif, digits=3) %>%
    filter(!(Sample %in% "Diss")) %>%
    arrange(Sample, (as.numeric(Mass)))
  
  
  repAverages <- numbers %>% 
    separate(`Sample ID`, c("Sample", "Rep", "Dilution"), sep = " ") %>%
    group_by(Sample, Rep, Analyte, Mass) %>%
    mutate_if(is.numeric, signif, digits=3) %>%
    filter(!(Sample %in% "Diss")) %>%
    arrange(Sample, (as.numeric(Mass)), Rep) %>%
    select(Sample, Rep, Analyte, Mass, MQL.vs.SampleConc)
  
  #Final tab for averages
  addedAverages <- repAverages %>%
    left_join(averages, by = c("Sample", "Analyte", "Mass"))
  
  # Do I need this is the code?
  averageRSD <- addedAverages %>% 
    select(Sample, Rep, Analyte, Mass, RSD)
  
  
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
    mutate(Error = ifelse(Error > 35, "N/A", paste("\u00B1", Error, "%")))
  
  #Final for error%
  error <- cbind(errors %>%
                   rename("Actual Error" = Error),
                 errorOnly) %>%
    mutate_if(is.numeric, signif, digits = 2) %>%
    separate(`Sample ID`, c("Sample", "Rep", "Dilution"), sep = " ") %>%
    group_by(Sample, Rep, Analyte, Mass) %>%
    arrange(Sample, (as.numeric(Mass)), Rep) %>%
    left_join(averageRSD) %>%
    select(-Dilution, - `Actual Error`) %>%
    rename("Average RSD" = RSD) %>%
    {.[,c(1:5,7:length(.),6)]}
  
  
  
  
  
  error2 <- error %>%
    left_join(MQL.vs.SampleCon, by = c("Sample", "Rep", "Analyte", "Mass")) %>%
    mutate(Error = case_when(
      str_detect(`MQL.vs.SampleConc`, "<") ~ "N/A", T ~ `Error`
    )) %>%
    select("Sample", "Rep", "Analyte", "Mass", "Conc. RSD", "Error")
  
  
  
  #max() is not working correctly
  #We want the highest error% per Sample. 
  #If there's an NA, NA is the highest
  highestError <- error2 %>%
    group_by(Sample, Analyte, Mass) %>%
    summarise(`Highest Error` =  Error[1]) %>%
    filter(!(Sample %in% "Diss"))
  
  
  #We want the highest average and highest error% per Sample.
  #Not sure if the ifelse statement will work in data that actually has NA's. This needs furthur testing
  results <- averages %>%
    right_join(highestError, by = c("Sample", "Analyte", "Mass")) %>%
    mutate(Unit = "ug/g") %>%
    select("Sample", "Analyte", "Mass", "Average", "Unit", "Highest Error") %>%
    mutate(Average = case_when(`Highest Error` == "N/A"~ paste("<", signif(Average, digits = 1)),
                               T~as.character(Average))) %>%
    arrange(Sample, as.numeric(Mass))
  
  
  #str_to_lower() makes it not case sensitive. For example blk also works for Blk.
  #blanks <- dat %>%
  #filter(!(Symbol %in% "|>")) %>%
  #select("Sample ID", "Analyte", "Mass", "Meas. Intens. Mean", "Conc. Mean") %>%
  #filter(str_detect(str_to_lower(`Sample ID`), "blk|blank|rinse")) %>%
  #mutate_if(is.numeric, signif, digits = 5)
  
  
  
  
  
  standards <- dat %>%
    filter(Symbol %in% "|>") %>%
    select("Sample ID", "Analyte", "Mass", "Meas. Intens. Mean") %>%
    arrange(Mass) %>%
    group_by(Analyte) %>%
    mutate(Average = mean(`Meas. Intens. Mean`)) %>%
    mutate(`% Difference` = signif((Average - `Meas. Intens. Mean`) / Average * 100, digits = 2)) %>%
    mutate(`-10%` = Average - (Average * .1)) %>%
    mutate(`+10%` = Average + (Average * .1)) %>%
    mutate(`-20%` = Average - (Average * .2)) %>%
    mutate(`+20%` = Average + (Average * .2)) %>%
    mutate(Average = case_when(
      row_number() == 1 ~ Average
    ))
  
  
  #standards%>%
  # mutate(new_wow = case_when(
  #  abs(`% Difference`) >= 20 ~ `% Difference`,
  # abs(`% Difference`) >= 10 ~ `% Difference`,
  #  ))
  
  
  checkStandards <- dat %>% 
    filter(str_detect(str_to_lower(`Sample ID`), "chk|std|check|standard")) %>%
    select(`Sample ID`, Analyte, Mass, `Conc. Mean`)
  
  
  
  
  ablines <-  standards%>%
    distinct(Average, `-10%`, `+10%`, `-20%`, `+20%`)%>%
    filter(!is.na(Average))%>%
    gather(Average,`-10%`, `+10%`, `-20%`, `+20%`,key = color,value = intercept)
  
  
  
  confPlots <- standards %>%
    ggplot(aes(x = `Sample ID`, y = `Meas. Intens. Mean`)) +
    geom_point() +
    geom_abline(data = ablines, aes(slope = 0, color = color, intercept = intercept)) +
    facet_grid(Analyte ~ ., scales = "free") +
    theme_bw() +
    theme(axis.text.x=element_blank()) +
    scale_color_manual(values = c( "yellow", "red", "yellow", "red", "green"))
  
  
  
  # doesn't export to excel
  slopes <- dat %>%
    select("Sample ID", "Analyte", "Mass", "Net Intens. Mean", "Conc. Mean") %>%
    mutate(Slope = `Conc. Mean` / `Net Intens. Mean`)
  
  
  
  files <- list("Raw Data" = dat, "Dilution Factor" = DF, "IQL" = IQL, "MQL" = MQL, 
                "MQL vs SampleConc" = MQL.vs.SampleCon, "Averages" = addedAverages, "Error %" = error, 
                "Results" = results, "Internal Standards" = standards, "Check Standards" = checkStandards,
                "Plots" = confPlots)
  files
  
  
}