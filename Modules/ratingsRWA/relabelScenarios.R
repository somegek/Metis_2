# add scenario probabilities to the labels
# data: Data table with a column with name of scenario
# ScenarioColumnName: name of the column in the data table with the scenarios
# CCAR: if T use EBA2018 labels, if F use EBA2018 labels

relabelScenario <- function(data, ScenarioColumnName, CCAR){
  #browser()
  data <- data[grep('Baseline|baseline',data[, get(ScenarioColumnName)]),(ScenarioColumnName):=paste0(grep('Baseline|baseline',data[, get(ScenarioColumnName)],value=T),'_Q0.5')]
  data <- data[grep('Extreme|extreme',data[, get(ScenarioColumnName)]),(ScenarioColumnName):=paste0(grep('Extreme|extreme',data[, get(ScenarioColumnName)],value=T),'_Q0.001')]
  data <- data[grep('Optimal|optimal',data[, get(ScenarioColumnName)]),(ScenarioColumnName):=paste0(grep('Optimal|optimal',data[, get(ScenarioColumnName)],value=T),'_Q0.9')]
  if (CCAR){
    data <- data[grep('Adverse|adverse',data[, get(ScenarioColumnName)]),(ScenarioColumnName):=paste0(grep('Adverse|adverse',data[, get(ScenarioColumnName)],value=T),'_Q0.15')]
    data <- data[grep('Severe|severe',data[, get(ScenarioColumnName)]),(ScenarioColumnName):=paste0(grep('Severe|severe',data[, get(ScenarioColumnName)],value=T),'_Q0.05')]
  }else{
    data <- data[grep('Adverse|adverse',data[, get(ScenarioColumnName)]),(ScenarioColumnName):=paste0(grep('Adverse|adverse',data[, get(ScenarioColumnName)],value=T),'_Q0.10')]
  }
  
  if(length(grep('2018',data[, get(ScenarioColumnName)]))>0){
    data <- data[,(ScenarioColumnName):=gsub('CCAR2018_','',data[, get(ScenarioColumnName)])]
    data <- data[,(ScenarioColumnName):=gsub('EBA2018_','',data[, get(ScenarioColumnName)])]
  }
  data <- data[,(ScenarioColumnName):=gsub('_',' ',data[, get(ScenarioColumnName)])]
  data <- data[,(ScenarioColumnName):=paste0(toupper(str_sub(data[, get(ScenarioColumnName)],1,1)),str_sub(data[, get(ScenarioColumnName)],2,-1))]

  return(data)
  }