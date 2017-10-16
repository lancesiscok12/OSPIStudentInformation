
PullOutcomes<-function(Metric,Year){
  library(tidyverse)

  dat<-read.csv('C:\\Users\\lance.sisco\\Documents\\R\\R\\R-3.3.1\\etc\\SourceData.csv')

  datasources<-dat %>%
    filter(School.Year==Year)

  if(nrow(datasources)==0 & !(Metric %in% c('Enrollment'))){print(paste(Year," not found in source Data.  Please use ",
                                       cbind(paste(dat %>% select(School.Year) %>% distinct(),collapse=','),"Enrollment"),
                                       sep=''))}


  datasources<-datasources %>%
    filter(Measure==Metric)

  if(nrow(datasources)==0 & !(Metric %in% c('Enrollment')))
     {print(paste(Metric," not found in source Data in the specific year.",sep=''))}


  location<-datasources %>%
    select(Table.or.File.Location)

  if(Metric=='Grad'){
    dat<-GradeRateStudentLevel(location$Table.or.File.Location)

  }

  if(Metric=='Proficiency'){
    dat<-annualAssessmentData(location$Table.or.File.Location)
  }

  if(Metric=='Growth'){
    dat<-pullSGP(location$Table.or.File.Location)
  }

  if(Metric=='Enrollment'){
    dat<-Enrollment(Year)
  }

  return(dat)
}

