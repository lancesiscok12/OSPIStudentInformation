
DaysEnrolled<-function(year){
  library(RODBC)
  library(tidyverse)

  cedars <- odbcDriverConnect('driver={SQL Server};
                              server=srv-sql03;
                              database=cedars;
                              trusted_connection=TRUE')



  schoolenrollment<-sqlQuery(cedars,paste("Select schoolyear,
                                          schoolorganizationid,
                                          ssid,
                                          dateenrolledinschool,
                                          dateexitedschool
                                          from schoolstudentenrollment
                                          where recenddate is null
                                          and schoolyear =",year,"
                                          and isprimaryschool='Y'",sep='')) %>%
    mutate(dateexitedschool=case_when(is.na(.$dateexitedschool) ~ as.POSIXct(paste(schoolyear,'-06-01',sep='')),
                                      .$dateexitedschool>as.POSIXct(paste(schoolyear,'-06-01',sep='')) ~ as.POSIXct(paste(schoolyear,'-06-01',sep='')),
                                      TRUE~.$dateexitedschool),
           dateenrolledinschool=case_when( .$dateenrolledinschool<as.POSIXct(paste(schoolyear-1,'-08-01',sep='')) ~ as.POSIXct(paste(schoolyear-1,'-08-01',sep='')),
                                           TRUE~.$dateenrolledinschool)) %>%
    mutate(daysEnrolled=difftime(dateexitedschool,dateenrolledinschool,units='days')) %>%
    filter(daysEnrolled>0) %>%
    group_by(schoolyear,schoolorganizationid,ssid) %>%
    summarize(DaysEnrolled=sum(daysEnrolled))


  org<-sqlQuery(cedars,'Select OrganizationId as schoolorganizationid,ospilegacycode as reportingschoolcode from ems.dbo.organization')

  schoolenrollment<-schoolenrollment %>%
    left_join(org) %>%
    ungroup %>%
    mutate(reportingschoolcode=as.integer(reportingschoolcode),
           schoolyear=paste(schoolyear-1,schoolyear,sep='-'))

  return(schoolenrollment)
}
