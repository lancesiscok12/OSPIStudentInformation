

annualAssessmentData<-function(SQLTable){
  options(stringsAsFactors = F)
  library(RODBC)
  library(tidyverse)
  library(data.table)

  #Identify fields to keep from Scorefile - not a necessary step but speeds up process considerably
  fieldstoKeep<-'schoolyear,reportingdistrictcode,reportingdistrictname,reportingschoolname,reportingschoolcode,ssid,resolvedtesttype,resolvedsubject,scalescore,course,metstandard,schooltype
  ,reportinggrade,gender,raceethnicity,isspecialed,isbilingual,islowincome,
  iss504,ismigrant,ishomeless,isgifted,accountability,testedgrade,homebased,private,isforeignexchange,attempted'

  dev07 <- odbcDriverConnect('driver={SQL Server};
                             server=dev-sql07;
                             database=assessmentanalysts;
                             trusted_connection=TRUE')
  ass<-sqlQuery(dev07,
                paste('Select ',fieldstoKeep,' from ',SQLTable,sep=''))

  #Identify attempt codes included in Denominator
  TestedNotTested<-c('TS','PP','AU','BL','IC','IS','IV','IG','RF','NB','NT')
  colnames(ass)<-tolower(colnames(ass))

  #Calculate student level results
  #Filter out home based, private, F-1 Visa, Juvenile Facilities, Voc Centers and Private Schools (N).  Also remove
  #any non-accountability test (12th graders testing, 11th graders taking an off-grade, etc)
  results<-ass %>%
    filter((accountability=='Y' | (reportinggrade==10 & testedgrade==11 & course=='ELA')),
           (course %in% c('ELA','Math','Science','Biology')),
           !(homebased %in% c('1','2')),
           !(private %in% c('1','2')),
           isforeignexchange!='Y',
           !(schooltype %in% c('J','V','N'))) %>%
    mutate(MetStandard=ifelse((attempted=='TS' & metstandard=='YES') |
                                attempted=='PP',1,0), #Set Meeting Standard as a student testing, or a student that previously passed
           TestedNotTested=ifelse(attempted %in% TestedNotTested,1,0)) %>%
    select(reportingschoolcode,ssid,reportinggrade,resolvedtesttype,resolvedsubject,course,MetStandard,TestedNotTested,scalescore) %>%
    mutate(resolvedtesttype=recode(resolvedtesttype,'BIOB'='BIO','MSPB'='MSP',.default=resolvedtesttype)) #Recode basic test types


  #Createa a demographic file for each student with proper naming conventions
  Demo<-ass %>%
    rowwise %>%
    as.data.table() %>%
    select(schoolyear,reportingdistrictcode,reportingdistrictname,reportingschoolname,reportingschoolcode,
           schooltype,ssid,reportinggrade,gender,raceethnicity,isspecialed,isbilingual,islowincome,
           iss504,ismigrant,ishomeless,isgifted) %>%
    mutate(All='All',
           raceethnicity=as.character(raceethnicity)) %>%
    distinct() %>%
    mutate(gender=recode(gender,'M'='Male','F'='Female',.default='Unknown',.missing='Unknown'),
           isspecialed=recode(isspecialed,'Y'='SPED',.default='Non-SPED',.missing='Non-SPED'),
           isbilingual=recode(isbilingual,'1'='ELL',.default='Non-ELL',.missing='Non-ELL'),
           islowincome=recode(islowincome,'Y'='FRL',.default='Non-FRL',.missing='Non-FRL'),
           ismigrant=recode(ismigrant,'Y'='Migrant',.default='Non-Migrant',.missing='Non-Migrant'),
           iss504=recode(iss504,'Y'='s504',.default='Non-s504',.missing='Non-s504'),
           ishomeless=recode(ishomeless,'Y'='Homeless',.default='Non-Homeless',.missing='Non-Homeless'),
           isgifted=recode(isgifted,'Y'='Gifted',.default='Non-Gifted',.missing='Non-Gifted'),
           raceethnicity=recode(raceethnicity,'1'='American Indian/Alaskan Native',
                                '2'='Asian',
                                '3'='Black/African American',
                                '4'='Hispanic',
                                '5'='White',
                                '6'='Pacific Islander',
                                '7'='Two or More Races',
                                .default='Unknown',
                                .missing='Unknown')) %>%
    gather(Group,Subgroup,-(schoolyear:reportinggrade))

  fin <- results %>%
    left_join(Demo) %>%
    distinct() %>%
    as.data.table()

  return(fin)
}
