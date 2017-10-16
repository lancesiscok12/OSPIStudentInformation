
pullSGP<-function(table){
  dev07 <- odbcDriverConnect('driver={SQL Server};
                             server=dev-sql07;
                             database=SGP;
                             trusted_connection=TRUE')

  sgp2017<-sqlQuery(dev07,paste('Select YEAR as schoolyear,
                                ID as ssid,
                                SCHOOL_NUMBER as reportingschoolcode,
                                TestType as resolvedtesttype,
                                CONTENT_AREA as course,
                                SGP
                                from ',table,'
                                where SGP is not null',sep='')) %>%
    mutate(course=recode(course,'MATHEMATICS'='Math','READING'='ELA'),
           schoolyear=case_when(.$schoolyear=='2016_2017' ~ '2016-2017',
                                .$schoolyear=='2015_2016' ~ '2015-2016',
                                .$schoolyear=='2014_2015' ~ '2014-2015'))
  return(sgp2017)
}

