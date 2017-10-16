
GradeRateStudentLevel<-function(file){
  library(RODBC)
  library(foreign)
  library(dplyr)
  library(tidyr)
  library(data.table)

  p210 <- odbcDriverConnect('driver={SQL Server};
                            server=srv-sql03;
                            database=p210report;
                            trusted_connection=TRUE')

  enrStatus<-sqlQuery(p210,'select EnrollmentStatusTypeCode as EnrCode,
                      EnrollmentStatusTypeName as EnrollmentStatus
                      from LuEnrollmentStatus')


  grad<-read.spss(file,to.data.frame = T) %>%
    as.data.table()

  if (unique(grad$SchlYr>=2014)){
    grad<-grad %>%
      mutate(EverHomeless=ifelse(Homeless4Yr==1  | Homeless5Yr==1,1,0))
  }

  if(unique(grad$SchlYr)==2013){
    grad <- grad %>%
      rename(schoolcode=SchoolCode,
             school=School,
             SPED=SpEd,
             district=District,
             IsFiveYearRate=IsFiveYear,
             dist=Dist,
             Y1DOut5YrCoh=DOut5YCohortYr1,
             Y2DOut5YrCoh=DOut5YCohortYr2
             ,Y3DOut5YrCoh=DOut5YCohortYr3
             ,Y4DOut5YrCoh=DOut5YCohortYr4
             ,Y5DOut5YrCoh=DOut5YCohortYr5
             ,Y1DOut4YrCoh=DOut4YCohortYr1
             ,Y2DOut4YrCoh=DOut4YCohortYr2
             ,Y3DOut4YrCoh=DOut4YCohortYr3
             ,Y4DOut4YrCoh=DOut4YCohortYr4
             ,CohortPlacement=CohortPlcmt
             ,DistSuppress=distsuppress) %>%
      mutate(dist=as.integer(dist),
             Y1DOut5YrCoh=ifelse(is.na(Y1DOut5YrCoh),0,Y1DOut5YrCoh),
             Y2DOut5YrCoh=ifelse(is.na(Y2DOut5YrCoh),0,Y2DOut5YrCoh),
             Y3DOut5YrCoh=ifelse(is.na(Y3DOut5YrCoh),0,Y3DOut5YrCoh),
             Y4DOut5YrCoh=ifelse(is.na(Y4DOut5YrCoh),0,Y4DOut5YrCoh),
             Y5DOut5YrCoh=ifelse(is.na(Y5DOut5YrCoh),0,Y5DOut5YrCoh),
             Y1DOut4YrCoh=ifelse(is.na(Y1DOut4YrCoh),0,Y1DOut4YrCoh),
             Y2DOut4YrCoh=ifelse(is.na(Y2DOut4YrCoh),0,Y2DOut4YrCoh),
             Y3DOut4YrCoh=ifelse(is.na(Y3DOut4YrCoh),0,Y3DOut4YrCoh),
             Y4DOut4YrCoh=ifelse(is.na(Y4DOut4YrCoh),0,Y4DOut4YrCoh))
  }


  if(unique(grad$SchlYr)<=2014)
  {grad<-grad %>%
    mutate(IsInDistrictAggregation=DistSuppress,
           IsInStateAggregation=1)}

  Student<-grad %>%
    mutate(AllStudents='AllStudents',
           FRL=recode(FRL,'Y'='Low Income','N'='Non-Low Income'),
           SPED=recode(SPED,'Y'='Special Education','N'='Non-Special Education'),
           LEP=recode(LEP,'Y'='English Learners','N'='Non-English Learners'),
           Migrant=recode(Migrant,'Y'='Migrant','N'='Non-Migrant'),
           Is504=recode(Is504,'Y'='Section 504','N'='Non-Section 504'),
           Gender=recode(Gender,'M'='Male','F'='Female'),
           EverHomeless=recode(EverHomeless,'1'='Homeless','0'='Non-Homeless'),
           Dropout=ifelse(Y1DOut4YrCoh==1 | Y1DOut5YrCoh==1,'Year 1',
                          ifelse(Y2DOut4YrCoh==1 | Y2DOut5YrCoh==1,'Year 2',
                                 ifelse(Y3DOut4YrCoh==1 | Y3DOut5YrCoh==1,'Year 3',
                                        ifelse(Y4DOut4YrCoh==1 | Y4DOut5YrCoh==1,'Year 4',
                                               ifelse(Y5DOut5YrCoh==1,'Year 5',NA)))))) %>%
    left_join(enrStatus) %>%
    select(SchlYr,
           dist,
           district,
           schoolcode,
           school,
           SSID,
           IsFiveYearRate,
           AllStudents,
           Gender,
           Ethnic,
           FRL,
           SPED,
           LEP,
           EverHomeless,
           Migrant,
           Is504,
           IsInDistrictAggregation,
           AdjCohort,
           Graduate,
           Continuing,
           Dropout,
           EnrollmentStatus,
           CohortPlacement,
           Y1DOut5YrCoh,
           Y2DOut5YrCoh,
           Y3DOut5YrCoh,
           Y4DOut5YrCoh,
           Y5DOut5YrCoh,
           Y1DOut4YrCoh,
           Y2DOut4YrCoh,
           Y3DOut4YrCoh,
           Y4DOut4YrCoh)
  return(Student)
}
