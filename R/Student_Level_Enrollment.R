Enrollment<-function(Year){
  library(RODBC)
  cedars <- odbcDriverConnect('driver={SQL Server};
                              server=srv-sql03;
                              database=stuinfo;
                              trusted_connection=TRUE')



  Enrollment<-sqlQuery(cedars,paste("Select * from fnEnrollment(",Year,",'",Sys.Date(),"','12/30/2099')"))
}
