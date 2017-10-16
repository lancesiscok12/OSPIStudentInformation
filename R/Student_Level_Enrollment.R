Enrollment<-function(Year){

  cedars <- odbcDriverConnect('driver={SQL Server};
                              server=srv-sql03;
                              database=stuinfo;
                              trusted_connection=TRUE')



  Enrollment<-sqlQuery(stuinfo,paste("Select * from fnEnrollment(",year,",'",Sys.Date(),"','12/30/2099')"))
}
