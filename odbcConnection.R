library(RODBC)
myConn<-odbcConnect("Teradata-qual",uid="xvic193",pwd="CautionQual7$")

-----------------------
 Quick test
-----------------------
Query1<- c("Select count(*) as cnt FROM DEV_ICE_SIMULATION_DATA.ShopVisit")

Q1<-sqlQuery(myConn,Query1)
Q1

------------------------------------
More compex query
------------------------------------

q<-paste("
SELECT 
	 ProcID
	,QueryID
	,WDName
	,FinalWDName
	,cal.day_of_week
	,pdr.starttime
	,ParserCPUTime
FROM PDCRINFO.DBQLogTbl pdr
INNER JOIN Sys_Calendar cal
    ON cal.calendar_date=pdr.LogDate
    WHERE pdr.starttime(DATE) >=CURRENT_DATE - interval '2' month
    AND AMPCPUTIME>0
    AND QueryText like '%INSERT INTO gtt_MV_MatrixCosts%'
")

Query2<-strwrap(q,width=1000,simplify=TRUE)
Q2<-sqlQuery(myConn,Query2)

 