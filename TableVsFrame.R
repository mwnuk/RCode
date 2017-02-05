https://www.analyticsvidhya.com/blog/2016/05/data-table-data-frame-work-large-data-sets/

#table is much faster then frame, excellent for  large sets

#creating a dummy data table
	library(data.table)
	DT <- data.table( ID = 1:50000,
                Capacity = sample(100:1000, size = 50000, replace = T),
                Code = sample(LETTERS[1:4], 50000, replace = T),
                State = rep(c("Alabama","Indiana","Texas","Nevada"), 50000))

names( DT)
summary(DT)
length(DT$Capacity)
#simple data.table command
DT[Code == "D"]
DT[, mean(Capacity), by = State]
DT[Code == "A", mean(Capacity)]

system.time(DT[, mean(Capacity), by = State])

summarise(sum(Transaction_Amount), mean(Var2))) #with dplyr



#########################
smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
colnames(smoke) <- c("High","Low","Middle")
rownames(smoke) <- c("current","former","never")
smoke <- as.table(smoke)
smoke


