

PRstem <- read.csv("E:/BASE_PR_20/Updated Census/ALL 25 data with issues.csv")

splist <- as.data.frame(unique(PRstem$sp))

#group species data for analysis,  
PRstem$sp <- sub("ACER", "ACSA3C", PRstem$sp) 
PRstem$sp <- sub("ACSA3", "ACSA3C", PRstem$sp) 
PRstem$sp <- sub("ACSA3CC", "ACSA3C", PRstem$sp) 
PRstem$sp <- sub("ACSAL", "ACSA3C", PRstem$sp) 
PRstem$sp <- sub("ACSAS2", "ACSA3C", PRstem$sp)

PRstem$sp <- sub("AESCU", "AEFL", PRstem$sp)

PRstem$sp <- sub("FRAM2", "FRAMCO", PRstem$sp) 
PRstem$sp <- sub("FRBI2", "FRAMCO", PRstem$sp) 
PRstem$sp <- sub("FRSM", "FRAMCO", PRstem$sp) 
PRstem$sp <- sub("FRAXI", "FRAMCO", PRstem$sp)

PRstem$sp <- sub("CASHAG", "CARYA", PRstem$sp) 
PRstem$sp <- sub("CANUT", "CARYA", PRstem$sp) 

PRstem$sp <- sub("QUERC", "QUMU", PRstem$sp)

PRstem$sp <- sub("ULOR", "ULMUS", PRstem$sp)

#write to disk
write.csv(PRstem, "E:/BASE_PR_20/Updated Census/ALLstems.csv")
