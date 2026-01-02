directory = "~/Dropbox/ENSEIGNEMENT/SummerSchool/student/"
d1=read.table(paste(directory,"student-mat.csv",sep=""),sep=";",header=TRUE)
#d2=read.table("~/Dropbox/ENSEIGNEMENT/SummerSchool/student/student-por.csv",sep=";",header=TRUE)

#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
d3 = d1

print(nrow(d3)) # 382 students

# Transformation to binary data
d3$age = d3$age>18
d3$Medu = d3$Medu>2
d3$Fedu = d3$Fedu>2
d3$Mjob = (d3$Mjob=="other")
d3$Fjob = (d3$Fjob=="other")
d3$guardian = (d3$guardian=="mother")
d3$traveltime = d3$traveltime>2 
d3$studytime = d3$studytime>2 
d3$famrel = d3$famrel>2
d3$freetime = d3$freetime>2
d3$goout = d3$goout>2
d3$Dalc = d3$Dalc>2
d3$Walc = d3$Walc>2
d3$health = d3$health>2
d3$G1 = d3$G1>10
d3$G2 = d3$G2>10
d3$G3 = d3$G3>10
d3$absences = d3$absences>2
d3$reason = d3$reason=="home" 
d3$failures = d3$failures>1

for (j in 1:ncol(d3)){
	d3[,j] = as.numeric(d3[,j])
	if (max(d3[,j])==2){d3[,j] = d3[,j]-1}
}

