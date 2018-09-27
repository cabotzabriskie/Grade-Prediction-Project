data<-read.csv("Input-Data/P112_F16-S18_Agg.csv", stringsAsFactors = F)
LVars<-read.csv("Intermediate-Data/Weekly Data Vars 2.csv", stringsAsFactors = F)

data<-subset(data, LabSection != "")

#data$FHwk4<-0
#data$FHwk7<-0
#data$FHwk11<-0
#data$FHwk14<-0


data$LabDay<-substring(data$LabSection, 1, 1)
data$LabTime<-substring(data$LabSection, 2)

data$FinalCourseGrade2<-data$FinalCourseGrade
data$FinalCourseGrade2<-ifelse(grepl("^A",data$FinalCourseGrade) == TRUE, paste("A"), paste(data$FinalCourseGrade2))
data$FinalCourseGrade2<-ifelse(grepl("^B",data$FinalCourseGrade) == TRUE, paste("B"), paste(data$FinalCourseGrade2))
data$FinalCourseGrade2<-ifelse(grepl("^C",data$FinalCourseGrade) == TRUE, paste("C"), paste(data$FinalCourseGrade2))
data$FinalCourseGrade2<-ifelse(grepl("^D",data$FinalCourseGrade) == TRUE, paste("D"), paste(data$FinalCourseGrade2))
data$FinalCourseGrade2<-ifelse(grepl("^F",data$FinalCourseGrade) == TRUE, paste("F"), paste(data$FinalCourseGrade2))

data$FinalCourseGradePF<-"F"
data$FinalCourseGradePF<-ifelse(data$FinalCourseGrade2 == "A" , paste("P"), paste(data$FinalCourseGradePF))
data$FinalCourseGradePF<-ifelse(data$FinalCourseGrade2 == "B" , paste("P"), paste(data$FinalCourseGradePF))
data$FinalCourseGradePF<-ifelse(data$FinalCourseGrade2 == "C" , paste("P"), paste(data$FinalCourseGradePF))

data$FinalCourseGradeAB_C_DF<-"NULL"
data$FinalCourseGradeAB_C_DF<-ifelse(grepl("^A",data$FinalCourseGrade) == TRUE, paste("AB"), paste(data$FinalCourseGradeAB_C_DF))
data$FinalCourseGradeAB_C_DF<-ifelse(grepl("^B",data$FinalCourseGrade) == TRUE, paste("AB"), paste(data$FinalCourseGradeAB_C_DF))
data$FinalCourseGradeAB_C_DF<-ifelse(grepl("^C",data$FinalCourseGrade) == TRUE, paste("C"), paste(data$FinalCourseGradeAB_C_DF))
data$FinalCourseGradeAB_C_DF<-ifelse(grepl("^D",data$FinalCourseGrade) == TRUE, paste("DF"), paste(data$FinalCourseGradeAB_C_DF))
data$FinalCourseGradeAB_C_DF<-ifelse(grepl("^F",data$FinalCourseGrade) == TRUE, paste("DF"), paste(data$FinalCourseGradeAB_C_DF))

data$FinalCourseGradeAB_Rest<-"NULL"
data$FinalCourseGradeAB_Rest<-ifelse(grepl("^A",data$FinalCourseGrade) == TRUE, paste("AB"), paste(data$FinalCourseGradeAB_Rest))
data$FinalCourseGradeAB_Rest<-ifelse(grepl("^B",data$FinalCourseGrade) == TRUE, paste("AB"), paste(data$FinalCourseGradeAB_Rest))
data$FinalCourseGradeAB_Rest<-ifelse(grepl("^C",data$FinalCourseGrade) == TRUE, paste("CDF"), paste(data$FinalCourseGradeAB_Rest))
data$FinalCourseGradeAB_Rest<-ifelse(grepl("^D",data$FinalCourseGrade) == TRUE, paste("CDF"), paste(data$FinalCourseGradeAB_Rest))
data$FinalCourseGradeAB_Rest<-ifelse(grepl("^F",data$FinalCourseGrade) == TRUE, paste("CDF"), paste(data$FinalCourseGradeAB_Rest))

vect.names<-NULL
vect.names2<-NULL
vect.names3<-NULL
vect.names4<-NULL
FHW.names<-NULL
FHWA.names<-NULL

for(i in 1:14){
  nam<-paste('Lab',i,"Miss",sep='') #create the name we want the variable to take
  
  vect.names<-c(vect.names,nam) #add the string to the variable "vect.names"
  
  nam2<-paste('Lab',i,"",sep='') #create the name we want the variable to take
  
  vect.names2<-c(vect.names2,nam2) #add the string to the variable "vect.names"
  
  
  
  FN<-paste('FHwk',i,"",sep='')
  FHW.names<-c( FHW.names,FN)
  
  FN2<-paste('DidFHwk',i,"",sep='')
  FHWA.names<-c( FHWA.names,FN2)
  
}

for(i in 1:47){
  nam3<-paste('Lec',i,"Miss",sep='') #create the name we want the variable to take
  
  vect.names3<-c(vect.names3,nam3) #add the string to the variable "vect.names"
  
  nam4<-paste('LecQ',i,"",sep='') #create the name we want the variable to take
  
  vect.names4<-c(vect.names4,nam4) #add the string to the variable "vect.names"
}

data$TakeCSEM<-"Y"
data$TakeCSEM<-ifelse(is.na(data$CSEMPretestPercent), "N", data$TakeCSEM)  
data$TakeCSEM<-as.factor(data$TakeCSEM)
data$CSEMPretestPercent[is.na(data$CSEMPretestPercent)]<-0
myfunct<-function(x){

y<-ifelse(x == -5, 0, 1)  
#  y<-1
#if(x == -5){
#  y<-0
#}
return(y)
}
myfunct2<-function(x){

y<-ifelse(x == "Needs Grading", "-1", x)
z<-ifelse(y == "", "-1", y)
  
return(z)
}

myfunct3<-function(x){
  
  y<-ifelse(x == -5, 0, x)
  
  return(y)
}

funct<-function(x){
  
  y<-ifelse(x == -1, 1, 0)
  
  return(y)
}

data[,vect.names4]<-lapply(data[,vect.names4], as.character)



data[,vect.names]<-0


for(i in 1:length(vect.names)){
data[,vect.names[i]]<-as.numeric(lapply(data[,vect.names2[i]], myfunct))


data[,FHWA.names[i]]<-as.numeric(lapply(data[,FHW.names[i]], myfunct))
data[,FHW.names[i]]<-as.numeric(lapply(data[,FHW.names[i]], myfunct3))
}

for(i in 1:length(vect.names3)){
  data[,vect.names4[i]]<-as.numeric(lapply(data[,vect.names4[i]], myfunct2))
  data[,vect.names3[i]]<-as.numeric(lapply(data[,vect.names4[i]], funct))
}

#Create and write files----------
for(i in 1:16){
  df<-data[colnames(data) %in% LVars[,i+1]]
  
  write.csv(df, paste("Intermediate-Data/Week_",i,"_data.csv",sep = ""))
  
}
