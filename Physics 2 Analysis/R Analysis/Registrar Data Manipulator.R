data<-read.csv("Input-Data/DemoRptMajSeq.csv", stringsAsFactors = F)
wk1<-read.csv("Intermediate-Data/Week_1_data.csv", stringsAsFactors = F)
data<-merge(wk1,data, by = "Username", all.x = T)

rm(wk1)

data<-subset(data, !is.na(P112LastSem) )

#Current CGPA

FCGPA<-c("FCGPASEM0", "FCGPASEM1","FCGPASEM2","FCGPASEM3","FCGPASEM4","FCGPASEM5",
           "FCGPASEM6", "FCGPASEM7", "FCGPASEM8",
           "FCGPASEM9", "FCGPASEM10", "FCGPASEM11","FCGPASEM12",
           "FCGPASEM13", "FCGPASEM14", "FCGPASEM15","FCGPASEM16", "FCGPASEM17", "FCGPASEM18",
           "FCGPASEM19", "FCGPASEM20","FCGPASEM21","FCGPASEM22","FCGPASEM23",
           "FCGPASEM24","FCGPASEM25",  "FCGPASEM26","FCGPASEM27","FCGPASEM28",
           "FCGPASEM29", "FCGPASEM30","FCGPASEM31","FCGPASEM32", "FCGPASEM33",
           "FCGPASEM34","FCGPASEM35", "FCGPASEM36","FCGPASEM37","FCGPASEM38",
           "FCGPASEM39", "FCGPASEM40","FCGPASEM41","FCGPASEM42","FCGPASEM43",
           "FCGPASEM44","FCGPASEM45","FCGPASEM46","FCGPASEM47")

EN<-c("ENSEM0", "ENSEM1","ENSEM2","ENSEM3","ENSEM4","ENSEM5",
         "ENSEM6", "ENSEM7", "ENSEM8",
         "ENSEM9", "ENSEM10", "ENSEM11","ENSEM12",
         "ENSEM13", "ENSEM14", "ENSEM15","ENSEM16", "ENSEM17", "ENSEM18",
         "ENSEM19", "ENSEM20","ENSEM21","ENSEM22","ENSEM23",
         "ENSEM24","ENSEM25",  "ENSEM26","ENSEM27","ENSEM28",
         "ENSEM29", "ENSEM30","ENSEM31","ENSEM32", "ENSEM33",
         "ENSEM34","ENSEM35", "ENSEM36","ENSEM37","ENSEM38",
         "ENSEM39", "ENSEM40","ENSEM41","ENSEM42","ENSEM43",
         "ENSEM44","ENSEM45","ENSEM46","ENSEM47")
CMP<-c("CMPSEM0", "CMPSEM1","CMPSEM2","CMPSEM3","CMPSEM4","CMPSEM5",
      "CMPSEM6", "CMPSEM7", "CMPSEM8",
      "CMPSEM9", "CMPSEM10", "CMPSEM11","CMPSEM12",
      "CMPSEM13", "CMPSEM14", "CMPSEM15","CMPSEM16", "CMPSEM17", "CMPSEM18",
      "CMPSEM19", "CMPSEM20","CMPSEM21","CMPSEM22","CMPSEM23",
      "CMPSEM24","CMPSEM25",  "CMPSEM26","CMPSEM27","CMPSEM28",
      "CMPSEM29", "CMPSEM30","CMPSEM31","CMPSEM32", "CMPSEM33",
      "CMPSEM34","CMPSEM35", "CMPSEM36","CMPSEM37","CMPSEM38",
      "CMPSEM39", "CMPSEM40","CMPSEM41","CMPSEM42","CMPSEM43",
      "CMPSEM44","CMPSEM45","CMPSEM46","CMPSEM47")
AZEN<-c("AZENSEM0", "AZENSEM1","AZENSEM2","AZENSEM3","AZENSEM4","AZENSEM5",
       "AZENSEM6", "AZENSEM7", "AZENSEM8",
       "AZENSEM9", "AZENSEM10", "AZENSEM11","AZENSEM12",
       "AZENSEM13", "AZENSEM14", "AZENSEM15","AZENSEM16", "AZENSEM17", "AZENSEM18",
       "AZENSEM19", "AZENSEM20","AZENSEM21","AZENSEM22","AZENSEM23",
       "AZENSEM24","AZENSEM25",  "AZENSEM26","AZENSEM27","AZENSEM28",
       "AZENSEM29", "AZENSEM30","AZENSEM31","AZENSEM32", "AZENSEM33",
       "AZENSEM34","AZENSEM35", "AZENSEM36","AZENSEM37","AZENSEM38",
       "AZENSEM39", "AZENSEM40","AZENSEM41","AZENSEM42","AZENSEM43",
       "AZENSEM44","AZENSEM45","AZENSEM46","AZENSEM47")
CUMCMP<-c("CUMCMPSEM0", "CUMCMPSEM1","CUMCMPSEM2","CUMCMPSEM3","CUMCMPSEM4","CUMCMPSEM5",
       "CUMCMPSEM6", "CUMCMPSEM7", "CUMCMPSEM8",
       "CUMCMPSEM9", "CUMCMPSEM10", "CUMCMPSEM11","CUMCMPSEM12",
       "CUMCMPSEM13", "CUMCMPSEM14", "CUMCMPSEM15","CUMCMPSEM16", "CUMCMPSEM17", "CUMCMPSEM18",
       "CUMCMPSEM19", "CUMCMPSEM20","CUMCMPSEM21","CUMCMPSEM22","CUMCMPSEM23",
       "CUMCMPSEM24","CUMCMPSEM25",  "CUMCMPSEM26","CUMCMPSEM27","CUMCMPSEM28",
       "CUMCMPSEM29", "CUMCMPSEM30","CUMCMPSEM31","CUMCMPSEM32", "CUMCMPSEM33",
       "CUMCMPSEM34","CUMCMPSEM35", "CUMCMPSEM36","CUMCMPSEM37","CUMCMPSEM38",
       "CUMCMPSEM39", "CUMCMPSEM40","CUMCMPSEM41","CUMCMPSEM42","CUMCMPSEM43",
       "CUMCMPSEM44","CUMCMPSEM45","CUMCMPSEM46","CUMCMPSEM47")


library(plyr)
library(dplyr)


for(i in 1:nrow(data)){
  
  p<-data$P112LastSem[i]
  
  data$CUREnroll[i]<-data[i,EN[p+1]]
  data$CURAZcount[i]<-data[i,AZEN[p+1]]
  if(p > 0){
  data$CURCred[i]<-data[i,CUMCMP[p+1]]
  data$CURGPA[i]<-data[i,FCGPA[p]]}
  else{
    data$CURCred[i]<-0
    data$CURGPA[i]<- -1
  }
}

data$CURCred<-ifelse(is.na(data$CURCred),0,data$CURCred)

for(i in 1:nrow(data)){
  p<-data$P112LastSem[i]
  if(p>1){
  data$CURCMP[i]<-rowMeans(data[i,CMP[0:p]], na.rm = T)
  }
  if(p == 0){
    data$CURCMP[i]<-100
  }
  else{
    data$CURCMP[i]<-data$CMPSEM0[i]
  }
}

data$MathEntry<-"Cal"
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M112", "LessCal", data$MathEntry)
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M126", "LessCal", data$MathEntry)
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M128", "LessCal", data$MathEntry)
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M129", "RemCal", data$MathEntry)
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M150", "RemCal", data$MathEntry)
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M153", "RemCal", data$MathEntry)
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M251", "Adv", data$MathEntry)
data$MathEntry<-ifelse(data$MathEntryPoint2 == "M261", "Adv", data$MathEntry)


data$P112FirstAttempt<-ifelse(data$P112WVUCount>1, 0, 1)
data$P111LastGradeAB_Rest<-ifelse(data$P111LastGrade == "A",1,0) 
data$P111LastGradeAB_Rest<-ifelse(data$P111LastGrade == "A-",1,data$P111LastGradeAB_Rest) 
data$P111LastGradeAB_Rest<-ifelse(data$P111LastGrade == "A+",1,data$P111LastGradeAB_Rest) 
data$P111LastGradeAB_Rest<-ifelse(data$P111LastGrade == "B",1,data$P111LastGradeAB_Rest) 
data$P111LastGradeAB_Rest<-ifelse(data$P111LastGrade == "B-",1,data$P111LastGradeAB_Rest) 
data$P111LastGradeAB_Rest<-ifelse(data$P111LastGrade == "B+",1,data$P111LastGradeAB_Rest) 

data$Cal1LastGradeAB_Rest<-ifelse(data$Cal1LastGrade == "A",1,0) 
data$Cal1LastGradeAB_Rest<-ifelse(data$Cal1LastGrade == "A-",1,data$Cal1LastGradeAB_Rest) 
data$Cal1LastGradeAB_Rest<-ifelse(data$Cal1LastGrade == "A+",1,data$Cal1LastGradeAB_Rest) 
data$Cal1LastGradeAB_Rest<-ifelse(data$Cal1LastGrade == "B",1,data$Cal1LastGradeAB_Rest) 
data$Cal1LastGradeAB_Rest<-ifelse(data$Cal1LastGrade == "B-",1,data$Cal1LastGradeAB_Rest) 
data$Cal1LastGradeAB_Rest<-ifelse(data$Cal1LastGrade == "B+",1,data$Cal1LastGradeAB_Rest) 


data2<-data[c("Username","CURCMP","CURGPA","CURAZcount","CURCred","CUREnroll",
              "P111LastGradeAB_Rest","P111WVUCount","HSGPA","ACTSATM","ACTSATV",
              "Cal1LastGradeAB_Rest","Cal1WVUCount","APCount","APCredit","TransferCount",
              "TransferCredit","IsFirstGen","IsFirstFall","MathEntry","IsWV", "P112LastSem",
              "P112FirstAttempt")]



data3<-na.exclude(data2)
data3<-subset(data3, HSGPA != -1)
data3<-subset(data3, ACTSATM != -1)


write.csv(data3,"Intermediate-Data/RegistrarData.csv", row.names = F)
