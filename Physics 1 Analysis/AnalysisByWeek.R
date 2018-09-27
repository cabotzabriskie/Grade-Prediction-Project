#Pull In Base Dataset---------------------------------
data <- read.csv("FullDataPhysics111.csv", stringsAsFactors=F)
Ratio <-  0.6175

library(randomForest)
library(lme4)
#methods(sigma)
#install.packages("pbkrtest", dependencies = TRUE)
#install.packages("caret", dependencies = TRUE)
library(caret)
library(caTools)
library(pROC)
library(varSelRF)
library(pscl)
library(MASS)
#Break Dataset(BorBetter is the binary)


#Week1---------------------------------------

cat("\n\n\nWeek 1:      B or Better\n\n\n")

datatemp <- data

datatemp[,"HwWeek1"] <- data["HW1B"]
datatemp[,"ClTotalWeek1"] <- data$ClWeek1

DataWork <- datatemp[c("PhysicsGradeSimpleNum", "Test1", "FMCE.PrePercent47", "HwWeek1", "ClTotalWeek1", "Username", "CURCMP", "CURGPA", "CURAZcount", "CURCred", "CUREnroll", "HSGPA", "ACTSATM", "ACTSATV", "Cal1LastGradeAB_Rest", "Cal1WVUCount", "APCount", "APCredit", "TransferCount", "TransferCredit", "IsFirstGen", "IsFirstFall", "MathEntry", "IsWV")]
#DataWork <- data.frame(data$PhysicsGradeSimpleNum, data$Test1, data$FMCE.PrePercent47, data$BFI1.Finished, data$FMCE.Participate, data$HW1B, data$ClWeek1, data$Username, data$CURCMP, data$CURGPA, data$CURAZcount, data$CURCred, data$CUREnroll, data$HSGPA, data$ACTSATM, data$ACTSATV, data$Cal1LastGradeAB_Rest, data$Cal1WVUCount, data$APCount, data$APCredit, data$TransferCount, data$TransferCredit, data$IsFirstGen, data$IsFirstFall, data$MathEntry, data$IsWV)
#colnames(DataWork) <- c("Grade", "SATMath", "SATVerbal", "HSGPA", "CumulativeGPA")

DataWork<-na.omit(DataWork)

#DataWork$Survey1 <- factor(DataWork$Survey1)
#DataWork$HSGPA <- as.numeric(levels(DataWork$HSGPA))[DataWork$HSGPA]

#DataWork<-na.omit(DataWork)

#B
#Setup
dataLog <- DataWork

dataLog[, "BorBetter"] <- F

for(i in 1:nrow(dataLog)){         #for loop
  if(dataLog$PhysicsGradeSimpleNum[i]>=3) dataLog$BorBetter[i]<-T
}

#dataLog$BorBetter <- as.factor(dataLog$BorBetter)
dataLog$MathEntry <- as.factor(dataLog$MathEntry)

#RandomForrest
set.seed(86)
split <- sample.split(dataLog$BorBetter, SplitRatio = Ratio)

#get training and test data
dataLogTrain <- subset(dataLog, split == TRUE)
dataLogTest <- subset(dataLog, split == FALSE)

TestActual<-ifelse(dataLogTest$BorBetter == TRUE, 1,0)

#Baseline
ZeroRulePred<-rep(1,329)

confusionMatrix(data = as.factor(ZeroRulePred), reference = as.factor(TestActual))

ZR.Roc<-roc(TestActual, ZeroRulePred)


#Registar Model-------------------------------------------------
fit <- randomForest(as.factor(BorBetter) ~ 
                      CURCMP +
                      CURGPA +
                      CURAZcount +
                      CURCred +
                      CUREnroll +
                      HSGPA +
                      ACTSATM +
                      ACTSATV +
                      Cal1LastGradeAB_Rest +
                      Cal1WVUCount +
                      APCount +
                      APCredit +
                      TransferCount +
                      TransferCredit +
                      IsFirstGen +
                      IsFirstFall +
                      MathEntry +
                      IsWV, data = dataLogTrain, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

fit2<-varSelRF(dataLogTrain[,c("CURCMP" ,
                          "CURGPA" ,
                          "CURAZcount" ,
                          "CURCred" ,
                          "CUREnroll" ,
                          "HSGPA" ,
                          "ACTSATM" ,
                          "ACTSATV" ,
                          "Cal1LastGradeAB_Rest" ,
                          "Cal1WVUCount" ,
                          "APCount" ,
                          "APCredit" ,
                          "TransferCount" ,
                          "TransferCredit" ,
                          'IsFirstGen' ,
                          "IsFirstFall" ,
                          "MathEntry" ,
                          "IsWV" )], as.factor(dataLogTrain$BorBetter), ntree = 10000, c.sd = 1)
fit2$selected.vars
fit2$selected.model

fit2.1<-randomForest(as.factor(dataLogTrain$BorBetter) ~ 
                       Cal1WVUCount + CURCMP + CURGPA
                          , data = dataLogTrain, importance = TRUE, ntree = 10000)
varImpPlot(fit2.1)

PredictionRandomF<-predict(fit2.1, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit2.1, newdata = dataLogTest, type = 'prob')
roc.mod2<-roc(TestActual, pred[,2])
plot.roc(roc.mod2,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod2),"\n")


#Class Fit------------------------------------------------------------

fitClass <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek1 +
                      ClTotalWeek1, data = dataLogTrain, ntree = 10000)

varImpPlot(fitClass)

PredictionRandomF<-predict(fitClass, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fitClass, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek1 +
                      ClTotalWeek1 +
                      Cal1WVUCount + CURCMP + CURGPA, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit really all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek1 +
                      ClTotalWeek1 +CURCMP +
                      CURGPA +
                      CURAZcount +
                      CURCred +
                      CUREnroll +
                      HSGPA +
                      ACTSATM +
                      ACTSATV +
                      Cal1LastGradeAB_Rest +
                      Cal1WVUCount +
                      APCount +
                      APCredit +
                      TransferCount +
                      TransferCredit +
                      IsFirstGen +
                      IsFirstFall +
                      MathEntry +
                      IsWV, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")


#Logistic Regression

#All Univariate

sink("Output Text/Univariate Registrar and Week 1 Logistic Models.txt")
lapply(c("CURCMP" , "CURGPA" , "CURAZcount" , "CURCred" ,"CUREnroll" ,
         "HSGPA" ,"ACTSATM" ,"ACTSATV" ,"Cal1LastGradeAB_Rest" ,
         "Cal1WVUCount" ,"APCount" ,"APCredit" ,"TransferCount" ,"TransferCredit" ,
         'IsFirstGen' , "IsFirstFall" ,"MathEntry" ,"IsWV","FMCE.PrePercent47",
          "HwWeek1","ClTotalWeek1"),
       #lapply(c("CSEMPretestPercent" ),      
       function(var) {
         
         formula    <- as.formula(paste("as.factor(BorBetter) ~", var))
         res.logist <- glm(formula, data = dataLogTrain, family = "binomial")
         
         print(summary(res.logist))
         print(pR2(res.logist))
         pred<-predict(res.logist, newdata = dataLogTest, type = 'response')
         print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
         roc.mod<-roc(TestActual, pred)
         # cat("AUC: ",auc(roc.mod), "\n")
         cat("AUC CI: ", ci.auc(roc.mod),"\n")
         cat("#######################################################################\n")
       })
sink()
sink.number()

#Registar Only

modL1<-glm(dataLogTrain$BorBetter ~ CURCMP + CURGPA  + CURCred +CUREnroll +
           HSGPA +ACTSATM +ACTSATV +Cal1LastGradeAB_Rest +
           Cal1WVUCount +APCount +APCredit +TransferCount +TransferCredit +
           MathEntry +IsWV,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$pval<-coef(summary(modL1))[,4]
modL1Odds$Wald<-coef(summary(modL1))[,3]

write.csv(modL1Odds,"Model Fits/All Registar Logistic Fit P1.csv", row.names = T)

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")

#remove high p
modL1.1<-glm(dataLogTrain$BorBetter ~ CURCMP + CURGPA  + Cal1LastGradeAB_Rest +
                          MathEntry,  data = dataLogTrain,  family = binomial)


print(summary(modL1.1))
exp((modL1.1$coefficients))

modL1.1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1.1), confint.default(modL1.1, level = 0.95))))
modL1.1Odds$Wald<-coef(summary(modL1.1))[,3]
modL1.1Odds$pval<-coef(summary(modL1.1))[,4]


write.csv(modL1.1Odds,"Model Fits/Optimal Registar Logistic Fit P1.csv", row.names = T)


vif(modL1.1)

anova(modL1.1, test = "Chisq")

pR2(modL1.1)

pred<-predict(modL1.1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1.1<-roc(TestActual, pred)

plot.roc(roc.modL1.1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1.1),"\n")
#Using decrease in AIC

stepAIC(modL1,direction = "backward")

#AIC selected

modLAIC1<-glm(BorBetter ~ CURCMP + CURGPA + HSGPA + ACTSATM + Cal1LastGradeAB_Rest +
                Cal1WVUCount + MathEntry + IsWV,
              data = dataLogTrain,  family = binomial)

print(summary(modLAIC1))

modLAIC2<-glm(BorBetter ~ CURCMP + CURGPA + ACTSATM + Cal1LastGradeAB_Rest +
                Cal1WVUCount + MathEntry + IsWV,
              data = dataLogTrain,  family = binomial)

print(summary(modLAIC2))

modLAIC3<-glm(BorBetter ~ CURCMP + CURGPA + ACTSATM + Cal1LastGradeAB_Rest +
                Cal1WVUCount + MathEntry,
              data = dataLogTrain,  family = binomial)

print(summary(modLAIC3))

modLAIC4<-glm(BorBetter ~ CURCMP + CURGPA + ACTSATM  +
                Cal1WVUCount + MathEntry,
              data = dataLogTrain,  family = binomial)

print(summary(modLAIC4))

modLAIC4Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modLAIC4), confint.default(modLAIC4, level = 0.95))))
modLAIC4Odds$Wald<-coef(summary(modLAIC4))[,3]
modLAIC4Odds$pval<-coef(summary(modLAIC4))[,4]


write.csv(modLAIC4Odds,"Model Fits/Optimal Registar Logistic Fit P1.csv", row.names = T)


vif(modLAIC4)

anova(modLAIC4, test = "Chisq")

pR2(modLAIC4)

pred<-predict(modLAIC4, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modLAIC4<-roc(TestActual, pred)

plot.roc(roc.modLAIC4, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modLAIC4),"\n")

# CLASS Only


modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 +
             HwWeek1 +
             ClTotalWeek1,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class Wk1 Logistic Fit P1.csv", row.names = T)


vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")

#Combined



modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 +
             HwWeek1 +
             ClTotalWeek1+CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount + MathEntry,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class and Registar Wk1 suboptimal Logistic Fit P1.csv", row.names = T)

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")

stepAIC(modL1,direction = "backward")


modL2<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 +
             HwWeek1  +CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount,  data = dataLogTrain,  family = binomial)


print(summary(modL2))
exp((modL2$coefficients))
modL2Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL2), confint.default(modL2, level = 0.95))))
modL2Odds$Wald<-coef(summary(modL2))[,3]
modL2Odds$pval<-coef(summary(modL2))[,4]


write.csv(modL2Odds,"Model Fits/Class and Registar Wk1 Optimal Logistic Fit P1.csv", row.names = T)

vif(modL2)

anova(modL2, test = "Chisq")

pR2(modL2)

pred<-predict(modL2, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL2<-roc(TestActual, pred)

plot.roc(roc.modL2, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL2),"\n")

#Week2---------------------------------------

cat("\n\n\nWeek 2:      B or Better\n\n\n")

datatemp <- data

datatemp[,"HwWeek2"] <- data$HW1B + data$HW2A + data$HW2B
datatemp[,"ClTotalWeek2"] <- data$ClWeek1 + data$ClWeek2

DataWork <- datatemp[c("PhysicsGradeSimpleNum", "Test1", "FMCE.PrePercent47", "HwWeek2", "ClTotalWeek2", "Username", "CURCMP", "CURGPA", "CURAZcount", "CURCred", "CUREnroll", "HSGPA", "ACTSATM", "ACTSATV", "Cal1LastGradeAB_Rest", "Cal1WVUCount", "APCount", "APCredit", "TransferCount", "TransferCredit", "IsFirstGen", "IsFirstFall", "MathEntry", "IsWV")]
#DataWork <- data.frame(data$PhysicsGradeSimpleNum, data$Test1, data$FMCE.PrePercent47, data$BFI1.Finished, data$FMCE.Participate, data$HW1B, data$ClWeek1, data$Username, data$CURCMP, data$CURGPA, data$CURAZcount, data$CURCred, data$CUREnroll, data$HSGPA, data$ACTSATM, data$ACTSATV, data$Cal1LastGradeAB_Rest, data$Cal1WVUCount, data$APCount, data$APCredit, data$TransferCount, data$TransferCredit, data$IsFirstGen, data$IsFirstFall, data$MathEntry, data$IsWV)
#colnames(DataWork) <- c("Grade", "SATMath", "SATVerbal", "HSGPA", "CumulativeGPA")

DataWork<-na.omit(DataWork)

#DataWork$Survey1 <- factor(DataWork$Survey1)
#DataWork$HSGPA <- as.numeric(levels(DataWork$HSGPA))[DataWork$HSGPA]

#DataWork<-na.omit(DataWork)

#B
#Setup
dataLog <- DataWork

dataLog[, "BorBetter"] <- F

for(i in 1:nrow(dataLog)){         #for loop
  if(dataLog$PhysicsGradeSimpleNum[i]>=3) dataLog$BorBetter[i]<-T
}

#dataLog$BorBetter <- as.factor(dataLog$BorBetter)
dataLog$MathEntry <- as.factor(dataLog$MathEntry)

#RandomForrest
set.seed(86)
split <- sample.split(dataLog$BorBetter, SplitRatio = Ratio)

#get training and test data
dataLogTrain <- subset(dataLog, split == TRUE)
dataLogTest <- subset(dataLog, split == FALSE)

#dataLogTrain$BorBetter <- as.factor(dataLogTrain$BorBetter)

#Class Fit------------------------------------------------------------

fitClass <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                           HwWeek2 +
                           ClTotalWeek2, data = dataLogTrain, ntree = 10000)

varImpPlot(fitClass)

PredictionRandomF<-predict(fitClass, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fitClass, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek2 +
                      ClTotalWeek2 +ACTSATM  +
                      Cal1WVUCount + MathEntry, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit really all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek2 +
                      ClTotalWeek2 +CURCMP +
                      CURGPA +
                      CURAZcount +
                      CURCred +
                      CUREnroll +
                      HSGPA +
                      ACTSATM +
                      ACTSATV +
                      Cal1LastGradeAB_Rest +
                      Cal1WVUCount +
                      APCount +
                      APCredit +
                      TransferCount +
                      TransferCredit +
                      IsFirstGen +
                      IsFirstFall +
                      MathEntry +
                      IsWV, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#Logistic Regression

#All Univariate

sink("Output Text/Univariate Week 2 Logistic Models.txt")
lapply(c("FMCE.PrePercent47",
         "HwWeek2","ClTotalWeek2"),
       #lapply(c("CSEMPretestPercent" ),      
       function(var) {
         
         formula    <- as.formula(paste("as.factor(BorBetter) ~", var))
         res.logist <- glm(formula, data = dataLogTrain, family = "binomial")
         
         print(summary(res.logist))
         print(pR2(res.logist))
         pred<-predict(res.logist, newdata = dataLogTest, type = 'response')
         print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
         roc.mod<-roc(TestActual, pred)
         # cat("AUC: ",auc(roc.mod), "\n")
         cat("AUC CI: ", ci.auc(roc.mod),"\n")
         cat("#######################################################################\n")
       })
sink()
sink.number()


# CLASS Only


modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek2 + ClTotalWeek2,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))

modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class Wk2 Logistic Fit P1.csv", row.names = T)

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")

#Combined



modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek2 + ClTotalWeek2+
             CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount + MathEntry,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class and Registar Wk2 suboptimal Logistic Fit P1.csv", row.names = T)

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)

stepAIC(modL1,direction = "backward")

modL2<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek2 + ClTotalWeek2+
             CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount,  data = dataLogTrain,  family = binomial)


print(summary(modL2))
exp((modL2$coefficients))
modL2Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL2), confint.default(modL2, level = 0.95))))
modL2Odds$Wald<-coef(summary(modL2))[,3]
modL2Odds$pval<-coef(summary(modL2))[,4]


write.csv(modL2Odds,"Model Fits/Class and Registar Wk2 Optimal Logistic Fit P1.csv", row.names = T)

vif(modL2)

anova(modL2, test = "Chisq")

pR2(modL2)

pred<-predict(modL2, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL2<-roc(TestActual, pred)

plot.roc(roc.modL2, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL2),"\n")

#Week3---------------------------------------

cat("\n\n\nWeek 3:      B or Better\n\n\n")

datatemp <- data

datatemp[,"HwWeek3"] <- data$HW1B + data$HW2A + data$HW2B + data$HW3A + data$HW3B
datatemp[,"ClTotalWeek3"] <- data$ClWeek1 + data$ClWeek2 + data$ClWeek3

DataWork <- datatemp[c("PhysicsGradeSimpleNum", "Test1", "FMCE.PrePercent47", "HwWeek3", "ClTotalWeek3", "Username", "CURCMP", "CURGPA", "CURAZcount", "CURCred", "CUREnroll", "HSGPA", "ACTSATM", "ACTSATV", "Cal1LastGradeAB_Rest", "Cal1WVUCount", "APCount", "APCredit", "TransferCount", "TransferCredit", "IsFirstGen", "IsFirstFall", "MathEntry", "IsWV")]
#DataWork <- data.frame(data$PhysicsGradeSimpleNum, data$Test1, data$FMCE.PrePercent47, data$BFI1.Finished, data$FMCE.Participate, data$HW1B, data$ClWeek1, data$Username, data$CURCMP, data$CURGPA, data$CURAZcount, data$CURCred, data$CUREnroll, data$HSGPA, data$ACTSATM, data$ACTSATV, data$Cal1LastGradeAB_Rest, data$Cal1WVUCount, data$APCount, data$APCredit, data$TransferCount, data$TransferCredit, data$IsFirstGen, data$IsFirstFall, data$MathEntry, data$IsWV)
#colnames(DataWork) <- c("Grade", "SATMath", "SATVerbal", "HSGPA", "CumulativeGPA")

DataWork<-na.omit(DataWork)

#DataWork$Survey1 <- factor(DataWork$Survey1)
#DataWork$HSGPA <- as.numeric(levels(DataWork$HSGPA))[DataWork$HSGPA]

#DataWork<-na.omit(DataWork)

#B
#Setup
dataLog <- DataWork

dataLog[, "BorBetter"] <- F

for(i in 1:nrow(dataLog)){         #for loop
  if(dataLog$PhysicsGradeSimpleNum[i]>=3) dataLog$BorBetter[i]<-T
}

#dataLog$BorBetter <- as.factor(dataLog$BorBetter)
dataLog$MathEntry <- as.factor(dataLog$MathEntry)

#RandomForrest
set.seed(86)
split <- sample.split(dataLog$BorBetter, SplitRatio = Ratio)

#get training and test data
dataLogTrain <- subset(dataLog, split == TRUE)
dataLogTest <- subset(dataLog, split == FALSE)

#dataLogTrain$BorBetter <- as.factor(dataLogTrain$BorBetter)

#Class Fit------------------------------------------------------------

fitClass <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                           HwWeek3 +
                           ClTotalWeek3, data = dataLogTrain, ntree = 10000)

varImpPlot(fitClass)

PredictionRandomF<-predict(fitClass, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fitClass, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek3 +
                      ClTotalWeek3 +
                      Cal1WVUCount + CURCMP + CURGPA, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit really all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek3 +
                      ClTotalWeek3 +CURCMP +
                      CURGPA +
                      CURAZcount +
                      CURCred +
                      CUREnroll +
                      HSGPA +
                      ACTSATM +
                      ACTSATV +
                      Cal1LastGradeAB_Rest +
                      Cal1WVUCount +
                      APCount +
                      APCredit +
                      TransferCount +
                      TransferCredit +
                      IsFirstGen +
                      IsFirstFall +
                      MathEntry +
                      IsWV, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")


#Logistic Regression

#All Univariate

sink("Output Text/Univariate Week 2 Logistic Models.txt")
lapply(c("FMCE.PrePercent47",
         "HwWeek3","ClTotalWeek3"),
       #lapply(c("CSEMPretestPercent" ),      
       function(var) {
         
         formula    <- as.formula(paste("as.factor(BorBetter) ~", var))
         res.logist <- glm(formula, data = dataLogTrain, family = "binomial")
         
         print(summary(res.logist))
         print(pR2(res.logist))
         pred<-predict(res.logist, newdata = dataLogTest, type = 'response')
         print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
         roc.mod<-roc(TestActual, pred)
         # cat("AUC: ",auc(roc.mod), "\n")
         cat("AUC CI: ", ci.auc(roc.mod),"\n")
         cat("#######################################################################\n")
       })
sink()
sink.number()


# CLASS Only


modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek3 + ClTotalWeek3,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class Wk3 Logistic Fit P1.csv", row.names = T)

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")

#Combined



modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek3 + ClTotalWeek3+
             CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount + MathEntry,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class and Registar Wk3 suboptimal Logistic Fit P1.csv", row.names = T)
vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")


stepAIC(modL1,direction = "backward")


modL2<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek3 + ClTotalWeek3+
             CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount,  data = dataLogTrain,  family = binomial)


print(summary(modL2))
exp((modL2$coefficients))
modL2Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL2), confint.default(modL2, level = 0.95))))
modL2Odds$Wald<-coef(summary(modL2))[,3]
modL2Odds$pval<-coef(summary(modL2))[,4]


write.csv(modL2Odds,"Model Fits/Class and Registar Wk3 Optimal Logistic Fit P1.csv", row.names = T)

vif(modL2)

anova(modL2, test = "Chisq")

pR2(modL2)

pred<-predict(modL2, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL2<-roc(TestActual, pred)

plot.roc(roc.modL2, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL2),"\n")



#Week4---------------------------------------

cat("\n\n\nWeek 4:      B or Better\n\n\n")

datatemp <- data

datatemp[,"HwWeek4"] <- data$HW1B + data$HW2A + data$HW2B + data$HW3A + data$HW3B + data$HW4A + data$HW4B
datatemp[,"ClTotalWeek4"] <- data$ClWeek1 + data$ClWeek2 + data$ClWeek3 + data$ClWeek4

DataWork <- datatemp[c("PhysicsGradeSimpleNum", "Test1", "FMCE.PrePercent47", "HwWeek4", "ClTotalWeek4", "Username", "CURCMP", "CURGPA", "CURAZcount", "CURCred", "CUREnroll", "HSGPA", "ACTSATM", "ACTSATV", "Cal1LastGradeAB_Rest", "Cal1WVUCount", "APCount", "APCredit", "TransferCount", "TransferCredit", "IsFirstGen", "IsFirstFall", "MathEntry", "IsWV")]
#DataWork <- data.frame(data$PhysicsGradeSimpleNum, data$Test1, data$FMCE.PrePercent47, data$BFI1.Finished, data$FMCE.Participate, data$HW1B, data$ClWeek1, data$Username, data$CURCMP, data$CURGPA, data$CURAZcount, data$CURCred, data$CUREnroll, data$HSGPA, data$ACTSATM, data$ACTSATV, data$Cal1LastGradeAB_Rest, data$Cal1WVUCount, data$APCount, data$APCredit, data$TransferCount, data$TransferCredit, data$IsFirstGen, data$IsFirstFall, data$MathEntry, data$IsWV)
#colnames(DataWork) <- c("Grade", "SATMath", "SATVerbal", "HSGPA", "CumulativeGPA")

DataWork<-na.omit(DataWork)

#DataWork$Survey1 <- factor(DataWork$Survey1)
#DataWork$HSGPA <- as.numeric(levels(DataWork$HSGPA))[DataWork$HSGPA]

#DataWork<-na.omit(DataWork)

#B
#Setup
dataLog <- DataWork

dataLog[, "BorBetter"] <- F

for(i in 1:nrow(dataLog)){         #for loop
  if(dataLog$PhysicsGradeSimpleNum[i]>=3) dataLog$BorBetter[i]<-T
}

#dataLog$BorBetter <- as.factor(dataLog$BorBetter)
dataLog$MathEntry <- as.factor(dataLog$MathEntry)

#RandomForrest
set.seed(86)
split <- sample.split(dataLog$BorBetter, SplitRatio = Ratio)

#get training and test data
dataLogTrain <- subset(dataLog, split == TRUE)
dataLogTest <- subset(dataLog, split == FALSE)

#dataLogTrain$BorBetter <- as.factor(dataLogTrain$BorBetter)

#Class Fit------------------------------------------------------------

fitClass <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                           HwWeek4 +
                           ClTotalWeek4, data = dataLogTrain, ntree = 10000)

varImpPlot(fitClass)

PredictionRandomF<-predict(fitClass, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fitClass, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek4 +
                      ClTotalWeek4 +
                      Cal1WVUCount + CURCMP + CURGPA, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit really all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek4 +
                      ClTotalWeek4 +CURCMP +
                      CURGPA +
                      CURAZcount +
                      CURCred +
                      CUREnroll +
                      HSGPA +
                      ACTSATM +
                      ACTSATV +
                      Cal1LastGradeAB_Rest +
                      Cal1WVUCount +
                      APCount +
                      APCredit +
                      TransferCount +
                      TransferCredit +
                      IsFirstGen +
                      IsFirstFall +
                      MathEntry +
                      IsWV, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#Logistic Regression

#All Univariate

sink("Output Text/Univariate Week 2 Logistic Models.txt")
lapply(c("FMCE.PrePercent47",
         "HwWeek4","ClTotalWeek4"),
       #lapply(c("CSEMPretestPercent" ),      
       function(var) {
         
         formula    <- as.formula(paste("as.factor(BorBetter) ~", var))
         res.logist <- glm(formula, data = dataLogTrain, family = "binomial")
         
         print(summary(res.logist))
         print(pR2(res.logist))
         pred<-predict(res.logist, newdata = dataLogTest, type = 'response')
         print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
         roc.mod<-roc(TestActual, pred)
         # cat("AUC: ",auc(roc.mod), "\n")
         cat("AUC CI: ", ci.auc(roc.mod),"\n")
         cat("#######################################################################\n")
       })
sink()
sink.number()


# CLASS Only


modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek4 + ClTotalWeek4,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class Wk4 Logistic Fit P1.csv", row.names = T)

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")

#Combined



modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek4 + ClTotalWeek4+
             CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount + MathEntry,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class and Registar Wk4 suboptimal Logistic Fit P1.csv", row.names = T)
vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)


stepAIC(modL1,direction = "backward")


modL2<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek4 + ClTotalWeek4+
             CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount,  data = dataLogTrain,  family = binomial)


print(summary(modL2))
exp((modL2$coefficients))
modL2Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL2), confint.default(modL2, level = 0.95))))
modL2Odds$Wald<-coef(summary(modL2))[,3]
modL2Odds$pval<-coef(summary(modL2))[,4]


write.csv(modL2Odds,"Model Fits/Class and Registar Wk4 Optimal Logistic Fit P1.csv", row.names = T)

vif(modL2)

anova(modL2, test = "Chisq")

pR2(modL2)

pred<-predict(modL2, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL2<-roc(TestActual, pred)

plot.roc(roc.modL2, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL2),"\n")




#Week5---------------------------------------

cat("\n\n\nWeek 5:      B or Better\n\n\n")

datatemp <- data

datatemp[,"HwWeek5"] <- data$HW1B + data$HW2A + data$HW2B + data$HW3A + data$HW3B + data$HW4A + data$HW4B + data$HW5A + data$HW5B
datatemp[,"ClTotalWeek5"] <- data$ClWeek1 + data$ClWeek2 + data$ClWeek3 + data$ClWeek4 + data$ClWeek5

DataWork <- datatemp[c("PhysicsGradeSimpleNum", "Test1", "FMCE.PrePercent47", "HwWeek5", "ClTotalWeek5", "Username", "CURCMP", "CURGPA", "CURAZcount", "CURCred", "CUREnroll", "HSGPA", "ACTSATM", "ACTSATV", "Cal1LastGradeAB_Rest", "Cal1WVUCount", "APCount", "APCredit", "TransferCount", "TransferCredit", "IsFirstGen", "IsFirstFall", "MathEntry", "IsWV")]
#DataWork <- data.frame(data$PhysicsGradeSimpleNum, data$Test1, data$FMCE.PrePercent47, data$BFI1.Finished, data$FMCE.Participate, data$HW1B, data$ClWeek1, data$Username, data$CURCMP, data$CURGPA, data$CURAZcount, data$CURCred, data$CUREnroll, data$HSGPA, data$ACTSATM, data$ACTSATV, data$Cal1LastGradeAB_Rest, data$Cal1WVUCount, data$APCount, data$APCredit, data$TransferCount, data$TransferCredit, data$IsFirstGen, data$IsFirstFall, data$MathEntry, data$IsWV)
#colnames(DataWork) <- c("Grade", "SATMath", "SATVerbal", "HSGPA", "CumulativeGPA")

DataWork<-na.omit(DataWork)

#DataWork$Survey1 <- factor(DataWork$Survey1)
#DataWork$HSGPA <- as.numeric(levels(DataWork$HSGPA))[DataWork$HSGPA]

#DataWork<-na.omit(DataWork)

#B
#Setup
dataLog <- DataWork

dataLog[, "BorBetter"] <- F

for(i in 1:nrow(dataLog)){         #for loop
  if(dataLog$PhysicsGradeSimpleNum[i]>=3) dataLog$BorBetter[i]<-T
}

#dataLog$BorBetter <- as.factor(dataLog$BorBetter)
dataLog$MathEntry <- as.factor(dataLog$MathEntry)

#RandomForrest
set.seed(86)
split <- sample.split(dataLog$BorBetter, SplitRatio = Ratio)

#get training and test data
dataLogTrain <- subset(dataLog, split == TRUE)
dataLogTest <- subset(dataLog, split == FALSE)

#dataLogTrain$BorBetter <- as.factor(dataLogTrain$BorBetter)

#Class Fit------------------------------------------------------------

fitClass <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                           HwWeek5 +
                           ClTotalWeek5 + Test1, data = dataLogTrain, ntree = 10000)

varImpPlot(fitClass)

PredictionRandomF<-predict(fitClass, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fitClass, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek5 +
                      ClTotalWeek5 + Test1+
                      Cal1WVUCount + CURCMP + CURGPA, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#fit really all------------------------

fit <- randomForest(as.factor(BorBetter) ~ FMCE.PrePercent47 +
                      HwWeek5 +
                      ClTotalWeek5 +
                      Test1+
                      CURCMP +
                      CURGPA +
                      CURAZcount +
                      CURCred +
                      CUREnroll +
                      HSGPA +
                      ACTSATM +
                      ACTSATV +
                      Cal1LastGradeAB_Rest +
                      Cal1WVUCount +
                      APCount +
                      APCredit +
                      TransferCount +
                      TransferCredit +
                      IsFirstGen +
                      IsFirstFall +
                      MathEntry +
                      IsWV, data = dataLogTrain, ntree = 10000)
#print(fit) 
print(importance(fit)) 

PredictionRandomF<-predict(fit, dataLogTest)
confusionMatrix(PredictionRandomF, as.factor(dataLogTest$BorBetter))
pred<-predict(fit, newdata = dataLogTest, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")



#Logistic Regression

#All Univariate

sink("Output Text/Univariate Week 2 Logistic Models.txt")
lapply(c("FMCE.PrePercent47",
         "HwWeek5","ClTotalWeek5","Test1"),
       #lapply(c("CSEMPretestPercent" ),      
       function(var) {
         
         formula    <- as.formula(paste("as.factor(BorBetter) ~", var))
         res.logist <- glm(formula, data = dataLogTrain, family = "binomial")
         
         print(summary(res.logist))
         print(pR2(res.logist))
         pred<-predict(res.logist, newdata = dataLogTest, type = 'response')
         print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
         roc.mod<-roc(TestActual, pred)
         # cat("AUC: ",auc(roc.mod), "\n")
         cat("AUC CI: ", ci.auc(roc.mod),"\n")
         cat("#######################################################################\n")
       })
sink()
sink.number()


# CLASS Only


modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek5 + ClTotalWeek5 + Test1,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class Wk5 Logistic Fit P1.csv", row.names = T)

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1),"\n")

#Combined



modL1<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek5 + ClTotalWeek5 + Test1+
             CURCMP + CURGPA + ACTSATM  +
             Cal1WVUCount + MathEntry,  data = dataLogTrain,  family = binomial)


print(summary(modL1))
exp((modL1$coefficients))
modL1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1), confint.default(modL1, level = 0.95))))
modL1Odds$Wald<-coef(summary(modL1))[,3]
modL1Odds$pval<-coef(summary(modL1))[,4]


write.csv(modL1Odds,"Model Fits/Class and Registar Wk5 suboptimal Logistic Fit P1.csv", row.names = T)
vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)


stepAIC(modL1,direction = "backward")

modL2<-glm(dataLogTrain$BorBetter ~ FMCE.PrePercent47 + HwWeek5 + ClTotalWeek5 + Test1+
             CURCMP + CURGPA ,  data = dataLogTrain,  family = binomial)


print(summary(modL2))
exp((modL2$coefficients))
modL2Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL2), confint.default(modL2, level = 0.95))))
modL2Odds$Wald<-coef(summary(modL2))[,3]
modL2Odds$pval<-coef(summary(modL2))[,4]


write.csv(modL2Odds,"Model Fits/Class and Registar Wk5 Optimal Logistic Fit P1.csv", row.names = T)

vif(modL2)

anova(modL2, test = "Chisq")

pR2(modL2)

pred<-predict(modL2, newdata = dataLogTest, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL2<-roc(TestActual, pred)

plot.roc(roc.modL2, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL2),"\n")
