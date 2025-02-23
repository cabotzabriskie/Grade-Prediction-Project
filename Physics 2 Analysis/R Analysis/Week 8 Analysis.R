library(caret)
library(car)
library(pscl)
library(ROCR)
library(pROC)
library(randomForest)
library(varSelRF)
library(MASS)
wk8<-read.csv("Intermediate-Data/Week_8_data.csv", stringsAsFactors = F)
wk8$dup1<-duplicated(wk8$Username, fromLast=F)
wk8<-subset(wk8, !is.na(LabSection))
wk8<-subset(wk8, dup1!=TRUE)

reg<-read.csv("Intermediate-Data/RegistrarData.csv", stringsAsFactors = F)

data<-merge(wk8, reg, by = "Username", all.y = T)
datasave<-data
rm(reg,wk8)



data<-subset(data, !is.na(HSGPA))
data<-subset(data, !is.na(ACTSATM))
data<-subset(data, !is.na(Test2))
#############
#New Vars----
#############

lecQs<-grep(".*(LecQ).", names(data))
labQs<-grep(".*(LabQ).", names(data))
labs<-grep(".*(Lab\\d).*", names(data))
Fhwks<-grep(".*(FHwk).", names(data))
LecMiss<-grep(".*(Lec)\\d(Miss).*", names(data))
LabMiss<-grep(".*(Lab)\\d(Miss).*", names(data))
Sbonus<-grep(".*(SBonus)\\d.*", names(data))
labs<-setdiff(labs, LabMiss)

data$LecQGrade<-rowMeans(subset(data, select = lecQs), na.rm = T)
data$LecQGrade<-ifelse(is.na(data$LecQGrade), 0, data$LecQGrade)
data$LabQGrade<-rowMeans(subset(data, select = labQs), na.rm = T)
data$LabQGrade<-ifelse(is.na(data$LabQGrade), 0, data$LabQGrade)
data$LabGrade<-rowMeans(subset(data, select = labs), na.rm = T)
data$LabGrade<-ifelse(is.na(data$LabGrade), 0, data$LabGrade)
data$HWGrade<-rowMeans(subset(data, select = Fhwks), na.rm = T)
data$HWGrade<-ifelse(is.na(data$HWGrade), 0, data$HWGrade)
data$MissLect<-rowSums(subset(data, select = LecMiss), na.rm = T)
data$MissLab<-rowSums(subset(data, select = LabMiss), na.rm = T)
data$MissAnyLectures<-ifelse(data$MissLect > 0, "Y","N")
data$MissAnyLabs<-ifelse(data$MissLab > 0, "Y","N")
data$BonusPoints<-rowMeans(subset(data, select = Sbonus), na.rm = T)
data$BonusPoints<-ifelse(is.na(data$BonusPoints), 0, data$BonusPoints)
data$BonusPointsTot<-rowSums(subset(data, select = Sbonus), na.rm = T)
data$BonusPointsTot<-ifelse(is.na(data$BonusPointsTot), 0, data$BonusPointsTot)

data$Semester<-as.factor(data$Semester)

#####################
#Train Test Split----
#####################

set.seed(416)

#train_index <- sample(1:nrow(data), 0.63 * nrow(data))
#test_index <- setdiff(1:nrow(data), train_index)

#write.csv(train_index,"Intermediate-Data/train_index.csv", row.names = F)
#write.csv(test_index,"Intermediate-Data/test_index.csv", row.names = F)
trI<-as.vector(read.csv("Intermediate-Data/train_index.csv", stringsAsFactors = F))
teI<-as.vector(read.csv("Intermediate-Data/test_index.csv", stringsAsFactors = F))
train_index<-trI[,1]
test_index<-teI[,1]

train<-data[train_index,]
test<-data[test_index,]

#############
#Analysis CLASS Variables----
#############

TestActual<-ifelse(test$FinalCourseGradeAB_Rest == "AB", 1,0)
###############
#baseline model---------
###############

#Zero Rule model: guess everyone gets an AB

ZeroRulePred<-rep(1,297)

confusionMatrix(data = as.factor(ZeroRulePred), reference = as.factor(TestActual))

ZR.Roc<-roc(TestActual, ZeroRulePred)

#Bootstrapped guessing parameter
rm(.Random.seed, envir=globalenv())
for(i in 1:10000){
  
  s<-sample(c(0,1), 292, prob = c(.3,.7), replace = T)
  cm<-confusionMatrix(as.factor(s), as.factor(TestActual))
  acc<-cm[["byClass"]][["Balanced Accuracy"]]
  if(i == 1){
    BLacc<-acc
  }
  else{
    BLacc<-append(BLacc,acc)
  }
}

BLacc2<-mean(BLacc)

set.seed(416)
###################################################################
#Logistic Regression: Doing this Right-----------------------------
###################################################################

#model building

train$FinalCourseGradeAB_Rest2<-ifelse(train$FinalCourseGradeAB_Rest == "AB",1,0)
test$FinalCourseGradeAB_Rest2<-ifelse(test$FinalCourseGradeAB_Rest == "AB",1,0)
train$TakeCSEM2<-ifelse(train$TakeCSEM == "Y",1,0)
test$TakeCSEM2<-ifelse(test$TakeCSEM == "Y",1,0)

####
#Repeated

sink("Output Text/Univariate wk8 Logistic Models.txt")
lapply(c("CSEMPretestPercent" , "TakeCSEM" , "LecQGrade" ,"LabQGrade" ,
         "HWGrade" ,"LabGrade","Semester","Test1","Test2", "BonusPoints","BonusPointsTot"),
       #lapply(c("CSEMPretestPercent" ),      
       function(var) {
         
         formula    <- as.formula(paste("as.factor(FinalCourseGradeAB_Rest2) ~", var))
         res.logist <- glm(formula, data = train, family = "binomial")
         
         print(summary(res.logist))
         
         pred<-predict(res.logist, newdata = test, type = 'response')
         print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
         roc.mod<-roc(TestActual, pred)
         # cat("AUC: ",auc(roc.mod), "\n")
         cat("AUC CI: ", ci.auc(roc.mod),"\n")
         cat("#######################################################################\n")
       })
sink()
sink.number()



##########################################
#Multivariable Logistic Model-------------
##########################################

#Using the traditional method
modL1<-glm(train$FinalCourseGradeAB_Rest2 ~ CSEMPretestPercent +
             #  TakeCSEM +
             LecQGrade +
             LabQGrade +
             HWGrade +
             LabGrade+
             Semester+
             Test1+
             Test2+
             # BonusPoints+
             BonusPointsTot, data = train, family = binomial)


print(summary(modL1))
exp((modL1$coefficients))

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)


#remove all p > .1 due to high p

modL1.1<-glm(train$FinalCourseGradeAB_Rest2 ~ #CSEMPretestPercent +
               #  TakeCSEM +
               LecQGrade +
               LabQGrade +
               HWGrade +
               LabGrade+
               #  Semester+
               Test1 +
               Test2
             # BonusPoints+
             #BonusPointsTot
             , data = train, family = "binomial"
)


print(summary(modL1.1))
exp((modL1.1$coefficients))

vif(modL1.1)

anova(modL1.1, test = "Chisq")

pR2(modL1.1)

pred<-predict(modL1.1, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1.1<-roc(TestActual, pred)

plot.roc(roc.modL1.1, print.thres = F, print.auc = T, legacy.axes = T)

#Remove More high P-valued

modL1.2<-glm(train$FinalCourseGradeAB_Rest2 ~ LecQGrade +
               LabQGrade +
               HWGrade +
               #LabGrade+
               #  Semester+
               Test1 +
               Test2
             , data = train, family = "binomial"
)

print(summary(modL1.2))
exp((modL1.2$coefficients))

vif(modL1.2)

anova(modL1.2, test = "Chisq")

pR2(modL1.2)

pred<-predict(modL1.2, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1.2<-roc(TestActual, pred)

plot.roc(roc.modL1.2, print.thres = F, print.auc = T, legacy.axes = T)

#Remove More high P-valued

modL1.3<-glm(train$FinalCourseGradeAB_Rest2 ~ #LecQGrade +
               LabQGrade +
               HWGrade +
               #LabGrade+
               #  Semester+
               Test1 +
               Test2
             , data = train, family = "binomial"
)

print(summary(modL1.3))
exp((modL1.3$coefficients))

vif(modL1.3)

anova(modL1.3, test = "Chisq")

pR2(modL1.3)

pred<-predict(modL1.3, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1.3<-roc(TestActual, pred)

plot.roc(roc.modL1.3, print.thres = F, print.auc = T, legacy.axes = T)

#compare ROC to last one

roc.test(roc.modL1,roc.modL1.1)
roc.test(roc.modL1.1,roc.modL1.2)
roc.test(roc.modL1.2,roc.modL1.3)




#Using AIC

stepAIC(modL1,direction = "backward")

#Select modL1.1




##################################
#Random Forests-------------------
##################################


#The usual way

fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                    CSEMPretestPercent +
                    TakeCSEM2 +
                    LecQGrade +
                    LabQGrade +
                    HWGrade +
                    LabGrade+
                    Semester+
                    Test1+
                    Test2+
                    BonusPoints+
                    BonusPointsTot, data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#Using varSelRF


fit2<-varSelRF(train[,c("CSEMPretestPercent" , "TakeCSEM2" , "LecQGrade" ,"LabQGrade" ,
                        "HWGrade" ,"LabGrade","Semester","Test1","Test2", "BonusPoints","BonusPointsTot")],
               as.factor(train$FinalCourseGradeAB_Rest2), ntree = 10000)
fit2$selected.vars
fit2$selected.model

fit2.1<-fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                            HWGrade +  Test1 + Test2
                          , data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit2.1)

PredictionRandomF<-predict(fit2.1, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit2.1, newdata = test, type = 'prob')
roc.mod2<-roc(TestActual, pred[,2])
plot.roc(roc.mod2,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod2),"\n")


roc.test(roc.mod,roc.mod2)






######################################
#Combined Model----------------------
######################################


#Logit


fitLog<-glm(train$FinalCourseGradeAB_Rest2 ~ 
              HWGrade + LabQGrade +
              #LecQGrade +
              Test1+
              #CURGPA +
              P111LastGradeAB_Rest, data = train, family = "binomial"
)


print(summary(fitLog))
exp((fitLog$coefficients))

vif(fitLog)

anova(fitLog, test = "Chisq")

pR2(fitLog)

pred<-predict(fitLog, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.fitLog<-roc(TestActual, pred)

plot.roc(roc.fitLog, print.thres = F, print.auc = T, legacy.axes = T)

#RF

fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                    HWGrade + LabQGrade + LecQGrade + Test1+
                    CURGPA +
                    P111LastGradeAB_Rest, data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")



