library(caret)
library(car)
library(pscl)
library(ROCR)
library(pROC)
library(glmnet)
library(plotmo)
library(randomForest)
library(varSelRF)
library(MASS)
wk3<-read.csv("Intermediate-Data/Week_3_data.csv", stringsAsFactors = F)
wk3$dup1<-duplicated(wk3$Username, fromLast=F)
wk3<-subset(wk3, !is.na(LabSection))
wk3<-subset(wk3, dup1!=TRUE)

reg<-read.csv("Intermediate-Data/RegistrarData.csv", stringsAsFactors = F)

data<-merge(wk3, reg, by = "Username", all.y = T)
datasave<-data
rm(reg,wk3)



data<-subset(data, !is.na(HSGPA))
data<-subset(data, !is.na(ACTSATM))
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

##########################################
#Logistic Regression: Doing this Right-----------------------------
###################################################################

#model building

train$FinalCourseGradeAB_Rest2<-ifelse(train$FinalCourseGradeAB_Rest == "AB",1,0)
test$FinalCourseGradeAB_Rest2<-ifelse(test$FinalCourseGradeAB_Rest == "AB",1,0)
train$TakeCSEM2<-ifelse(train$TakeCSEM == "Y",1,0)
test$TakeCSEM2<-ifelse(test$TakeCSEM == "Y",1,0)

####
#Repeated

sink("Output Text/Univariate wk3 Logistic Models.txt")
lapply(c("CSEMPretestPercent" , "TakeCSEM" , "LecQGrade" ,"LabQGrade" ,
         "HWGrade" ,"LabGrade","Semester","BonusPointsTot"),
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
             # BonusPoints+
             BonusPointsTot, data = train, family = "binomial"
)


print(summary(modL1))
exp((modL1$coefficients))

vif(modL1)

anova(modL1, test = "Chisq")

pR2(modL1)

pred<-predict(modL1, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1<-roc(TestActual, pred)

plot.roc(roc.modL1, print.thres = F, print.auc = T, legacy.axes = T)


#remove lecQGrade due to high p

modL1.1<-glm(train$FinalCourseGradeAB_Rest2 ~ 
               LabQGrade + HWGrade +CSEMPretestPercent, data = train, family = "binomial"
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

#remove more p

modL1.2<-glm(train$FinalCourseGradeAB_Rest2 ~ 
               LabQGrade + HWGrade, data = train, family = "binomial"
)


print(summary(modL1.2))
exp((modL1.2$coefficients))

modL1.2Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1.2), confint.default(modL1.2, level = 0.95))))
modL1.2Odds$Wald<-coef(summary(modL1.2))[,3]
modL1.2Odds$pval<-coef(summary(modL1.2))[,4]

write.csv(modL1.2Odds,"Model Fits/Class Wk3 Logistic Fit P2.csv", row.names = T)

vif(modL1.2)

anova(modL1.2, test = "Chisq")

pR2(modL1.2)

pred<-predict(modL1.2, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1.2<-roc(TestActual, pred)

plot.roc(roc.modL1.2, print.thres = F, print.auc = T, legacy.axes = T)

cat("AUC CI: ", ci.auc(roc.modL1.2),"\n")
#compare ROC to last one

roc.test(roc.modL1,roc.modL1.1)
#not significantly different therefore we select modL1.1 here



#Using AIC

stepAIC(modL1,direction = "backward")

#Select modL1.1




##################################
#Random Forests-------------------
##################################


#The usual way

fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                    LecQGrade +
                    LabQGrade + 
                    HWGrade + 
                    TakeCSEM2 +
                    CSEMPretestPercent +
                    LabGrade +
                    BonusPointsTot+
                    Semester, data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#Using varSelRF


fit2<-varSelRF(train[,c("CSEMPretestPercent" , "TakeCSEM2" , "LecQGrade" ,"LabQGrade" ,
                        "HWGrade" ,"LabGrade","Semester","BonusPointsTot" )],
               as.factor(train$FinalCourseGradeAB_Rest2), ntree = 10000)
fit2$selected.vars
fit2$selected.model

fit2.1<-fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                       #     CSEMPretestPercent +
                            HWGrade +
                            LabQGrade +
                            LecQGrade
                          , data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod2<-roc(TestActual, pred[,2])
plot.roc(roc.mod2,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod2),"\n")


roc.test(roc.mod,roc.mod2)






######################################
#Combined Model----------------------
######################################


#Logit

fitLog<-glm(train$FinalCourseGradeAB_Rest2 ~ 
           #   LecQGrade +
              HWGrade+
              LabQGrade+ 
              CURGPA +
              P111LastGradeAB_Rest, data = train, family = "binomial"
)


print(summary(fitLog))
exp((fitLog$coefficients))

fitLogOdds<-as.data.frame(exp(cbind("Odds ratio" = coef(fitLog), confint.default(fitLog, level = 0.95))))
fitLogOdds$Wald<-coef(summary(fitLog))[,3]
fitLogOdds$pval<-coef(summary(fitLog))[,4]

write.csv(fitLogOdds,"Model Fits/Class and registrar Wk3 Logistic Fit P2.csv", row.names = T)

vif(fitLog)

anova(fitLog, test = "Chisq")

pR2(fitLog)

pred<-predict(fitLog, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.fitLog<-roc(TestActual, pred)

plot.roc(roc.fitLog, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.fitLog),"\n")

#RF

fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                 #   CSEMPretestPercent +
                    LecQGrade +
                    HWGrade+
                    LabQGrade+ 
                    CURGPA +
                    P111LastGradeAB_Rest, data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")



