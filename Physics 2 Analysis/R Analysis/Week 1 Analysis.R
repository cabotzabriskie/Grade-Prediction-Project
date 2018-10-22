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
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(Rmisc)
wk1<-read.csv("Intermediate-Data/Week_1_data.csv", stringsAsFactors = F)
wk1$dup1<-duplicated(wk1$Username, fromLast=F)
wk1<-subset(wk1, !is.na(LabSection))
wk1<-subset(wk1, dup1!=TRUE)

reg<-read.csv("Intermediate-Data/RegistrarData.csv", stringsAsFactors = F)
#agg<-read.csv("Intermediate-Data/AggregatedData-PHYS111-112-F15-S18-6-3-2018.csv", stringsAsFactors = F)
#agg<-subset(agg, Course == 112)
#agg<-subset(agg, SemesterNum > 11)
#agg$SemesterNum<-as.character(agg$SemesterNum)
#agg$SemesterNum<-ifelse(agg$SemesterNum == "15", "S18", agg$SemesterNum)
#agg$SemesterNum<-ifelse(agg$SemesterNum == "14", "F17", agg$SemesterNum)
#agg$SemesterNum<-ifelse(agg$SemesterNum == "13", "S17", agg$SemesterNum)
#agg$SemesterNum<-ifelse(agg$SemesterNum == "12", "F16", agg$SemesterNum)
#agg$Semester<-agg$SemesterNum
#agg2<-agg[c("Username","Semester","IsFall","IsHonors","Gender","CitzCode",
 #           "TAexperience","PhysicsGradeSimple")]
#agg2<-agg[c("Username","Semester","IsFall","IsHonors","Gender","CitzCode",
 #          "TAexperience","PhysicsGradeSimple")]


data<-merge(wk1, reg, by = "Username", all.y = T)
#data<-merge(data, agg2, by = c("Username","Semester"), all.x = T)
datasave<-data
rm(reg,wk1)



data<-subset(data, !is.na(HSGPA))
data<-subset(data, !is.na(ACTSATM))
#data$FinalCourseGradeAB_Rest<-ifelse(data$FinalCourseGradeAB_Rest == "AB", 1, 0)
#data$FinalCourseGradeAB_Rest2<-as.factor(data$FinalCourseGradeAB_Rest)
#data$TakeCSEM<-as.factor(data$TakeCSEM)
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

sink("Output Text/Univariate WK1 Logistic Models.txt")
lapply(c("CSEMPretestPercent" , "TakeCSEM" , "LecQGrade" ,"LabQGrade" ,
         "HWGrade" ,"LabGrade","Semester"),
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
modL1<-glm(train$FinalCourseGradeAB_Rest2 ~ LecQGrade +
            LabQGrade + HWGrade + TakeCSEM2, data = train, family = "binomial"
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


#remove lecQGrade and take CSEM due to high p

modL1.1<-glm(train$FinalCourseGradeAB_Rest2 ~ 
             LabQGrade + HWGrade, data = train, family = "binomial"
)


print(summary(modL1.1))
exp((modL1.1$coefficients))

modL1.1Odds<-as.data.frame(exp(cbind("Odds ratio" = coef(modL1.1), confint.default(modL1.1, level = 0.95))))
modL1.1Odds$Wald<-coef(summary(modL1.1))[,3]
modL1.1Odds$pval<-coef(summary(modL1.1))[,4]

write.csv(modL1.1Odds,"Model Fits/Class Wk1 Logistic Fit P2.csv", row.names = T)
vif(modL1.1)

anova(modL1.1, test = "Chisq")

pR2(modL1.1)

pred<-predict(modL1.1, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.modL1.1<-roc(TestActual, pred)

plot.roc(roc.modL1.1, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.modL1.1),"\n")
#compare ROC to last one

rocobj<-plot.roc(TestActual, pred, print.thres = F, print.auc = T, legacy.axes = T,
                 main="", percent=F,
                 ci=TRUE)
ciobj<-ci.se(rocobj, specificities = seq(0,1,.05))

plot(ciobj, type = "shape", col="#79BD8F")
plot(ci(rocobj, of = "thresholds", thresholds = "best"))


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
                    Semester, data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

#Using varSelRF


fit2<-varSelRF(train[,c("CSEMPretestPercent" , "TakeCSEM2" , 
                        "LecQGrade" ,"LabQGrade" , "HWGrade","Semester","LabGrade" )],
               as.factor(train$FinalCourseGradeAB_Rest2), ntree = 10000)
fit2$selected.vars
fit2$selected.model

fit2.1<-fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                            LecQGrade +
                            HWGrade
                            , data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod2<-roc(TestActual, pred[,2])
plot.roc(roc.mod2,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod2),"\n")

rocobj<-plot.roc(TestActual, pred[,2], print.thres = F, print.auc = T, legacy.axes = T,
                 main="", percent=F,
                 ci=TRUE)
ciobj<-ci.se(rocobj, specificities = seq(0,1,.05))

plot(ciobj, type = "shape", col="#79BD8F")
plot(ci(rocobj, of = "thresholds", thresholds = "best"))


roc.test(roc.mod,roc.mod2)






######################################
#Combined Model----------------------
######################################


#Logit


fitLog<-glm(train$FinalCourseGradeAB_Rest2 ~
               LabQGrade +
               HWGrade + 
               CURGPA +
               P111LastGradeAB_Rest, data = train, family = "binomial"
)


print(summary(fitLog))
exp((fitLog$coefficients))

fitLogOdds<-as.data.frame(exp(cbind("Odds ratio" = coef(fitLog), confint.default(fitLog, level = 0.95))))
fitLogOdds$Wald<-coef(summary(fitLog))[,3]
fitLogOdds$pval<-coef(summary(fitLog))[,4]

write.csv(fitLogOdds,"Model Fits/Class and registrar Wk1 Logistic Fit P2.csv", row.names = T)
vif(fitLog)

anova(fitLog, test = "Chisq")

pR2(fitLog)

pred<-predict(fitLog, newdata = test, type = 'response')
print(confusionMatrix(data = as.factor(as.numeric(pred>0.5)), reference = as.factor(TestActual)))
roc.fitLog<-roc(TestActual, pred)

plot.roc(roc.fitLog, print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.fitLog),"\n")

rocobj<-plot.roc(TestActual, pred, print.thres = F, print.auc = T, legacy.axes = T,
                 main="", percent=F,
                 ci=TRUE)
ciobj<-ci.se(rocobj, specificities = seq(0,1,.05))

plot(ciobj, type = "shape", col="#79BD8F")
plot(ci(rocobj, of = "thresholds", thresholds = "best"))



#RF

fit<-randomForest(as.factor(train$FinalCourseGradeAB_Rest2) ~ 
                    LecQGrade +
                    HWGrade + 
                    CURGPA +
                    P111LastGradeAB_Rest, data = train, importance = TRUE, ntree = 10000)

varImpPlot(fit)

PredictionRandomF<-predict(fit, test)
confusionMatrix(PredictionRandomF, as.factor(test$FinalCourseGradeAB_Rest2))
pred<-predict(fit, newdata = test, type = 'prob')
roc.mod<-roc(TestActual, pred[,2])
plot.roc(roc.mod,print.thres = F, print.auc = T, legacy.axes = T)
cat("AUC CI: ", ci.auc(roc.mod),"\n")

rocobj<-plot.roc(TestActual, pred[,2], print.thres = F, print.auc = T, legacy.axes = T,
                 main="", percent=F,
                 ci=TRUE)
ciobj<-ci.se(rocobj, specificities = seq(0,1,.05))

plot(ciobj, type = "shape", col="#79BD8F")
plot(ci(rocobj, of = "thresholds", thresholds = "best"))


fitImp<-as.data.frame(fit[["importance"]])
fitImp$names<-c("Lecture Quiz", "HW", "GPA", "Phys 1 Grade")

p1<-ggplot(fitImp, aes(x = reorder(names, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy))+
  geom_bar(stat = "identity", fill = "steelblue") +
  ylab("Mean Decrease in Accuracy")+
  xlab("Variable")+
  coord_flip()+
  theme_few()

p2<-ggplot(fitImp, aes(x = reorder(names, MeanDecreaseGini), y = MeanDecreaseGini))+
  geom_bar(stat = "identity", fill = "steelblue") +
  ylab("Mean Decrease in Gini")+
  xlab("Variable")+
  coord_flip()+
  theme_few()

multiplot(p1, p2, cols = 2)
