data<-read.csv("P1 Performance data for plots.csv", stringsAsFactors = F)
data$WK<-c(1,2,3,4,5)

library(ggplot2)
library(reshape2)
library(ggthemes)
library(RColorBrewer)

Accuracy<-data[c("WK","AccRFClass",
                "AccRFCR", "AccLogClass", "AccLogCR")]

colnames(Accuracy)<-c("WK","RF Class", "RF Class and Registrar", "Logistic Class", "Logistic Class and Registrar")

Kappa<-data[c("WK","KappaRFClass",
                "KappaRFCR", "KappaLogClass", "KappaLogCR")]

colnames(Kappa)<-c("WK","RF Class", "RF Class and Registrar", "Logistic Class", "Logistic Class and Registrar")


AUC<-data[c("WK","AUCRFClass",
                "AUCRFCR", "AUCLogClass", "AUCLogCR")]

colnames(AUC)<-c("WK","RF Class", "RF Class and Registrar", "Logistic Class", "Logistic Class and Registrar")


Acc2<-melt(Accuracy, id = "WK")
colnames(Acc2)[2]<-"Model"
Kap2<-melt(Kappa, id = "WK")
colnames(Kap2)[2]<-"Model"
AUC2<-melt(AUC, id = "WK")
colnames(AUC2)[2]<-"Model"

setEPS()
postscript("Model Accuracy.eps", width = 15, height = 10, pointsize = 24)
ggplot(Acc2, aes(x = WK, y = value, colour = Model, group = Model, size = 1)) +

  # xlim(1,5) +
  scale_x_continuous(limits = c(1,5), expand = c(0,0), 
                     labels = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5"))+
  scale_y_continuous(limits = c(.5,1), expand = c(0,0))+
  # ylim(0.5,1)+
#  geom_ribbon(aes(ymin = 0.5, ymax = 0.632),
              #  alpha = 0.25,
   #           linetype = 0,
    #          fill = "blue4")+
  #geom_ribbon(aes(ymin = 0.632, ymax = 0.693),
              #  alpha = 0.15,
     #         linetype = 0,
    #          fill = "dodgerblue2")+
 # geom_ribbon(aes(ymin = 0.693, ymax = 0.723),
              # alpha = 0.07,
  #            linetype = 0,
   #           fill = "dodgerblue1")+
  geom_line( size = 1.5) +
  geom_point(size = 2.8) +
  scale_color_manual(values=c("#ae62e8", "#78D4D0", "#87A765", "#FB9800")) +
  #scale_color_brewer(palette="Paired") +
  geom_hline(yintercept = .632, linetype = "dotted") +
  xlab("Time")+
  ylab("Accuracy") +
  ggtitle("  ") +
  theme_few()+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold", vjust = 1.5), 
        axis.text.y = element_text(size = 25),
        axis.title=element_text(size=30,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0 , r = 20, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, angle = 30, vjust = .5),
        legend.text=element_text(size=20),
        legend.title = element_text(size = 24))# +
  

dev.off()

setEPS()
postscript("Model Kappa.eps", width = 15, height = 10, pointsize = 24)
ggplot(Kap2, aes(x = WK, y = value, colour = Model, group = Model, size = 1)) +
  scale_x_continuous(limits = c(1,5), expand = c(0,0), 
                     labels = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5"))+
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  geom_line( size = 1.5) +
  geom_point(size = 2.8) +
  scale_color_manual(values=c("#ae62e8", "#78D4D0", "#87A765", "#FB9800")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("Time")+
  ylab("Cohen's Kappa") +
  ggtitle("  ") +
  theme_few()+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold", vjust = 1.5), 
        axis.text.y = element_text(size = 25),
        axis.title=element_text(size=30,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0 , r = 20, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, angle = 30, vjust = .5),
        legend.text=element_text(size=20),
        legend.title = element_text(size = 24))# +


dev.off()

setEPS()
postscript("Model AUC.eps", width = 15, height = 10, pointsize = 24)
ggplot(AUC2, aes(x = WK, y = value, colour = Model, group = Model, size = 1)) +
  scale_x_continuous(limits = c(1,5), expand = c(0,0), 
                     labels = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5"))+
  scale_y_continuous(limits = c(.45,1), expand = c(0,0))+
  geom_line( size = 1.5) +
  geom_point(size = 2.8) +
  scale_color_manual(values=c("#ae62e8", "#78D4D0", "#87A765", "#FB9800")) +
  geom_hline(yintercept = .5, linetype = "dotted") +
  xlab("Time")+
  ylab("AUC") +
  ggtitle("  ") +
  theme_few()+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold", vjust = 1.5), 
        axis.text.y = element_text(size = 25),
        axis.title=element_text(size=30,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0 , r = 20, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, angle = 30, vjust = .5),
        legend.text=element_text(size=20),
        legend.title = element_text(size = 24))# +


dev.off()