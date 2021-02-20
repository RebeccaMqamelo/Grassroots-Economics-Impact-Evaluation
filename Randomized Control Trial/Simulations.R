# click the link below to be taken to the Google Form for this activity:
library(arm)
library(sandwich)
library(arm)
library(jtools)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(plotly)
options(scipen = 999)

transfer = 1200/41.4

data_raw <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Data/new_attrition.csv")
data <- data_raw[which(data_raw$area_name =='Nairobi'), ]
data$trt_factor <- ordered(data$trt_label, levels=c(0, 1), labels=c("control","treatment"))
data$sex_factor <- ordered(data$gender, levels=c("male", "female"), labels=c("male","female"))
data$mpc_2 <- (data$svol_out_2 - data$svol_out_1)/(data$svol_in_2 - data$svol_in_1)
data$mpc_4 <- (data$svol_out_4 - data$svol_out_1)/(data$svol_in_4 - data$svol_in_1)
data$mpc_5 <- (data$svol_out_5 - data$svol_out_1)/(data$svol_in_5 - data$svol_in_1)
data$ave_vol_out_2 <- (data$svol_out_2 - data$svol_out_1)/(data$stxns_out_2 - data$stxns_out_1)
data$ave_vol_out_4 <- (data$svol_out_4 - data$svol_out_1)/(data$stxns_out_4 - data$stxns_out_1)
data$ave_vol_out_5 <- (data$svol_out_5 - data$svol_out_1)/(data$stxns_out_5 - data$stxns_out_1)
data[mapply(is.infinite, data)] <- NA

trt <- data[which(data$trt_label == 1), ]
ctrl <- data[which(data$trt_label == 0), ]
trt_fem <- trt[which(trt$gender == 'female'), ]
trt_male <- trt[which(trt$gender == 'male'), ]
ctrl_fem <- ctrl[which(ctrl$gender == 'female'), ]
ctrl_male <- ctrl[which(ctrl$gender == 'male'), ]
write.csv(trt_fem, "/Users/rebeccamqamelo/Desktop/RCT/Data/trt_fem.csv")
write.csv(trt_male, "/Users/rebeccamqamelo/Desktop/RCT/Data/trt_male.csv")
write.csv(ctrl_fem, "/Users/rebeccamqamelo/Desktop/RCT/Data/ctrl_fem.csv")
write.csv(ctrl_male, "/Users/rebeccamqamelo/Desktop/RCT/Data/ctrl_male.csv")
trt_fem <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Data/trt_fem.csv")
trt_male <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Data/trt_male.csv")
ctrl_fem <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Data/ctrl_fem.csv")
ctrl_male <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Data/ctrl_male.csv")
trt_fem <- trt
trt_male <- trt
ctrl_fem <- ctrl
ctrl_male <- ctrl

trt_fem <- c(1, 1, 1, mean(trt_fem$bal_1), 
             mean(trt_fem$svol_in_1), mean(trt_fem$svol_out_1), 
             mean(trt_fem$stxns_in_1), mean(trt_fem$stxns_out_1),
             mean(trt_fem$sunique_in_1), mean(trt_fem$sunique_out_1),
             mean(trt_fem$fem_support_1), 
             mean(trt_fem$food_water_vol_1), mean(trt_fem$education_vol_1), mean(trt_fem$health_vol_1), mean(trt_fem$savings_vol_1))

trt_male <- c(1, 0, 0, mean(trt_male$bal_1), 
              mean(trt_male$svol_in_1), mean(trt_male$svol_out_1), 
              mean(trt_male$stxns_in_1), mean(trt_male$stxns_out_1),
              mean(trt_male$sunique_in_1), mean(trt_male$sunique_out_1),
              mean(trt_male$fem_support_1), 
              mean(trt_male$food_water_vol_1), mean(trt_male$education_vol_1), mean(trt_male$health_vol_1), mean(trt_male$savings_vol_1))

ctrl_fem <- c(0, 1, 0, mean(ctrl_fem$bal_1), 
              mean(ctrl_fem$svol_in_1), mean(ctrl_fem$svol_out_1), 
              mean(ctrl_fem$stxns_in_1), mean(ctrl_fem$stxns_out_1),
              mean(ctrl_fem$sunique_in_1), mean(ctrl_fem$sunique_out_1),
              mean(ctrl_fem$fem_support_1), 
              mean(ctrl_fem$food_water_vol_1), mean(ctrl_fem$education_vol_1), mean(ctrl_fem$health_vol_1), mean(ctrl_fem$savings_vol_1))

ctrl_male <- c(0, 0, 0, mean(ctrl_male$bal_1), 
               mean(ctrl_male$svol_in_1), mean(ctrl_male$svol_out_1), 
               mean(ctrl_male$stxns_in_1), mean(ctrl_male$stxns_out_1),
               mean(ctrl_male$sunique_in_1), mean(ctrl_male$sunique_out_1),
               mean(ctrl_male$fem_support_1), 
               mean(ctrl_male$food_water_vol_1), mean(ctrl_male$education_vol_1), mean(ctrl_male$health_vol_1), mean(ctrl_male$savings_vol_1))

OutcomeList <- data.frame(rbind('bal_5', 
                                'svol_in_5', 'svol_out_5', 'mpc_5', 'ave_vol_out_5',
                                'stxns_in_5', 'stxns_out_5', 
                                'sunique_in_5', 'sunique_out_5', 
                                'fem_support_5', 
                                'food_water_vol_5', 'education_vol_5', 'health_vol_5'))

N <- dim(OutcomeList)[1]

BaselineList <- data.frame(rbind('bal_1',
                                 'svol_in_1', 'svol_out_1', 
                                 'stxns_in_1', 'stxns_out_1', 
                                 'sunique_in_1', 'sunique_out_1', 
                                 'fem_support_1', 
                                 'food_water_vol_1', 'education_vol_1', 'health_vol_1'))


get_pred <- function(coefs, person) {
  EV <- coefs[1] + #intercept
    person[1]*coefs[2] + #trt_label
    person[2]*coefs[3] + #sex
    person[3]*coefs[4] + #interaction
    person[4]*coefs[5] + #bal baseline
    person[5]*coefs[6] + #svol_in baseline
    person[6]*coefs[7] + #svol_out baseline
    person[7]*coefs[8] + #stxns_in baseline
    person[8]*coefs[9] + #stxns_out baseline
    person[9]*coefs[10] + #sunqiue_in baseline
    person[10]*coefs[11] #sunique_out baseline
  
  return(EV)
}

storage.matrix_te_fem <- matrix(NA, nrow = 1000, ncol = N+1) # 12 outcome predictions + assignment label + gender label 
colnames(storage.matrix_te_fem) <- c("Gender", 
                                     "Wallet Balance", 
                                     "Income", "Expenditure", "MPC", "Ave Trade",  
                                     "Sales", "Purchases", 
                                     "Customers", "Vendors",
                                     "Female Support",
                                     "Food", "Education", "Health")
storage.matrix_te_male <- matrix(NA, nrow = 1000, ncol = N+1)
colnames(storage.matrix_te_male) <- c("Gender", 
                                      "Wallet Balance", 
                                      "Income", "Expenditure", "MPC", "Ave Trade",  
                                      "Sales", "Purchases", 
                                      "Customers", "Vendors",
                                      "Female Support",
                                      "Food", "Education", "Health")
storage.matrix_trt_fem <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_trt_fem) <- c("Assignment", "Gender", 
                                      "Wallet Balance", 
                                      "Income", "Expenditure", "MPC", "Ave Trade", 
                                      "Sales", "Purchases", 
                                      "Customers", "Vendors",
                                      "Female Support",
                                      "Food", "Education", "Health")
storage.matrix_trt_male <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_trt_male) <- c("Assignment", "Gender", 
                                       "Wallet Balance", 
                                       "Income", "Expenditure", "MPC", "Ave Trade", 
                                       "Sales", "Purchases", 
                                       "Customers", "Vendors",
                                       "Female Support",
                                       "Food", "Education", "Health")
storage.matrix_ctrl_fem <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_ctrl_fem) <- c("Assignment", "Gender", 
                                       "Wallet Balance", 
                                       "Income", "Expenditure", "MPC", "Ave Trade", 
                                       "Sales", "Purchases", 
                                       "Customers", "Vendors",
                                       "Female Support",
                                       "Food", "Education", "Health")
storage.matrix_ctrl_male <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_ctrl_male) <- c("Assignment", "Gender", 
                                        "Wallet Balance", "MPC", "Ave Trade", 
                                        "Income", "Expenditure", 
                                        "Sales", "Purchases", 
                                        "Customers", "Vendors",
                                        "Female Support",
                                        "Food", "Education", "Health")

for(j in 1:nrow(OutcomeList)){
  
  Y <- data[,which(colnames(data) %in% as.character(OutcomeList[j,1]))]
  Y <- as.numeric(Y)
  #base <- data[,which(colnames(data) %in% as.character(BaselineList[j,1]))]
  #base <- as.numeric(base)
  
  #lm <- glm(Y ~ trt_label + sex + base + I(trt_label*sex), data=data, family="gaussian", na.action(na.pass))
  lm <- lm(Y ~ trt_label + sex + I(trt_label*sex) + 
             bal_1 + svol_in_1 + svol_out_1 + stxns_in_1 + stxns_out_1 + sunique_in_1 + sunique_out_1 +
             fem_support_1 + food_water_vol_1 + education_vol_1 + health_vol_1, 
           data=data, na.action=na.exclude)
  sim.glm <- sim(lm, 1000)
  
  for (i in 1:1000) {
    storage.matrix_trt_fem[i, 1] <- "Treatment"
    storage.matrix_trt_fem[i, 2] <- "Female"
    storage.matrix_trt_fem[i, 2+j] <- get_pred(sim.glm@coef[i, ], trt_fem)
    
    storage.matrix_trt_male[i, 1] <- "Treatment"
    storage.matrix_trt_male[i, 2] <- "Male"
    storage.matrix_trt_male[i, 2+j] <- get_pred(sim.glm@coef[i, ], trt_male)
    
    storage.matrix_ctrl_fem[i, 1] <- "Control"
    storage.matrix_ctrl_fem[i, 2] <- "Female"
    storage.matrix_ctrl_fem[i, 2+j] <- get_pred(sim.glm@coef[i, ], ctrl_fem)
    
    storage.matrix_ctrl_male[i, 1] <- "Control"
    storage.matrix_ctrl_male[i, 2] <- "Male"
    storage.matrix_ctrl_male[i, 2+j] <- get_pred(sim.glm@coef[i, ], ctrl_male)
    
    diff_fem <- (as.numeric(storage.matrix_trt_fem[i, 2+j]) - as.numeric(storage.matrix_ctrl_fem[i, 2+j]))
    diff_male <- (as.numeric(storage.matrix_trt_male[i, 2+j]) - as.numeric(storage.matrix_ctrl_male[i, 2+j]))
    storage.matrix_te_fem[i, 1] <- "Female"
    storage.matrix_te_fem[i, 1+j] <- diff_fem
    
    storage.matrix_te_male[i, 1] <- "Male"
    storage.matrix_te_male[i, 1+j] <- diff_male
  }
}

estimates <- do.call(rbind, list(storage.matrix_trt_fem, storage.matrix_trt_male, storage.matrix_ctrl_fem, storage.matrix_ctrl_male))
write.csv(estimates, "/Users/rebeccamqamelo/Desktop/RCT/Predictions/estimates.csv")

te <- do.call(rbind, list(storage.matrix_te_fem, storage.matrix_te_male))
write.csv(te, "/Users/rebeccamqamelo/Desktop/RCT/Predictions/te.csv")

estimates <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Predictions/estimates.csv")
te <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Predictions/te.csv")
te_fem <- te[which(te$Gender =='Female'), ]
te_fem <- te[which(te$Gender =='Male'), ]

storage.te_fem_CI <- matrix(NA, nrow = 1, ncol = N)
storage.te_male_CI <- matrix(NA, nrow = 1, ncol = N)
colnames(storage.te_fem_CI) <- c("Wallet Balance", 
                                 "Income", "Expenditure",  "MPC", "Ave Trade Amnt", 
                                 "Sales", "Purchases", 
                                 "Customers", "Vendors",
                                 "Female Support",
                                 "Food", "Education", "Health")
colnames(storage.te_male_CI) <- c("Wallet Balance", 
                                  "Income", "Expenditure",  "MPC", "Ave Trade Amnt", 
                                  "Sales", "Purchases", 
                                  "Customers", "Vendors",
                                  "Female Support",
                                  "Food", "Education", "Health")

for (i in 1:N) {
  storage.te_fem_CI[1, i] = paste(as.character(round(quantile(as.numeric(te_fem[, i+2]), probs = c(0.005, 0.995))), 2)[1], 
                                  as.character(round(quantile(as.numeric(te_fem[, i+2]), probs = c(0.005, 0.995))), 2)[2], 
                                  sep = ", ")
  storage.te_male_CI[1, i] = paste(as.character(round(quantile(as.numeric(te_male[, i+2]), probs = c(0.005, 0.995))), 2)[1], 
                                   as.character(round(quantile(as.numeric(te_male[, i+2]), probs = c(0.005, 0.995))), 2)[2], 
                                   sep = ", ")
}

te_CIs <- do.call(rbind, list(storage.te_male_CI, storage.te_fem_CI))
write.csv(te_CIs, "/Users/rebeccamqamelo/Desktop/RCT/Predictions/te_CIs.csv")

#Score
library(gridExtra)
conf.intervals_bal <- quantile(as.numeric(combined_estimates[, 3]), probs = c(0.005, 0.995))
write.csv(conf.intervals_bal, "/Users/rebeccamqamelo/Desktop/RCT/Predictions/conf_int_bal_2.csv")
conf.intervals_bal <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Predictions/conf_int_bal_2.csv")

p1 <- ggplot(data=estimates, aes(x=Gender, y=Wallet.Balance, fill=Assignment)) + geom_boxplot() + ggtitle("Predictions by Cohort") + 
  theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.x = element_blank())
#p2 <- ggplot(data=te, aes(x=Gender, y=Wallet.Balance, fill=Gender)) + geom_boxplot()
p2 <- ggbarplot(te, x = "Gender", y = "Wallet.Balance", add = "mean", fill = "Gender", show.legend = FALSE) + ggtitle("Ave. Treatment Effect by Gender") + 
  theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.x = element_blank()) +
  scale_fill_manual(values = c("#999999", "#d3d3d3"))

p3 <- ggplot(data=estimates, aes(x=Gender, y=Income, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank())
#p4 <- ggplot(data=te, aes(x=Gender, y=Income, fill=Gender)) + geom_boxplot()
p4 <- ggbarplot(te, x = "Gender", y = "Income", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p5 <- ggplot(data=estimates, aes(x=Gender, y=Expenditure, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank()) 
#p6 <- ggplot(data=te, aes(x=Gender, y=Expenditure, fill=Gender)) + geom_boxplot()
p6 <- ggbarplot(te, x = "Gender", y = "Expenditure", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p7 <- ggplot(data=estimates, aes(x=Gender, y=MPC, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank())
#p8 <- ggplot(data=te, aes(x=Gender, y=MPC, fill=Gender)) + geom_boxplot()
p8 <- ggbarplot(te, x = "Gender", y = "MPC", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p9 <- ggplot(data=estimates, aes(x=Gender, y=Ave.Trade, fill=Assignment)) + geom_boxplot() + ggtitle("Predictions by Cohort") + 
  theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.x = element_blank())
#p10 <- ggplot(data=te, aes(x=Gender, y=Ave.Trade.Amnt, fill=Gender)) + geom_boxplot()
p10 <- ggbarplot(te, x = "Gender", y = "Ave.Trade", add = "mean", fill = "Gender") + ggtitle("Ave. Treatment Effect by Gender") + 
  theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p11 <- ggplot(data=estimates, aes(x=Gender, y=Sales, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank())
#p12 <- ggplot(data=te, aes(x=Gender, y=Sales, fill=Gender)) + geom_boxplot() 
p12 <- ggbarplot(te, x = "Gender", y = "Sales", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p13 <- ggplot(data=estimates, aes(x=Gender, y=Purchases, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank()) 
#p14 <- ggplot(data=te, aes(x=Gender, y=Purchases, fill=Gender)) + geom_boxplot() 
p14 <- ggbarplot(te, x = "Gender", y = "Purchases", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p15 <- ggplot(data=estimates, aes(x=Gender, y=Customers, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank()) 
#p14 <- ggplot(data=te, aes(x=Gender, y=Customers, fill=Gender)) + geom_boxplot()
p16 <- ggbarplot(te, x = "Gender", y = "Customers", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p17 <- ggplot(data=estimates, aes(x=Gender, y=Vendors, fill=Assignment)) + geom_boxplot() + ggtitle("Predictions by Cohort") + 
  theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.x = element_blank())
#p18 <- ggplot(data=te, aes(x=Gender, y=Vendors, fill=Gender)) + geom_boxplot()
p18 <- ggbarplot(te, x = "Gender", y = "Vendors", add = "mean", fill = "Gender") + ggtitle("Ave. Treatment Effect by Gender") + 
  theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p19 <- ggplot(data=estimates, aes(x=Gender, y=Female.Support, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank()) 
#p20 <- ggplot(data=te, aes(x=Gender, y=Female.Support, fill=Gender)) + geom_boxplot()
p20 <- ggbarplot(te, x = "Gender", y = "Female.Support", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

p21 <- ggplot(data=estimates, aes(x=Gender, y=Food, fill=Assignment)) + geom_boxplot() + theme(axis.title.x = element_blank()) 
#p22 <- ggplot(data=te, aes(x=Gender, y=Food, fill=Gender)) + geom_boxplot()
p22 <- ggbarplot(te, x = "Gender", y = "Food", add = "mean", fill = "Gender") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("#999999", "#d3d3d3"))

ggsave(filename="/Users/rebeccamqamelo/Desktop/RCT/Predictions/test_1.pdf", 
       ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,  
                 ncol=2, nrow=4, common.legend = TRUE, legend="bottom"),
       width = 8, height = 11)

ggsave(filename="/Users/rebeccamqamelo/Desktop/RCT/Predictions/test_2.pdf", 
       ggarrange(p9, p10, p11, p12, p13, p14, p15, p16,
                 ncol=2, nrow=4, common.legend = TRUE, legend="bottom"), 
       width = 8, height = 11)

ggsave(filename="/Users/rebeccamqamelo/Desktop/RCT/Predictions/test_3.pdf", 
       ggarrange(p17, p18, p19, p20, p21, p22,
                 ncol=2, nrow=3, common.legend = TRUE, legend="bottom"), 
       width = 8, height = 11)