# click the link below to be taken to the Google Form for this activity:
library(arm)
library(sandwich)
library(arm)
library(jtools)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(plotly)
library(ggthemes)
library(gridExtra)
library(janitor)

options(scipen = 999)
theme_set(theme_bw())

transfer = 1200/41.4

data_raw <- read.csv("/Users/rebeccamqamelo/Desktop/RCT/Data/new.csv")
data <- data_raw[which(data_raw$area_name =='Nairobi'), ]
data$trt_factor <- ordered(data$trt_label, levels=c(0, 1), labels=c("Control","Treatment"))
data$sex_factor <- ordered(data$gender, levels=c("Female", "Male"), labels=c("Female","Male"))
data$monthly_income_5 <- (data$svol_in_5 - data$svol_in_4)
data$monthly_spending_5 <- (data$svol_out_5 - data$svol_out_4)
data$MPC_5 <- (data$svol_out_5 - data$svol_out_1)/(data$svol_in_5 - data$svol_in_1)
data$ave_vol_out_5 <- (data$svol_out_5 - data$svol_out_1)/(data$stxns_out_5 - data$stxns_out_1)
data$sales_5 <- (data$stxns_in_5 - data$stxns_in_1)
data$purchases_5 <- (data$stxns_out_5 - data$stxns_out_1)
data$food_exp_5 <- (data$food_water_vol_5 - data$food_water_vol_1)
data[mapply(is.infinite, data)] <- NA

trt <- data[which(data$trt_label == 1), ]
ctrl <- data[which(data$trt_label == 0), ]
trt_fem <- trt[which(trt$gender == 'female'), ]
trt_male <- trt[which(trt$gender == 'male'), ]
ctrl_fem <- ctrl[which(ctrl$gender == 'female'), ]
ctrl_male <- ctrl[which(ctrl$gender == 'male'), ]

trt_fem <- c(1, 1, 1, mean(trt_fem$bal_1), 
             mean(trt_fem$svol_in_1), mean(trt_fem$svol_out_1), 
             mean(trt_fem$stxns_in_1), mean(trt_fem$stxns_out_1),
             mean(trt_fem$sunique_in_1), mean(trt_fem$sunique_out_1),
             mean(trt_fem$food_water_vol_1), mean(trt_fem$education_vol_1), mean(trt_fem$health_vol_1), mean(trt_fem$savings_vol_1))

trt_male <- c(1, 0, 0, mean(trt_male$bal_1), 
              mean(trt_male$svol_in_1), mean(trt_male$svol_out_1), 
              mean(trt_male$stxns_in_1), mean(trt_male$stxns_out_1),
              mean(trt_male$sunique_in_1), mean(trt_male$sunique_out_1),
              mean(trt_male$food_water_vol_1), mean(trt_male$education_vol_1), mean(trt_male$health_vol_1), mean(trt_male$savings_vol_1))

ctrl_fem <- c(0, 1, 0, mean(ctrl_fem$bal_1), 
              mean(ctrl_fem$svol_in_1), mean(ctrl_fem$svol_out_1), 
              mean(ctrl_fem$stxns_in_1), mean(ctrl_fem$stxns_out_1),
              mean(ctrl_fem$sunique_in_1), mean(ctrl_fem$sunique_out_1),
              mean(ctrl_fem$food_water_vol_1), mean(ctrl_fem$education_vol_1), mean(ctrl_fem$health_vol_1), mean(ctrl_fem$savings_vol_1))

ctrl_male <- c(0, 0, 0, mean(ctrl_male$bal_1), 
               mean(ctrl_male$svol_in_1), mean(ctrl_male$svol_out_1), 
               mean(ctrl_male$stxns_in_1), mean(ctrl_male$stxns_out_1),
               mean(ctrl_male$sunique_in_1), mean(ctrl_male$sunique_out_1),
               mean(ctrl_male$food_water_vol_1), mean(ctrl_male$education_vol_1), mean(ctrl_male$health_vol_1), mean(ctrl_male$savings_vol_1))

OutcomeList <- data.frame(rbind('bal_5', 
                                'monthly_income_5', 'monthly_spending_5', 
                                'MPC_5', 'ave_vol_out_5', 
                                'sales_5', 'purchases_5',
                                'food_exp_5'))

N <- dim(OutcomeList)[1]

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
    person[10]*coefs[11] + #sunique_out baseline
    person[11]*coefs[12] + #food baseline
    person[12]*coefs[13] + #education baseline
    person[13]*coefs[14] + #health baseline
    person[14]*coefs[15]   #savings baseline
  
  return(EV)
}

storage_matrix_names = data.frame(rbind("storage.matrix_te_fem", 
                                        "storage.matrix_te_male", 
                                        "storage.matrix_trt_fem", 
                                        "storage.matrix_trt_male", 
                                        "storage.matrix_ctrl_fem",
                                        "storage.matrix_ctrl_male"))
'
col_names <- c("Assignment", "Gender", 
               "Wallet Balance", 
               "Monthly Income", "Monthly Spending",
               "Sales", "Purchases",
               "MPC", "Ave Trade",  
               "Food")

for (i in 1:dim(storage_matrix_names)[1]) {
  
  assign(paste("", as.character(storage_matrix_names[i,1]), sep = ""), sapply(col_names, FUN = function(y) {
    matrix(NA, nrow = 1000, ncol = N+2)})) # 7 outcome predictions + assignment label + gender label
}
'

storage.matrix_te_fem <- matrix(NA, nrow = 1000, ncol = N+2) # 12 outcome predictions + assignment label + gender label 
colnames(storage.matrix_te_fem) <- c("Assignment", "Gender", 
                                     "Wallet Balance", 
                                     "Monthly Income", "Monthly Spending", 
                                     "MPC", "Ave Trade",  
                                     "Sales", "Purchases",
                                     "Food")
storage.matrix_te_male <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_te_male) <- c("Assignment", "Gender", 
                                      "Wallet Balance", 
                                      "Monthly Income", "Monthly Spending", 
                                      "MPC", "Ave Trade",  
                                      "Sales", "Purchases",
                                      "Food")
storage.matrix_trt_fem <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_trt_fem) <- c("Assignment", "Gender", 
                                      "Wallet Balance", 
                                      "Monthly Income", "Monthly Spending", 
                                      "MPC", "Ave Trade",  
                                      "Sales", "Purchases",
                                      "Food")
storage.matrix_trt_male <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_trt_male) <- c("Assignment", "Gender", 
                                       "Wallet Balance", 
                                       "Monthly Income", "Monthly Spending", 
                                       "MPC", "Ave Trade",  
                                       "Sales", "Purchases",
                                       "Food")
storage.matrix_ctrl_fem <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_ctrl_fem) <- c("Assignment", "Gender", 
                                       "Wallet Balance", 
                                       "Monthly Income", "Monthly Spending", 
                                       "MPC", "Ave Trade",  
                                       "Sales", "Purchases",
                                       "Food")
storage.matrix_ctrl_male <- matrix(NA, nrow = 1000, ncol = N+2)
colnames(storage.matrix_ctrl_male) <- c("Assignment", "Gender", 
                                        "Wallet Balance",
                                        "Monthly Income", "Monthly Spending", 
                                        "MPC", "Ave Trade",  
                                        "Sales", "Purchases",
                                        "Food")

for(j in 1:nrow(OutcomeList)){
  
  Y <- data[,which(colnames(data) %in% as.character(OutcomeList[j,1]))]
  Y <- as.numeric(Y)
  print(as.character(OutcomeList[j,1]))
  lm <- lm(Y ~ trt_label + sex + I(trt_label*sex) + bal_1 + svol_in_1 + svol_out_1 + stxns_in_1 + stxns_out_1 + sunique_in_1 + sunique_out_1 +
             food_water_vol_1 + education_vol_1 + health_vol_1 + savings_vol_1, data=data, na.action=na.exclude)
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
    storage.matrix_te_fem[i, 1] <- "Treatment"
    storage.matrix_te_fem[i, 2] <- "Female"
    storage.matrix_te_fem[i, 2+j] <- diff_fem
    
    storage.matrix_te_male[i, 1] <- "Treatment"
    storage.matrix_te_male[i, 2] <- "Male"
    storage.matrix_te_male[i, 2+j] <- diff_male
  }
}

combined_est <- do.call(rbind, list(storage.matrix_trt_fem, storage.matrix_trt_male, storage.matrix_ctrl_fem, storage.matrix_ctrl_male))
write.csv(combined_est, "/Users/rebeccamqamelo/Desktop/RCT/Predictions/est.csv")
combined_trt_effect <- do.call(rbind, list(storage.matrix_te_fem, storage.matrix_te_male))
write.csv(combined_trt_effect, "/Users/rebeccamqamelo/Desktop/RCT/Predictions/trt_effect.csv")
est_new <- as.data.frame(read.csv("/Users/rebeccamqamelo/Desktop/RCT/Predictions/est.csv"))[1:4000, ]
trt_effect_new <- as.data.frame(read.csv("/Users/rebeccamqamelo/Desktop/RCT/Predictions/trt_effect.csv"))[1:2000, ]

# PLOTS
plot_outcome = function(yvar, ylab, ytitle) {
  ggplot(data=est_new, aes_string(x="Gender", y=yvar, fill="Assignment")) + 
    geom_boxplot(lwd=0.5) + 
    scale_fill_manual(values=c("white", "#a9a9a9")) +
    ggtitle(ytitle) + 
    theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), 
          axis.title.x = element_blank(), legend.position="top", legend.title=element_text(size = 5), legend.text=element_text(size = 5)) + 
    scale_y_continuous(name=ylab) + 
    theme_bw()
}

plot_te = function(yvar, ylab, ytitle) {
  ggbarplot(data=trt_effect_new, x="Gender", y=yvar, add="mean", fill="Gender", show.legend = FALSE) + 
    ggtitle(ytitle) + 
    theme(plot.title = element_text(color="black", size=15, face="bold", hjust = 0.5, margin = margin(t = 0, r = 0, b = 20, l = 0)), axis.title.x = element_blank()) +
    scale_fill_manual(values = c("#F28E2B", "#4E79A7")) + 
    scale_y_continuous(name=ylab) + 
    theme_bw() 
}

p1 <- plot_outcome("Wallet.Balance", "Wallet Balance (USD)", "Wallet Balance - Prediction")
p2 <- plot_outcome("Monthly.Income", "Monthly Income (USD)", "Monthly Income - Prediction")
p3 <- plot_outcome("Monthly.Spending", "Monthly Expenditure (USD)", "Monthly Expenditure - Prediction")
p4 <- plot_outcome("MPC", "Marginal Prop. to Consume", "MPC - Prediction")
p5 <- plot_outcome("Ave.Trade", "Average Trade Size (USD)", "Average Trade Size - Prediction")
p6 <- plot_outcome("Sales", "N. Sales", "N. Sales - Prediction")
p7 <- plot_outcome("Purchases", "N. Purchases", "N. Purchases - Prediction")
p8 <- plot_outcome("Food", "Food Expenditure (USD)", "Food Expenditure - Prediction")

t1 <- plot_te("Wallet.Balance", "Wallet Balance (USD)", "Wallet Balance - Treatment Effect")
t2 <- plot_te("Monthly.Income", "Monthly Income (USD)", "Monthly Income - Treatment Effect")
t3 <- plot_te("Monthly.Spending", "Monthly Expenditure (USD)", "Monthly Expenditure - Treatment Effect")
t4 <- plot_te("MPC", "Marginal Prop. to Consume", "MPC - Treatment Effect")
t5 <- plot_te("Ave.Trade", "Average Trade Size (USD)", "Average Trade Size - Treatment Effect")
t6 <- plot_te("Sales", "N. Sales", "N. Sales - Treatment Effect")
t7 <- plot_te("Purchases", "N. Purchases", "N. Purchases - Treatment Effect")
t8 <- plot_te("Food", "Food Expenditure (USD)", "Food Expenditure - Treatment Effect")

ggsave(filename="/Users/rebeccamqamelo/Desktop/RCT/Predictions/graph_1.pdf", 
       ggarrange(p1, t1, p2, t2, p3, t3, p4, t4,
                 ncol=2, nrow=4, common.legend=TRUE, legend="bottom"),
       width = 8, height = 11)

ggsave(filename="/Users/rebeccamqamelo/Desktop/RCT/Predictions/graph_3.pdf", 
       ggarrange(p5, t5, p6, t6, p7, t7, p8, t8,
                 ncol=2, nrow=4, common.legend=TRUE, legend="bottom"), 
       width = 8, height = 11)