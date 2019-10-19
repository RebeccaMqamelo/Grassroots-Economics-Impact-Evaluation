### GRASSROOTS ECONOMICS BASELINE MATCHING ###

library(Matching)
library(rbounds)
library(ggplot2)

raw_data <- read.csv("/Users/apple/Desktop/GE.Bancor/GE_Baseline_Raw.csv")
names(raw_data)
summary(raw_data)

######################
### DATA WRANGLING ###
######################

#own_business has three levels: yes (3), not applicable (2), and no (1). 
#"Not applicable" implies the person does not own their own business,
#while "no" means they own their own business but do not own their business premesis.
#Therefore "no" and "yes" need to be combined to mean the same thing i.e. the person owns their own business. 
raw_data$own_business[which(raw_data$own_business == 3)] <- 1

#Check that own_business now only contains 1s and 2s
raw_data$own_business

#Merge "not-applicable" (1) under income_total with the lowest income bracket (2 = KSH0 - KSH500)
raw_data$income_total[which(raw_data$income_total == 2)] <- 1
raw_data$income_total[which(raw_data$income_total == 3)] <- 2
raw_data$income_total[which(raw_data$income_total == 4)] <- 3
raw_data$income_total[which(raw_data$income_total == 5)] <- 4
raw_data$income_total[which(raw_data$income_total == 6)] <- 5
raw_data$income_total[which(raw_data$income_total == 7)] <- 6
raw_data$income_total[which(raw_data$income_total == 8)] <- 7
raw_data$income_total[which(raw_data$income_total == 9)] <- 8
raw_data$income_total[which(raw_data$income_total == 10)] <- 9
raw_data$income_total[which(raw_data$income_total == 11)] <- 10
raw_data$income_total[which(raw_data$income_total == 12)] <- 11
raw_data$income_total[which(raw_data$income_total == 13)] <- 12

#Check income_total
raw_data$income_total <- as.numeric(factor(raw_data$income_total)) 
raw_data$income_total

#Define variables
treatment <- raw_data[which(raw_data$type == 1),]
control <- raw_data[which(raw_data$type == 0),]
all_men <- raw_data[which(raw_data$gender == 1),]
all_women <- raw_data[which(raw_data$gender == 2),]
trt_men <- treatment[which(treatment$gender == 1),]
trt_women <- treatment[which(treatment$gender == 2),]
ctrl_men <- control[which(control$gender == 1),]
ctrl_women <- control[which(control$gender == 2),]

#Check distribution of covariates between treatment and control groups: Are there any "red flag" differences?
#For data visualization purposes only; more details in logistic regression below

frequencies <- c(length(which(treatment$income_sources == 1)),length(which(treatment$income_sources == 2)),length(which(treatment$income_sources == 3)),length(which(treatment$income_sources == 4)),length(which(treatment$income_sources == 5)),length(which(treatment$income_sources == 6)),length(which(treatment$income_sources == 7)))
names <- c("No income","Business","Salary/wages","Remittances","Pensions/grants","Farming","Other")
income_sources_all <- data.frame(names, frequencies)
income_sources_all

frequencies <- c(length(which(trt_women$income_sources == 1)),length(which(trt_women$income_sources == 2)),length(which(trt_women$income_sources == 3)),length(which(trt_women$income_sources == 4)),length(which(trt_women$income_sources == 5)),length(which(trt_women$income_sources == 6)),length(which(trt_women$income_sources == 7)))
names <- c("No income","Business","Salary/wages","Remittances","Pensions/grants","Farming","Other")
income_sources_women <- data.frame(names, frequencies)
income_sources_women

plot((treatment$income_sources), main="Age (treatment vs control)", xlab="Age", col='blue')
legend(50, 0.06, legend=c("Treatment", "Control"), col=c("blue", "red"), lty=1:1, cex=0.8)

plot(density(treatment$age), main="Age (treatment vs control)", xlab="Age", ylim=c(0,0.07), col='blue')
lines(density(control$age), lty=1, cex=0.2, col='red')
legend(50, 0.06, legend=c("Treatment", "Control"), col=c("blue", "red"), lty=1:1, cex=0.8)

plot(density(trt_women$forced_to_consult), main="Are women forced to consult?", xlab="Key", col='blue')
lines(density(ctrl_women$forced_to_consult), lty=1, cex=0.2, col='red')
legend(0.5, 1.0, legend=c("Treatment", "Control", "1 = Never", "2 = Almost never", "3 = Sometimes", "4 = Quite often", "5 = Always"), col=c("blue","red","black","black","black","black","black"), lty=c(1,1,0,0,0,0,0), cex=0.6)

plot(density(treatment$educ_level), main="Education level (treatment vs control)", xlab="Key", xlim=c(0,8), col='blue')
lines(density(control$educ_level), lty=1, cex=0.2, col='red')
legend(4.4, 1.25, legend=c("Treatment", "Control", "1 = No school", "2 = National Technical Certificate", "3 = Primary school", "4 = Secondary school", "5 = Diploma", "6 = Bachelors", "7 = Higher degree"), 
       col=c("blue","red","black","black","black","black","black","black","black"), lty=c(1,1,0,0,0,0,0,0,0), cex=0.5)

plot(density(treatment$muni_support), main="Municipal support (treatment vs control)", xlab="No (1) or Yes (2)", ylim=c(0, 4.5), col='blue')
lines(density(control$muni_support), lty=1, cex=0.2, col='red')
legend(1.75, 4, legend=c("Treatment", "Control"), col=c("blue", "red"), lty=1:1, cex=0.8)

plot(density(treatment$income_total), main="Income (treatment vs control)", xlab="Income (USD adjusted)", ylim=c(0,0.4), col='blue')
lines(density(control$income_total), lty=1, cex=0.2, col='red')
legend(10, 0.4, legend=c("Treatment", "Control", "1 = 0-5", "2 = 5-10", "3 = 10-20", "4 = 20-30", "5 = 30-50", "6 = 50-100", "7 = 100-200", "8 = 200-400", "9 = 400-600", "10 = 600-800", "11 = 800-1000", "12 = >1000"), 
       col=c("blue","red","black","black","black","black","black","black","black","black","black","black","black","black"), lty=c(1,1,0,0,0,0,0,0,0,0,0,0,0,0), cex=0.6)

plot(density(treatment$years_in_area), main="Years in Area (treatment vs control)", xlab="Years", col='blue')
lines(density(control$years_in_area), lty=1, cex=0.2, col='red')
legend(40, 0.07, legend=c("Treatment", "Control"), col=c("blue", "red"), lty=1:1, cex=0.8)

################
### MATCHING ###
################

m1 <- glm(type~years_in_area+age+gender+educ_level+civil_status+fin_status+muni_support+forced_to_consult+bank+own_business, 
          data=raw_data, family='binomial')
summary(m1)

Tr <- raw_data$type
X <- cbind(raw_data$years_in_area,raw_data$age,raw_data$gender,raw_data$educ_level,raw_data$civil_status,raw_data$fin_status,raw_data$muni_support,raw_data$forced_to_consult,raw_data$bank,raw_data$own_business)
Y <- raw_data$income_total

m1 <- glm(type~years_in_area+age+gender+educ_level+civil_status+fin_status+muni_support+forced_to_consult+bank+own_business, 
          data=raw_data, family='binomial')
summary(m1)

genout <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1, caliper=1,
                   pop.size=1000, max.generations=10, wait.generations=2, replace=TRUE)
match_fun <- Match(Tr=Tr, X=X, Y=Y, estimand='ATT', M=1, caliper=1, Weight.matrix=genout, replace=TRUE)
summary(match_fun)

dropped <- raw_data[match_fun$index.dropped,]
nrow(dropped)
included <- raw_data[-match_fun$index.dropped,]
nrow(dropped)/nrow(included)

match_bal <- MatchBalance(type~years_in_area+age+gender+educ_level+fin_status+muni_support+forced_to_consult+bank, data=raw_data, match.out=match_fun, nboots=1000)
summary(match_bal)

############################
### MORE INTERPRETATIONS ###
############################

# INCOME DISTRIBUTION FOR TREATMENT BY COMMUNITY
levels(factor(raw_data$cc_name))

bangladesh1 <- raw_data[which(raw_data$cc_name == "Bangla-pesa"),]
gatina <- raw_data[which(raw_data$cc_name == "Gatina-Pesa"),] 
lindi <- raw_data[which(raw_data$cc_name == "Lindi-Pesa"),]
ngombeni <- raw_data[which(raw_data$cc_name == "Ngombeni-Pesa"),]
sarafu_kibera <- raw_data[which(raw_data$cc_name == "Sarafu-Credit"),]

plot(density(bangladesh1$income_total), main="Income Distribution Across Communities (Treatment)", xlab="Income level", ylim=c(0,0.5), col='blue', cex.main=1)
lines(density(gatina$income_total), lty=1, cex=0.2, col='red')
lines(density(ngombeni$income_total), lty=1, cex=0.2, col='green')
lines(density(lindi$income_total), lty=1, cex=0.2, col='purple')
lines(density(sarafu_kibera$income_total), lty=1, cex=0.2, col='orange')
legend(-1.5, 0.5, legend=c("Bangladesh", "Gatina", "Lindi", "Ngombeni", "Kibera", "1 = 0-5", "2 = 5-10", "3 = 10-20", "4 = 20-30", "5 = 30-50", "6 = 50-100", "7 = 100-200", "8 = 200-400", "9 = 400-600", "10 = 600-800", "11 = 800-1000", "12 = >1000"), col=c("blue", "red", "green", "purple", "orange","black","black","black","black","black","black","black","black","black","black","black","black"), lty=c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0), cex=0.45)

# INCOME DISTRIBUTION FOR ALL BY COMMUNITY

levels(factor(raw_data$community))

bahakwenu <- raw_data[which(raw_data$community == "Bahakwenu"),]
bangladesh2 <- raw_data[which(raw_data$community == "Bangladesh"),]
chaani <- raw_data[which(raw_data$community == "Chaani"),]
kangemi <- raw_data[which(raw_data$community == "Kangemi"),]
kawangware <- raw_data[which(raw_data$community == "Kawangware"),]
kibera <- raw_data[which(raw_data$community == "Kibera"),]
kilifi <- raw_data[which(raw_data$community == "Kilifi"),]
kwangombe <- raw_data[which(raw_data$community == "Kwa-Ngombe"),]
likoni <- raw_data[which(raw_data$community == "Likoni"),]
miyani <- raw_data[which(raw_data$community == "Miyani"),]
mombasa <- raw_data[which(raw_data$community == "Mombasa"),]
pungu <- raw_data[which(raw_data$community == "Pungu"),]
takaungu <- raw_data[which(raw_data$community == "Takaungu"),]
other <- raw_data[which(raw_data$community == "other"),]

plot(density(bahakwenu$income_total), main="Income Distribution Across Communities (All)", xlab="Income level", ylim=c(0,1), col='blue', cex.main=1)
lines(density(bangladesh2$income_total), lty=1, cex=0.2, col='red')
lines(density(chaani$income_total), lty=1, cex=0.2, col='green')
lines(density(kangemi$income_total), lty=1, cex=0.2, col='purple')
lines(density(kawangware$income_total), lty=1, cex=0.2, col='orange')
lines(density(kibera$income_total), lty=1, cex=0.2, col='darkslategray4')
lines(density(kilifi$income_total), lty=1, cex=0.2, col='deepskyblue')
lines(density(kwangombe$income_total), lty=1, cex=0.2, col='grey')
lines(density(likoni$income_total), lty=1, cex=0.2, col='cyan')
lines(density(miyani$income_total), lty=1, cex=0.2, col='pink')
lines(density(mombasa$income_total), lty=1, cex=0.2, col='cyan4')
lines(density(pungu$income_total), lty=1, cex=0.2, col='brown')
lines(density(takaungu$income_total), lty=1, cex=0.2, col='coral')
lines(density(other$income_total), lty=1, cex=0.2, col='darkgreen')
legend(-1.5, 1, legend=c("Bahakwenu","Bangladesh","Chaani","Kangemi","Kawangware","Kibera","Kilifi","Kwa-Ngombe","Likoni","Miyani","Mombasa","Pungu","Takaungu","Other"), col=c("blue", "red", "green", "purple", "orange","darkslategray4","deepskyblue","grey","cyan","pink","cyan4","brown","coral","darkgreen"), lty=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), cex=0.45)

# PERCEIVED OUTCOME FOR TREATED

treatment$benefit
plot(hist(treatment$benefit), main="Perceived benefit for CC users", xlab="Benefit", xlim=c(0,12), ylim=c(1,250),
     col=c("blue","red","orange","green","pink","purple","cyan","brown","coral","cyan4","darkgreen","black"))
legend(6, 250,legend=c("1=Increased sales","2=Increased savings","3=Increased loans","4=Increased community identity","5=Increased togetherness in commuity","6=Increased trust in community","7=Better environment","8=Education of children","9=Increased trade","10=Increased security","11=Other","12=No benefits"), cex=0.6)

###################################
### FINAL: SENSITIVITY ANALYSIS ###
###################################

# Question: Just how biased are these results, given the flaws in the study design?

# 1. Propensity score matching model

match_fun_prop <- Match(Tr=Tr, X=m1$fitted.values, Y=Y, estimand="ATT", replace=TRUE)
match_bal_prop <- MatchBalance(type~years_in_area+age+gender+educ_level+fin_status+muni_support+forced_to_consult+bank, data=raw_data, match.out=match_fun_prop, nboots=1000)
summary(match_bal)

?binarysens
binarysens(match_fun_prop, Gamma=10, GammaInc=.1)

##################
### MORE STATS ###
##################

mean(trt_women$income_total)
mean(trt_men$income_total)

own_biz <- treatment[which(treatment$own_business == 1),]
nrow(own_biz[which(own_biz$sales_increase == 3),])/nrow(own_biz)

nrow(treatment[which(treatment$benefit == 2),])/nrow(treatment)

nrow(trt_women[which(trt_women$benefit == 2),])/nrow(trt_women)
nrow(trt_men[which(trt_men$benefit == 2),])/nrow(trt_men)

mean(men_trt$income_total)
mean(treatment$income_total_cc)
mean(trt_women$income_total_cc)
mean(trt_men$income_total_cc)

mean(treatment$food_daily_cc)
