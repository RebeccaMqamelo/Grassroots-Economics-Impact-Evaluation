clear
import delimited "/Users/rebeccamqamelo/Desktop/RCT/Data/new.csv"

drop if area_name == "Kwale"

* Encode string userIDs, gender levels (female, male)
encode xdai_blockchain_address, gen(idcode)
encode gender, gen(sex)
fvset base 0 sex
fvset base 0 trt_label

generate trt_fem = 0
replace trt_fem = 1 if (sex == 1 & trt_label == 1)

/*******************************************************************************
********************** COMPUTE ADDITIONAL OUTCOMES *****************************
********************************************************************************

Suffix keys:
_0 = 2020-10-20 i.e. one month before 1st intervention
_1 = 2020-11-19 i.e. one day before 1st intervention (analysis baseline)
_2 = 2020-12-04 i.e. day of final transfer
_3 = 2020-12-11 i.e. one week after final transfer
_4 = 2021-01-04 i.e. one month after final transfer
_5 = 2021-02-04 i.e. two months after final transfer
_6 = 2021-03-04 i.e. three months after final transfer		 
*/

gen transfer = 1200/41.1

* ONE-WEEK POST-TREATEMENT ANALYSIS (EXPLORATORY, NOT REPORTED)
gen monthly_income_3 = svol_in_3 - svol_in_1
* Accoumt for transfer income during the first month:
*replace monthly_income_3 = (monthly_income_3 - transfer) if trt_label == 1 
gen monthly_spending_3 = svol_out_3 - svol_out_1
* Account for transfer income during the first month:
*replace monthly_spending_3 = (monthly_spending_3 - transfer) if trt_label == 1
gen MPC_3  = (svol_out_3 - svol_out_1)/(svol_in_3 - svol_in_1)
gen ave_vol_out_3 = (svol_out_3 - svol_out_1)/(stxns_out_3 - stxns_out_1)
gen sales_3 = stxns_in_3 - stxns_in_1
gen purchases_3 = stxns_out_3 - stxns_out_1
gen customers_3 = sunique_in_3 - sunique_in_1
gen vendors_3 = sunique_out_3 - sunique_out_1
gen food_exp_3 = food_water_vol_3 - food_water_vol_1
gen shop_exp_3 = shop_vol_3 - shop_vol_1
gen lab_exp_3 = labour_vol_3 - labour_vol_1

* TWO-MONTH POST-TREATEMENT ANALYSIS
gen monthly_income_5 = svol_in_5 - svol_in_4
gen monthly_spending_5 = svol_out_5 - svol_out_4
gen MPC_5  = (svol_out_5 - svol_out_1)/(svol_in_5 - svol_in_1) 
*gen MPC_exp_multiplier_5 = (1)/(1 - MPC_5)
gen ave_vol_out_5 = (svol_out_5 - svol_out_1)/(stxns_out_5 - stxns_out_1)
gen sales_5 = stxns_in_5 - stxns_in_1
gen purchases_5 = stxns_out_5 - stxns_out_1
gen food_exp_5 = food_water_vol_5 - food_water_vol_1

gen exp_multiplier_2 = (svol_out_5 - svol_out_1)/transfer
gen exp_multiplier_5 = (svol_out_5 - svol_out_1)/transfer
mean(exp_multiplier_5) if trt_label == 1
mean(exp_multiplier_5) if trt_label == 0

********************************************************************************
**************************** MAIN REGRESSION ***********************************
********************************************************************************

local ylist bal monthly_income monthly_spending MPC ave_vol_out sales purchases food_exp

.occup=.object.new
.occup.Declare array list
.occup.list[1]="Wallet Balance (USD)"
.occup.list[2]="Monthly Income (USD)"
.occup.list[3]="Monthly Expenditure (USD)"
.occup.list[4]="Marginal Propensity to Consume"
.occup.list[5]="Ave. Trade Size (USD)"
.occup.list[6]="N. Sales"
.occup.list[7]="N. Purchases"
.occup.list[8]="Monthly Food/Water Expenditure (USD)"

* 1 month after final transfer
local suffixes 5

foreach suf of local suffixes {

	* TREATMENT EFFECT ON FINAL OUTCOMES WITH BASELINE ADJUSTMENT (PPP)
	local i = 1
	local graphs ""
	foreach y of local ylist {
		reg `y'_`suf' trt_label i.sex trt_fem bal_1 svol_in_1 svol_out_1 stxns_in_1 stxns_out_1 sunique_in_1 sunique_out_1 food_water_vol_1 education_vol_1 health_vol_1 savings_vol_1, vce(robust)
		estimates store m`i', title(`y')
		predict `y'_`suf'_pred 
		graph box `y'_`suf'_pred, over(trt_label, relabel(1 "Cont." 2 "Tr.")) over(gender, relabel(1 "Male" 2 "Female")) title(`.occup.list[`i']', size(medium)) legend(size(small)) scheme(s2color) yla(, ang(h) ) asyvars noout name(g`i', replace) nodraw
		
		local graphs "`graphs' g`i'"
		
		local ++i
		}
		
	esttab m1 m2 m3 m4 m5 m6 m7 m8 using /Users/rebeccamqamelo/Desktop/RCT/Results/nairobi_`suf'.csv, keep(trt_label trt_fem) label starlevels("" 1 " *" 0.10 " **" 0.05 " ***" 0.010) cells(b(star fmt(2)) ci(par((  )-(  )) fmt(2))) replace
	
	graph combine `graphs', col(2) xsize(10) ysize(20) iscale(*1)
	graph export /Users/rebeccamqamelo/Desktop/RCT/Results/graphs_`suf'.pdf, replace
}

********************************************************************************
********************** MULTIPLE HYPOTHESIS TESTING *****************************
********************************************************************************

*rwolf bal_2 monthly_income_2 monthly_spending_2 MPC_2 ave_vol_out_2 food_water_vol_2, indepvar(trt_label sex trt_fem) controls(bal_1 svol_in_1 svol_out_1 stxns_in_1 stxns_out_1 sunique_in_1 sunique_out_1 food_water_vol_1 education_vol_1 health_vol_1 savings_vol_1) method(regress) vce(robust) cluster(idcode) reps(1000)

rwolf bal_5 monthly_income_5 monthly_spending_5 MPC_5 ave_vol_out_5 sales_5 purchases_5 food_exp_5, indepvar(trt_label sex trt_fem) controls(bal_1 svol_in_1 svol_out_1 stxns_in_1 stxns_out_1 sunique_in_1 sunique_out_1 food_water_vol_1 education_vol_1 health_vol_1 savings_vol_1) method(regress) vce(robust) cluster(idcode) reps(1000)

/*
mean bal_5 if trt_label == 0
mean svol_in_5 if trt_label == 0
mean svol_out_5 if trt_label == 0
mean MPC_5 if trt_label == 0
mean ave_vol_out_5 if trt_label == 0
mean stxns_in_5 if trt_label == 0
mean stxns_out_5 if trt_label == 0
mean sunique_in_5 if trt_label == 0
mean sunique_out_5 if trt_label == 0
mean fem_support_5 if trt_label == 0
mean food_water_vol_5 if trt_label == 0
*/
