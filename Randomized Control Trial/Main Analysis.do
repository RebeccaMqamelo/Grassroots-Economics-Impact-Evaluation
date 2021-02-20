clear
import delimited "/Users/rebeccamqamelo/Desktop/RCT/Data/new.csv"

drop if area_name == "Kwale"
*drop if area_name == "Nairobi"

* Encode string userIDs, gender levels (female, male, unknown)
encode xdai_blockchain_address, gen(idcode)
fvset base 0 sex
fvset base 0 trt_label


generate trt_fem = 0
replace trt_fem = 1 if (sex == 1 & trt_label == 1)

gen transfer = 1200/41.1

gen exp_multiplier_1 = 0
gen exp_multiplier_2 = (svol_out_2 - svol_out_1)/transfer
gen exp_multiplier_4 = (svol_out_4 - svol_out_1)/transfer
gen exp_multiplier_5 = (svol_out_5 - svol_out_1)/transfer
mean(exp_multiplier_5) if trt_label == 1
mean(exp_multiplier_5) if trt_label == 0

gen MPC_1 = (svol_out_1 - svol_out_0)/(svol_in_1 - svol_in_0)
gen MPC_2  = (svol_out_2 - svol_out_1)/(svol_in_2 - svol_in_1)
gen MPC_4  = (svol_out_4 - svol_out_1)/(svol_in_4 - svol_in_1)
gen MPC_5  = (svol_out_5 - svol_out_1)/(svol_in_5 - svol_in_1)

gen ave_vol_out_4 = (svol_out_4 - svol_out_1)/(stxns_out_4 - stxns_out_1) 
gen ave_vol_out_5 = (svol_out_5 - svol_out_1)/(stxns_out_5 - stxns_out_1) 

gen MPC_exp_multiplier_2 = (1)/(1 - MPC_2)
gen MPC_exp_multiplier_4 = (1)/(1 - MPC_4)
gen MPC_exp_multiplier_5 = (1)/(1 - MPC_5)
mean(MPC_5) if trt_label == 1
mean(MPC_5) if trt_label == 0
mean(MPC_exp_multiplier_5) if trt_label == 1
mean(MPC_exp_multiplier_5) if trt_label == 0

gen food_bs_5 = (food_water_vol_5 - food_water_vol_1)/(svol_out_5 - svol_out_1)
gen education_bs_5 = (education_vol_5 - education_vol_1)/(svol_out_5 - svol_out_1)
gen health_bs_5 = (health_vol_5 - health_vol_1)/(svol_out_5 - svol_out_1)

local ylist bal svol_in svol_out MPC ave_vol_out stxns_in stxns_out sunique_in sunique_out fem_support food_water_vol education_vol health_vol savings_vol

.occup=.object.new
.occup.Declare array list
.occup.list[1]="Wallet Balance"
.occup.list[2]="Income"
.occup.list[3]="Expenditure"
.occup.list[4]="Ave. Trade Size"
.occup.list[5]="Propensity to Consume"
.occup.list[6]="N. Sales"
.occup.list[7]="N. Purchases"
.occup.list[8]="Customers"
.occup.list[9]="Vendors"
.occup.list[10]="Female Business Support"
.occup.list[11]="Food/Water"
.occup.list[12]="Education"
.occup.list[13]="Health"
.occup.list[14]="Savings"

* 1 month after final transfer
local suffixes 5

foreach suf of local suffixes {

	* TREATMENT EFFECT ON FINAL OUTCOMES WITH BASELINE ADJUSTMENT (PPP)
	local i = 1
	local graphs ""
	foreach y of local ylist {
		reg `y'_`suf' trt_label i.sex trt_fem bal_1 svol_in_1 svol_out_1 stxns_in_1 stxns_out_1 sunique_in_1 sunique_out_1 fem_support_1 food_water_vol_1 education_vol_1 health_vol_1, vce(robust)
		*reg `y'_`suf' trt_label i.sex trt_fem days_active bal_1 svol_in_1 svol_out_1 stxns_in_1 stxns_out_1 sunique_in_1 sunique_out_1, vce(robust)
		estimates store m`i', title(`y')
		predict `y'_`suf'_pred 
		graph box `y'_`suf'_pred, over(trt_label, relabel(1 "Cont." 2 "Tr.")) over(gender, relabel(1 "Male" 2 "Female")) title(`.occup.list[`i']', size(medium)) legend(size(small)) scheme(s2color) yla(, ang(h) ) asyvars noout name(g`i', replace) nodraw
		
		local graphs "`graphs' g`i'"
		
		local ++i
		}
	esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 using /Users/rebeccamqamelo/Desktop/RCT/Results/nairobi_`suf'.csv, keep(trt_label trt_fem) label starlevels("" 1 " *" 0.10 " **" 0.05 " ***" 0.010) cells(b(star fmt(2)) ci(par((  )-(  )) fmt(2))) replace
	*esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 using /Users/rebeccamqamelo/Desktop/RCT/Results/nairobi_`suf'.csv, label starlevels("" 1 " *" 0.10 " **" 0.05 " ***" 0.010) cells(b(star fmt(2)) se(par(( )))) replace
	
	graph combine `graphs', col(3) xsize(10) ysize(20) iscale(*1)
	graph export /Users/rebeccamqamelo/Desktop/RCT/Results/graphs_`suf'.pdf, replace
	***graph export /Users/rebeccamqamelo/Desktop/RCT/Results/graphs_`suf'.png, height(16000) width(14000) replace
	
	*rwolf bal_2 svol_in_2 svol_out_2 stxns_in_2 stxns_out_2 sunique_in_2 sunique_out_2 fem_support_2 food_water_vol_2, indepvar(trt_label sex trt_fem) bl(_1) method(regress) vce(robust) reps(1000)
	
}
