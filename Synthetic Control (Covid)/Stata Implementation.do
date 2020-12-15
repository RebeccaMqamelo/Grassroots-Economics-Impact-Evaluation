// SYNTHETIC CONTROL: EFFECT OF COVID-19 LOCKDOWN ON COMMUNITY CURRENCY TRADE IN NAIROBI
// SS154 FINAL PROJECT

**************************************************************
* name: /Users/rebeccamqamelo/Desktop/SS154/FP/Mqamelo SS154 Final Project.do
* author: Rebecca Mqamelo (Minerva Schools at KGI, Economics and Data Science major)
* description: estimates the causal effect of Covid-19 lockdown on daily_total
*			   trade volume in a community currency network in Nairobi, Kenya
* note: code based on synthetic control method discusssed in Scott Cuninghams's 
*       textbook, "The Mixtape" (2018)
* date: April 23, 2020
**************************************************************

// Install these first:
* ssc install synth, replace all
* ssc install mat2txt

clear
use "/Users/rebeccamqamelo/Desktop/SS154/FP/txnDataCleaned.dta"
tsset location date

* generate fortnightly moving average:
tssmooth ma daily_total_ma  = daily_total, window(14)
recode daily_total_ma (mis = 0)

rename daily_total daily_total_raw
rename daily_total_ma daily_total


* collapse daily_total, by(date)
* twoway line daily_total date if date > 1

* Ignore 14 (Kilifi), 19 (Kwale), 33 (Mombasa),  35 (Nairobi = treatment unit) and 42 (GE System)

// Synthetic control for Nairobi
// Include two-week time period instead of 1:
synth daily_total daily_total(7) daily_total(21) daily_total(35) daily_total(49) daily_total(63) daily_total(77) main_purpose(7) main_purpose(21) main_purpose(35) main_purpose(49) main_purpose(63) main_purpose(77) gender_ratio_buyer(7) gender_ratio_buyer(21) gender_ratio_buyer(35) gender_ratio_buyer(49) gender_ratio_buyer(63) gender_ratio_buyer(77) gender_ratio_seller(7) gender_ratio_seller(21) gender_ratio_seller(35) gender_ratio_seller(49) gender_ratio_seller(63) gender_ratio_seller(77) n_trades(7) n_trades(21) n_trades(35) n_trades(49) n_trades(63) n_trades(77) n_new_users(7) n_new_users(21) n_new_users(35) n_new_users(49) n_new_users(63) n_new_users(77), trunit(35) trperiod(60) counit(0 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 20 21 22 23 24 25 26 27 28 29 30 31 32 34 36 37 38 39 40 41 43 44 45 46 47) keep (/Users/rebeccamqamelo/Desktop/SS154/FP/main_synth_results.dta) replace fig

/* Weekly covariate moving averages:
synth daily_total daily_total(7) daily_total(14) daily_total(21) daily_total(28) daily_total(35) daily_total(42) daily_total(49) daily_total(56) daily_total(63) daily_total(70) daily_total(77) main_purpose(7) main_purpose(14) main_purpose(21) main_purpose(28) main_purpose(35) main_purpose(42) main_purpose(49) main_purpose(56) main_purpose(63) main_purpose(70) main_purpose(77) gender_ratio_buyer(7) gender_ratio_buyer(14) gender_ratio_buyer(21) gender_ratio_buyer(28) gender_ratio_buyer(35) gender_ratio_buyer(42)  gender_ratio_buyer(49) gender_ratio_buyer(56) gender_ratio_buyer(63) gender_ratio_buyer(70) gender_ratio_buyer(77) gender_ratio_seller(7) gender_ratio_seller(14)  gender_ratio_seller(21) gender_ratio_seller(28) gender_ratio_seller(35) gender_ratio_seller(42)  gender_ratio_seller(49) gender_ratio_seller(56) gender_ratio_seller(63) gender_ratio_seller(70) gender_ratio_seller(77) n_trades(7) n_trades(14) n_trades(21) n_trades(28) n_trades(35) n_trades(42) n_trades(49) n_trades(56) n_trades(63) n_trades(70) n_trades(77) n_new_users(7) n_new_users(14) n_new_users(21) n_new_users(28) n_new_users(35) n_new_users(42) n_new_users(49) n_new_users(56) n_new_users(63) n_new_users(70) n_new_users(77), trunit(35) trperiod(60) counit(0 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 20 21 22 23 24 25 26 27 28 29 30 31 32 34 36 37 38 39 40 41 43 44 45 46 47) keep (/Users/rebeccamqamelo/Desktop/SS154/FP/main_synth_results.dta) replace fig
*/

mat list e(V_matrix)

use /Users/rebeccamqamelo/Desktop/SS154/FP/main_synth_results.dta, clear
keep _Y_treated _Y_synthetic _time
drop if _time==.
rename _time day
rename _Y_treated  treat
rename _Y_synthetic counterfact
gen gap35 = treat-counterfact
sort day
twoway (line gap35 day,lp(solid)lw(vthin)lcolor(black)), yline(0, lpattern(shortdash) lcolor(black)) ytitle("Gap in daily trade volume prediction error", size(small)) legend(off)xline(60, lpattern(shortdash) lcolor(black)) xtitle("", si(medsmall)) xlabel(#10) 

**************************************************************
* Inference 1 placebo test

set more off
use "/Users/rebeccamqamelo/Desktop/SS154/FP/txnDataCleaned.dta", replace
tsset location date
 
local area_list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 20 21 22 23 24 25 26 27 28 29 30 31 32 34 36 37 38 39 40 41 43 44 45 46 47

* set trace on (trace helps identify errors)
foreach i of local area_list {

synth daily_total daily_total(7) daily_total(21) daily_total(35) daily_total(49) daily_total(63) daily_total(77) main_purpose(7) main_purpose(21) main_purpose(35) main_purpose(49) main_purpose(63) main_purpose(77) gender_ratio_buyer(7) gender_ratio_buyer(21) gender_ratio_buyer(35) gender_ratio_buyer(49) gender_ratio_buyer(63) gender_ratio_buyer(77) gender_ratio_seller(7) gender_ratio_seller(21) gender_ratio_seller(35) gender_ratio_seller(49) gender_ratio_seller(63) gender_ratio_seller(77) n_trades(7) n_trades(21) n_trades(35) n_trades(49) n_trades(63) n_trades(77) n_new_users(7) n_new_users(21) n_new_users(35) n_new_users(49) n_new_users(63) n_new_users(77), trunit(`i') trperiod(60) keep(/Users/rebeccamqamelo/Desktop/SS154/FP/synth_results_`i'.dta) replace
matrix location`i' = e(RMSPE) 
}
* set trace off 

 foreach i of local area_list {
 matrix rownames location`i'=`i'
 matlist location`i', names(rows)
 }
**************************************************************
* Omit 14, 19, 33 + 42 (Kilifi, Kwale and Mombasa were the 3 other counties that received lockdown)
local area_list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 20 21 22 23 24 25 26 27 28 29 30 31 32 34 36 37 38 39 40 41 43 44 45 46 47

foreach i of local area_list {

use /Users/rebeccamqamelo/Desktop/SS154/FP/synth_results_`i'.dta ,clear
keep _Y_treated _Y_synthetic _time
drop if _time==.
rename _time day
rename _Y_treated  treat`i'
rename _Y_synthetic counterfact`i'
gen gap`i'=treat`i'-counterfact`i'
sort day
save /Users/rebeccamqamelo/Desktop/SS154/FP/synth_gap_results_`i', replace
}

use /Users/rebeccamqamelo/Desktop/SS154/FP/synth_gap_results_35.dta, clear
sort day
save /Users/rebeccamqamelo/Desktop/SS154/FP/placebo_results_35.dta, replace

foreach i of local area_list {

merge m:m day using /Users/rebeccamqamelo/Desktop/SS154/FP/synth_gap_results_`i'
drop _merge
sort day
save /Users/rebeccamqamelo/Desktop/SS154/FP/placebo_results.dta, replace
}

**************************************************************
* Inference 2: Estimate the pre- and post-RMSPE and calculate the ratio of the post-pre RMSPE	
set more off
local area_list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 20 21 22 23 24 25 26 27 28 29 30 31 32 34 36 37 38 39 40 41 43 44 45 46 47

foreach i of local area_list {

use /Users/rebeccamqamelo/Desktop/SS154/FP/synth_gap_results_`i', clear
gen gap_cubed=gap`i'*gap`i'
egen postmean=mean(gap_cubed) if day>60
egen premean=mean(gap_cubed) if day<=60
gen rmspe=sqrt(premean) if day<=60
replace rmspe=sqrt(postmean) if day>60
gen ratio=rmspe/rmspe[_n-1] if day==61
gen rmspe_post=sqrt(postmean) if day>60
gen rmspe_pre=rmspe[_n-1] if day==61
mkmat rmspe_pre rmspe_post ratio if day==61, matrix (location`i')
}

**************************************************************
* show post/pre-expansion RMSPE ratio for all states, generate histogram
foreach i of local area_list {
matrix rownames location`i'=`i'
matlist location`i', names(rows)
}

mat location=location0\location1\location2\location3\location4\location5\location6\location7\location8\location9\location10\location11\location12\location13\location15\location16\location17\location18\location20\location21\location22\location23\location24\location25\location26\location27\location28\location29\location30\location31\location32\location34\location35\location36\location37\location38\location39\location40\location41\location43\location44\location45\location46\location47

mat2txt, matrix(location) saving(/Users/rebeccamqamelo/Desktop/SS154/FP/rmspe_results.txt) replace
insheet using /Users/rebeccamqamelo/Desktop/SS154/FP/rmspe_results.txt, clear
ren v1 location
drop v5
gsort -ratio
gen rank=_n
gen p=rank/42

export excel using /Users/rebeccamqamelo/Desktop/SS154/FP/rmspe_results, firstrow(variables) replace
import excel /Users/rebeccamqamelo/Desktop/SS154/FP/rmspe_results.xls, sheet("Sheet1") firstrow clear

histogram ratio, bin(20) frequency fcolor(gs13) lcolor(black) ylabel(0(2)6) xtitle(Post/pre RMSPE ratio) xlabel(0(1)5)

* Show the post/pre RMSPE ratio for specific locations
* Post RMSE / Pre RMSE -> Plot a histogram for each state 
* CA has the largest ratio out of 20 states; what’s the p-value? 
* 1/20 == 0.05 
* In this case the p value is 1/36 = 0.0278 
* So you need at least 20 units in a group to get a p-value of 0.05 for a result to be statistically significant
list rank p if location==35
list rank p if location==0

**************************************************************
* Inference 3: all the placeboes on the same picture
use "/Users/rebeccamqamelo/Desktop/SS154/FP/placebo_results.dta", replace

* Picture of the full sample, including outlier RSMPE
#delimit;	

* Potentially also drop outliers where RMSPE is 5 times more than Nairobi: (drops 1)
twoway (line gap0 day ,lp(solid)lw(vthin)) (line gap1 day ,lp(solid)lw(vthin)) (line gap2 day ,lp(solid)lw(vthin)) (line gap3 day ,lp(solid)lw(vthin)) (line gap4 day ,lp(solid)lw(vthin)) (line gap5 day ,lp(solid)lw(vthin)) (line gap6 day ,lp(solid)lw(vthin)) (line gap7 day ,lp(solid)lw(vthin)) (line gap8 day ,lp(solid)lw(vthin)) (line gap9 day ,lp(solid)lw(vthin)) (line gap10 day ,lp(solid)lw(vthin)) (line gap11 day ,lp(solid)lw(vthin)) (line gap12 day ,lp(solid)lw(vthin)) (line gap13 day ,lp(solid)lw(vthin)) (line gap15 day ,lp(solid)lw(vthin)) (line gap16 day ,lp(solid)lw(vthin)) (line gap17 day ,lp(solid)lw(vthin)) (line gap18 day ,lp(solid)lw(vthin)) (line gap20 day ,lp(solid)lw(vthin)) (line gap21 day ,lp(solid)lw(vthin)) (line gap22 day ,lp(solid)lw(vthin)) (line gap23 day ,lp(solid)lw(vthin)) (line gap24 day ,lp(solid)lw(vthin)) (line gap25 day ,lp(solid)lw(vthin)) (line gap26 day ,lp(solid)lw(vthin)) (line gap27 day ,lp(solid)lw(vthin)) (line gap28 day ,lp(solid)lw(vthin)) (line gap29 day ,lp(solid)lw(vthin)) (line gap30 day ,lp(solid)lw(vthin)) (line gap31 day ,lp(solid)lw(vthin)) (line gap32 day ,lp(solid)lw(vthin)) (line gap34 day ,lp(solid)lw(vthin)) (line gap35 day ,lp(solid)lw(vthin)) (line gap36 day ,lp(solid)lw(vthin)) (line gap37 day ,lp(solid)lw(vthin)) (line gap38 day ,lp(solid)lw(vthin)) (line gap39 day ,lp(solid)lw(vthin)) (line gap40 day ,lp(solid)lw(vthin)) (line gap41 day ,lp(solid)lw(vthin)) (line gap43 day ,lp(solid)lw(vthin)) (line gap44 day ,lp(solid)lw(vthin)) (line gap45 day ,lp(solid)lw(vthin)) (line gap46 day ,lp(solid)lw(vthin)) (line gap47 day ,lp(solid)lw(vthin)) (line gap35 day ,lp(solid)lw(thick)lcolor(black)), yline(0, lpattern(shortdash) lcolor(black)) xline(60, lpattern(shortdash) lcolor(black)) xtitle("",si(small)) xlabel(#10) ytitle("Gap in daily trade volume prediction error", size(small)) legend(off)

#delimit cr

graph save Graph "/Users/rebeccamqamelo/Desktop/SS154/FP/synth_placebo_results.gph", replace
