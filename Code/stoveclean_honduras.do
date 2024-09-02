********************************************************************************
**** HONDURAS ****
********************************************************************************

use "$datapath/stove_estimations.dta", clear
set matsize 11000
keep if country=="honduras"

cap drop maleeduc 

gen maleeduc=.
replace maleeduc=0 if male_educ=="1" | male_educ=="11"
replace maleeduc=1 if male_educ=="2" | male_educ=="3"
replace maleeduc=2 if male_educ=="4" | male_educ=="5"
replace maleeduc=3 if male_educ=="6" | male_educ=="7"| male_educ=="8" | male_educ=="9" | male_educ=="10"

label define school_honduras 0 "none" 1 "primary" 2 "secondary" 3 "post-secondary" 
label values maleeduc school_honduras

gen headgender=real(head_gender)

********************************************************************************
**** Logistic regressions ****
********************************************************************************

*** Estimations included in main paper ***

* (1) *******************************************************************************

* Logistic: Basic specification - empowerment
sum stove_clean empower_zscore headge i.maleeduc wealth_zscore
logistic stove_clean empower_zscore i.maleeduc wealth_zscore [pweight=weight] , vce(cluster psu) or 
estat ic
outreg2 using "$outputpath/logit_hon_main1.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* Average marginal effect
margins, dydx(empower_zscore)

* Average predicted probability
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of using clean cooking fuel", color(black)) title("Predictive margin of female decision-making power on clean cooking fuel", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_stove1.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_stove1.png", replace

* Average marginal effect of empowerment score

* (2) *******************************************************************************

* clogit with fixed effects at psu - empowerment
clogit stove_clean empower_zscore i.maleeduc wealth_zscore [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain2
outreg2 using "$outputpath/logit_hon_main2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* (3) *******************************************************************************

* Logistic: Basic specification - headgender
logistic stove_clean i.headgender i.maleeduc wealth_zscore [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain3
outreg2 using "$outputpath/logit_hon_main3.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

margins headgender, vce(unconditional)
margins headgender, atmeans vce(unconditional)

* (4) *******************************************************************************

* clogit with fixed effects at psu - headgender
clogit stove_clean headgender i.maleeduc wealth_zscore [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain4
outreg2 using "$outputpath/logit_hon_main4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform


*** Robustness: Estimations without male edu ***

* Logistic: Basic specification - empowerment_old
logistic stove_clean empower_zscore wealth_zscore [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain1
*outreg2 using "$outputpath/logit_hon_main1.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - empowerment
clogit stove_clean empower_zscore wealth_zscore [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain2
*outreg2 using "$outputpath/logit_hon_main2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* Logistic: Basic specification - headgender
logistic stove_clean i.headgender wealth_zscore [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain3
*outreg2 using "$outputpath/logit_hon_main3.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - headgender
clogit stove_clean headgender wealth_zscore [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain4
*outreg2 using "$outputpath/logit_hon_main4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform



*** Repeat table 1 only if e(sample)

tab stove_clean [aweight=weight] if e(sample)

local varlist fan_own fridge_own iron_own pc_own radio_own tv_own kettle_ownr vcd_dvd_ownr microwave_ownr washing_machine_ownr water_heater_ownr blender_ownr rice_cooker_ownr water_pump_ownr solar_water_heater_ownr
foreach z of local varlist {
tab `z' [aweight=weight] if e(sample), mis
}

local varlist grid mg shs
foreach z of local varlist {
tab `z' [aweight=weight] if e(sample), mis
}

sum years_grid years_mg years_shs hours_grid hours_mg hours_shs if e(sample) [aweight=weight]

