********************************************************************************
**** NEPAL ****
********************************************************************************

use "$datapath/stove_estimations.dta", clear
set matsize 11000
keep if country=="nepal"

gen maleeduc=.
replace maleeduc=0 if male_educ=="0"
replace maleeduc=1 if male_educ=="2"
replace maleeduc=2 if male_educ=="3" | male_educ=="4"
replace maleeduc=3 if male_educ=="5" | male_educ=="6" | male_educ=="7"

label define school_nepal 0 "none" 1 "primary" 2 "secondary" 3 "post-secondary" 
label values maleeduc school_nepal

gen headgender=real(head_gender)

********************************************************************************
**** Logistic regressions ****
********************************************************************************

*** Estimations included in main paper ***

* Logistic: Basic specification 
sum stove_clean empower_zscore i.maleeduc wealth_zscore headgender
logit stove_clean empower_zscore i.maleeduc wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain1
outreg2 using "$outputpath/logit_nepal_main1.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* Average marginal effect
margins, dydx(empower_zscore)

* Average predicted probability
sum empower_zscore
margins, at(empower_zscore=(  -3.198068 (.01) 1.972939)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of using clean cooking fuel", color(black)) title("Predictive margin of female decision-making power on clean cooking fuel", color(black))
graph save Graph "$graphpath/nep_predictive_margins_empowerment_stove1.gph", replace
graph export "$graphpath/nep_predictive_margins_empowerment_stove1.png", replace

* logit with fixed effects at psu
logit stove_clean empower_zscore i.maleeduc wealth_zscore i.psu [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain2
outreg2 using "$outputpath/logit_nepal_main2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform
 
* Logistic: Basic specification - headgender 
logit stove_clean i.headgender i.maleeduc wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain3
outreg2 using "$outputpath/logit_nepal_main3.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

margins headgender, vce(unconditional)
margins headgender, atmeans vce(unconditional)

* logit with fixed effects at psu - headgender
logit stove_clean headgender i.maleeduc wealth_zscore i.psu [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain4
outreg2 using "$outputpath/logit_nepal_main4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform



*** Robustness: Estimations without male education ***

* Logistic: Basic specification 
logit stove_clean empower_zscore wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain1
*outreg2 using "$outputpath/logit_nepal_main1.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* logit with fixed effects at psu
logit stove_clean empower_zscore wealth_zscore i.psu [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain2
*outreg2 using "$outputpath/logit_nepal_main2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

*  Logistic: Basic specification - headgender 
logit stove_clean i.headgender  wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain3
*outreg2 using "$outputpath/logit_nepal_main3.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* logit with fixed effects at psu - headgender
logit stove_clean headgender  wealth_zscore i.psu [pweight=weight], vce(cluster psu) or
estat ic
estimates store logisticmain4
*outreg2 using "$outputpath/logit_nepal_main4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform






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
