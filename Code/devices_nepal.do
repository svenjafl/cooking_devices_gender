

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

replace years_gridr=0 if years_gridr==. & gridr==0
replace hours_gridr=0 if hours_gridr==. & gridr==0
replace years_mgr=0 if years_mgr==. & mgr==0
replace hours_mgr=0 if hours_mgr==. & mgr==0
replace years_shsr=0 if years_shsr==. & shsr==0
replace hours_shsr=0 if hours_shsr==. & shsr==0

local varlist fan_own fridge_own iron_own pc_own radio_own rice_cooker_ownr tv_own water_pump_ownr
sum `varlist'
foreach z of local varlist {

* Logistic: Basic specification - empowerment 
logit `z' empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain1
outreg2 using "$outputpath/logit_nep_`z'1.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

/*
* clogit with fixed effects at psu - empowerment
logit `z' empower_zscore  maleeduc wealth_zscore hours_gridr  hours_shsr i.psu  [pweight=weight],  vce(cluster psu) or
estat ic
estimates store logisticmain2
outreg2 using "$outputpath/logit_nep_`z'2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform
*/


* Logistic: Basic specification - headgender
logit `z' headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr  [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain3
outreg2 using "$outputpath/logit_nep_`z'3.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

/*
* clogit with fixed effects at psu - headgender
logit `z' headgender  maleeduc wealth_zscore hours_gridr  hours_shsr i.psu [pweight=weight],  vce(cluster psu) or
estat ic
estimates store logisticmain4
outreg2 using "$outputpath/logit_nep_`z'4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform
*/
}

/* mit village fixed effect (not reported in paper) */
local varlist fan_own fridge_own iron_own pc_own radio_own rice_cooker_ownr tv_own water_pump_ownr
sum `varlist'
foreach z of local varlist {

* Logistic: Basic specification - empowerment 
logit `z' empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr i.psu [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain1
outreg2 using "$outputpath/logit_nep_`z'1_fe.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

/*
* clogit with fixed effects at psu - empowerment
logit `z' empower_zscore  maleeduc wealth_zscore hours_gridr  hours_shsr i.psu  [pweight=weight],  vce(cluster psu) or
estat ic
estimates store logisticmain2
outreg2 using "$outputpath/logit_nep_`z'2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform
*/


* Logistic: Basic specification - headgender
logit `z' headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr i.psu [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain3
outreg2 using "$outputpath/logit_nep_`z'3_fe.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

/*
* clogit with fixed effects at psu - headgender
logit `z' headgender  maleeduc wealth_zscore hours_gridr  hours_shsr i.psu [pweight=weight],  vce(cluster psu) or
estat ic
estimates store logisticmain4
outreg2 using "$outputpath/logit_nep_`z'4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform
*/
}

* AMEs for paper *
* Fan
logit fridge_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight], vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

* Iron
logit iron_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight], vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

* PC
logit pc_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight], vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

* Radio
logit radio_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight], vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

* Ricecooker
logit rice_cooker_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight], vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

* TV
logit tv_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight], vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

* Water pump
logit water_pump_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight], vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

* Predicted probability by HH gender

* Fan
logit fan_own i.headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr  [pweight=weight] , vce(cluster psu) or
margins headgender, vce(unconditional)
margins headgender, atmeans vce(unconditional)

* Water pump
logit water_pump_own i.headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr  [pweight=weight] , vce(cluster psu) or
margins headgender, vce(unconditional)
margins headgender, atmeans vce(unconditional)


* Robustness: without controlling for electricity access

local varlist fan_own fridge_own iron_own pc_own radio_own rice_cooker_ownr tv_own water_pump_ownr
sum `varlist'
foreach z of local varlist {

* Logistic: Basic specification - empowerment 
logit `z' empower_zscore i.maleeduc wealth_zscore  [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain1
*outreg2 using "$outputpath/logit_nep_`z'5.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

/*
* clogit with fixed effects at psu - empowerment
logit `z' empower_zscore  maleeduc wealth_zscore hours_gridr  hours_shsr i.psu  [pweight=weight],  vce(cluster psu) or
estat ic
estimates store logisticmain2
outreg2 using "$outputpath/logit_nep_`z'2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform
*/


* Logistic: Basic specification - headgender
logit `z' headgender i.maleeduc wealth_zscore  [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain3
*outreg2 using "$outputpath/logit_nep_`z'7.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

/*
* clogit with fixed effects at psu - headgender
logit `z' headgender  maleeduc wealth_zscore hours_gridr  hours_shsr i.psu [pweight=weight],  vce(cluster psu) or
estat ic
estimates store logisticmain4
outreg2 using "$outputpath/logit_nep_`z'4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform
*/
}

logit rice_cooker_own i.headgender i.maleeduc wealth_zscore  [pweight=weight] , vce(cluster psu) or
margins headgender, vce(unconditional)
margins headgender, atmeans vce(unconditional)
