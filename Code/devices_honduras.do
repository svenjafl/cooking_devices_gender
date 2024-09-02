

use "$datapath/stove_estimations.dta", clear
set matsize 11000
keep if country=="honduras"

gen maleeduc=.
replace maleeduc=0 if male_educ=="1" | male_educ=="11"
replace maleeduc=1 if male_educ=="2" | male_educ=="3"
replace maleeduc=2 if male_educ=="4" | male_educ=="5"
replace maleeduc=3 if male_educ=="6" | male_educ=="7"| male_educ=="8" | male_educ=="9" | male_educ=="10"

label define school_honduras 0 "none" 1 "primary" 2 "secondary" 3 "post-secondary" 
label values maleeduc school_honduras


gen headgender=real(head_gender)

replace years_gridr=0 if years_gridr==. & gridr==0
replace hours_gridr=0 if hours_gridr==. & gridr==0
replace years_mgr=0 if years_mgr==. & mgr==0
replace hours_mgr=0 if hours_mgr==. & mgr==0
replace years_shsr=0 if years_shsr==. & shsr==0
replace hours_shsr=0 if hours_shsr==. & shsr==0

local varlist fan_own fridge_own iron_own microwave_own pc_own radio_own tv_own washing_machine_ownr
sum `varlist'
foreach z of local varlist {

* Logistic: Basic specification - empowerment 
logit `z' empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain1
outreg2 using "$outputpath/logit_hon_`z'1.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - empowerment
clogit `z' empower_zscore i.maleeduc wealth_zscore hours_gridr  hours_shsr   [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain2
outreg2 using "$outputpath/logit_hon_`z'2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* Logistic: Basic specification - headgender
logit `z' headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr  [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain3
outreg2 using "$outputpath/logit_hon_`z'3.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - headgender
clogit `z' headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr  [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain4
outreg2 using "$outputpath/logit_hon_`z'4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

}

* AMEs for paper *
* Fan
logit fan_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight] , vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a fan", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_fan.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_fan.png", replace

clogit fan_own empower_zscore i.maleeduc wealth_zscore hours_gridr  hours_shsr   [pweight=weight], group(id_psu)  vce(cluster psu) or
margins, dydx(empower_zscore)

* Fridge (clogit only)
clogit fridge_own empower_zscore i.maleeduc wealth_zscore hours_gridr  hours_shsr   [pweight=weight], group(id_psu)  vce(cluster psu) or
margins, dydx(empower_zscore)

* Iron
logit iron_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight] , vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning an iron", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_iron.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_iron.png", replace

clogit iron_own empower_zscore i.maleeduc wealth_zscore hours_gridr  hours_shsr   [pweight=weight], group(id_psu)  vce(cluster psu) or
margins, dydx(empower_zscore)

* Microwave (logit only)
logit microwave_own empower_zscore i.maleeduc wealth_zscore hours_gridr hours_shsr [pweight=weight] , vce(cluster psu) or
margins, dydx(empower_zscore)
sum empower_zscore
margins, at(empower_zscore=( -2.312215  (.01) 1.6523)) 
marginsplot, graphregion(color(white)) xtitle("Female decision-making power index", color(black)) ytitle("Probability of owning a microwave", color(black)) title("Honduras - Fan", color(black))
graph save Graph "$graphpath/hond_predictive_margins_empowerment_microwave.gph", replace
graph export "$graphpath/hond_predictive_margins_empowerment_microwave.png", replace

* TV
logit tv_own i.headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr  [pweight=weight] , vce(cluster psu) or
margins headgender, vce(unconditional)
margins headgender, atmeans vce(unconditional)

clogit tv_own i.headgender i.maleeduc wealth_zscore hours_gridr  hours_shsr  [pweight=weight], group(id_psu)  vce(cluster psu) or
margins headgender, vce(unconditional)
margins headgender, atmeans vce(unconditional)

* Robustness: without controlling for electricity access

local varlist fan_own fridge_own iron_own microwave_own pc_own radio_own tv_own washing_machine_ownr
sum `varlist'
foreach z of local varlist {

* Logistic: Basic specification - empowerment 
logit `z' empower_zscore i.maleeduc wealth_zscore  [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain1
*outreg2 using "$outputpath/logit_hon_`z'5.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - empowerment
clogit `z' empower_zscore i.maleeduc wealth_zscore   [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain2
*outreg2 using "$outputpath/logit_hon_`z'6.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* Logistic: Basic specification - headgender
logit `z' headgender i.maleeduc wealth_zscore   [pweight=weight] , vce(cluster psu) or
estat ic
estimates store logisticmain3
*outreg2 using "$outputpath/logit_hon_`z'7.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - headgender
clogit `z' headgender i.maleeduc wealth_zscore   [pweight=weight], group(id_psu)  vce(cluster psu) or
estat ic
estimates store logisticmain4
*outreg2 using "$outputpath/logit_hon_`z'8.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

}

logit tv_own empower_zscore i.maleeduc wealth_zscore  [pweight=weight] , vce(cluster psu) or
margins, dydx(empower_zscore)

