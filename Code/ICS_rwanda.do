********************************************************************************
**** RWANDA ****
********************************************************************************

use "$datapath/stove_estimations.dta", clear
set matsize 11000
keep if country=="rwanda"

gen headgender=real(head_gender)

gen maleeduc=.
replace maleeduc=0 if male_educ=="0"
replace maleeduc=1 if male_educ=="1"
replace maleeduc=2 if male_educ=="2" | male_educ=="3"
replace maleeduc=3 if male_educ=="4" | male_educ=="5"| male_educ=="6" | male_educ=="7"

label define school_rwanda 0 "none" 1 "primary" 2 "secondary" 3 "post-secondary" 
label values maleeduc school_rwanda


gen cleanimproved=0
replace cleanimproved=1 if stovetype==1 | stovetype==2
replace cleanimproved=. if stovetype==.


********************************************************************************
**** Logistic regressions ****
********************************************************************************

*** Empowerment ***
sum cleanimproved empower_zscore maleeduc wealth_zscore

* Logistic: Basic specification - empowerment 
logistic cleanimproved empower_zscore i.maleeduc wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logistic1
outreg2 using "$outputpath/logit_rwa_cleanimproved_1.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - empowerment
clogit cleanimproved empower_zscore i.maleeduc wealth_zscore [pweight=weight], group(id_psu)  or vce(cluster psu) 
estat ic
estimates store logistic2
outreg2 using "$outputpath/logit_rwa_cleanimproved_2.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* Logistic: Basic specification - empowerment 
logistic cleanimproved headgender i.maleeduc wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logistic3
outreg2 using "$outputpath/logit_rwa_cleanimproved_3.tex", label tex alpha(0.01, 0.05, 0.1) replace eform

* clogit with fixed effects at psu - headgender
clogit cleanimproved headgender i.maleeduc wealth_zscore [pweight=weight], group(id_psu)  or vce(cluster psu) 
estat ic
estimates store logistic4
outreg2 using "$outputpath/logit_rwa_cleanimproved_4.tex", label tex alpha(0.01, 0.05, 0.1) replace eform




*** Robustness: estimations without male edu

* Logistic: Basic specification - empowerment 
logistic cleanimproved empower_zscore  wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logistic1

* clogit with fixed effects at psu - empowerment
clogit cleanimproved empower_zscore wealth_zscore [pweight=weight], group(id_psu)  or vce(cluster psu) 
estat ic
estimates store logistic2

* Logistic: Basic specification - empowerment 
logistic cleanimproved headgender wealth_zscore [pweight=weight], vce(cluster psu) or
estat ic
estimates store logistic3

* clogit with fixed effects at psu - headgender
clogit cleanimproved headgender wealth_zscore [pweight=weight], group(id_psu)  or vce(cluster psu) 
estat ic
estimates store logistic4

