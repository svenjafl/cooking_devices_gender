
***************************************
*** Requires the data file "alccountry.csv" stored in the data sub folder ***


*** HONDURAS ***

import delimited $datahpath/allcountry.csv, clear
keep if country=="honduras"

**** data preparation
gen headgender=real(head_gender)
gen meanemploy_women_over_15 = real(mean_employ_women_over_15)
gen shareadultwomen = real(share_adult_women)
gen groupmem=real(women_group_mem)

gen women_visitpeople=.
replace women_visitpeople=3 if womemp_visitpeople=="1"
replace women_visitpeople=2 if womemp_visitpeople=="2"
replace women_visitpeople=1 if womemp_visitpeople=="3"
replace women_visitpeople=. if womemp_visitpeople=="555" | womemp_visitpeople=="NA"

gen women_visitmarket=.
replace women_visitmarket=3 if womemp_vismarket=="1"
replace women_visitmarket=2 if womemp_vismarket=="2"
replace women_visitmarket=1 if womemp_vismarket=="3"
replace women_visitmarket=. if womemp_vismarket=="555" | womemp_vismarket=="NA"

gen women_leavevill=.
replace women_leavevill=3 if womenp_leavevill=="1"
replace women_leavevill=2 if womenp_leavevill=="2"
replace women_leavevill=1 if womenp_leavevill=="3"
replace women_leavevill=. if womenp_leavevill=="555" | womenp_leavevill=="NA"

gen femaleeduc=.
replace femaleeduc=0 if female_educ=="1" | female_educ=="11"
replace femaleeduc=1 if female_educ=="2" | female_educ=="3"
replace femaleeduc=2 if female_educ=="4" | female_educ=="5"
replace femaleeduc=3 if female_educ=="6" | female_educ=="7"| male_educ=="8" | male_educ=="9" | male_educ=="10"

label define school_honduras 0 "none" 1 "primary" 2 "secondary" 3 "post-secondary" 
label values femaleeduc school_honduras

labe define mobility 3 "woman decides" 2 "woman decides with husband" 1 "husband decides" 
label values women_visitmarket women_visitpeople women_leavevill mobility

local varlist  femaleeduc shareadultwomen meanemploy_women_over_15 women_visitpeople women_visitmarket women_leavevill  
*local varlist headgender women_visitpeople women_visitmarket women_leavevill

sum `varlist' 
polychoric `varlist', pw
return list
polychoricpca `varlist' [pweight=weight], pw score(empowerment) nscore(1)
return list

matrix Eigenvalues = r(eigenvalues)

* DRAW SCREEPLOT
preserve
svmat Eigenvalues
keep Eigenvalues*
keep in 1
xpose, clear
rename v1 Eigenvalues
gen byte Number = _n

twoway connected Eigenvalues Number, ///
	title("Scree plot of eigenvalues, `pca_type' PCA") ///
	subtitle("`survey_name'") ///
	xtitle("Principal component") ///
	ytitle("Eigenvalue") ///
	saving(scree_plot, replace)
graph export "$graphpath/scree_plot_honduras_empowerment.png", replace
restore

kdensity empowerment1 [aweight=weight], graphregion(color(white)) lcolor(black) title(Honduras, color(black)) color(navy) xtitle("") note("")

keep hhid country empowerment1 weight

sum empowerment1
gen empower_zscore = (empowerment1   - r(mean)) / r(sd)
sum empower_zscore

kdensity empower_zscore [aweight=weight], graphregion(color(white)) lcolor(black) title(Honduras, color(black)) color(navy) xtitle("") note("")
graph save "$graphpath/PCA_plot_honduras_empowermentr.gph", replace

save "$datapath/pca_honduras_empowermentr.dta", replace

*** NEPAL ***

import delimited $datahpath/allcountry.csv, clear
drop if country!="nepal"

**** data preparation
gen headgender=real(head_gender)
gen meanemploy_women_over_15 = real(mean_employ_women_over_15)
gen women_groupmen = real(women_group_mem)
gen shareadultwomen = real(share_adult_women)
gen groupmem=real(women_group_mem)

gen women_visitpeople=.
replace women_visitpeople=3 if womemp_visitpeople=="1"
replace women_visitpeople=2 if womemp_visitpeople=="2"
replace women_visitpeople=1 if womemp_visitpeople=="3"
replace women_visitpeople=. if womemp_visitpeople=="555" | womemp_visitpeople=="NA"

gen women_visitmarket=.
replace women_visitmarket=3 if womemp_vismarket=="1"
replace women_visitmarket=2 if womemp_vismarket=="2"
replace women_visitmarket=1 if womemp_vismarket=="3"
replace women_visitmarket=. if womemp_vismarket=="555" | womemp_vismarket=="NA"

gen women_leavevill=.
replace women_leavevill=3 if womenp_leavevill=="1"
replace women_leavevill=2 if womenp_leavevill=="2"
replace women_leavevill=1 if womenp_leavevill=="3"
replace women_leavevill=. if womenp_leavevill=="555" | womenp_leavevill=="NA"

label define mobility 3 "woman decides" 2 "woman decides with husband" 1 "husband decides" 
label values women_visitmarket women_visitpeople women_leavevill mobility

gen femaleeduc=.
replace femaleeduc=0 if female_educ=="0"
replace femaleeduc=1 if female_educ=="2"
replace femaleeduc=2 if female_educ=="3" | female_educ=="4"
replace femaleeduc=3 if female_educ=="5" | female_educ=="6" | female_educ=="7"

label define school_nepal 0 "none" 1 "primary" 2 "secondary" 3 "post-secondary" 
label values femaleeduc school_nepal

local varlist  femaleeduc shareadultwomen meanemploy_women_over_15 women_visitpeople women_visitmarket women_leavevill 
*local varlist headgender women_visitpeople women_visitmarket women_leavevill

sum `varlist'
polychoric `varlist', pw
polychoricpca `varlist' [pweight=weight], pw score(empowerment) nscore(1)

matrix Eigenvalues = r(eigenvalues)

* DRAW SCREEPLOT
preserve
svmat Eigenvalues
keep Eigenvalues*
keep in 1
xpose, clear
rename v1 Eigenvalues
gen byte Number = _n

twoway connected Eigenvalues Number, ///
	title("Scree plot of eigenvalues, `pca_type' PCA") ///
	subtitle("`survey_name'") ///
	xtitle("Principal component") ///
	ytitle("Eigenvalue") ///
	saving(scree_plot, replace)
graph export "$graphpath/scree_plot_nepal_empowerment.png", replace
restore

kdensity empowerment1 [aweight=weight], graphregion(color(white)) lcolor(black) title(Nepal, color(black)) color(navy) xtitle("") note("")

keep hhid country empowerment1 weight

sum empowerment1
gen empower_zscore = (empowerment1 - r(mean)) / r(sd)
sum empower_zscore 

kdensity empower_zscore [aweight=weight], graphregion(color(white)) lcolor(black) title(Nepal, color(black)) color(navy) xtitle("") note("")
graph save "$graphpath/PCA_plot_nepal_empowermentr.gph", replace

save "$datapath/pca_nepal_empowermentr.dta", replace

*** RWANDA ***

import delimited $datahpath/allcountry.csv, clear
keep if country=="rwanda"

**** data preparation
gen headgender=real(head_gender)
*gen femaleeduc=real(female_educ) /* recode in categories because of literacy */
gen meanemploy_women_over_15 = real(mean_employ_women_over_15)
gen shareadultwomen = real(share_adult_women)
gen groupmem=real(women_group_mem)

gen women_visitpeople=.
replace women_visitpeople=3 if womemp_visitpeople=="1"
replace women_visitpeople=2 if womemp_visitpeople=="2"
replace women_visitpeople=1 if womemp_visitpeople=="3"
replace women_visitpeople=. if womemp_visitpeople=="555" | womemp_visitpeople=="NA"

gen women_visitmarket=.
replace women_visitmarket=3 if womemp_vismarket=="1"
replace women_visitmarket=2 if womemp_vismarket=="2"
replace women_visitmarket=1 if womemp_vismarket=="3"
replace women_visitmarket=. if womemp_vismarket=="555" | womemp_vismarket=="NA"

gen women_leavevill=.
replace women_leavevill=3 if womenp_leavevill=="1"
replace women_leavevill=2 if womenp_leavevill=="2"
replace women_leavevill=1 if womenp_leavevill=="3"
replace women_leavevill=. if womenp_leavevill=="555" | womenp_leavevill=="NA"

label define mobility 3 "woman decides" 2 "woman decides with husband" 1 "husband decides" 
label values women_visitmarket women_visitpeople women_leavevill mobility

gen femaleeduc=.
replace femaleeduc=0 if female_educ=="0"
replace femaleeduc=1 if female_educ=="1"
replace femaleeduc=2 if female_educ=="2" | female_educ=="3"
replace femaleeduc=3 if female_educ=="4" | female_educ=="5"| female_educ=="6" | female_educ=="7"

label define school_rwanda 0 "none" 1 "primary" 2 "secondary" 3 "post-secondary" 
label values femaleeduc school_rwanda

local varlist  femaleeduc meanemploy_women_over_15 women_visitmarket women_visitpeople women_leavevill  shareadultwomen 
*local varlist headgender women_visitmarket women_visitpeople women_leavevill
polychoric `varlist', pw
polychoricpca `varlist' [pweight=weight], pw score(empowerment) nscore(1)
return list

matrix Eigenvalues = r(eigenvalues)

* DRAW SCREEPLOT
preserve
svmat Eigenvalues
keep Eigenvalues*
keep in 1
xpose, clear
rename v1 Eigenvalues
gen byte Number = _n

twoway connected Eigenvalues Number, ///
	title("Scree plot of eigenvalues, `pca_type' PCA") ///
	subtitle("`survey_name'") ///
	xtitle("Principal component") ///
	ytitle("Eigenvalue") ///
	saving(scree_plot, replace)
graph export "$graphpath/scree_plot_rwanda_empowerment.png", replace
restore

kdensity empowerment1 [aweight=weight], graphregion(color(white)) title(Rwanda, color(black)) color(navy) xtitle("") note("")

keep hhid country empowerment1 weight

sum empowerment1
gen empower_zscore = (empowerment1  - r(mean)) / r(sd)
sum empower_zscore 

kdensity empower_zscore [aweight=weight], graphregion(color(white)) lcolor(black) title(Rwanda, color(black)) xtitle("") note("")
graph save "$graphpath/PCA_plot_rwanda_empowermentr.gph", replace

save "$datapath/pca_rwanda_empowermentr.dta", replace

***************************************
*** Common dataset ***

use "$datapath/pca_nepal_empowermentr.dta", clear
append using "$datapath/pca_rwanda_empowermentr.dta"
append using "$datapath/pca_honduras_empowermentr.dta"

save "$datapath/pca_empowermentr.dta", replace

***************************************
*** Common graph ***

gr combine "$graphpath/PCA_plot_honduras_empowerment.gph" ///
"$graphpath/PCA_plot_nepal_empowerment.gph" ///
"$graphpath/PCA_plot_rwanda_empowerment.gph" ///
, col(2) iscale(.5) graphregion(color(white)) ycommon xcommon 
graph export "$graphpath/empowerment_new.pdf", replace
graph export "$graphpath/empowerment_new.png", replace

