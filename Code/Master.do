
/*-------------------------------
Created by: Svenja Flechtner
Created on: 6 August 2022

Last edited by: Svenja Flechtner
Last edited on: 6 March 2024
-------------------------------*/

clear all

version 15 
set more off
set logtype text
set linesize 255 

*** Current directory - adjust your directory
cd " "

*** Macros
global outputpath "./Results"
global datapath "./Data/"
global graphpath "./Graphs"
global outputname "Log_2024"  

set matsize 10000
cap log close

*** 
log using "$outputpfad\$outputname.log", replace


********************************************************************************
*** 1) PCA Women empowerment 
********************************************************************************

do ./pca_empowerment.do
* Prepares the data and runs the principal component analysis to produce the empowerment indicator. Produces "$datapath/pca_empowerment.dta" 

********************************************************************************
*** 2) PCA Wealth 
********************************************************************************

do ./pca_wealth.do
* Prepares the data and runs the principal component analysis to produce the wealth indicator. Produces "$datapath/pca_wealth.dta"

********************************************************************************
*** 3) Combine dataset with indices
********************************************************************************

import delimited $datahpath/allcountry.csv, clear

merge 1:1 hhid using "$datapath/pca_wealth.dta"
drop _merge
merge 1:1 hhid using "$datapath/pca_empowermentr.dta"
drop _merge

sort admin1
egen id_admin1 = group(admin1)
sort admin2
egen id_admin2 = group(admin2)
sort psu
egen id_psu = group(psu)

save "$datapath/allcountries_bothindices.dta", replace
use "$datapath/allcountries_bothindices.dta", clear

kdensity empower_zscore [aweight=weight] if country=="honduras" & head_gender=="0", graphregion(color(white)) lcolor(black) title("Honduras, male-headed households", color(black)) xtitle("") note("")
graph save "$graphpath/empowerment_maleHH_hond.gph", replace
kdensity empower_zscore [aweight=weight] if country=="honduras" & head_gender=="1", graphregion(color(white)) lcolor(black) title("Honduras, female-headed households", color(black)) xtitle("") note("")
graph save "$graphpath/empowerment_femaleHH_hond.gph", replace
kdensity empower_zscore [aweight=weight] if country=="nepal" & head_gender=="0", graphregion(color(white)) lcolor(black) title("Nepal, male-headed households", color(black)) xtitle("") note("")
graph save "$graphpath/empowerment_maleHH_nep.gph", replace
kdensity empower_zscore [aweight=weight] if country=="nepal" & head_gender=="1", graphregion(color(white)) lcolor(black) title("Nepal, female-headed households", color(black)) xtitle("") note("")
graph save "$graphpath/empowerment_femaleHH_nep.gph", replace
kdensity empower_zscore [aweight=weight] if country=="rwanda" & head_gender=="0", graphregion(color(white)) lcolor(black) title("Rwanda, male-headed households", color(black)) xtitle("") note("")
graph save "$graphpath/empowerment_maleHH_rwa.gph", replace
kdensity empower_zscore [aweight=weight] if country=="rwanda" & head_gender=="1", graphregion(color(white)) lcolor(black) title("Rwanda, female-headed households", color(black)) xtitle("") note("")
graph save "$graphpath/empowerment_femaleHH_rwa.gph", replace

grc1leg "$graphpath/empowerment_maleHH_hond.gph" ///
"$graphpath/empowerment_maleHH_nep.gph"  ///
"$graphpath/empowerment_maleHH_rwa.gph"  ///
"$graphpath/empowerment_femaleHH_hond.gph" ///
"$graphpath/empowerment_femaleHH_nep.gph"  ///
"$graphpath/empowerment_femaleHH_rwa.gph"  ///
, col(3) ycommon iscale(.5) graphregion(color(white)) 

graph save "$graphpath/agencyindex_by_HH.gph", replace
graph export "$graphpath/agencyindex_by_HH.pdf", replace

********************************************************************************
*** 4) Stove data
********************************************************************************

import delimited "$datapath/rwanda_cooking.csv", clear 
keep hhid hh_id stove_type stove_1stfuel stove_2ndfuel stove_obtain_hhmember stove_main country
save "$datapath/rwanda_cooking.dta", replace

use "$datapath/allcountries_bothindices.dta", clear

merge m:m hhid using "$datapath/rwanda_cooking.dta"
drop _merge

duplicates report hhid if country=="rwanda"
duplicates report hhid
duplicates tag hhid, generate(dup)

tab dup if stove_main=="No"
drop if stove_main=="No"

drop dup
duplicates tag hhid, generate(dup)
tab country

gen stovetype=.
replace stovetype=1 if stove_type=="Clean fuel stove"
replace stovetype=2 if stove_type=="Improved biomass stove"
replace stovetype=3 if stove_type=="Three stone stove"
replace stovetype=4 if stove_type=="Traditional stove"
label define stovetype 1 "clean fuel stove" 2 "improved biomass stove" 3 "three stone stove" 4 "traditional stove"
label values stovetype stovetype

rename primarystoveclean_stated stove_clean

save "$datapath/dataset_for_table1.dta", replace

********************************************************************************
*** 4) Summary statistics (table 1 in paper)
********************************************************************************

do "./table 1.do"

********************************************************************************
*** 4) Estimations: clean cooking fuel
********************************************************************************

do ./stoveclean_honduras.do
do ./stoveclean_nepal.do

grc1leg "$graphpath/hond_predictive_margins_empowerment_stove1.gph" ///
"$graphpath/nep_predictive_margins_empowerment_stove1.gph" ///
, col(2) ycommon iscale(.5) graphregion(color(white)) 
graph save "$graphpath/predictive_margins_stoves_honnep.gph", replace
graph export "$graphpath/predictive_margins_stoves_honnep.pdf", replace

* do ./threestone_rwanda.do
do ./ICS_rwanda.do

********************************************************************************
*** 5) Estimations: devices
********************************************************************************

do ./devices_honduras.do
do ./devices_nepal.do
do ./devices_rwanda.do




