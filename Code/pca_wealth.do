
***************************************
*** Requires the data file "alccountry.csv" stored in the data sub folder ***

********************************************************************************
*** HONDURAS ***

import delimited $datahpath/allcountry.csv, clear
keep if country=="honduras"

gen hhexpannual=real(hh_exp_annual)
gen ownhouse=real(own_house)
* gen drwater=real(drinkingwater)
gen tlet = real(toilet)
drop toilet

gen toilet=.
replace toilet=1 if tlet==1
replace toilet=2 if tlet==7 | tlet==8 | tlet==9
replace toilet=3 if tlet==5 | tlet==6
replace toilet=4 if tlet==3 | tlet==4
replace toilet=5 if tlet==2
label define toilet 1 "no toilet (open field)" 2 "open latrine/ cesspool" 3 "washable latrine" 4 "flush to tank/river" 5 "flush to sewage", replace
label values toilet toilet

gen flo=real(floor)
drop floor
gen floor=.
replace floor=1 if flo==1
replace floor=2 if flo== 2 | flo==3 |flo==4 | flo==5
replace floor = 3 if flo==6
label define floor 1 "mud/dung" 2 "solid (cement, wood planks, mudbrick)" 3 "ceramic/ marble tiles"
label values floor floor

gen ro=real(roof)
drop roof
gen roof=.
replace roof=1 if ro>5 | ro==2 | ro==3
replace roof=2 if ro==1 | ro==4
label define roof 1 "corrugated, bambu, scrap, single, asbesto" 2 "cement or clay tile"
label values roof roof

gen wal=real(wall)
drop wall
gen wall=.
replace wall=3 if wal<=2
replace wall=2 if wal==3 | wal==4
replace wall=1 if wal>4
label define wall 3 "bricks or plastered blocks" 2 "mud bricks or unplastered blocks" 1 "stone, wood, zins, palm, scrap material", replace
label values wall wall

gen ownland=real(land)
gen hhmemroom=real(hhmem_room)

gen bicycl=real(bicycle)
recode bicycl (3 4 5 8 10=2)

gen motorcyc=real(motorcycle)
replace motorcyc=1 if motorcyc>1

gen car=real(vehicle)
replace car=2 if car>2

local varlist  toilet floor roof wall   bicycl  car
sum `varlist'
polychoric `varlist', pw
polychoricpca `varlist' [pweight=weight], pw score(pc_scores) nscore(1)

matrix correlation = r(R)
estout matrix(correlation) using "$outputpath/wealth_corr_honduras.xls", replace

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
graph export "$graphpath/scree_plot_honduras.png", replace
restore

histogram pc_scores1, graphregion(color(white)) title(Honduras, color(black)) color(navy) xtitle("")
graph save "$graphpath/PCA_plot_honduras.gph", replace

keep hhid pc_scores1

sum pc_scores1
return list
gen wealth_zscore = (pc_scores1 - r(mean)) / r(sd)
sum wealth_zscore

histogram wealth_zscore, graphregion(color(white)) title(Honduras, color(black)) color(gs6) xtitle("")
graph save "$graphpath/PCA_plot_honduras.gph", replace

save "$datapath/pca_honduras.dta", replace

********************************************************************************
*** NEPAL ***

import delimited $datahpath/allcountry.csv, clear
keep if country=="nepal"

gen hhexpannual=real(hh_exp_annual)
gen ownhouse=real(own_house)
* gen drwater=real(drinkingwater)
gen tlet = real(toilet)
drop toilet

gen toilet=.
replace toilet=1 if tlet==4
replace toilet=2 if tlet==5 | tlet==6 | tlet==7
replace toilet=3 if tlet==3
replace toilet=4 if tlet==2
label define toilet 1 "pail/ bucket" 2 "latrine" 3 "flush to tank" 4 "flush to sewage", replace
label values toilet toilet

gen flo=real(floor)
drop floor
gen floor=.
replace floor=1 if flo==1 | flo==2
replace floor=2 if flo== 6 | flo==3 |flo==7 | flo==5 | flo==8
replace floor=3 if flo==4 | flo==9
label define floor 1 "mud/dung/reed/bamboo" 2 "solid (cement screed, wood planks, tiles)" 3 "ceramic/ marble tiles r parquet/polished wood"
label values floor floor

gen ro=real(roof)
drop roof
gen roof=.
replace roof=1 if ro<3 | ro==5 | ro==6 | ro==7 | ro==8
replace roof=2 if ro==3 | ro==4 | ro==9
label define roof 1 "corrugated, bambu, scrap, single, asbesto" 2 "cement or clay tile, concrete, stone"
label values roof roof

gen wal=real(wall)
drop wall
gen wall=.
replace wall=3 if wal==7 | wal==9 | wal==13
replace wall=2 if wal==10 | wal==11 | wal==8  | wal==6
replace wall=1 if wal<6 | wal==12 | wal>13
label define wall 3 "bricks or plastered blocks" 2 "mud bricks or unplastered blocks" 1 "stone, wood, zins, palm, scrap material", replace
label values wall wall

gen ownland=real(land)
gen hhmemroom=real(hhmem_room) /* too many missings*/

gen bicycl=real(bicycle)
replace bicycl=3 if bicycl>3 /* too many missings*/

gen motorcyc=real(motorcycle)
replace motorcyc=2 if motorcyc>2 /* too many missings*/

gen car=real(vehicle)
replace car=3 if car>3

local varlist toilet floor roof wall ownland car
sum `varlist'
polychoric `varlist', pw
polychoricpca `varlist' [pweight=weight], pw score(pc_scores) nscore(1)

matrix correlation = r(R)
estout matrix(correlation) using "$outputpath/wealth_corr_nepal.xls", replace

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
graph export "$graphpath/scree_plot_nepal.png", replace
restore

keep hhid pc_scores1

sum pc_scores1 
gen wealth_zscore = (pc_scores1 - r(mean)) / r(sd)
sum wealth_zscore

histogram wealth_zscore, graphregion(color(white)) title(Nepal, color(black)) color(gs6) xtitle("")
graph save "$graphpath/PCA_plot_nepal.gph", replace

save "$datapath/pca_nepal.dta", replace

********************************************************************************
*** RWANDA ***

import delimited $datahpath/allcountry.csv, clear
keep if country=="rwanda"

gen ownhouse=real(own_house)
* gen drwater=real(drinkingwater)
gen tlet = real(toilet)
drop toilet

gen toilet=. 
replace toilet=1 if tlet==4 | tlet==8
replace toilet=2 if tlet==5 | tlet==6 | tlet==7
replace toilet=3 if tlet==3
replace toilet=4 if tlet==2 | tlet==1
label define toilet 1 "open field or pail/ bucket" 2 "latrine" 3 "flush to tank" 4 "flush to sewage or on water", replace
label values toilet toilet

gen flo=real(floor)
drop floor
gen floor=.
replace floor=1 if flo==1 | flo==2
replace floor=2 if flo==3 
replace floor=3 if flo==4 | flo==5 |flo==6 
label define floor 1 "earth, dung" 2 "wood" 3 "bricks, cement, clay tiles"
label values floor floor

gen ro=real(roof)
drop roof
gen roof=.
replace roof=1 if ro==5 | ro==1 
replace roof=2 if ro==2
replace roof=3 if ro==3 | ro==4
label define roof 1 "thatch/leaves/grass or plastic/immpermanent" 2 "metal sheets, corrugated" 3 "tiles clay, concrete"
label values roof roof

gen wal=real(wall)
drop wall
gen wall=.
replace wall=1 if wal==7 | wal==9 | wal==8
replace wall=2 if wal==1 | wal==3 | wal==5  | wal==6
replace wall=3 if wal==2 | wal==4 
label define wall 1 "tree trunks, plastic sheeting" 2 "mud bricks, oven fired bricks, wooden planks, stones" 3 "mud bricks with cement, cement blocks", replace
label values wall wall

gen ownland=real(land)
gen hhmemroom=real(hhmem_room) 

gen bicycl=real(bicycle)
replace bicycl=1 if bicycl>1 

gen motorcyc=real(motorcycle)
replace motorcyc=1 if motorcyc==2 

gen car=real(vehicle)
replace car=3 if car>3

local varlist  toilet floor roof wall  hhmemroom bicycl motorcyc car
sum `varlist'
polychoric `varlist', pw
return list
polychoricpca `varlist' [pweight=weight], pw score(pc_scores) nscore(1)
return list

matrix correlation = r(R)
estout matrix(correlation) using "$outputpath/wealth_corr_rwanda.xls", replace

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
graph export "$graphpath/scree_plot_rwanda.png", replace
restore

histogram pc_scores1, graphregion(color(white)) title(Rwanda, color(black)) color(gs6) xtitle("")
graph save "$graphpath/PCA_plot_rwanda.gph", replace

keep hhid pc_scores1

sum pc_scores1 
gen wealth_zscore = (pc_scores1 - r(mean)) / r(sd)
sum wealth_zscore

save "$datapath/pca_rwanda.dta", replace

***************************************
*** Common dataset ***

use "$datapath/pca_nepal.dta", clear
append using "$datapath/pca_rwanda.dta"
append using "$datapath/pca_honduras.dta"
save "$datapath/pca_wealth.dta", replace

***************************************
*** Common graph ***

gr combine  "$graphpath/PCA_plot_honduras.gph"  "$graphpath/PCA_plot_nepal.gph" ///
"$graphpath/PCA_plot_rwanda.gph"  , col(2) iscale(.5) graphregion(color(white)) ycommon xcommon
graph export "$graphpath/wealth_neu.pdf", replace 
graph export "$graphpath/wealth_neu.png", replace




