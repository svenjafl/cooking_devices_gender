
use "$datapath/dataset_for_table1.dta", clear

tab stove_clean country [aweight=weight], col /* for all countries*/
tab stovetype [aweight=weight] if country=="rwanda" /* Rwanda only */

tab stove_clean country, col mis /* for all countries*/
tab stovetype  if country=="rwanda", mis /* Rwanda only */

local varlist kettle_own vcd_dvd_own microwave_own washing_machine_own water_heater_own blender_own rice_cooker_own water_pump_own solar_water_heater_own
foreach i of local varlist {
gen `i'r = real(`i')  
}
drop `varlist'

local varlist fan_own fridge_own iron_own pc_own radio_own tv_own kettle_ownr vcd_dvd_ownr microwave_ownr washing_machine_ownr water_heater_ownr blender_ownr rice_cooker_ownr water_pump_ownr solar_water_heater_ownr
foreach z of local varlist {
tab `z' country [aweight=weight], col mis
}

local varlist fan_own fridge_own iron_own pc_own radio_own tv_own kettle_ownr vcd_dvd_ownr microwave_ownr washing_machine_ownr water_heater_ownr blender_ownr rice_cooker_ownr water_pump_ownr solar_water_heater_ownr
foreach z of local varlist {
tab `z' country , col mis
}


local varlist grid mg shs years_grid years_mg years_shs hours_grid hours_mg hours_shs
foreach i of local varlist {
gen `i'r = real(`i')  
}
drop `varlist'

local varlist gridr mgr shsr
foreach z of local varlist {
tab `z' country [aweight=weight], col mis
}

local country honduras nepal rwanda
foreach z of local country {
sum years_grid years_mg years_shs hours_grid hours_mg hours_shs if country=="`z'" [aweight=weight]
}

save "$datapath/stove_estimations.dta", replace
