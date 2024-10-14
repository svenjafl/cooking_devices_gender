# TITLE

# Paper authors

# Script authors Ulli Lich, Setu Pelz

# Load Necessary Packages ------------------------------------------------------
library(pacman)

# Data Manipulation and Tidying
p_load(dplyr, tidyr, purrr, forcats, here, foreign, readstata13, srvyr, stringr)

# Visualization
p_load(ggplot2, ggrepel, patchwork, ggpubr, factoextra)

# Statistical Modeling and Analysis
p_load(fixest, margins, sandwich, estimatr, summarytools, plm, modelsummary, 
       flextable, haven, stargazer, nnet, mlogit, AER, fastDummies)

# Data reading and writing
p_load(here, readr)

# Custom Functions
naconvert <- function(x) {ifelse(x < 0 | x == 888 | x == "888" | x == 555 | x == "555", NA, x)}

natozero <- function(x) {ifelse(is.na(x), 0, x)}

# Prevent scientific notation and set environment language
options(scipen = 999)
Sys.setenv(LANG = "en")

# DATA READ AND PROCESSING BY COUNTRY ------------------------------------------

# RWANDA -----------------------------------------------------------------------

# Data read --------------------------------------------------------------------

rwanda_main <- read.dta13(here("Data", "rwanda", "Main dataset.dta"),
                          convert.factors = F)

rwanda_hh <- read.dta13(here("Data", "rwanda", "Section A.dta"),
                        convert.factors = F)

rwanda_solar <- read.dta13(here("Data", "rwanda", "Section C.dta"),
                           convert.factors = F)

rwanda_stove <- read.dta13(here::here("Data", "rwanda", "Section I.dta"),
                           convert.factors = F)

rwanda_leisure <- read.dta13(here::here("Data", "rwanda", "Section P.dta"),
                             convert.factors = F)

# convert.factors = T here because variable levels do not match survey
rwanda_assets <- read.dta13(here::here("Data", "rwanda", "Section L.dta"),
                            convert.factors = T, nonint.factors = T)

# General household information ------------------------------------------------
r_gen <- rwanda_main %>% 
  transmute(HHID = HHID,
            rural = as.numeric(Locality == 0),
            admin1 = Province,
            admin2 = District,
            strata = strata,
            psu = cluster,
            weight = sample_weight)

# Household characteristics -----------------------------------------------------
r_hh <- rwanda_hh %>% 
  group_by(HHID) %>% 
  summarise(
    head_gender = max((A4 == 1)*A3, na.rm = T),
    head_educ = max((A4 == 1)*A9, na.rm = T),
    female_educ = max((A3 == 2)*A9, na.rm = T),
    male_educ = max((A3 == 1)*A9, na.rm = T),
    head_polyg = max((A4 == 1)*(A11 == 2), na.rm = T),
    head_employed = max((A4 == 1)*(A12 %in% 1:7), na.rm = T)
  ) %>% 
  mutate(head_gender = as.numeric(head_gender == 2)) %>% 
  mutate_all(~naconvert(.)) %>%  # Converts values were max(NA) returns -Inf back to NA
  mutate(hh_exp_annual = NA) # No expenditures data gathered in Rwanda

# House ownership, members per room and other home characteristics -------------
r_housing <- rwanda_main %>%
  transmute(HHID = HHID,
            own_house = ifelse(rwanda_main$B8 == 1, 1, ifelse(rwanda_main$B8 == 2, 0, NA)),
            number_rooms = B16,
            drinkingwater = B21,
            toilet = B20,
            floor = B19,
            roof = B18,
            wall = B17,
            land = ifelse(M1A == 1, 1, ifelse(M1A == 2, 0, NA))
            ) %>%
  mutate(across(where(is.numeric), naconvert)) %>%
  left_join(rwanda_hh %>%
              dplyr::count(HHID, name = "hh_mem")) %>% 
  mutate(HHID = HHID,
            hhmem_room = hh_mem/number_rooms
  ) %>%
  mutate(across(where(is.numeric), naconvert)) %>% 
  select(-c(number_rooms, hh_mem))

# Assets of households ---------------------------------------------------------
r_assets <- rwanda_assets %>%
  mutate(
    Item = case_when(
      Item %in% c("Vehicle (Car, pickup truck, etc)") ~ "vehicle",
      Item %in% c("Motorcycle") ~ "motorcycle",
      Item %in% c("Bicycle") ~ "bicycle",
      Item %in% c("Motor boat") ~ "motor_boat",
      Item %in% c("Boat") ~ "boat",
      Item %in% c("Animal drawn cart") ~ "cart",
      Item %in% c("Two-wheel tractor") ~ "tractor2",
      Item %in% c("Four-wheel tractor") ~ "tractor4",
      Item %in% c("Thresher") ~ "thresher",
      Item %in% c("Domestic water pump") ~ "water_pump",
      Item %in% c("Transplant rice seeding machine") ~ "rice_seeding_machine",
      Item %in% c("Rice contended machine") ~ "rice_machine",
      Item %in% c("Water pump for irrigation") ~ "irrigation_pump",
      Item %in% c("Miller") ~ "miller",
      Item %in% c("Hand plough (jembe)") ~ "hand_plough",
      Item %in% c("Electric power saw") ~ "electric_saw",
      Item %in% c("Electric power drill") ~ "electric_drill",
      Item %in% c("Welding machine") ~ "weld_machine",
      Item %in% c("Electric planer") ~ "electric_planer",
      Item %in% c("Electric motor") ~ "electric_motor",
      Item %in% c("Chain saw (gasoline)") ~ "chain_saw",
      Item %in% c("Ox") ~ "ox",
      Item %in% c("Cow/bull/calves") ~ "cows",
      Item %in% c("Water buffalo") ~ "buffalo",
      Item %in% c("Horse/donkey") ~ "hor_donk",
      Item %in% c("Sheep") ~ "sheep",
      Item %in% c("Goat") ~ "goat",
      Item %in% c("Pig") ~ "pig",
      Item %in% c("Poultry (Chicken, Duck, Turkey, Goose)") ~ "poultry",
      Item %in% c("Rabbit") ~ "rabbit",
      Item %in% c("Fish") ~ "fish"
    )
  ) %>%
  filter(!is.na(Item)) %>%
  group_by(HHID, Item) %>%
  summarise(own = sum(naconvert(La), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(id_cols = HHID, names_from = Item, values_from = own, values_fill = list(own = 0))

# Household expenditures -------------------------------------------------------
# Not collected in Rwanda

# Electricity access -----------------------------------------------------------
r_elec <- rwanda_main %>% 
  transmute(HHID = HHID,
            grid = as.numeric(C2 == 1),
            mg = as.numeric(C42 == 1),
            shs = as.numeric(C143 == 1),
            years_grid = C6,
            years_mg = C45,
            hours_grid = C26B,
            hours_mg = C68B,
            hours_shs = C172B) %>% 
  mutate_all(~naconvert(.))

shs <- rwanda_solar %>% 
  transmute(HHID = HHID, years_shs = C156) %>%
  group_by(HHID) %>%
  slice(1) %>%
  ungroup()

r_elec <- left_join(r_elec, shs) %>% 
  select(HHID, grid, mg, shs, matches("years"), matches("hours"))

# Clean cooking access ---------------------------------------------------------
r_stove <- rwanda_stove %>%
  transmute(HHID = HHID,
            stoveid = I2,
            primarystoveclean_stated = ifelse(I18A %in% c(6,14,15,16,17), "clean", "solids"))

r_stove <- rwanda_main %>% 
  select(HHID, stoveid = I35) %>% 
  left_join(r_stove, by = c("HHID", "stoveid")) %>% 
  select(-stoveid) %>% 
  mutate(primarystoveclean_stated = as.numeric(primarystoveclean_stated == "clean" &
                                                 !is.na(primarystoveclean_stated)))

r_stove2 <- rwanda_stove %>%
  transmute(HHID = HHID,
            stoveid = I2,
            metal_stove = ifelse(rwanda_stove$I9 == 1, 1, ifelse(rwanda_stove$I9 == 2, 0, NA)))

r_stove2 <- rwanda_main %>% 
  select(HHID, stoveid = I35) %>% 
  left_join(r_stove2, by = c("HHID", "stoveid")) %>% 
  select(-stoveid)

# Extract time use of solid and clean stoves
r_stove3 <- rwanda_stove %>% 
  mutate(across(I24:I26, ~naconvert(.))) %>% 
  mutate(primarystovetype_af = ifelse(I18A %in% c(6,14,15,16,17),"clean","solids")
  ) %>% 
  group_by(HHID, primarystovetype_af) %>% 
  summarise(across(c(I24:I26), ~sum(.,na.rm = T))) %>% 
  group_by(HHID, primarystovetype_af) %>% 
  mutate(stoveuseminutes = I24+I25+I26) %>% 
  pivot_wider(id_cols = HHID, names_from = primarystovetype_af,
              values_from = stoveuseminutes, names_prefix = "stoveuseminutes_") %>% 
  mutate(across(matches("stoveuseminutes"), ~natozero(.))
  ) %>% 
  rowwise() %>% 
  mutate(
    stoveuseminutes_clean = 
      ifelse((stoveuseminutes_solids + stoveuseminutes_clean) == 0, NA, stoveuseminutes_clean),
    stoveuseminutes_solids = 
      ifelse(is.na(stoveuseminutes_clean), NA, stoveuseminutes_solids),
  ) # Creates NAs in case both clean and solid fuel timeuse is 0

# Extract if electricity used for cooking
r_elcook <- rwanda_stove %>% 
  group_by(HHID) %>% 
  summarise(elcook = sum(I18A == 17)) %>% 
  mutate(elcook = natozero(elcook))

# Leisure time use -------------------------------------------------------------
r_leisure <- rwanda_leisure %>% 
  group_by(HHID) %>% 
  summarise(
    leisuremins_avgfem_u15 = mean(P12B, na.rm = T),
    leisuremins_avgfem_o15 = mean(P12A, na.rm = T),
    leisuremins_avgmale_u15 = mean(P12D, na.rm = T),
    leisuremins_avgmale_o15 = mean(P12C, na.rm = T),
    oohworkmins_avgfem_u15 = mean(P10B, na.rm = T),
    oohworkmins_avgfem_o15 = mean(P10A, na.rm = T),
    oohworkmins_avgmale_u15 = mean(P10D, na.rm = T),
    oohworkmins_avgmale_o15 = mean(P10C, na.rm = T),
  ) %>% 
  mutate_all(~naconvert(round(.)))  # Converts values were mean(NA) returns -Inf back to NA

# Gendered decision making -----------------------------------------------------
r_budget <- rwanda_main %>% 
  select(HHID, patri_budget = R54, patri_energybudget = R55)

# Women's mobility -------------------------------------------------------------
r_womenmob <- rwanda_main %>% 
  select(HHID, womemp_visitpeople = S1, womemp_vismarket = S2, womenp_leavevill = S3)

# Mean education of women over 15 years within household -----------------------

## Ulli, you cannot use naconvert(A9) here, as these are levels, not years of education. ##
## You need to decide how to deal with this, either imputing years, or removing this variable ##

r_mean_educ_women <- rwanda_hh %>%
  filter(A3 == 2, A5 >= 15) %>% 
  group_by(HHID) %>%
  summarise(
    mean_educ_women_over_15 = mean(naconvert(A9), na.rm = TRUE))

# Women over 15 employed in paid work ------------------------------------------
r_employ_women <- rwanda_hh %>%
  filter(A3 == 2, A5 >= 15) %>% 
  group_by(HHID) %>%
  summarise(
    #variables from A12 that are <= 7 count as employment
    mean_employ_women_over_15 = mean((naconvert(A12) <= 7), na.rm = TRUE)) 

# Women's access to group membership -------------------------------------------

## Ulli, please check this code again and make sure you are happy with the logic ##

r_group_mem <- rwanda_main %>%
  transmute(
    HHID = HHID,
    women_group_mem = case_when(
      S5_1 == 1 ~ 0,  # Non-members based on S5_1
      S5_11 == 555 ~ 1,  # Membership in other based on S5_11 == 555
      S5_2 == 2 | S5_3 == 3 | S5_4 == 4 | S5_5 == 5 | 
        S5_6 == 6 | S5_7 == 7 | S5_8 == 8 | S5_9 == 9 | S5_10 == 10 ~ 1,  # Check specific group membership
      TRUE ~ NA_real_  # Assign NA to all others that don't fit the conditions
    )
  )

# Women's access to bank account -----------------------------------------------

## Ulli, this is mostly NA, I would not trust it or use it ##

r_women_bank_account <- rwanda_main %>%
  transmute(
    HHID = HHID,
    women_bank_account = case_when(S9 == 1 ~ 0, # If S9 == 1, no account
                                   S9 == 2 ~ 1, # If S9 == 2, own account
                                   S9 == 3 ~ 1, # If S9 == 3, joint account, I argue this should not be coded differently.
                                   TRUE ~ NA_real_))

# Share adult women in relation to total household members ---------------------
r_share_women <- rwanda_hh %>%
  group_by(HHID) %>%
  summarise(
    women_over_15_count = sum(A3 == 2 & A5 >= 15, na.rm = TRUE), # Count of women over 15
    hh_mem = max(A1, na.rm = TRUE) # Use the maximum value of A1 to get the total count of household members
  ) %>%
  mutate(
    share_adult_women = ifelse(women_over_15_count == 0, 0, 
                               ifelse(is.na(women_over_15_count) | is.na(hh_mem), NA, 
                                      women_over_15_count / hh_mem))
  ) %>%
  mutate(across(where(is.numeric), naconvert)) %>% 
  select(-c(women_over_15_count, hh_mem))

# Subjective gender differences ------------------------------------------------

## Ulli, I suggest a straightforward binary coding here ##

r_emp_statements <- rwanda_main %>%
  transmute(
    HHID = HHID,
    men_women_use_energy_different = as.numeric(R53 %in% c(1,2)),
    men_usually_decide_about_family_budget = as.numeric(R54 %in% c(1,2)),
    men_usually_decide_about_purchase_of_energy_and_devices = as.numeric(R55 %in% c(1,2)),
  )

# Devices owned and desired ----------------------------------------------------
devices_desired <- rwanda_main %>% 
  rowwise() %>% 
  transmute(HHID = HHID,
            fan_desired = R6_1,
            radio_desired = R6_2,
            tv_desired = R6_3,
            fridge_desired = R6_4,
            pc_desired = sum(R6_6, R6_5, na.rm = T))%>% 
  mutate(across(fan_desired:pc_desired, ~as.numeric(natozero(.) > 0)))

devices_owned <- rwanda_assets %>%
  mutate(Item = case_when(
    Item %in% c("Fan") ~ "fan",
    Item %in% c("Radio Receiver", "Radio/CD Players/sound system") ~ "radio",
    Item %in% c("Flat color TV", "Regular Color TV", "Black & White TV") ~ "tv",
    Item %in% c("Refrigerator") ~ "fridge",
    Item %in% c("Electronic Tablet", "Computer") ~ "pc",
    Item %in% c("Electric hot water pot/kettle") ~ "kettle",
    Item %in% c("Electric Iron") ~ "iron",
    Item %in% c("VCD/DVD") ~ "vcd_dvd")) %>%
  filter(!is.na(Item)) %>%
  group_by(HHID, Item) %>%
  summarise(count = sum(naconvert(La), na.rm = TRUE),
            own = as.numeric(count > 0), .groups = 'drop') %>%
  pivot_wider(id_cols = HHID, names_from = Item,
    names_glue = "{Item}_{.value}", values_from = c(count, own),
    values_fill = list(count = 0, own = 0))

r_devices <- full_join(devices_owned, devices_desired, by = "HHID")

# Combine all processed datasets -----------------------------------------------

rwanda <- list(r_gen, r_hh, r_housing, r_assets, r_elec, r_stove, r_stove2, r_stove3, 
               r_elcook, r_devices, r_share_women, r_womenmob, r_women_bank_account,
               r_mean_educ_women, r_budget, r_employ_women, r_group_mem, r_emp_statements, 
               r_leisure) %>% 
  reduce(left_join, by = "HHID") %>% 
  mutate(across(c(HHID, admin1, admin2, strata), ~as.character(.)))

rwanda$country <- "rwanda"

# HONDURAS ---------------------------------------------------------------------

# Data read --------------------------------------------------------------------

honduras_main <- read.dta13(here("Data", "honduras", "MTF_HN17_HH_clean.dta"),
                            convert.factors = F)

honduras_solar <- read.dta13(here("Data", "honduras", "MTF_HN17_C_solar_clean.dta"),
                             convert.factors = F)

honduras_stove <- read.dta13(here("Data", "honduras", "MTF_HN17_I_ALL.dta"),
                             convert.factors = F)

honduras_fuel <- read.dta13(here("Data", "honduras", "MTF_HN17_H_clean.dta"),
                            convert.factors = F)

honduras_leisure <- read.dta13(here("Data", "honduras", "MTF_HN17_P.dta"),
                               convert.factors = F)

honduras_educ <- read.dta13(here("Data", "honduras", "MTF_HN17_02_B_K_PERSONA_0809.dta"),
                            convert.factors = F)

honduras_house <- read_dta(here("Data", "honduras", "MTF_HN17_01_HOGAR_0809.dta"))

honduras_land <- read_dta(here("Data", "honduras", "raw-data_honduras", "DATA", "MTF_HN17_N.dta"))

# General household information ------------------------------------------------
h_gen <- honduras_main %>% 
  transmute(HHID = idhh,
            rural = as.numeric(urban == 0),
            admin1 = Department,
            admin2 = Municipality,
            strata = strata,
            psu = psu_id,
            weight = weight)

# Household characteristics ----------------------------------------------------
hh <- honduras_main %>% 
  transmute(
    HHID = idhh,
    head_gender = as.numeric(sexhead == 0),
    head_educ = case_when(
      noprimhead == 1 ~ 0,
      primhead == 1 ~ 1,
      midhead == 1 ~ 2,
      sechead == 1 ~ 3,
      techead == 1 ~ 4,
      higherhead == 1 ~ 5,
      TRUE ~ NA_real_),
    head_polyg = NA_real_, #Unable to find this variable in the data
    head_employed = as.numeric(k003 %in% c(1,2,3,4,5))
  ) %>% 
  mutate_all(~naconvert(.))  # Converts values were max(NA) returns -Inf back to NA

educ <- honduras_educ %>% 
  transmute(
    HHID = idhh,
    gender = ifelse(b003 == 1, "male", "female"),
    positioninhh = b004,
    education = naconvert(b009)) %>% 
  group_by(HHID, gender) %>% 
  summarise(maxeduc = max(education, na.rm = T)) %>% 
  pivot_wider(id_cols = HHID, names_from = gender, values_from = maxeduc, names_prefix = "educ_") %>% 
  mutate_all(~ifelse(. < 0, NA, .)) %>% 
  rename(female_educ = educ_female, male_educ = educ_male)

h_hh <- left_join(hh, educ)

# House ownership, members per room and other home characteristics -------------
houseown_rooms <- honduras_main %>%
  transmute(HHID = idhh,
            own_house = ifelse(a007 == 1, 1, ifelse(a007 == 2, 0, NA)),
            number_rooms = a009) %>% 
  left_join(
    honduras_educ %>%
      group_by(idhh) %>%
      summarise(HHID = idhh,
                hh_mem = max(b001, na.rm = TRUE)) %>%  # Total count of household members) %>%
                ungroup() %>%
                  group_by(HHID) %>%
                  slice(1) %>%
                  ungroup()) %>%
  transmute(HHID = HHID,
            own_house = own_house,
            number_rooms = number_rooms,
            hh_mem = hh_mem,
            hhmem_room = ifelse(number_rooms == 0, NA_real_, hh_mem/number_rooms)
            ) %>%
  mutate(across(where(is.numeric), naconvert)) %>% 
  select(-c(number_rooms, hh_mem))

house <- honduras_house %>%
  transmute(HHID = idhh,
            drinkingwater = naconvert(as.numeric(a014)),
            toilet = case_when(
              a013__2 == 1 ~ 2,
              a013__3 == 1 ~ 3,
              a013__4 == 1 ~ 4,
              a013__5 == 1 ~ 5,
              a013__6 == 1 ~ 6,
              a013__7 == 1 ~ 7,
              a013__1 == 1 ~ 1,
              a013__8 == 1 ~ 8,
              a013__9 == 1 ~ 9,
              TRUE ~ NA_real_
            ),
            floor = naconvert(as.numeric(a012)),
            roof = naconvert(as.numeric(a011)),
            wall = naconvert(as.numeric(a010))
  )


land <- honduras_land %>%
  transmute(HHID = idhh,
            land = ifelse(n0011 == 1, 1, ifelse(n0011 == 2, 0, NA))
  )

h_housing <- list(houseown_rooms, house, land) %>% 
  reduce(full_join, by = "HHID")

# Assets of households ---------------------------------------------------------
h_assets <- honduras_main %>% 
  transmute(
    HHID = idhh,
    vehicle = replace_na(m001, 0),
    motorcycle = replace_na(m002, 0),
    bicycle = replace_na(m003, 0),
    motor_boat = replace_na(m004, 0),
    boat = replace_na(m005, 0),
    tractor = replace_na(m006, 0),
    water_pump = replace_na(m007, 0),
    cows = replace_na(m008, 0),
    buffalo = replace_na(m009, 0),
    hor_donk = replace_na(m010, 0),
    sheep = replace_na(m011, 0),
    goat = replace_na(m012, 0),
    pig = replace_na(m013, 0),
    rabbit = replace_na(m014, 0),
    fish = replace_na(m015, 0)
  )

# Household expenditures -------------------------------------------------------
h_exp <- honduras_main %>% 
  mutate(HHID = idhh) %>% 
  mutate(across(c(l001:l025), ~naconvert(.))
  ) %>% 
  select(HHID, l001:l025) %>% 
  summarise(
    HHID = HHID,
    hh_exp1 = ifelse(select(.,l001:l009) %>% rowSums(., na.rm = T) < l009a,
                     l009a,select(.,l001:l009) %>% rowSums(., na.rm = T)), 
    hh_exp2 = select(.,l010:l017) %>% rowSums(., na.rm = T), 
    hh_exp3 = select(.,l018:l025) %>% rowSums(., na.rm = T) # Annual G&S expenditure
  ) %>%
  mutate(hh_exp1 = hh_exp1*52, # Weekly consumption to annual
         hh_exp2 = hh_exp2*12 # Monthly G&S expenditure to annual
  ) %>% 
  transmute(HHID = HHID,
            hh_exp_annual = select(.,hh_exp1:hh_exp3) %>% rowSums(.,na.rm = T))

# Electricity access -----------------------------------------------------------
h_elec <- honduras_main %>% 
  transmute(HHID = idhh,
            grid = as.numeric(c003 == 1),
            mg = as.numeric(c047 == 1),
            shs = as.numeric(c132 == 1),
            years_grid = c007,
            years_mg = c047,
            hours_grid = c026a,
            hours_mg = c072a,
            hours_shs = c159a) %>% 
  mutate_all(~naconvert(.))

shs <- honduras_solar %>% 
  mutate(HHID = idhh) %>% 
  group_by(HHID) %>% 
  summarise(
    years_shs = max(c145, na.rm = T)
  ) %>% 
  mutate_all(~naconvert(round(.)))  # Converts values were max(NA) returns -Inf back to NA

h_elec <- left_join(h_elec, shs) %>% 
  select(HHID, grid, mg, shs, matches("years"), matches("hours"))

# Clean cooking access ---------------------------------------------------------
h_stove <- honduras_stove %>% 
  transmute(HHID = idhh,
            metal_stove = NA,
            primarystoveclean_stated = 
              as.numeric((select(.,i004__4:i004__6) %>% rowSums(.,na.rm = T))>0)
  ) %>% 
  full_join(hh %>% select(HHID))

# Extract time use of solid and clean stoves
h_stove2 <- honduras_stove %>% 
  mutate(HHID = idhh,
         across(i024:i026, ~naconvert(.))) %>% 
  mutate(primarystovetype_af = ifelse(select(.,i004__4:i004__6) %>% 
                                        rowSums(.,na.rm = T)>0, "clean", "solids")
  ) %>% 
  group_by(HHID, primarystovetype_af) %>% 
  summarise(across(c(i024:i026), ~sum(.,na.rm = T))) %>% 
  group_by(HHID, primarystovetype_af) %>% 
  mutate(stoveuseminutes = i024 + i025 + i026) %>% 
  pivot_wider(id_cols = HHID, names_from = primarystovetype_af,
              values_from = stoveuseminutes, names_prefix = "stoveuseminutes_") %>% 
  mutate(across(matches("stoveuseminutes"), ~natozero(.))
  ) %>% 
  filter(HHID %in% hh$HHID) %>% 
  full_join(hh %>% select(HHID)) %>%
  rowwise() %>% 
  mutate(
    stoveuseminutes_clean = 
      ifelse((stoveuseminutes_solids + stoveuseminutes_clean) == 0, NA, stoveuseminutes_clean),
    stoveuseminutes_solids = 
      ifelse(is.na(stoveuseminutes_clean), NA, stoveuseminutes_solids),
  ) # Creates NAs in case both clean and solid fuel timeuse is 0

# Extract if electricity used for cooking
h_elcook <- honduras_fuel %>% 
  mutate(HHID = idhh) %>% 
  filter(h002 == 4) %>% 
  group_by(HHID) %>% 
  summarise(elcook = sum(h005 == 1)) %>% 
  mutate(elcook = ifelse(elcook > 0 & !is.na(elcook), 1,0))  %>% 
  full_join(hh %>% select(HHID))

# Leisure time use -------------------------------------------------------------
h_leisure <- honduras_leisure %>% 
  mutate(HHID = idhh) %>% 
  group_by(HHID) %>% 
  summarise(
    leisuremins_avgfem_u15 = mean(p002a13, na.rm = T),
    leisuremins_avgfem_o15 = mean(p002b13, na.rm = T),
    leisuremins_avgmale_u15 = mean(p002d13, na.rm = T),
    leisuremins_avgmale_o15 = mean(p002c13, na.rm = T),
    oohworkmins_avgfem_u15 = mean(p002a11, na.rm = T),
    oohworkmins_avgfem_o15 = mean(p002b11, na.rm = T),
    oohworkmins_avgmale_u15 = mean(p002d11, na.rm = T),
    oohworkmins_avgmale_o15 = mean(p002c11, na.rm = T)
  ) %>% 
  mutate_all(~naconvert(round(.)))  # Converts values were mean(NA) returns -Inf back to NA

# Gendered decision making -----------------------------------------------------
h_budget <- honduras_main %>% # Budget information not collected in Honduras
  transmute(HHID = idhh, patri_budget = NA, patri_energybudget = NA)

# Women's mobility -------------------------------------------------------------
h_womenmob <- honduras_main %>% 
  select(HHID = idhh, womemp_visitpeople = s007a, womemp_vismarket = s008a, womenp_leavevill = s009a)

# Mean education of women over 15 years within household -----------------------

## Ulli, you cannot use naconvert(b009) here, as these are levels, not years of education. ##
## You need to decide how to deal with this, either imputing years, or removing this variable ##

h_mean_educ_women <- honduras_educ %>%
  filter(b003 == 2, b005 >= 15) %>% 
  mutate(HHID = idhh) %>%
  group_by(HHID) %>%
  summarise(
    mean_educ_women_over_15 = mean(naconvert(b009), na.rm = TRUE))

# Women over 15 employed in paid work ------------------------------------------
h_employ_women <- honduras_educ %>%
  filter(b003 == 2, b005 >= 15) %>% 
  mutate(HHID = idhh) %>%
  group_by(HHID) %>%
  summarise(
    #variables from k003 that are <= 5 or = 8 count as employment
    mean_employ_women_over_15 = mean(((naconvert(k003) <= 5 | naconvert(k003) == 8)), na.rm = TRUE))

# Women's access to group membership -------------------------------------------
h_group_mem <- honduras_main %>%
  transmute(HHID = idhh,
            women_group_mem = case_when(
              s003 == 1 ~ 1,  #yes
              s003 == 2 ~ 0  #no
              ))

# Women's access to bank account -----------------------------------------------
h_women_bank_account <- honduras_main %>%
  transmute(
    HHID = idhh,
    women_bank_account = case_when(
      # Join accounts counts as access (consistent across all countries)
      s006 == 1 ~ 0, 
      s006 == 2 ~ 1, 
      s006 == 3 ~ 1,
      s006 == 4 ~ 1,
      TRUE ~ NA_real_))

# Share adult women in relation to total household members ---------------------
h_share_women <- honduras_educ %>%
  group_by(idhh) %>%
  summarise(
    HHID = idhh,
    women_over_15_count = sum(b003 == 2 & b005 >= 15, na.rm = TRUE), # Count of women over 15
    hh_mem = max(b001, na.rm = TRUE) # Total count of household members
  ) %>%
  mutate(
    share_adult_women = ifelse(women_over_15_count == 0, 0, 
                               ifelse(is.na(women_over_15_count) | is.na(hh_mem), NA, 
                                      women_over_15_count / hh_mem))
  ) %>%
  mutate(across(where(is.numeric), naconvert)) %>%
  group_by(HHID) %>%
  slice(1) %>%
  ungroup() %>% 
  select(-c(women_over_15_count, idhh, hh_mem))

# Subjective gender differences ------------------------------------------------
h_emp_statements <- honduras_main %>%
  transmute(
    HHID = idhh,
    men_want_decide_type_of_household_electricity_distribution = as.numeric(r19 %in% c(1,2)),
    men_want_decide_over_payment_of_electricity = as.numeric(r20 %in% c(1,2)),
    men_decide_if_women_can_work_outside_home = as.numeric(s010a %in% c(3)))

# Devices owned and desired ----------------------------------------------------
devices_desired <- honduras_main %>% 
  rowwise() %>% 
  transmute(HHID = idhh,
            fan_desired = c164__2,
            radio_desired = c164__4,
            tv_desired = c164__1,
            fridge_desired = c164__3,
            pc_desired = c164__5
  )%>% 
  mutate(across(fan_desired:pc_desired, ~as.numeric(natozero(.) > 0)))

devices_owned <- honduras_main %>%
  transmute(
    HHID = idhh,
    fan = replace_na(m024a, 0),
    radio = replace_na(m022a, 0),
    tv = replace_na(m038a, 0) + replace_na(m039a, 0) + replace_na(m040a, 0),
    fridge = replace_na(m025, 0),
    pc = replace_na(m034, 0),
    microwave = replace_na(m026, 0),
    iron = replace_na(m027, 0),
    washing_machine = replace_na(m028, 0)
  ) %>%
  pivot_longer(cols = -HHID, names_to = "Item", values_to = "count") %>%
  mutate(own = as.numeric(count > 0)) %>%
  group_by(HHID, Item) %>%
  summarise(count = sum(count, na.rm = TRUE), # Ensure summation for each item type
            own = as.numeric(count > 0), .groups = 'drop') %>%
  pivot_wider(id_cols = HHID, names_from = Item, names_glue = "{Item}_{.value}",
              values_from = c(count, own), values_fill = list(count = 0, own = 0))

h_devices <- full_join(devices_owned, devices_desired, by = "HHID")

# Combine all processed datasets -----------------------------------------------

honduras <- list(h_gen, h_hh, h_housing, h_assets, h_exp, h_elec, h_stove, h_stove2, 
               h_elcook, h_devices, h_share_women, h_womenmob, h_women_bank_account,
               h_mean_educ_women, h_budget, h_employ_women, h_group_mem, h_emp_statements, 
               h_leisure) %>% 
  reduce(left_join, by = "HHID") %>% 
  mutate(across(c(HHID, admin1, admin2, strata), ~as.character(.)))

honduras$country <- "honduras"

# NEPAL ------------------------------------------------------------------------

# Data read --------------------------------------------------------------------

nepal_main <- read.dta13(here("Data", "nepal", "maindataset.dta"),
                         convert.factors = F)

nepal_weights <- read.dta13(here("Data", "nepal", "sample_weights.dta"),
                            convert.factors = F)

nepal_hh <- read.dta13(here("Data", "nepal", "FamilyMembers.dta"),
                       convert.factors = F)

nepal_exp <- read.dta13(here("Data", "nepal", "M_consumption.dta"),
                        convert.factors = F)

nepal_solar <- read.dta13(here("Data", "nepal", "C_SolarBasedDevice.dta"),
                          convert.factors = F)

nepal_stove <- read.dta13(here("Data", "nepal", "J_Cookstove.dta"),
                          convert.factors = F)

nepal_leisure <- read.dta13(here("Data", "nepal", "R_time_use.dta"),
                            convert.factors = F)

nepal_assets <- read.dta13(here("Data", "nepal", "NLIST2_Appliance.dta"),
                           convert.factors = F)

# General household information ------------------------------------------------
n_gen <- nepal_main %>% 
  transmute(HHID = HHID,
            admin1 = Province,
            admin2 = District,
            strata = paste0(admin1,"_", LOCALITY), # assumes stratification at province by rural/urban
            psu = EA,
            rural = as.numeric(LOCALITY == 2)) 

# Household characteristics ----------------------------------------------------
n_hh <- nepal_hh %>% 
  group_by(HHID) %>% 
  summarise(
    head_gender = max((A4 == 1)*A3, na.rm = T),
    metal_stove = NA,
    head_educ = max((A4 == 1)*A9, na.rm = T),
    female_educ = max((A3 == 2)*A9, na.rm = T),
    male_educ = max((A3 == 1)*A9, na.rm = T),
    head_polyg = NA, #Households are not polygamous in Nepal
    head_employed = max((A4 == 1)*(A14 %in% 1:7), na.rm = T) # A14 1:7 == employed
  ) %>% 
  mutate(head_gender = as.numeric(head_gender == 2)) %>% 
  mutate_all(~naconvert(.)) %>%  # Converts values were max(NA) returns -Inf back to NA
  left_join(nepal_weights %>% select(HHID, weight = hh_weight))

# House ownership, members per room and other home characteristics -------------
n_housing <- nepal_main %>%
  transmute(HHID = HHID,
            own_house = ifelse(B7 == 1, 1, ifelse(B7 == 2, 0, NA)),
            hhmem_room = HH_MEMBER_NUMBER / B9,
            drinkingwater = B14,
            toilet = case_when(
              B13__2 == 1 ~ 2,
              B13__3 == 1 ~ 3,
              B13__5 == 1 ~ 5,
              B13__6 == 1 ~ 6,
              B13__7 == 1 ~ 7,
              B13__4 == 1 ~ 4,
              B13__1 == 1 ~ 2,
              TRUE ~ NA_real_
            ),
            floor = B12,
            roof = B11,
            wall = B10,
            land = ifelse(O1 == 1, 1, ifelse(O1 == 0, 0, NA))
  ) %>%
  mutate(across(where(is.numeric), naconvert)) 

# Assets of households ---------------------------------------------------------
n_assets <- nepal_assets %>% 
  mutate(
    Id = case_when(
      Id %in% c(1) ~ "vehicle",
      Id %in% c(2) ~ "motorcycle",
      Id %in% c(3) ~ "bicycle",
      Id %in% c(5) ~ "boat",
      Id %in% c(6) ~ "tractor",
      Id %in% c(7) ~ "water_pump",
      Id %in% c(8) ~ "cows",
      Id %in% c(9) ~ "buffalo",
      Id %in% c(10) ~ "hor_donk",
      Id %in% c(11) ~ "sheep",
      Id %in% c(12) ~ "goat",
      Id %in% c(13) ~ "pig",
      Id %in% c(14) ~ "rabbit",
      Id %in% c(15) ~ "fish",
      Id %in% c(17) ~ "chicken",
      Id %in% c(18) ~ "duck"
    )
  ) %>%
  filter(!is.na(Id)) %>%
  group_by(HHID, Id) %>%
  summarise(own = sum(naconvert(N17_45_A), na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(id_cols = HHID, names_from = Id, values_from = own, values_fill = list(own = 0))

# Household expenditures -------------------------------------------------------
n_exp <- nepal_exp %>% 
  group_by(HHID) %>% 
  summarise(hh_exp1 = sum(M_A)*52) # Weekly consumption to annual

n_exp <- nepal_main %>% 
  mutate(across(c(M12:M33), ~naconvert(.))
  ) %>% 
  select(HHID, M12:M33) %>% 
  summarise(
    HHID = HHID,
    hh_exp2 = select(.,M12:M19) %>% rowSums(., na.rm = T), 
    hh_exp3 = select(.,M20:M33) %>% rowSums(., na.rm = T) # Annual G&S expenditure
  ) %>%
  mutate(hh_exp2 = hh_exp2*12 # Monthly G&S expenditure to annual
  ) %>% 
  left_join(n_exp) %>% 
  transmute(HHID = HHID,
            hh_exp_annual = select(.,hh_exp1:hh_exp3) %>% rowSums(.,na.rm = T))

# Electricity access -----------------------------------------------------------
n_elec <- nepal_main %>% 
  transmute(HHID = HHID,
            grid = as.numeric(C2 == 1),
            mg = as.numeric(C44 %in% c(1,2,4)),
            shs = as.numeric(C135__2 == 1 | C135__3 == 1),
            years_grid = C8,
            years_mg = C44,
            hours_grid = C29_B,
            hours_mg = C69_B,
            hours_shs = C163_B) %>% 
  mutate_all(~naconvert(.))

shs <- nepal_solar %>% 
  group_by(HHID) %>% 
  summarise(
    years_shs = max(C150, na.rm = T)
  ) %>% 
  mutate(years_shs = naconvert(round(years_shs)))  # Converts values were max(NA) returns -Inf back to NA

n_elec <- left_join(n_elec, shs) %>% 
  select(HHID, grid, mg, shs, matches("years"), matches("hours"))

# Clean cooking access ---------------------------------------------------------
n_stove <- nepal_stove %>% 
  filter(J34 == 1) %>% 
  transmute(
    HHID = HHID,
    primarystoveclean_stated = as.numeric(J20_A %in% c(1,12,14))
  ) %>% 
  group_by(HHID) %>% 
  summarise(primarystoveclean_stated = ifelse(
    sum(primarystoveclean_stated) > 0,1,0
  ))

# Extract clean and solid stove timeuse
n_stove2 <- nepal_stove %>% 
  mutate(across(J25:J27, ~naconvert(.))) %>% 
  mutate(primarystovetype_af = ifelse(J20_A %in% c(1,12,14),"clean","solids")
  ) %>% 
  group_by(HHID, primarystovetype_af) %>% 
  summarise(across(c(J25:J27), ~sum(.,na.rm = T))) %>% 
  group_by(HHID, primarystovetype_af) %>% 
  mutate(stoveuseminutes = J25 + J26 + J27) %>% 
  pivot_wider(id_cols = HHID, names_from = primarystovetype_af,
              values_from = stoveuseminutes, names_prefix = "stoveuseminutes_") %>% 
  mutate(across(matches("stoveuseminutes"), ~natozero(.))
  ) %>% 
  rowwise() %>% 
  mutate(
    stoveuseminutes_clean = 
      ifelse((stoveuseminutes_solids + stoveuseminutes_clean) == 0, NA, stoveuseminutes_clean),
    stoveuseminutes_solids = 
      ifelse(is.na(stoveuseminutes_clean), NA, stoveuseminutes_solids),
  ) # Creates NAs in case both clean and solid fuel timeuse is 0

# Extract if electricity used for cooking
n_elcook <- nepal_stove %>% 
  group_by(HHID) %>% 
  summarise(elcook = sum(J20_A == 12, na.rm = T)) %>% 
  mutate(elcook = natozero(elcook))

# Leisure time use -------------------------------------------------------------
n_leisure <- nepal_leisure %>% 
  group_by(HHID) %>% 
  summarise(
    leisuremins_avgfem_u15 = mean((Id == 2)*R28_A, na.rm = T),
    leisuremins_avgfem_o15 = mean((Id == 1)*R28_A, na.rm = T),
    leisuremins_avgmale_u15 = mean((Id == 4)*R28_A, na.rm = T),
    leisuremins_avgmale_o15 = mean((Id == 3)*R28_A, na.rm = T),
    oohworkmins_avgfem_u15 = mean((Id == 2)*R24_A, na.rm = T),
    oohworkmins_avgfem_o15 = mean((Id == 1)*R24_A, na.rm = T),
    oohworkmins_avgmale_u15 = mean((Id == 4)*R24_A, na.rm = T),
    oohworkmins_avgmale_o15 = mean((Id == 3)*R24_A, na.rm = T)
  ) %>% 
  mutate(across(c(leisuremins_avgfem_u15:oohworkmins_avgmale_o15),
                ~naconvert(round(.))))  # Converts values were mean(NA) returns -Inf back to NA

# Budget and energy budget -----------------------------------------------------

n_budget <- nepal_main %>% # Not collected in Nepal
  transmute(HHID = HHID, patri_budget = NA, patri_energybudget = NA)

# Women's mobility -------------------------------------------------------------
n_womenmob <- nepal_main %>% 
  select(HHID, womemp_visitpeople = U1, womemp_vismarket = U2, womenp_leavevill = U3)

# Mean education of women over 15 years within household -----------------------

n_mean_educ_women <- nepal_hh %>%
  filter(A3 == 2, A5 >= 15) %>% 
  group_by(HHID) %>%
  summarise(
    mean_educ_women_over_15 = mean(naconvert(A9), na.rm = TRUE)
  )

# Women over 15 employed in paid work ------------------------------------------
n_employ_women <- nepal_hh %>%
  filter(A3 == 2, A5 >= 15) %>% 
  group_by(HHID) %>%
  summarise(
    #variables from A14 that are <= 7 count as employment
    mean_employ_women_over_15 = mean(naconvert(A14) <= 7, na.rm = TRUE) 
  )

# Women's access to group membership -------------------------------------------
n_group_mem <- nepal_main %>%
  transmute(
    HHID = HHID,
    women_group_mem = case_when(
      U4__1 == 1 ~ 0,  
      U4__555 == 1 ~ 1, 
      U4__2 == 1 | U4__3 == 1 | U4__4 == 1 | U4__5 == 1 | 
        U4__6 == 1 | U4__7 == 1 | U4__8 == 1 | U4__9 == 1 ~ 1,  # Check specific group membership
      TRUE ~ NA_real_  # Default case for all others
    )
  )

# Women's access to bank account -----------------------------------------------
n_women_bank_account <- nepal_main %>%
  transmute(
    HHID = HHID,
    women_bank_account = case_when(
      U8 == 1 ~ 0,
      U8 == 2 ~ 1, 
      U8 == 3 ~ 1,
      U8 == 4 ~ 1,
      TRUE ~ NA_real_))

# Share adult women in relation to total household members ---------------------
n_share_women <- nepal_hh %>%
  group_by(HHID) %>%
  summarise(
    women_over_15_count = sum(A3 == 2 & A5 >= 15, na.rm = TRUE), # Count of women over 15
    hh_mem = max(A1, na.rm = TRUE) # Total count of household members
  ) %>%
  mutate(
    share_adult_women = ifelse(women_over_15_count == 0, 0, 
                               ifelse(is.na(women_over_15_count) | is.na(hh_mem), NA, 
                                      women_over_15_count / hh_mem))
  ) %>%
  mutate(across(where(is.numeric), naconvert)) %>% 
  select(-c(women_over_15_count, hh_mem))

# Subjective gender differences ------------------------------------------------
# Not collected in Nepal

# Devices owned and desired ----------------------------------------------------
devices_desired <- nepal_main %>% 
  rowwise() %>% 
  transmute(HHID = HHID,
            fan_desired = C168__2,
            radio_desired = C168__4,
            tv_desired = C168__1,
            fridge_desired = C168__3,
            pc_desired = C168__5
  ) %>% 
  mutate(across(fan_desired:pc_desired, ~as.numeric(natozero(.) > 0)))

devices_owned <- nepal_assets %>%
  mutate(
    Id = case_when(
      Id %in% c(7) ~ "fan",
      Id %in% c(5) ~ "radio",
      Id %in% c(25, 26, 27) ~ "tv",
      Id %in% c(8) ~ "fridge",
      Id %in% c(21) ~ "pc",
      Id %in% c(10) ~ "blender",
      Id %in% c(22) ~ "kettle",
      Id %in% c(13) ~ "iron",
      Id %in% c(11) ~ "rice_cooker",
      Id %in% c(19) ~ "water_heater",
      Id %in% c(28) ~ "water_pump",
      Id %in% c(20) ~ "solar_water_heater"
    )
  ) %>%
  filter(!is.na(Id)) %>%
  group_by(HHID, Id) %>%
  summarise(own = as.numeric(sum(naconvert(N17_45_A), na.rm = T) > 0),
            count = sum(naconvert(N17_45_A), na.rm = T)) %>% 
  pivot_wider(id_cols = HHID, names_from = Id, names_glue = "{Id}_{.value}",
              values_from = c(count, own), values_fill = list(count = 0, own = 0))

n_devices <- full_join(devices_owned, devices_desired, by = "HHID")  %>% 
  # make implicit lack of ownership explicit
  mutate(across(matches("own"), ~natozero(.)))

# Combine all processed datasets -----------------------------------------------

nepal <- list(n_gen, n_hh, n_housing, n_assets, n_exp, n_elec, n_stove, n_stove2, 
                 n_elcook, n_devices, n_share_women, n_womenmob, n_women_bank_account,
                 n_mean_educ_women, n_budget, n_employ_women, n_group_mem, 
                 n_leisure) %>% 
  reduce(left_join, by = "HHID") %>% 
  mutate(across(c(HHID, admin1, admin2, strata), ~as.character(.)))

nepal$country <- "nepal"

# COMBINE ALL COUNTRY DATASETS -------------------------------------------------

varcheck <- list(tibble("country" = "rwanda", "variables" = names(rwanda)),
     tibble("country" = "honduras", "variables" = names(honduras)), 
     tibble("country" = "nepal", "variables" = names(nepal))) %>% 
  reduce(full_join, by = "variables") %>% 
  select(variables, everything())

write_csv(varcheck, here("Data", "processed", "varcheck_allcountry.csv"))

allcountry <- list(rwanda, honduras, nepal) %>% 
  reduce(bind_rows) %>% 
  select(country, everything())

write_csv(allcountry, here("Data", "processed", "allcountry.csv"))
