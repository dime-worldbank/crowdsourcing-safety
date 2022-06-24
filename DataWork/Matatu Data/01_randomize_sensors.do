* PSV Rider Feedback Master

*** Load data
import delimited "$db_matatu_data_dir/RawData/vehicle_info.csv", clear
sort id
	
*** Subset
* 1. Only keep original 150 (some may be replaced if vehicle goes out of service)
* 2. Drop one route (Nairobi to Marsabit)

keep if route != "Nairobi to Marsabit"
keep if original_150 == 1
keep if phase == "june2022-sensor-driverfeedback"

*** Encode variables
* randtreat doesn't allow strings as inputs
encode sacco, gen(sacco_enc)	
	
*** Randomize
randtreat, strata(sacco_enc) multiple(4) misfits(global) generate(drvr_feedback_treat_id) setseed(42)

* Check balance
tab sacco drvr_feedback_treat_id 

*** Name treatments
gen drvr_feedback_treat = ""
replace drvr_feedback_treat = "sticker-feedback"       if drvr_feedback_treat_id == 0
replace drvr_feedback_treat = "sticker-no feedback"    if drvr_feedback_treat_id == 1
replace drvr_feedback_treat = "no sticker-feedback"    if drvr_feedback_treat_id == 2
replace drvr_feedback_treat = "no sticker-no feedback" if drvr_feedback_treat_id == 3

gen drvr_feedback_treat_sticker = inlist(drvr_feedback_treat_id, 0, 1)
gen drvr_feedback_treat_feedback = inlist(drvr_feedback_treat_id, 0, 2)

*** Keep select variables
keep reg_no drvr_feedback_treat_id drvr_feedback_treat drvr_feedback_treat_sticker drvr_feedback_treat_feedback

*** Export
export delimited using "$db_matatu_data_dir/FinalData/individual_files/sensors_driverfeedback_randomization.csv", replace

