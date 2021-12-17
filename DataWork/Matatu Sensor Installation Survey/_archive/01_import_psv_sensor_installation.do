* import_psv_sensor_installation.do
*
* 	Imports and aggregates "psv_sensor_installation" (ID: psv_sensor_installation) data.
*
*	Inputs:  "~/Dropbox/World Bank/IEs/PSV Rider Feedback/Data/Matatu Sensor Installation Survey/RawData/psv_sensor_installation_WIDE.csv/psv_sensor_installation_WIDE.csv"
*	Outputs: "psv_sensor_installation.dta"
*
*	Output by SurveyCTO June 17, 2021 12:33 PM.

* initialize Stata
clear all
set more off
set mem 100m

* initialize workflow-specific parameters
*	Set overwrite_old_data to 1 if you use the review and correction
*	workflow and allow un-approving of submissions. If you do this,
*	incoming data will overwrite old data, so you won't want to make
*	changes to data in your local .dta file (such changes can be
*	overwritten with each new import).
local overwrite_old_data 0

* initialize form-specific parameters
* INPUTS
local csvfile "$onedrive_dir/Data/Matatu Sensor Installation Survey/RawData/psv_sensor_installation_WIDE.csv"
local corrfile "$onedrive_dir/Data/Matatu Sensor Installation Survey/RawData/psv_sensor_installation_WIDE.csv"

* OUTPUTS
local dtafile "$onedrive_dir/Data/Matatu Sensor Installation Survey/FinalData/psv_sensor_installation.dta"
local csvfile_out "$onedrive_dir/Data/Matatu Sensor Installation Survey/FinalData/psv_sensor_installation.csv"

local note_fields1 ""
local text_fields1 "deviceid subscriberid simid devicephonenum matatu_regno gps_serial_no matatu_sacco sacco_name_other matatu_other_route driver_name driver_phone_no owner_name owner_phone_no driver_tenure"
local text_fields2 "driver_route_tenure n_drivers_per_veh_q matatu_seats matatu_amenities matatu_other instanceid"
local date_fields1 ""
local datetime_fields1 "submissiondate starttime endtime"

disp
disp "Starting import of: `csvfile'"
disp

* import data from primary .csv file
insheet using "`csvfile'", names clear

* drop extra table-list columns
cap drop reserved_name_for_field_*
cap drop generated_table_list_lab*

xx

* continue only if there's at least one row of data to import
if _N>0 {
	* drop note fields (since they don't contain any real data)
	forvalues i = 1/100 {
		if "`note_fields`i''" ~= "" {
			drop `note_fields`i''
		}
	}
	
	* format date and date/time fields
	forvalues i = 1/100 {
		if "`datetime_fields`i''" ~= "" {
			foreach dtvarlist in `datetime_fields`i'' {
				cap unab dtvarlist : `dtvarlist'
				if _rc==0 {
					foreach dtvar in `dtvarlist' {
						tempvar tempdtvar
						rename `dtvar' `tempdtvar'
						gen double `dtvar'=.
						cap replace `dtvar'=clock(`tempdtvar',"MDYhms",2025)
						* automatically try without seconds, just in case
						cap replace `dtvar'=clock(`tempdtvar',"MDYhm",2025) if `dtvar'==. & `tempdtvar'~=""
						format %tc `dtvar'
						drop `tempdtvar'
					}
				}
			}
		}
		if "`date_fields`i''" ~= "" {
			foreach dtvarlist in `date_fields`i'' {
				cap unab dtvarlist : `dtvarlist'
				if _rc==0 {
					foreach dtvar in `dtvarlist' {
						tempvar tempdtvar
						rename `dtvar' `tempdtvar'
						gen double `dtvar'=.
						cap replace `dtvar'=date(`tempdtvar',"MDY",2025)
						format %td `dtvar'
						drop `tempdtvar'
					}
				}
			}
		}
	}

	* ensure that text fields are always imported as strings (with "" for missing values)
	* (note that we treat "calculate" fields as text; you can destring later if you wish)
	tempvar ismissingvar
	quietly: gen `ismissingvar'=.
	forvalues i = 1/100 {
		if "`text_fields`i''" ~= "" {
			foreach svarlist in `text_fields`i'' {
				cap unab svarlist : `svarlist'
				if _rc==0 {
					foreach stringvar in `svarlist' {
						quietly: replace `ismissingvar'=.
						quietly: cap replace `ismissingvar'=1 if `stringvar'==.
						cap tostring `stringvar', format(%100.0g) replace
						cap replace `stringvar'="" if `ismissingvar'==1
					}
				}
			}
		}
	}
	quietly: drop `ismissingvar'


	* consolidate unique ID into "key" variable
	replace key=instanceid if key==""
	drop instanceid


	* label variables
	label variable key "Unique submission ID"
	cap label variable submissiondate "Date/time submitted"
	cap label variable formdef_version "Form version used on device"
	cap label variable review_status "Review status"
	cap label variable review_comments "Comments made during review"
	cap label variable review_corrections "Corrections made during review"


	label variable matatu_regno "What is the reg number of the matatu?"
	note matatu_regno: "What is the reg number of the matatu?"

	label variable matatu_exp_freq "Does the matatu make frequent stops or is it an express vehicle?"
	note matatu_exp_freq: "Does the matatu make frequent stops or is it an express vehicle?"
	label define matatu_exp_freq 1 "Makes frequent stops" 2 "Express vehicle"
	label values matatu_exp_freq matatu_exp_freq

	label variable gps_serial_no "What is the serial number on the GPS device?"
	note gps_serial_no: "What is the serial number on the GPS device?"

	label variable matatu_sacco "What SACCO does \${matatu_regno} belong to?"
	note matatu_sacco: "What SACCO does \${matatu_regno} belong to?"

	label variable sacco_name_other "What is the name of the SACCO?"
	note sacco_name_other: "What is the name of the SACCO?"

	label variable matatu_other_route "What route does this SACCO/matatu travel on?"
	note matatu_other_route: "What route does this SACCO/matatu travel on?"

	label variable continue_q "Confirm the above notes and select continue."
	note continue_q: "Confirm the above notes and select continue."
	label define continue_q 1 "Continue"
	label values continue_q continue_q

	label variable driver_name "What is the driver's name?"
	note driver_name: "What is the driver's name?"

	label variable driver_phone_no "What is the driver's phone number?"
	note driver_phone_no: "What is the driver's phone number?"

	label variable driver_is_matatu_owner "Is the driver the matatu owner?"
	note driver_is_matatu_owner: "Is the driver the matatu owner?"
	label define driver_is_matatu_owner 1 "Yes" 0 "No"
	label values driver_is_matatu_owner driver_is_matatu_owner

	label variable owner_name "What is the matatu owner's name?"
	note owner_name: "What is the matatu owner's name?"

	label variable owner_phone_no "What is the matatu owner's phone number?"
	note owner_phone_no: "What is the matatu owner's phone number?"

	label variable driver_age "What is the driver's age?"
	note driver_age: "What is the driver's age?"

	label variable driver_tenure "How many years has the driver been driving matatus?"
	note driver_tenure: "How many years has the driver been driving matatus?"

	label variable driver_route_tenure "How long has the driver been driving this specific matatu route?"
	note driver_route_tenure: "How long has the driver been driving this specific matatu route?"

	label variable driver_contract "Does the driver earn a fixed wage or have a target contract?"
	note driver_contract: "Does the driver earn a fixed wage or have a target contract?"
	label define driver_contract 1 "Fixed Wage" 2 "Target"
	label values driver_contract driver_contract

	label variable n_drivers_per_veh_q "How many drivers drive this matatu?"
	note n_drivers_per_veh_q: "How many drivers drive this matatu?"

	label variable matatu_seats "How many seats are on the matatu (feel free to approximate)"
	note matatu_seats: "How many seats are on the matatu (feel free to approximate)"

	label variable matatu_quality_q "Overall, how does the matatu seem?"
	note matatu_quality_q: "Overall, how does the matatu seem?"
	label define matatu_quality_q 1 "Good as new - (Few to no dents or scratches. Very little wear and tear)" 2 "Somewhat worn down - (Some minor dents or scratches, a bit of wear and tea)." 3 "Very run down - (Notalbe dents or scratches, significant wear and tear)."
	label values matatu_quality_q matatu_quality_q

	label variable matatu_paint_q "What is the paint job on the outside of the matatu?"
	note matatu_paint_q: "What is the paint job on the outside of the matatu?"
	label define matatu_paint_q 1 "The matatu has a unique design for its paint job (eg, intricate and unique desig" 2 "The matatu has a standard / common paint job"
	label values matatu_paint_q matatu_paint_q

	label variable matatu_wifi "Does the matatu provide wifi?"
	note matatu_wifi: "Does the matatu provide wifi?"
	label define matatu_wifi 1 "Yes" 0 "No" -1 "Don't Know"
	label values matatu_wifi matatu_wifi

	label variable matatu_amenities "Describe any amenities that the matatu provides? (eg, loud music, very nice seat"
	note matatu_amenities: "Describe any amenities that the matatu provides? (eg, loud music, very nice seats, etc)."

	label variable matatu_other "Provide any other comments about the matatu (optional)"
	note matatu_other: "Provide any other comments about the matatu (optional)"






	* append old, previously-imported data (if any)
	cap confirm file "`dtafile'"
	if _rc == 0 {
		* mark all new data before merging with old data
		gen new_data_row=1
		
		* pull in old data
		append using "`dtafile'"
		
		* drop duplicates in favor of old, previously-imported data if overwrite_old_data is 0
		* (alternatively drop in favor of new data if overwrite_old_data is 1)
		sort key
		by key: gen num_for_key = _N
		drop if num_for_key > 1 & ((`overwrite_old_data' == 0 & new_data_row == 1) | (`overwrite_old_data' == 1 & new_data_row ~= 1))
		drop num_for_key

		* drop new-data flag
		drop new_data_row
	}
	
	* save data to Stata format
	save "`dtafile'", replace
	export delimited using "`csvfile_out'", replace


	* show codebook and notes
	codebook
	notes list
}

disp
disp "Finished import of: `csvfile'"
disp

* OPTIONAL: LOCALLY-APPLIED STATA CORRECTIONS
*
* Rather than using SurveyCTO's review and correction workflow, the code below can apply a list of corrections
* listed in a local .csv file. Feel free to use, ignore, or delete this code.
*
*   Corrections file path and filename:  ~/Dropbox/World Bank/IEs/PSV Rider Feedback/Data/Matatu Sensor Installation Survey/RawData/psv_sensor_installation_WIDE.csv/psv_sensor_installation_corrections.csv
*
*   Corrections file columns (in order): key, fieldname, value, notes

capture confirm file "`corrfile'"
if _rc==0 {
	disp
	disp "Starting application of corrections in: `corrfile'"
	disp

	* save primary data in memory
	preserve

	* load corrections
	insheet using "`corrfile'", names clear
	
	if _N>0 {
		* number all rows (with +1 offset so that it matches row numbers in Excel)
		gen rownum=_n+1
		
		* drop notes field (for information only)
		drop notes
		
		* make sure that all values are in string format to start
		gen origvalue=value
		tostring value, format(%100.0g) replace
		cap replace value="" if origvalue==.
		drop origvalue
		replace value=trim(value)
		
		* correct field names to match Stata field names (lowercase, drop -'s and .'s)
		replace fieldname=lower(subinstr(subinstr(fieldname,"-","",.),".","",.))
		
		* format date and date/time fields (taking account of possible wildcards for repeat groups)
		forvalues i = 1/100 {
			if "`datetime_fields`i''" ~= "" {
				foreach dtvar in `datetime_fields`i'' {
					* skip fields that aren't yet in the data
					cap unab dtvarignore : `dtvar'
					if _rc==0 {
						gen origvalue=value
						replace value=string(clock(value,"MDYhms",2025),"%25.0g") if strmatch(fieldname,"`dtvar'")
						* allow for cases where seconds haven't been specified
						replace value=string(clock(origvalue,"MDYhm",2025),"%25.0g") if strmatch(fieldname,"`dtvar'") & value=="." & origvalue~="."
						drop origvalue
					}
				}
			}
			if "`date_fields`i''" ~= "" {
				foreach dtvar in `date_fields`i'' {
					* skip fields that aren't yet in the data
					cap unab dtvarignore : `dtvar'
					if _rc==0 {
						replace value=string(clock(value,"MDY",2025),"%25.0g") if strmatch(fieldname,"`dtvar'")
					}
				}
			}
		}

		* write out a temp file with the commands necessary to apply each correction
		tempfile tempdo
		file open dofile using "`tempdo'", write replace
		local N = _N
		forvalues i = 1/`N' {
			local fieldnameval=fieldname[`i']
			local valueval=value[`i']
			local keyval=key[`i']
			local rownumval=rownum[`i']
			file write dofile `"cap replace `fieldnameval'="`valueval'" if key=="`keyval'""' _n
			file write dofile `"if _rc ~= 0 {"' _n
			if "`valueval'" == "" {
				file write dofile _tab `"cap replace `fieldnameval'=. if key=="`keyval'""' _n
			}
			else {
				file write dofile _tab `"cap replace `fieldnameval'=`valueval' if key=="`keyval'""' _n
			}
			file write dofile _tab `"if _rc ~= 0 {"' _n
			file write dofile _tab _tab `"disp"' _n
			file write dofile _tab _tab `"disp "CAN'T APPLY CORRECTION IN ROW #`rownumval'""' _n
			file write dofile _tab _tab `"disp"' _n
			file write dofile _tab `"}"' _n
			file write dofile `"}"' _n
		}
		file close dofile
	
		* restore primary data
		restore
		
		* execute the .do file to actually apply all corrections
		do "`tempdo'"

		* re-save data
		save "`dtafile'", replace
		export delimited using "`csvfile_out'", replace
	}
	else {
		* restore primary data		
		restore
	}

	disp
	disp "Finished applying corrections in: `corrfile'"
	disp
}
