* PSV Rider Feedback Master

* Packages to install
* ssc install randtreat

* Parameters -------------------------------------------------------------------
global RUN_CODE 0 // 1 = Yes; 0 = No

* Filepaths --------------------------------------------------------------------
di "The user, in this case, is: " c(username)

	if ("`c(username)'" == "robmarty") {
		global onedrive_dir "" // OneDrive not on personal computer
		global dropbox_dir "/Users/robmarty/Dropbox/World Bank/IEs/PSV Rider Feedback"
		global github_dir "/Users/robmarty/Documents/Github/PSV-Rider-Feedback"
	}	
	else if ( "`c(username)'" == "WB521633" ) { // Rob
		global onedrive_dir "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"
		global dropbox_dir "C:/Users/wb521633/Dropbox/World Bank/IEs/PSV Rider Feedback"
		global github_dir "C:/Users/wb521633/Documents/Github/PSV-Rider-Feedback"
	}
	else if ( "`c(username)'" == "WB575963" ) { // Ruiwen 
		global onedrive_dir "C:/Users/wb575963/WBG/Robert Andrew Marty - PSV Rider Feedback"
		global github_dir "C:/Users/wb575963/Documents/Github/PSV-Rider-Feedback/"
	}

global db_data_dir        "$dropbox_dir/Data" 
global db_sensors_dir     "$db_data_dir/Sensor Data"
global db_matatu_data_dir "$db_data_dir/Matatu Data"

* Code -------------------------------------------------------------------------
if ($RUN_CODE == 1){

	* Matatu Installation Survey -----------------------------------------------
	do "$github_dir/DataWork/Matatu Sensor Installation Survey/01_import_psv_sensor_installation.do"	
	
}





