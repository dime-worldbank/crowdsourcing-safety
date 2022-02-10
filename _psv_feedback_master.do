* PSV Rider Feedback Master

* Parameters -------------------------------------------------------------------
global RUN_CODE 1 // 1 = Yes; 0 = No

* Filepaths --------------------------------------------------------------------
di "The user, in this case, is: " c(username)

	if ("`c(username)'" == "robmarty") {
		global onedrive_dir "" // OneDrive not on personal computer
		global dropbox_dir "/Users/robmarty/Dropbox/PSV Rider Feedback"
		global github_dir "/Users/robmarty/Documents/Github/PSV Rider Feedback"
	}	
	else if ( "`c(username)'" == "WB521633" ) { // Rob
		global onedrive_dir "C:/Users/wb521633/OneDrive - WBG/PSV Rider Feedback"
		global github_dir "C:/Users/wb521633/Documents/Github/PSV-Rider-Feedback"
	}
	else if ( "`c(username)'" == "WB575963" ) { // Ruiwen 
		global onedrive_dir "C:/Users/wb575963/WBG/Robert Andrew Marty - PSV Rider Feedback"
		global github_dir "C:/Users/wb575963/Documents/Github/PSV-Rider-Feedback/"
	}
	
* Code -------------------------------------------------------------------------
if ($RUN_CODE == 1){

	* Matatu Installation Survey -----------------------------------------------
	do "$github_dir/DataWork/Matatu Sensor Installation Survey/01_import_psv_sensor_installation.do"	
	
}





