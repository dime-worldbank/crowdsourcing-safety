* smarTTrans Master

di "The user, in this case, is: " c(username)

	if ("`c(username)'" == "robmarty") {
		global dropbox_dir "/Users/robmarty/Dropbox/World Bank/IEs/PSV Rider Feedback"
		global github_dir "/Users/robmarty/Documents/Github/PSV Rider Feedback"
	}	
	else if ( "`c(username)'" == "franceschen" ) {
		global dropbox_dir "/Users/franceschen/Dropbox/PSV Rider Feedback"
		global github_dir "/Users/franceschen/Documents/GitHub/PSV Rider Feedback"
	}
	
	
	
