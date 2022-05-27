# Testing out Twilio

twilio_keys_df <- read_csv(file.path(driver_message_dir, "twilio_keys.csv"))

Sys.setenv(TWILIO_SID   = twilio_keys_df$key[twilio_keys_df$name %in% "TWILIO_SID"])
Sys.setenv(TWILIO_TOKEN = twilio_keys_df$key[twilio_keys_df$name %in% "TWILIO_TOKEN"])

tw_send_message(to = "15133793170", 
                from = "19706934726", 
                "Testing out")


