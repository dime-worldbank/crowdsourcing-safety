# Tom Harris
# Practice script

#### Before any new scripts, open and run the master script! Sets all root directories


df <- readRDS(
  file.path(sensors_dir, "FinalData", "sensor_day.Rds"))


df_2 <- readRDS(
  file.path(rider_feedback_dir, "FinalData", "rider_feedback.Rds"))

df_2_trans <- readRDS(
  file.path(rider_feedback_dir, "FinalData", "rider_feedback_translated.Rds"))


## common id is probs regno_clean

## first step, aggregate everything to the vehicle level
#how safe does sensor data say vehicle, how safe does feedback data say it is.... do these corr? 
# how do we define 'safe' . e,g, avg speed, make otehr indicators .... proportion of time spent over 80 kmh etc. 
# eventually some kidn of index of dangerous driving .... 

