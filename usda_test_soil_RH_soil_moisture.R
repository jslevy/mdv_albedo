#Plotting up some USDA data....

setwd("~/Dropbox/r_projects/mdv_albedo")

#import from raw csvs

bull_data <- read.csv("raw_data/bull_usda_2017.csv")


#clip out jan  data

jan_bull = bull_data[bull_data$DoyOfYear < 32,]


#recalculate ground T-->RH. Now using Magnus eqns. If I really wanted to be clever, I could test to see if the calculated soil RH matches the measured RH....

#site_rh_radn_air_soil_jan_midnight_to_2am$satvappresair <- ifelse(site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M > 0, 610.94*exp((17.625*site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M)/(243.04+site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M)), 611.21*exp((22.587*site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M)/(273.86+site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M))) 
#site_rh_radn_air_soil_jan_midnight_to_2am$satvapsoil <- ifelse(site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM > 0, 610.94*exp((17.625*site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)/(243.04+site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)), 611.21*exp((22.587*site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)/(273.86+site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)))

#site_rh_radn_air_soil_jan_midnight_to_2am$pvapair <- (site_rh_radn_air_soil_jan_midnight_to_2am$RH/100)*site_rh_radn_air_soil_jan_midnight_to_2am$satvappresair

#site_rh_radn_air_soil_jan_midnight_to_2am$soilRH <- (site_rh_radn_air_soil_jan_midnight_to_2am$pvapair/site_rh_radn_air_soil_jan_midnight_to_2am$satvapsoil)*100
  

  #makes the linear model for filtering the data
  
  vwc_lm_thisyear <- lm(soil_moist_wfv_2cm~SoilRH2cm, data=jan_bull)
  
  vwc_lm_coeffs_thisyear <- summary(vwc_lm_thisyear)
  
  intercept_thisyear <- vwc_lm_coeffs_thisyear$coefficients[1,1]
  
  slope_thisyear <- vwc_lm_coeffs_thisyear$coefficients[2,1]
  
  slope_pval_thisyear <- vwc_lm_coeffs_thisyear$coefficients[2,4]
  
  vwc_min <- min(jan_bull$soil_moist_wfv_2cm, na.rm=TRUE)
  
  vwc_max <- max(jan_bull$soil_moist_wfv_2cm, na.rm=TRUE)
  
  pct_change_vwc <- ((vwc_max-vwc_min)/vwc_max)
  
  this_year_coeffs <- data.frame(slope=slope_thisyear, intercept=intercept_thisyear, pval=slope_pval_thisyear, vwc_min=vwc_min, vwc_max=vwc_max, pct_change_vwc)
  
  
  #figure making
  
  quartz()
 
  # colorRamp produces custom palettes, but needs values between 0 and 1
  colorFunction <- colorRamp(c("darkblue", "black", "red"))
  daycolor <- with(jan_bull, (DoyOfYear - min(DoyOfYear)) / (max(DoyOfYear)-min(DoyOfYear)))
  
  # Apply colorRamp and switch to hexadecimal representation
  zMatrix <- colorFunction(daycolor)
  zColors <- rgb(zMatrix, maxColorValue=255)
  plot(jan_bull$soil_moist_wfv_2cm~jan_bull$SoilRH2cm, col = zColors,
       xlab = "Soil RH",
       ylab = "Soil Moisture (v/v)",
       xlim=c(20, 100), ylim=c(0.0, 0.2),
       pch = 16,)
  
  abline(vwc_lm_thisyear)
  
  #round coeffs to show better output
  
  cf <- round(coef(vwc_lm_coeffs_thisyear),4)
  
  eq <- paste0("Soil Moisture =", cf[2,1], " • soilRH + ",cf[1,1])
  pval <- paste0(ifelse(cf[2,4]<0.01, " P < 0.01 ", " P > 0.01 "))
  
  # printing of the equation
  
  mtext(eq, 3, line=-2)    
  mtext(pval, 3, line=-3)
  
  #       col = yeardata$cols)
  
  #export the csv
  
  write.csv(this_year_coeffs, file="working_data/bull_2017_soilRH_vwc_coeffs.csv")
  
  #export the figure
  
  quartz.save(file="working_data/bull_2017_soilRH_vwc.png", type = "png", device = dev.cur(), dpi = 600)
  
  