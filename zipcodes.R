library(sf)
library(dplyr)

crime_la_2010_2019 <- read.csv("Raw_Data/Crime_Data_from_2010_to_2019.csv")

crime_la_2020_present <- read.csv("Raw_Data/Crime_Data_from_2020_to_Present.csv")

# Combine the two datasets
crime_la <- rbind(crime_la_2010_2019, crime_la_2020_present)

crime_la$DateParsed <- as.POSIXct(crime_la$Date.Rptd, 
                                  format = "%m/%d/%Y %I:%M:%S %p", 
                                  tz = "America/Los_Angeles")

crime_la$YearWeek <- format(crime_la$DateParsed, "%Y-%U")

crime_la$year <- as.integer(stringr::str_sub(crime_la$YearWeek, 1, 4))

coords <- crime_la %>% select(LAT, LON, DR_NO, year)

coords$universal_idx <- 1:nrow(coords)

print(head(coords))

zcta <- read_sf(dsn = "Raw_Data/cb_2020_us_zcta520_500k", layer = "cb_2020_us_zcta520_500k")

  points <- st_as_sf(coords, coords = c("LON", "LAT"), crs = 4326)  # WGS84
  
  print(head(points))
  
  zcta <- st_transform(zcta, 4326)  # Convert ZCTA to WGS84 if needed
  
  cat("Starting Zip Code Retrieval \n")
  
  result <- st_join(points, zcta)
  
  print(head(result))

  cat("Finished Zip Code Retrieval \n")
  
  write.csv(result, "crime_zip_files/all_zips.csv")
