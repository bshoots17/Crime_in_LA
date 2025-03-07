library(sf)
census_blocks <- read_sf(dsn = "0_Data/Raw_Data/cb_2018_06_bg_500k", layer = "cb_2018_06_bg_500k")

crime_la <- read.csv("0_Data/Cleaned_Data/all_crimes_2010_present.csv")

print("Loaded Data")
points <- st_as_sf(crime_la, coords = c("LON", "LAT"), crs = 4326)

# Ensure CRS consistency (convert ZCTA to WGS84 if needed)
census_blocks <- st_transform(census_blocks, 4326)

# Use spatial indexing to improve performance
census_blocks <- st_make_valid(census_blocks)  # Ensure geometries are valid

# Perform spatial join efficiently
result <- st_join(points, census_blocks, join = st_within, left = FALSE)

print("Joined")

write.csv(result, "0_Data/Cleaned_Data/crimes_by_census_block.csv")