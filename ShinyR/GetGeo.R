getgeo <- function(cityname){
  
  # Creating parameters for the GET request
  base <- "http://open.mapquestapi.com/geocoding/v1/address?key="
  key <- "XIJS0al55bI7xLquiUOVudBOoyG2r8lY"
  endpoint <- "location"
  city <- cityname
  
  # Creating API call string
  call <- paste(base,key,"&",endpoint,"=",city, sep="")
  
  # Make API request
  get_location <- GET(call, type = "basic")
  
  # Convert results to dataframe 
  get_location_text <- content(get_location, "text")
  get_location_json <- fromJSON(get_location_text, flatten = TRUE)
  long_lat <- get_location_json$results$locations
  long_lat_df <- as.data.frame(long_lat)
  
  #Get longitude and latitude value (First county)
  latitude <- long_lat_df$displayLatLng.lat[1]
  longitude <- long_lat_df$displayLatLng.lng[1]
  
  return (list(latitude, longitude))
}