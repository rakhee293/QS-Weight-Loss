getFoursquare <- function(foursquare.access) {
  # Useful libraries
  library("RCurl");
  library("rjson");
  
  # Do an initial call to obtain total number of checkins available 
  foursquare.checkins <- getURL(paste("https://api.foursquare.com/v2/users/self/checkins?oauth_token=", foursquare.access, "&v=20130430", sep=''));
  foursquare.checkins <- fromJSON(foursquare.checkins);
  total.checkins <- foursquare.checkins$response$checkins$count;
  
  # The variable we'll be extracting to
  checkins.extracted <- c();
  
  # Iterate total # of checkins/limit of checkins per call times to obtain all data
  for(i in 0:ceiling(total.checkins/250)) {
    
    # API call and parse
    foursquare.checkins <- getURL(paste("https://api.foursquare.com/v2/users/self/checkins?oauth_token=", foursquare.access, "&v=20130430&limit=250&offset=", i*250, sep=''));
    foursquare.checkins <- fromJSON(foursquare.checkins)$response$checkins$items;
    
    # If there's no checkins then I guess we went too far :-/
    if(length(foursquare.checkins) == 0) {
      break;
    }
    
    # Iterate through checkin list and extract data we're interested in
    for(j in 1:length(foursquare.checkins)) {
      checkins.extracted <- rbind(checkins.extracted, c(id = foursquare.checkins[[j]]$id,
                                                        createdat = foursquare.checkins[[j]]$createdAt,
                                                        venue_id = ifelse(is.null(foursquare.checkins[[j]]$venue$id), NA, foursquare.checkins[[j]]$venue$id),
                                                        venue_name = ifelse(is.null(foursquare.checkins[[j]]$venue$name), NA, foursquare.checkins[[j]]$venue$name),
                                                        venue_address = ifelse(is.null(foursquare.checkins[[j]]$venue$location$address), NA, foursquare.checkins[[j]]$venue$location$address),
                                                        venue_lat = ifelse(is.null(foursquare.checkins[[j]]$venue$location$lat), NA, foursquare.checkins[[j]]$venue$location$lat),
                                                        venue_lng = ifelse(is.null(foursquare.checkins[[j]]$venue$location$lng), NA, foursquare.checkins[[j]]$venue$location$lng),
                                                        venue_city = ifelse(is.null(foursquare.checkins[[j]]$venue$location$city), NA, foursquare.checkins[[j]]$venue$location$city),
                                                        venue_state = ifelse(is.null(foursquare.checkins[[j]]$venue$location$state), NA, foursquare.checkins[[j]]$venue$location$state),
                                                        venue_cat = ifelse(length(foursquare.checkins[[j]]$venue$categories) > 0, foursquare.checkins[[j]]$venue$categories[[1]]$name, NA)));
    }
  }
  
  return(checkins.extracted);
}
