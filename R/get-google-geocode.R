get_google_geocode <- function(address) {
    url <- httr::build_url(
        structure(
            list(
                scheme = "http",
                hostname = "maps.googleapis.com",
                path = "maps/api/geocode/json",
                query = list(address = address)
            ), 
            class = "url"
        )
    )
    res <- httr::GET(url)
    
    
    json <- httr::content(res,
                          as = "text",
                          type = "application/json",
                          encoding = "UTF-8")
    
    # convert json into a list
    reslist <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    
    valid <- check_results(reslist)
    
    if (!valid) return(data.frame(lat = NA, lon = NA))
    loc <- reslist$results[[1]]$geometry$location
    
    data.frame(lat = loc$lat,
               lon = loc$lng,
               stringsAsFactors = FALSE)
}

check_results <- function(reslist) {
    # status will tell you if there was a problem
    if (reslist$status != "OK") return(FALSE)
    
    # if there were multiple results, then we don't know which one to pick
    # if (length(reslist$results) > 1L) return(FALSE)
    
    # if the returned address has no zip code, then i can't trust it
    comps <- unlist(lapply(reslist$results[[1]]$address_components, function(x) x$types))
    if (is.null(comps)) return(FALSE)
    if (!"postal_code" %in% comps) return(FALSE)
    
    # if the location_type is not accurate enough
    loctype <- reslist$results[[1]]$geometry$location_type
    if (!is.null(loctype) && 
        loctype %in% c("APPROXIMATE")) return(FALSE)
    
    # if we didn't get geocodes for any other reason
    loc <- reslist$results[[1]]$geometry$location
    if (is.null(loc)) return(FALSE)
    
    # finally, TRUE if we passed the tests
    TRUE
}