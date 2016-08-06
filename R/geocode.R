geocode_mapquest_batch <- function(addresses, 
                                   key = api_key("mapquest")) {
    options(warn=-1)
    # build a search url
    geo_url <- "http://open.mapquestapi.com/geocoding/v1/batch/"
    loclist = paste("location=", addresses, sep="", collapse="&")
    
    query <- list(key = key,
                  maxResults = 1L,
                  thumbMaps = "false",
                  Format = "kvp")
    query <- paste(names(query), query, sep="=", collapse="&")
    query <- paste(query, loclist, sep="&")
    con <- paste(geo_url, query, sep="?")
    
    # get and parse results
    resp <- readLines(con)
    resp <- jsonlite::fromJSON(resp)
    
    # fill in blank data frames with NA to keep lat/lng frames same dimensions
    # as the original address vector
    
    latlng <- lapply(resp$results$locations, 
                     function(x) if(length(x) == 0) {
                         data.frame(lat = NA_real_, lng = NA_real_)} else {
                             x$latLng[,c("lat", "lng")]})
    latlng[sapply(latlng, is.null)] <- data.frame(lat = NA_real_, lng = NA_real_)
    latlng <- do.call("rbind", latlng)
    
    # output data frame of address, latitude, longitude
    outp <- cbind(resp$results$providedLocation, latlng)
    names(outp) <- c("address", "latitude", "longitude")
    outp
}

#' Geocode addresses using the Mapquest Open Geocoding API
#' 
#' You will need an API key to use this function. You can request one from
#' \url{http://developer.mapquest.com/web/products/open/geocoding-service}.
#' 
#' @param addresses A character vector of addresses
#' @param key Your mapquest API key
#' @param batch Reserved for future use
#' @param batch_size Addresses will be geocoded using the batch geocoding 
#' service. The \code{batch_size} controls how large each batch should be.
geocode_mapquest <- function(addresses, 
                             key = api_key("mapquest"), 
                             batch = TRUE, 
                             batch_size = 100L) {
    
    # returns a list of vectors w/ at most batch_size elements each
    batched_addresses <- chunk(addresses, batch_size)
    num_batches <- length(batched_addresses)
    
    geocodes <- vector("list", num_batches)
    for (b in 1:num_batches) {
        geocodes[[b]] <- geocode_mapquest_batch(batched_addresses[[b]],
                                                key)
        Sys.sleep(1)
    }
    do.call("rbind", geocodes)
}

#' Geocode addresses using the Google Maps API
#' 
#' This is \code{geocode} with \code{provider = "google"}. Note that by using this 
#' function, you're agreeing to the Google Maps API terms of service. See 
#' \url{https://developers.google.com/maps/terms#section_10_12}.
#' 
#' @note You will be limited to 2,500 addresses per 24-hour period.
#' 
#' @param addresses A character vector of addresses
#' @return A data frame with three columns: \code{address}, \code{latitude},
#' and \code{longitude}.
#' 
geocode_google <- function(addresses) {
    geocodes <- vector("list", length=length(addresses))
    
    for (a in 1:length(addresses)) {
        if(!(a %% 5L))  Sys.sleep(1L)
        geocodes[[a]] <- RgoogleMaps::getGeoCode(addresses[a])
        names(geocodes)[a] <- addresses[a]
    }
    
    geocodes <- do.call("rbind", geocodes)
    gc <- data.frame(address = rownames(geocodes), 
                     latitude = geocodes[,1],
                     longitude = geocodes[,2],
                     stringsAsFactors=FALSE)
    rownames(gc) <- NULL
    gc
}

#' Get geo-codes from addresses
#' 
#' @return A \code{data.frame} with three columns: \code{address}, 
#' \code{latitude}, and \code{longitude}
#' 
#' @param addresses A character vector of addresses
#' @param provider Which geo-coding API to use? ("google" or "mapquest")
#' 
#' @seealso \code{\link{geocode_google}}, \code{\link{geocode_mapquest}}
#' @export
geocode <- function(addresses, provider = "google", ...) {
    switch(provider, 
           google = geocode_google(addresses),
           mapquest = geocode_mapquest(addresses, ...))
}

#' Append geocodes to a data frame that includes addresses
#' 
#' @param df A \code{data.frame}
#' @param cols A vector of names or index numbers of the columns that identify 
#' the address-related columns in the data. Ideally, these should be in a 
#' natural order (street, city, state, zip, country).
#' @param ... Other arguments passed to \code{\link{geocode}}
#' 
#' @return The original \code{data.frame} with the added columns \code{address},
#' \code{lat}, and \code{lon}
#' @export
append_geocode <- function(df, cols, ...) {
    unrecognized_columns <- !cols %in% colnames(df)
    if (any(unrecognized_columns))
        stop("These columns are not recognized: ", 
             paste(cols[unrecognized_columns], collapse = ", "))
    
    addresses <- do.call("paste", df[,cols, drop = FALSE])
    gc <- geocode(addresses, ...)
    res <- cbind(df, gc[2:3])
    na_indices <- is.na(res$latitude) | is.na(res$longitude)
    na_cnt <- sum(na_indices)
    if (any(na_indices)) 
        warning("could not geocode ", na_cnt, " rows, so dropped them from the results")
    res[!na_indices, , drop = FALSE]
}