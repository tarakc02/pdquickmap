#' @import RgoogleMaps
geocode_google <- function(addresses) {
    geocodes <- vector("list", length=length(addresses))
    
    for (a in 1:length(addresses)) {
        if(!(a %% 5))  Sys.sleep(1)
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
#' \code{lat}, and \code{lon}
#' 
#' @param addresses A character vector of addresses
#' @param src Which geo-coding API to use? Currently "google" is the only 
#' available option
#' @export
geocode <- function(addresses, src = "google") {
    geocode_google(addresses)
}

#' Append geocodes to a data frame that includes addresses
#' 
#' @param df A \code{data.frame}
#' @param cols A vector of names or index numbers of the columns that identify 
#' the address-related columns in the data. Ideally, these should be in a 
#' natural order (street, city, state, zip, country).
#' @param ... Other arguments passed to \code{geocode}
#' 
#' @return The original \code{data.frame} with the added columns \code{address},
#' \code{lat}, and \code{lon}
#' @export
append_geocode <- function(df, cols, ...) {
    addresses <- do.call("paste", df[,cols])
    gc <- geocode(addresses, ...)
    cbind(df, gc[2:3])
}


