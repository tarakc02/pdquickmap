make_popup_content <- function(geo_df, popup_columns) {
    htmlEscape <- htmltools::htmlEscape
    popup_df <- geo_df[, popup_columns, drop = FALSE]
    popup <- do.call(paste,
                     lapply(popup_columns,
                            function(x)
                                paste(x, ": ", htmlEscape(popup_df[[x]]),
                                      "<br>", sep = "")))
    popup
    
}

find_center <- function(lat, long) {
    c(lat = mean(lat),
      long = mean(long))
}


#' Make a leaflet map
#' 
#' @param map_data Data frame to be mapped -- at the very least, this should 
#' include the columns \code{latitude} and \code{longitude}. Other data can be 
#' passed to the text popups
#' @param width Width in pixels for the map
#' @param height height in pixels for the map
#' @param map_center A vector witht the latitude and longitude where the map 
#' should be centered when it is first opened. Defaults to the lat/long of the 
#' first row in \code{map_data}
#' @param default_zoom The zoom level that the map will be at when first opened
#' @param ... Other arguments passed on to \code{\link{prep_leaf_data}}
#' @export
make_leaflet <- function(map_data, popup_columns = NULL,
                         width = 900,
                         height = 600,
                         map_center = NULL,
                         default_zoom = 9, 
                         ...) {
    
    `%>%` <- leaflet::`%>%`
    leaflet <- leaflet::leaflet
    addProviderTiles <- leaflet::addProviderTiles
    addCircleMarkers <- leaflet::addCircleMarkers
    setView <- leaflet::setView
    
    if (is.null(popup_columns)) 
        popup_columns <- colnames(map_data)[!(colnames(map_data) %in% c("latitude", "longitude"))]
    if (is.null(map_center))
        center <- find_center(map_data$latitude, map_data$longitude)
    
    unrecognized_columns <- !popup_columns %in% colnames(map_data)
    if (any(unrecognized_columns))
        stop("These columns are not recognized: ", 
             paste(popup_columns[unrecognized_columns], collapse = ", "))
    
    popup <- make_popup_content(map_data, popup_columns)
    
    leaflet(map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(lng = ~longitude,
                         lat = ~latitude,
                         popup = popup,
                         radius = 7,
                         fillColor = "#003262",
                         color = "#FDB515",
                         weight = 1,
                         opacity = 1,
                         fillOpacity = .7) %>%
        setView(lng = center[["long"]],
                lat = center[["lat"]],
                zoom = default_zoom)
}

#' Save a map you've already made to an HTML file
#' 
#' Save a leaflet map to a standalone HTML file. This will output a file that 
#' you can send to clients or upload. By default, you'll be prompted with a 
#' file window asking where you want to save the file.
#' @param leafmap The leaflet map you'd like to save
#' @param location If you know where you'd like to save the file, enter the 
#' full path (including the name of the file). If you don't enter a location, 
#' you'll get a graphical prompt to click your way to a save location.
#' @export
#' @examples
#' map_data <- append_geocode(urel)
#' leaf <- make_leaflet(map_data)
#' save_leaflet(leaf)  ## will bring up a save file dialog window
#' ## alternatively:
#' save_leaflet(leaf, "my map.html")
save_leaflet <- function(leafmap, location = NULL) {
    if (!inherits(leafmap, "leaflet")) 
        stop("This function only works with a leaflet map object")
    if (is.null(location)) location <- file.choose(TRUE)
    htmlwidgets::saveWidget(leafmap, location, selfcontained = TRUE)
}