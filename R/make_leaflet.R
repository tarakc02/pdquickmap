#' Rearrange geocoded data to a leaflet map-ready format
#' 
#' @param geo_data A data frame that has columns \code{latitude} and 
#' \code{longitude}
#' @param popup_columns The names of the columns in the data frame that should 
#' be used to populate the pop-ups on the map. By default, this will include all 
#' columns except for the latitude and longitude columns. 
prep_leaf_data <- function(geo_data, 
                           popup_columns = NULL) {    
    if (is.null(popup_columns)) 
        popup_columns <-  colnames(geo_data)[!(colnames(geo_data) %in% c("latitude", "longitude"))]
    geo_data <- geo_data[complete.cases(geo_data),]
    leaf_map_data <- lapply(1:nrow(geo_data), function(x) {
        r <- as.list(geo_data[x,])
        c(r,
          popup = iconv(
              paste(
                  paste("<b>", 
                        popup_columns, 
                        ": </b>", 
                        sep=""), 
                  r[popup_columns], 
                  "<br>", 
                  sep="",
                  collapse=""), 
              from = 'latin1', to = 'UTF-8'))
    })
    leaf_map_data
}

#' Make a leaflet map
#' 
#' @import rCharts
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
make_leaflet <- function(map_data, 
                         width = 900,
                         height = 600,
                         map_center = NULL,
                         default_zoom = 11, 
                         ...) {
    map_data <- map_data[complete.cases(map_data),]
    if (is.null(map_center)) map_center <- as.numeric(map_data[1,c("latitude", "longitude")])
    leaf_map_data <- prep_leaf_data(map_data, ...)
    
    L1 <- Leaflet$new()
    L1$tileLayer(provider = 'Stamen.TonerLite')
    L1$setView(map_center, default_zoom)
    L1$set(width = width, height = height)
    L1$geoJson(toGeoJSON(leaf_map_data),
               onEachFeature = '#! function(feature, layer){
                layer.bindPopup(feature.properties.popup)
           } !#',
               pointToLayer = "#! function(feature, latlng){
                return L.circleMarker(latlng, {
                    radius: 8,
                    weight: 1,
                    color: '#003262',
                    stroke: true,
                    weight: 2,
                    fillColor: '#FDB515',
                    fillOpacity: .6,
           })
          } !#")
    L1$enablePopover(FALSE)
    L1$fullScreen(TRUE)
    L1
}

#' Save a map you've already made to an HTML file
#' 
#' Save a leaflet map to a standalone HTML file. This will output a file that 
#' you can send to clients or upload. By default, you'll be prompted with a 
#' file window asking where you want to save the file.
#' @import rCharts
#' @import base64enc
#' @param leafmap The leaflet map you'd like to save
#' @param location If you know where you'd like to save the file, enter the 
#' full path (including the name of the file). If you don't enter a location, 
#' you'll get a graphical prompt to click your way to a save location.
#' @export
#' @examples
#' map_data <- append_geocode(urel)
#' leaf <- make_leaflet(map_data)
#' save_leaflet(leaf)  ## will bring up a save file dialog window
#' # alternatively:
#' save_leaflet(leaf, "my map.html")
save_leaflet <- function(leafmap, location = NULL) {
    if (class(leafmap) != "Leaflet") 
        stop("This function only works with a leaflet map object")
    if (is.null(location)) location <- file.choose(TRUE)
    leafmap$save(location, standalone=TRUE)
}