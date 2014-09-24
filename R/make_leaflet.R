prep_leaf_data <- function(geo_data, 
                           popup_columns = colnames(geo_data)[!(colnames(geo_data) %in% c("latitude", "longitude"))]) {    
    
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
#' should be centered when it is first opened
#' @param default_zoom The zoom level that the map will be at when first opened
#' @export
make_leaflet <- function(map_data, 
                         width = 900,
                         height = 600,
                         map_center = as.numeric(map_data[1,c("latitude", "longitude")]),
                         default_zoom = 11, 
                         ...) {
    
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