# JS for map --------------------------------------------------------------

geocoder <- htmlDependency("leaflet.geocoder", "1.6.0",
                           src = c(href = "https://unpkg.com/leaflet-control-geocoder/dist"),
                           script = "Control.Geocoder.js",
                           stylesheet = "Control.Geocoder.css"
)

fontawsome_markers <- htmlDependency("fontawesome", "2.0.4",
                                     src = c(href = "https://unpkg.com/leaflet.awesome-markers@2.0.4/dist"),
                                     script = "leaflet.awesome-markers.js",
                                     stylesheet = "leaflet.awesome-markers.css")

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}


geocode_js <- "function(el, x, data) {
  var MapGeoCoderProvider = L.Control.Geocoder.google(data.key, {}),
    marker;
  map = this;  
  
  L.Control.geocoder({geocoder:MapGeoCoderProvider, defaultMarkGeocode: false, collapsed: false})
     .on('markgeocode', function(e) {
          var bbox = e.geocode.bbox;
          var poly = L.polygon([
                 bbox.getSouthEast(),
                 bbox.getNorthEast(),
                 bbox.getNorthWest(),
                 bbox.getSouthWest()
            ]);
          map.fitBounds(poly.getBounds());
          
          if (marker) {
            marker.setLatLng(e.geocode.center);
          } else {
            marker = L.circleMarker(e.geocode.center, {weight: 1, fillOpacity: .5}).addTo(map);
          }
          
          
      })
  .addTo(this);
}"
