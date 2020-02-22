
// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("LATITUDE");
  var long = $el.data("LONGITUDE");
  var API = $el.data("API");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    API: API,
    nonce: Math.random()
  });
});