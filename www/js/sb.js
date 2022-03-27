function(el, x) {
  this.on('mousemove', function(e) {
      var lat = e.latlng.lat;
      var lng = e.latlng.lng;
      var coord = [lat, lng];
      Shiny.onInputChange('hover_coordinates', coord)
  });
  this.on('mouseout', function(e) {
      Shiny.onInputChange('hover_coordinates', null)
  })
}
