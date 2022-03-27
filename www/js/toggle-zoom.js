function(el, x) {
  var map = this
  var group = "heatmap"
  layerGroup = getLayerGroup(map, group)
  var fillOpacity = layerGroup._layers[Object.keys(layerGroup._layers)[1]].options.fillOpacity
  addEventListeners(map, layerGroup, fillOpacity)
  removeLayers(map, group)
  addLayerGroup(map, layerGroup)
  map.on('zoomend', function() {
    if (map.getZoom() <= 5 && isHiddenLayerGroup(layerGroup)) {
      showLayerGroup(layerGroup, fillOpacity)
    }
    if (map.getZoom() > 5 && !isHiddenLayerGroup(layerGroup)) {
      hideLayerGroup(layerGroup)
    }
  })
  function getLayerGroup(m, g) {
    var layers = []
    m.eachLayer(function(l) {
      if (l.options.group == g) {
        layers.push(l)
      }
    })
    return L.layerGroup(layers)
  }
  function addEventListeners(m, lg, fo) {
    lg.eachLayer(function(l) {
      l.on("mouseover", function(e) {
        e.target.setStyle({fillOpacity: 0})
      })
      l.on("mouseout", function(e) {
        if (m.getZoom() <= 5) {
          e.target.setStyle({fillOpacity: fo})
        }
      })
    })
  }
  function reducer(a,b) {
    return Number.parseFloat(a) + Number.parseFloat(b)
  }
  function isHiddenLayerGroup(lg) {
    var fillOpacities = []
    lg.eachLayer(function(l) {
      fillOpacities.push(l.options.fillOpacity)
    })
    if (fillOpacities.reduce(reducer) === 0) {
      return true
    }
    return false 
  }
  function hideLayerGroup(lg) {
    lg.eachLayer(function(l) {
      l.setStyle(
        {
          fillOpacity: "0"
        }
      )
    })
  }
  function showLayerGroup(lg, fo) {
    lg.eachLayer(function(l) {
      l.setStyle(
        {
          fillOpacity: fo
        }
      )
    })
  }
  function removeLayers(m, g) {
    var layers = []
    m.eachLayer(function(l) {
      if (l.options.group == g) {
        layers.push(l)
      }
    })
    layers.forEach(function(l) {
      l.remove()
    })
  }
  function addLayerGroup(m, lg) {
    lg.addTo(m)
  }
  function removeLayerGroup(m, lg) {
    lg.remove()
  }
  function hasLayerGroup(m, lg) {
    return m.hasLayer(lg)
  }
}
