## Source at start of FeasibilityApp
## Kiri Daust

bcgov_tileserver <- "http://159.203.20.90/data/tiles/{z}/{x}/{y}.pbf"
bcgov_tilelayer <- "WNA_MAP"
subzones_colours_ref <- fread("./inputs/WNA_v12_HexCols.csv")
setnames(subzones_colours_ref,c("BGC","Col"))

plugins <- {list(vgplugin = 
         htmltools::htmlDependency(
           name = "leaflet.vectorgrid",
           version = "1.3.0",
           src = "htmlwidgets",
           script = "lfx-vgrid-prod.js"
         ),
       sliderplugin = htmltools::htmlDependency(
         name = "leaflet.slider",
         version = "1.0.0",
         stylesheet = "lfx-slider.css",
         src = "htmlwidgets",
         script = "lfx-slider.js"
       )
  )
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
jscode_feas <- paste0('window.LeafletWidget.methods.addGridTiles = function(BGC,Colour,Lab) {
      var subzoneColors = {};
      var tooltipLabs = {};
      BGC.forEach((bec,i) => {
        const col = Colour[i];
        const label = Lab[i];
        subzoneColors[bec] = col;
        tooltipLabs[bec] = label;
      });
      
      var map = this;
      
      var vectorTileOptions=function(layerName, layerId, activ,
                                     lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: 1
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', bcgov_tileserver, '",
        vectorTileOptions("bec_feas", "', bcgov_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      console.log(subzLayer);
      this.layerManager.addLayer(subzLayer, "tile", "bec_feas", "Feasibility")
      var selectHighlight = "SBSdk";
      
      subzLayer.on("click", function(e){
        console.log(e.layer);
        subzLayer.resetFeatureStyle(selectHighlight);
        Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
        var properties = e.layer.properties
  			  highlight = properties.MAP_LABEL
  			  var style = {
            weight: 1,
            color: "#fc036f",
            fillColor: subzoneColors[properties.MAP_LABEL],
            fillOpacity: 1,
            fill: true
          }
          subzLayer.setFeatureStyle(properties.MAP_LABEL, style);
      });
      
      
      var highlight
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  }
		  
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })
		  
      subzLayer.bindTooltip(function(e) {
        return tooltipLabs[e.properties.MAP_LABEL]
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();
      
      var styleHL = {
            weight: 1.5,
            color: "#fc036f",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: true
          };
          
      
      Shiny.addCustomMessageHandler("highlightBEC",function(BECSelect){
        console.log(BECSelect);
        if(selectHighlight){
          subzLayer.resetFeatureStyle(selectHighlight);
          selectHighlight = BECSelect;
          subzLayer.setFeatureStyle(BECSelect, styleHL);
        }
        
      });
      
    };')

leafletjs_feas <-  tags$head(
  tags$script(HTML(
    jscode_feas
  ))
)

jscode_fh <- paste0('window.LeafletWidget.methods.addFHTiles = function(BGC,Colour,Lab) {
      var subzoneColors = {};
      var tooltipLabs = {};
      BGC.forEach((bec,i) => {
        const col = Colour[i];
        const label = Lab[i];
        subzoneColors[bec] = col;
        tooltipLabs[bec] = label;
      });
      
      var map = this;
      
      var vectorTileOptions=function(layerName, layerId, activ,
                                     lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, 
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: 1
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', bcgov_tileserver, '",
        vectorTileOptions("bec_fh", "', bcgov_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      console.log(subzLayer);
      this.layerManager.addLayer(subzLayer, "tile", "bec_fh", "Feasibility")

      subzLayer.on("click", function(e){
        Shiny.setInputValue("fh_click",e.layer.properties.MAP_LABEL);
      });

      subzLayer.bindTooltip(function(e) {
        return tooltipLabs[e.properties.MAP_LABEL]
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();
      
      var prevPest = ["SBSdk","IDFdk3"];
      //update style for pests
      Shiny.addCustomMessageHandler("colourPest",function(fhDat){
        var pestBGC = fhDat["bgc"];
        var fhCols = fhDat["fhcol"];
        console.log(fhDat);
        prevPest.forEach((hl,i) => {
          subzLayer.resetFeatureStyle(hl);
        });
        prevPest = pestBGC;
        pestBGC.forEach((ID,i) => {
          let styleFH = {
            weight: 0,
            fillColor: fhCols[i],
            fillOpacity: 1,
            fill: true
          };
          subzLayer.setFeatureStyle(ID, styleFH);
        });

      });
      
    };')

leafletjs_fh <-  tags$head(
  tags$script(HTML(
    jscode_fh
  ))
)

addBGCTiles <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$BGC, "':'", subzones_colours_ref$Col,"'", collapse = ","), "}"), '
      
      L.bec_layer_opacity = 0.2
      
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: L.bec_layer_opacity
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', bcgov_tileserver, '",
        vectorTileOptions("bec_map", "', bcgov_tilelayer, '", false,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_map", "BGCs");
      
      updateOpacity = function(value) {
        L.bec_layer_opacity = parseFloat(value);
      }
      
      var opacityslider = L.control.slider(updateOpacity, {
        id:"opacity_slider",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="www/opacity.svg" />\',
        max:1,
        step:0.01,
        syncSlider:true,
        size:"250px",
        // Starting opacity value for bec map layers
        value:0.2,
        showValue:true
      })
      
      opacityslider.addTo(this)
    }'
  ))
  map
}
