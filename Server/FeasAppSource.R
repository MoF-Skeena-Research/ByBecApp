## Source at start of FeasibilityApp
## Kiri Daust

bgc_tileserver <- "https://tileserver.thebeczone.ca/data/WNA_MAP/{z}/{x}/{y}.pbf"
bgc_tilelayer <- "WNA_MAP"
district_tileserver <- "https://tileserver.thebeczone.ca/data/Districts/{z}/{x}/{y}.pbf"
district_tilelayer <- "Districts"

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
        "', bgc_tileserver, '",
        vectorTileOptions("bec_feas", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      //console.log(subzLayer);
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
          
      
      Shiny.addCustomMessageHandler("highlight",function(BECSelect){
        if(selectHighlight){
          subzLayer.resetFeatureStyle(selectHighlight);
          selectHighlight = BECSelect;
          subzLayer.setFeatureStyle(BECSelect, styleHL);
        }
      });
      
      //make layer transparent if no data
      let styleCLR = {
            weight: 0,
            fillColor: "#9d32a8",
            fillOpacity: 0,
            fill: true
          };
      
      Shiny.addCustomMessageHandler("clearLayer",function(x){
        console.log(x);
        BGC.forEach((ID,i) => {
          subzLayer.setFeatureStyle(ID, styleCLR);
        });

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
        "', bgc_tileserver, '",
        vectorTileOptions("bec_fh", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      console.log(subzLayer);
      this.layerManager.addLayer(subzLayer, "tile", "bec_fh", "Pests");
      Shiny.setInputValue("fh_click",null);

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
      
      Shiny.addCustomMessageHandler("clearPest",function(dat){
        prevPest.forEach((hl,i) => {
          subzLayer.resetFeatureStyle(hl);
        });
      });
      
    };')

leafletjs_fh <-  tags$head(
  tags$script(HTML(
    jscode_fh
  ))
)
#########################

jscode_clim <- paste0('window.LeafletWidget.methods.addClimSumTiles = function() {
      
      var map = this;
      
      var vectorTileOptions=function(layerName, layerId, activ,
                                     lfPane, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, 
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: "#ffffff",
                fill: true,
                fillOpacity: 0
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
        "', bgc_tileserver, '",
        vectorTileOptions("bec_fh", "', bgc_tilelayer, '", true,
                          "tilePane", "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_clim", "Climate");
      Shiny.setInputValue("clim_click",null);

      subzLayer.on("click", function(e){
        Shiny.setInputValue("clim_click",e.layer.properties.MAP_LABEL);
      });

      
      subzLayer.bringToFront();
      
      //update style for climate
      Shiny.addCustomMessageHandler("colourClimate",function(climDat){
        var climBGC = climDat["bgc"];
        var climCols = climDat["col"];
        var climLabs = climDat["lab"];
        var tooltipLabs = {};
        
        climBGC.forEach((bec,i) => {
          const label = climLabs[i];
          tooltipLabs[bec] = label;
        });
        subzLayer.bindTooltip(function(e) {
          return tooltipLabs[e.properties.MAP_LABEL];
        }, {sticky: true, textsize: "10px", opacity: 1});

        climBGC.forEach((ID,i) => {
          let styleFH = {
            weight: 0,
            fillColor: climCols[i],
            fillOpacity: 1,
            fill: true
          };
          subzLayer.setFeatureStyle(ID, styleFH);
        });

      });
      
    };')

leafletjs_clim <-  tags$head(
  tags$script(HTML(
    jscode_clim
  ))
)

addPlugins <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map
}

addBGCTiles <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$BGC, "':'", subzones_colours_ref$Col,"'", collapse = ","), "}"), '
      
      //L.bec_layer_opacity = 0.2
      
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
        "', bgc_tileserver, '",
        vectorTileOptions("bec_map", "', bgc_tilelayer, '", false,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_map", "BGCs");
      
      //Now districts regions
      var vectorTileOptionsDist=function(layerName, layerId, activ,
                                     lfPane, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0.5,
                color: "#000000",
                fill: false,
                fillOpacity: 0
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
      };
      var distLayer = L.vectorGrid.protobuf(
        "', district_tileserver, '",
        vectorTileOptionsDist("Districts", "', district_tilelayer, '", true,
                          "tilePane", "dist_code", "dist_code")
      )
      this.layerManager.addLayer(distLayer, "tile", "Districts", "Districts")
      // end districts
      
      updateOpacity = function(value) {
        L.bec_layer_opacity = parseFloat(value);
      }
      
      var opacityslider = L.control.slider(updateOpacity, {
        id:"opacity_slider",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="opacity.svg" />\',
        max:1,
        title: "BGC Opacity",
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

##for find BGC tab
addSelectBEC <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$BGC, "':'", subzones_colours_ref$Col,"'", collapse = ","), "}"), '
      
      L.bec_layer_opacity2 = 0.25
      
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
                fillOpacity: L.bec_layer_opacity2
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
        "', bgc_tileserver, '",
        vectorTileOptions("bec_select", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_select", "BEC");
      
      //highlight on click
      var styleHL = {
            weight: 1.5,
            color: "#fc036f",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: true
          };
      var selectHighlight = ["SBSdk","SBSmc2"];
      subzLayer.on("click", function(e){
        console.log("click");
        selectHighlight.forEach((ID,i) => {
          subzLayer.resetFeatureStyle(ID);
        });
        Shiny.setInputValue("becselect_click",e.layer.properties.MAP_LABEL);
        var properties = e.layer.properties
  			  highlight = properties.MAP_LABEL
          subzLayer.setFeatureStyle(properties.MAP_LABEL, styleHL);
      });

      var highlight;
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  }
		  
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })

      Shiny.addCustomMessageHandler("highlightBEC",function(BECSelect){
        console.log(BECSelect);
        if(!Array.isArray(BECSelect)){
          BECSelect = [BECSelect];
        }
        if(selectHighlight){
          selectHighlight.forEach((ID,i) => {
            subzLayer.resetFeatureStyle(ID);
          });
          selectHighlight = BECSelect;
          BECSelect.forEach((ID,i) => {
            subzLayer.setFeatureStyle(ID, styleHL);
          });
          Shiny.setInputValue("becselect_click",BECSelect);
        }
      });
      
      Shiny.addCustomMessageHandler("clearBEC",function(x){
          selectHighlight.forEach((ID,i) => {
            subzLayer.resetFeatureStyle(ID);
          });
      });
      
      subzLayer.bindTooltip(function(e) {
        return e.properties.MAP_LABEL
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();
      
      updateOpacity = function(value) {
        L.bec_layer_opacity2 = parseFloat(value);
      }
      
      var opacityslider2 = L.control.slider(updateOpacity, {
        id:"opacity_slider2",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="opacity.svg" />\',
        max:1,
        step:0.01,
        syncSlider:true,
        size:"250px",
        title: "Adjust BGC Opacity",
        // Starting opacity value for bec map layers
        value:0.25,
        showValue:true
      })
      opacityslider2.addTo(this);
    }'
  ))
  map
}

