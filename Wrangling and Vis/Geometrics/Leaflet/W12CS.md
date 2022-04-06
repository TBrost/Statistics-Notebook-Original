---
title: "Map of Campus in r"
author: "Tyson Brost"
date: "March 31, 2022"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 8
    fig_width: 6
    fig_align: 'center'
---






```r
# Use this R-Chunk to import all your datasets!
BYUI <- data.frame(Building = c("STC","Austin","BYU-I Center","Hart","snow","Romney","Spori","Kirkham", "Clarke", "Mckay", "Smith", "Manwaring", "Kimball", "Taylor", "Hinckley", "Ricks", "Benson"), Lat = c(43.8144,43.81582909024388,43.81855179894589,43.81957363416934,43.82109432348679,43.820281522138444, 43.820916282233114, 43.82110980530984, 43.82008025533344, 43.819499674209986, 43.81936033390029, 43.81821463458064, 43.81703020609232, 43.81693730874667, 43.81573737168207, 43.814916755695485, 43.815381256695574 ), Long = c(-111.78459,-111.78448729257276,-111.78459523261063,-111.784820538147,-111.78355920298482,-111.78302276123155, -111.78255069248866, -111.78159582616784, -111.78179967403408, -111.78240048879773,-111.78144562247691, -111.78221809860163, -111.78145635131197,-111.78248631947825, -111.77987921247086, -111.78131687636964, -111.78314077833078), MainSubjects = c("Computer Science","Engineering","Devo","Recreation","Performance","Physical Sciences","Art","Sculpting?", "Nursing/Family Science", "Library", "Economics/Business", "Dance/University services", "University services", "Religion", "Languages", "Math/Psychology", "Plant/Animal Science"))
```

## Background

_Place Task Background Here_

## Data Wrangling


```r
# Use this R-Chunk to clean & wrangle your data!
getColor <- function(BYUI) {
  sapply(BYUI$Building, function(Building) {
  if(Building %in% c("STC","Austin","Romney", "Smith", "Ricks", "Benson")) {
    "lightblue"
  } else if(Building %in% c("BYU-I Center","Hart", "Mckay","Manwaring", "Kimball")) {
    "green"
  } else {
    "firebrick"
  } })
}
colors <- getColor(BYUI)
colors <- unname(colors)
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = colors
)
```

## Data Visualization


```r
# Use this R-Chunk to plot & visualize your data!
m <- leaflet(data = BYUI) %>% setView(lng = -111.784, lat = 43.818, zoom = 15.5) %>% 
  addTiles() %>%
  addAwesomeMarkers(~Long, ~Lat, icon=icons, popup = ~as.character(MainSubjects), label=~as.character(Building))
m
```

```{=html}
<div id="htmlwidget-c4c32b247546e36e3921" style="width:576px;height:768px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-c4c32b247546e36e3921">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[43.818,-111.784],15.5,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addAwesomeMarkers","args":[[43.8144,43.8158290902439,43.8185517989459,43.8195736341693,43.8210943234868,43.8202815221384,43.8209162822331,43.8211098053098,43.8200802553334,43.81949967421,43.8193603339003,43.8182146345806,43.8170302060923,43.8169373087467,43.8157373716821,43.8149167556955,43.8153812566956],[-111.78459,-111.784487292573,-111.784595232611,-111.784820538147,-111.783559202985,-111.783022761232,-111.782550692489,-111.781595826168,-111.781799674034,-111.782400488798,-111.781445622477,-111.782218098602,-111.781456351312,-111.782486319478,-111.779879212471,-111.78131687637,-111.783140778331],{"icon":"ios-close","markerColor":["lightblue","lightblue","green","green","firebrick","lightblue","firebrick","firebrick","firebrick","green","lightblue","green","green","firebrick","firebrick","lightblue","lightblue"],"iconColor":"black","spin":false,"squareMarker":false,"iconRotate":0,"font":"monospace","prefix":"ion"},null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Computer Science","Engineering","Devo","Recreation","Performance","Physical Sciences","Art","Sculpting?","Nursing/Family Science","Library","Economics/Business","Dance/University services","University services","Religion","Languages","Math/Psychology","Plant/Animal Science"],null,null,null,["STC","Austin","BYU-I Center","Hart","snow","Romney","Spori","Kirkham","Clarke","Mckay","Smith","Manwaring","Kimball","Taylor","Hinckley","Ricks","Benson"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[43.8144,43.8211098053098],"lng":[-111.784820538147,-111.779879212471]}},"evals":[],"jsHooks":[]}</script>
```

## Conclusions
I pulled the Long and Lat data from google maps and built a quick DF to store the data. Then I used a couple functions from a leaflet I did back in 325 to add colors based on sublists of the building names, the rough categories are; blue -> STEM, green -> University services/ETC, red -> Arts
the Building names appears on hover, click shows the primary subject taught there.
