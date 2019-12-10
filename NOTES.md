


Historic Parishes of England and Wales : an Electronic Map of Boundaries before 1850 with a Gazetteer and Metadata
https://beta.ukdataservice.ac.uk/datacatalogue/doi/?id=4348#!#1


GIS of the Ancient Parishes of England and Wales, 1500-1850
https://beta.ukdataservice.ac.uk/datacatalogue/doi/?id=4828#!#1




 1851 England and Wales census registration districts 
http://reshare.ukdataservice.ac.uk/852948/


 1851 England and Wales Census registration counties 
http://reshare.ukdataservice.ac.uk/852949/


 1851 England and Wales census parishes, townships and places 
http://reshare.ukdataservice.ac.uk/852816/




1851 Census Report Registration District Occupational Data
https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5433#!/details


1851 Census Report: County Occupational Data
https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5431#!/details



1851 England, Wales and Scotland Rail Lines 
https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=852991


Primary, Secondary and Tertiary (PST) Occupational Codes for the 1851 Census Report
https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5434#!/details



https://maps.nls.uk/geo/explore/#zoom=7&lat=53.2274&lon=-0.7334&layers=1&b=1
https://maps.nls.uk/projects/api/
https://digimap.edina.ac.uk/historic
https://github.com/Luqqk/wms-tiles-downloader


https://geoserver2.nls.uk/geoserver/wfs?service=WFS&version=1.1.0&request=GetFeature&typename=nls:OS_One_Inch_GB_WFS&PropertyName=(the_geom,IMAGEURL,WFS_TITLE)&outputFormat=text/javascript&format_options=callback:loadFeatures&srsname=EPSG:3857&bbox=939258.2035682462,7200979.560689885,1252344.2714243282,7514065.628545967,EPSG:3857&_=1572778103319


Historisch Leiden in Kaart: Toelichting en handleiding
https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:67498
https://www.leidseregioinkaart.nl/kaarten/hlik/
https://www.leidseregioinkaart.nl/kaarten/1583/





pleiades_url <- 'http://atlantides.org/downloads/pleiades/kml/pleiades-latest.kmz'
download.file(pleiades_url, 'pleiades-latest.kmz')
pleiades <- st_read('pleiades-latest.kmz')

ew = st_read(here::here('data/england_wales_1851/1851EngWalesParishandPlace.shp'))

mapview(ew %>% slice(1:500))

mapview(pleiades %>%
  slice(1:42, 44:50))



qtm(pleiades[0:10])

st_crs(pleiades) = NA
mapview(pleiades)

u_kmz <- "http://coagisweb.cabq.gov/datadownload/BikePaths.kmz"
download.file(u_kmz, "BikePaths.kmz")
bikraces <- st_read("bikeraces.kml")
