# OptiTruk Notes

# Cleaning OSM

# 1) Download from Geofabrik http://download.geofabrik.de/europe.html
# 2) Download OSM Convert https://wiki.openstreetmap.org/wiki/Osmconvert#Windows
# 3) Convert to o5m file format
# 4) Download OSM filter
# 5) Run filter

# osmfilter europe-raw.o5m --keep="highway=primary =secondary = trunk = motorway =motorway_link =trunk_link =primary_link =secondary_link =tertiary_link" --drop-author >roads.osm

# 5) Use the prep_osm script to remove unneded data
