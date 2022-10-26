# Supermarket_Store_Optimisation
A project in R to cluster open source geospatial store data

## Source Data
The location data used for this project was the 20th Edition of Geolytix open Supermarket Retail Points dataset (https://geolytix.com/blog/open-data-supermarket-retail-points-v20/)

The files themselves can be downloaded from a Google Drive: https://drive.google.com/file/d/1B8M7m86rQg2sx2TsHhFa2d-x-dZ1DbSy/view

Note: the main file used is the v24_202206.xlsx file, which has also been included in this repo for convenience/repeatability

## External Dependency - Open Source Routing Machine
This project makes use of an open source project called Project OSRM (https://project-osrm.org/)

While there is a public API available (https://project-osrm.org/docs/v5.22.0/api/#general-options), there is a limited volume of requests that the demo server can accept - so we have opted to download and run a docker image locally instead

Full details of the isntallation and run instructions can be found in "install_osrm_docker.R" script

