# Supermarket_Store_Optimisation
A project in R to cluster open source geospatial store data

## Setup
For full details on how to clone this repository as a tempalte to starting a new R porject, please refer to the following documentation: https://argoshare.is.ed.ac.uk/healthyr_book/clone-an-existing-github-project-to-new-rstudio-project.html

## Source Data
The location data used for this project was the 20th Edition of Geolytix open Supermarket Retail Points dataset (https://geolytix.com/blog/open-data-supermarket-retail-points-v20/)

The files themselves can be downloaded from a Google Drive: https://drive.google.com/file/d/1B8M7m86rQg2sx2TsHhFa2d-x-dZ1DbSy/view

Note: the main file used is the v24_202206.xlsx file, which has also been included in this repo for convenience/repeatability

## External Dependency - Open Source Routing Machine
This project makes use of an open source project called Project OSRM (https://project-osrm.org/)

While there is a public API available (https://project-osrm.org/docs/v5.22.0/api/#general-options), there is a limited volume of requests that the demo server can accept - so we have opted to download and run a docker image locally instead

Full details of the installation and run instructions can be found in "install_osrm_docker.Rmd" notebook (or associated html render file)

## Analysis
To see full details of the approach taken (or replicate after cloning the repo), please see the main "store_location_analytics.Rmd" notebook file.

Overall the project imagines you are a single brand (Tesco) and interested in acquiring a smaller competitor - following which you combine store portfolios and opimise for the location of your logistics depots to restock your stores.

Overall, the approach taken is as follows:
1. Load in open source dataset containing geospatial data for supermarket chain store locations
2. For each target, compute % of stores within 500m of Tesco location based on great circle distance
3. For brands with lower % of stores within 500m, assess similarity of profile using ChiSq formula
4. Identify store pairs across Tesco and acquired brand (based on this within 500m)
5. Create geographic regions by applying hierarchical clustering of geospatial data
6. Refine clustering based on drive duration - using open-source Docker app from Project OSRM
7. Apply business logic to decide which locations are most suitable for master vs sub depots/warehouses
8. Discuss areas of future improvement
