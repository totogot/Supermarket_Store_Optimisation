####################################################################################################################
####################################################################################################################

# INSTALL DOCKER AND OSRM DOCKER IMAGES

# this can be a little painful, so i suggest doing the following steps in order:

# first to ensure compatibility, install Linux from the following link: https://learn.microsoft.com/en-gb/windows/wsl/install-manual#step-4---download-the-linux-kernel-update-package
# download the Linux update from step 4 and activate WSL 2 in step 5
# next download Ubuntu (22.04.1 LTS): https://apps.microsoft.com/store/detail/ubuntu-22041-lts/9PN20MSR04DW?hl=en-gb&gl=gb
# restart your PC
# next, install Docker from here: https://docs.docker.com/desktop/install/windows-install/
# upon opening, ensure your "Docker engine" starts correctly
# if so go to settings > resources > WSL Integration > select the Ubuntu distribution downloaded

# next download OSRM
# instructions taken from here: https://github.com/Project-OSRM/osrm-backend
# create a directory into which you'll install your repositories you wish to unpack for Docker
# in Powershell Prompt:
# $ mkdir Documents\Docker_Repos\OSRM
# $ cd  Documents\Docker_Repos\OSRM
# $ wget http://download.geofabrik.de/europe/britain-and-ireland-latest.osm.pbf
# or download from here and move to the folder (if you have memory issues like i do)
# http://download.geofabrik.de/europe.html

# check the instructions in "Using Docker" section of here: https://github.com/Project-OSRM/osrm-backend
# start the docker container and create directory "/data" inside the docker container 
# and makes the current working directory "%cd%" available there
# $ docker run -t -v "%cd%:/data" osrm/osrm-backend osrm-extract -p /opt/car.lua /data/britain-and-ireland-latest.osm.pbf
# ^ he above can take some time to run for the first time

# you will now see a running container on your Docker Desktop (with IMAGE showing as osrm-backend)
# next execute:
# $ docker run -t -v "%cd%:/data" osrm/osrm-backend osrm-partition /data/britain-and-ireland-latest.osrm
# lastly:
# $ docker run -t -v "%cd%:/data" osrm/osrm-backend osrm-customize /data/britain-and-ireland-latest.osrm

# we can now delete all running containers in Docker

# now before we run the docker container, we want to set up a private network in which we'll run it
# this avoids the risk of different dockers interacting with apps in an unintended manner

# $ docker network create OSRM_routing
# $ docker network ls

# now we can launch the container (ensuring you are in the correct directory)
# $ cd Documents/Docker_Repos/OSRM
# $ docker run --net OSRM_routing --name OSRM -t -i -p 5000:5000 -v "%cd%:/data" osrm/osrm-backend osrm-routed --algorithm mld /data/britain-and-ireland-latest.osrm 

# test it in another command prompt window using:
# $ curl "http://127.0.0.1:5000/route/v1/driving/-1.923127,52.517037;0.563423,52.496891?steps=true"

# API usage details: https://project-osrm.org/docs/v5.22.0/api/#route-service

# to get the nearest distance between location pairs, we can use the table method:
# $ curl "http://127.0.0.1:5000/table/v1/driving/-1.923127,52.517037;0.563423,52.496891;-1.520016,52.421238"
# in the command prompt we can see the responses returned from the app as a string
# the response returned is a 3x3 matrix showing the duration between each pair
# we are now ready to continue in R...


# execute the same command and take a response in R
response <- GET("http://127.0.0.1:5000/table/v1/driving/-1.923127,52.517037;0.563423,52.496891;-1.520016,52.421238")
result <- content(response, as='parsed')

duration_matrix <- as.matrix(do.call("cbind", result$durations))
print(duration_matrix)
#      [,1]   [,2]   [,3]  
# [1,] 0      9343.4 1976.6
# [2,] 9291.9 0      8172  
# [3,] 1963.2 8201.2 0  
