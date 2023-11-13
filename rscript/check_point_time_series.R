library(dygraphs)
library(geotopbricks)

wpath <- '/stablo001/local/simulations/venosta_2023/run/TEST_Venosta_3D_034/' 
tz="Etc/GMT-1" ## check on geotop.inpts

check_points <- get.geotop.points(prefix="CoordinatePoint",suffix=c("ID"),wpath=wpath)
check_points <- check_points %>% st_transform(crs=4326)
### WEATHER STATIONS