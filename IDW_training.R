

library(rgdal)
library(tmap)

# Load precipitation data
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds"))
P <- readRDS(z)

# Load Texas boudary map
z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds"))
W <- readRDS(z)

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)





library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons

# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(P)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(P)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, P, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)


library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function



POS_FACE_MAP_DATA <- POS_FACE_MAP %>% as_data_frame()


POS_FACE_MAP_BLOCK <- POS_FACE_MAP_DATA %>% filter(!is.na(BLOCK_LOCATIONX),
                                                           !is.na(LEVEL)) %>%
  st_as_sf(coords = c("BLOCK_LOCATIONX", "LEVEL"))



POS_FACE_MAP_SP <- POS_FACE_MAP_BLOCK %>% filter(fn_ROCKCODE == "MST2 420",
                                           !is.na(COMP_AU)) %>% 
  as("Spatial")




# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(POS_FACE_MAP_SP, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(POS_FACE_MAP_SP) <- proj4string(POS_FACE_MAP_SP) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(POS_FACE_MAP_SP)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(COMP_AU ~ 1, POS_FACE_MAP_SP , newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)



W <- POS_FACE_MAP_SP@bbox
r.m     <- mask(r, W)

# Plot
tm_shape(r) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# Finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp   <- raster::intersect(W,th.spdf)

# Map the data
tm_shape(th.clp) + 
  tm_polygons(col="Precip_in", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted precipitation \n(in inches)") +
  tm_legend(legend.outside=TRUE)
