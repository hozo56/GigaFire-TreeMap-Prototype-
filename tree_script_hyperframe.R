+### Get packages installed (first time unless you need to do updates)
  # spatstat + notes
install.packages("spatstat")
install.packages("devtools")
devtools::install_github('hunter-stanke/rFIA')
# observation window

# Download FIA data:
### Download FIA data (first time only):
library("rFIA")
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\ca_borders"
# First time:
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\ca_borders"
# First time, have R download FIA for CA and adjoining states.  This can take a bit of time.
ca_borders <- getFIA(states=c("CA","NV","AZ","OR"),dir=FIA_data_folder,load=FALSE)
# If you look at FIA_data_folder with your file browser you'll see all the files it downloaded.

# To reload (memory safe approach:
### To reload after downloaded the first time (memory safe approach):
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\ca_borders"
db <- readFIA(FIA_data_folder)

# Let's try to do some plot stuff.  
# Notes, db has ALL the FIA data in it.  You can see the tables by:
names(db)
# For documentation on the FIA db, see fia.fs.fed.us/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf

### Create stem maps and plot boundaries from data:
library(sf)
library(units)

# types = CENTER,SUBP,MACR

fia_to_sf <- function(db,sf,feature="PLOT",type="CENTER",working_crs="+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
{
  db_plot <- db$PLOT
  
  # Get rid of plots without points:
  db_plot <- db_plot[!is.na(db_plot$LAT) & !is.na(db_plot$LON),]
  db_plot_sf <- st_as_sf(db_plot,coords=c("LON","LAT"),crs="+proj=longlat +datum=NAD83")
  
  if(feature=="PLOT") 
  {
    return(db_plot_sf)
  }
  
  # First get coordinates in the working_crs to allow tweaks:
  db_plot_sf_repro <- st_transform(db_plot_sf,crs="+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  db_plot_sf_repro_coords <- st_coordinates(db_plot_sf_repro)
  colnames(db_plot_sf_repro_coords) <- c("PLOT_X","PLOT_Y")
  db_plot <- cbind(db_plot,db_plot_sf_repro_coords)
  
  
  db_subplot <- db$SUBPLOT
  
  # Copy over the plot coordinates:
  db_subplot_merge <- merge(db_subplot,db_plot,by.x="PLT_CN",by.y="CN",all.x=T)
  
  # We are going to only use PLOT.DESIGNCD = 1 for now,
  # other plot designs can be incorporated later.
  
  db_subplot_merge <- db_subplot_merge[db_subplot_merge$DESIGNCD==c(1,501),]
  
  # Subplot 2: 0 degrees, 120ft
  # Subplot 3: 120 degrees, 120ft
  # Subplot 4: 240 degrees, 120ft
  
  subplot_offset_m <- set_units(as_units(120,"ft"),m)
  
  subplot_distance_azimuth <- matrix(c(
    1,0,0,  
    2,subplot_offset_m,set_units(as_units(0,"degrees"),"radians"),
    3,subplot_offset_m,set_units(as_units(120,"degrees"),"radians"),
    4,subplot_offset_m,set_units(as_units(240,"degrees"),"radians")),
    ncol=3,nrow=4,byrow=T
  )
  colnames(subplot_distance_azimuth) <- c("SUBP","SUBPLOT_DIST","SUBPLOT_AZIMUTH")
  
  db_subplot_merge <- merge(db_subplot_merge,subplot_distance_azimuth,by="SUBP",all.x=T)
  db_subplot_merge$SUBPLOT_X <- db_subplot_merge$PLOT_X + db_subplot_merge$SUBPLOT_DIST*sin(db_subplot_merge$SUBPLOT_AZIMUTH)
  db_subplot_merge$SUBPLOT_Y <- db_subplot_merge$PLOT_Y + db_subplot_merge$SUBPLOT_DIST*cos(db_subplot_merge$SUBPLOT_AZIMUTH)
  
  db
  
  
  if(feature=="SUBPLOT") 
  {
    db_subplot_sf <- st_as_sf(db_subplot_merge,coords=c("SUBPLOT_X","SUBPLOT_Y"),crs=working_crs)
    
    if(type=="CENTER")
    {
      return(db_subplot_sf)
    }
    
    if(type=="MACR")
    {
      macr_radius_m <- set_units(as_units(58.9,"ft"),m)
      db_subplot_sf <- st_buffer(db_subplot_sf,macr_radius_m)
      return(db_subplot_sf)
      
    }
    
    if(type=="SUBP")
    {
      subp_radius_m <- set_units(as_units(24,"ft"),m)
      db_subplot_sf <- st_buffer(db_subplot_sf,subp_radius_m)
      return(db_subplot_sf)
      
    }
    # TODO: MICROPLOTS
  }
  
  db_tree <- db$TREE
  
  # Create a single merge field to work with:
  db_subplot_merge$PLT_SUBP_CN <- paste(db_subplot_merge$PLT_CN,db_subplot_merge$SUBP,sep="_")
  db_tree$PLT_SUBP_CN <- paste(db_tree$PLT_CN,db_tree$SUBP,sep="_")
  
  db_tree_merge <- merge(db_tree,db_subplot_merge,by="PLT_SUBP_CN",all.y=T)
  
  db_tree_merge$TREE_DIST <- as.numeric(set_units(as_units(db_tree_merge$DIST,"ft"),m))
  db_tree_merge$TREE_AZIMUTH <- as.numeric(set_units(as_units(db_tree_merge$AZIMUTH,"degrees"),"radians"))
  
  db_tree_merge$TREE_X <- db_tree_merge$SUBPLOT_X + db_tree_merge$TREE_DIST*sin(db_tree_merge$TREE_AZIMUTH)
  db_tree_merge$TREE_Y <- db_tree_merge$SUBPLOT_Y + db_tree_merge$TREE_DIST*cos(db_tree_merge$TREE_AZIMUTH)
  
  if(feature=="TREE")
  {
    db_tree_sf <- st_as_sf(db_tree_merge,coords=c("TREE_X","TREE_Y"),crs=working_crs)
    
    
  }
  
  
}


# Grab the combined plot table out of the main db (this is a convienence):
db_plot <- db$PLOT

# Now we'll do some QA/QC.
# Get rid of plots without coordinates:
db_plot <- db_plot[!is.na(db_plot$LAT) & !is.na(db_plot$LON),]
is.na(db_plot$PLOT_Y)

# Convert to an sf spatial object and reproject to a projected coordinate system, then add back the 
# project coordinate to the original db_plot:
db_plot_sf <- st_as_sf(db_plot,coords=c("LON","LAT"),crs="+proj=longlat +datum=NAD83")
working_crs="+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
db_plot_sf_repro <- st_transform(db_plot_sf,crs=working_crs)
db_plot_sf_repro_coords <- st_coordinates(db_plot_sf_repro)
colnames(db_plot_sf_repro_coords) <- c("PLOT_X","PLOT_Y")
db_plot <- cbind(db_plot,db_plot_sf_repro_coords)

# Grab the subplot table:
db_subplot <- db$SUBPLOT

# Merge the projected plot coordinates previous determined.
# PLT_CN in the subplot table is the same as the CN field in the Plot table
# See https://www.fia.fs.fed.us/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf
#   page 148.

db_subplot_merge <- merge(db_subplot,db_plot,by.x="PLT_CN",by.y="CN",all.x=T)
write.csv(db_subplot_merge, "suplot.csv")
# We are going to only use PLOT.DESIGNCD = 1 for now,
# other plot designs can be incorporated later.
# See https://www.fia.fs.fed.us/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf
#   page 937 for description.
# TODO: many of the other designs are likely useful, so add those in later. https://github.com/gearslaboratory/carb_treelist/issues/11

db_subplot_merge <- db_subplot_merge[db_subplot_merge$DESIGNCD==501,]

# Subplot 2: 0 degrees, 120ft
# Subplot 3: 120 degrees, 120ft
# Subplot 4: 240 degrees, 120ft

subplot_offset_m <- set_units(as_units(120,"ft"),m)

subplot_distance_azimuth <- matrix(c(
  1,0,0,  
  2,subplot_offset_m,set_units(as_units(0,"degrees"),"radians"),
  3,subplot_offset_m,set_units(as_units(120,"degrees"),"radians"),
  4,subplot_offset_m,set_units(as_units(240,"degrees"),"radians")),
  ncol=3,nrow=4,byrow=T
)
colnames(subplot_distance_azimuth) <- c("SUBP","SUBPLOT_DIST","SUBPLOT_AZIMUTH")

# Now offset the subplots properly based on their range and azimuth from plot center:
db_subplot_merge <- merge(db_subplot_merge,subplot_distance_azimuth,by="SUBP",all.x=T)
db_subplot_merge$SUBPLOT_X <- db_subplot_merge$PLOT_X + db_subplot_merge$SUBPLOT_DIST*sin(db_subplot_merge$SUBPLOT_AZIMUTH)
db_subplot_merge$SUBPLOT_Y <- db_subplot_merge$PLOT_Y + db_subplot_merge$SUBPLOT_DIST*cos(db_subplot_merge$SUBPLOT_AZIMUTH)

# Finally, determine the tree positions:
db_tree <- db$TREE

# Create a single merge field to work with. PLT_CN is the same as Plot's CN and Subplot's PLT_CN, 
# and SUBP is the same as Subplot's CN.
# https://www.fia.fs.fed.us/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf
#   page 180.
db_subplot_merge$PLT_SUBP_CN <- paste(db_subplot_merge$PLT_CN,db_subplot_merge$SUBP,sep="_")
db_tree$PLT_SUBP_CN <- paste(db_tree$PLT_CN,db_tree$SUBP,sep="_")
db_tree_merge <- merge(db_tree,db_subplot_merge,by="PLT_SUBP_CN",all.y=T)

# Convert range and azimuth from feet and degrees to meters and radians:
db_tree_merge$TREE_DIST <- as.numeric(set_units(as_units(db_tree_merge$DIST,"ft"),m))
db_tree_merge$TREE_AZIMUTH <- as.numeric(set_units(as_units(db_tree_merge$AZIMUTH,"degrees"),"radians"))

# Now offset the trees properly based on their range and azimuth from subplot center:
db_tree_merge$TREE_X <- db_tree_merge$SUBPLOT_X + db_tree_merge$TREE_DIST*sin(db_tree_merge$TREE_AZIMUTH)
db_tree_merge$TREE_Y <- db_tree_merge$SUBPLOT_Y + db_tree_merge$TREE_DIST*cos(db_tree_merge$TREE_AZIMUTH)

# Determine subplots and years that have trees with missing range/azimuth and remove from dataset:
db_tree_nopos_index <- is.na(db_tree_merge$TREE_X)
db_tree_merge$PLT_SUBP_CN_INVYR <- paste(db_tree_merge$PLT_SUBP_CN,db_tree_merge$INVYR,sep="_")
db_subplot_invyr_npos_index <- unique(db_tree_merge$PLT_SUBP_CN_INVYR[db_tree_nopos_index])

db_tree_merge <- db_tree_merge[!(db_tree_merge$PLT_SUBP_CN_INVYR %in% db_subplot_invyr_npos_index),]

# Some subplots have NO good years with tree positions, let's get rid of those as well:
db_subplot_ok <- unique(db_tree_merge$PLT_SUBP_CN)
db_subplot_merge <- db_subplot_merge[(db_subplot_merge$PLT_SUBP_CN %in% db_subplot_ok),]


# Convert subplot boundary and tree list to spatial objects and (optionally) export.

# Convert subplots to points, and then buffer.
db_subplot_sf <- st_as_sf(db_subplot_merge,coords=c("SUBPLOT_X","SUBPLOT_Y"),crs=working_crs)
subp_radius_m <- set_units(as_units(24,"ft"),m)
db_subplot_sf <- st_buffer(db_subplot_sf,subp_radius_m)

# Convert trees to points (note that you can get multiple years):
db_tree_sf <- st_as_sf(db_tree_merge,coords=c("TREE_X","TREE_Y"),crs=working_crs)

# Finally, we need to clip out any trees falling outside of the subplot:
# Fun with spatial subsetting in sf!
db_tree_ok <- db_tree_sf[db_subplot_sf,,]
summary(db_tree_ok)
### Save these files to visualize in a GIS:
output_folder="R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\output\\tree_map_protocol"
st_write(db_subplot_sf,dsn=file.path(output_folder,"db_subplot_sf_california_3.gpkg"),driver="GPKG")
st_write(db_tree_ok,dsn=file.path(output_folder,"db_tree_ok_california_3.gpkg"),driver="GPKG")

names(db_tree_ok)
#####Only choose marks needed for trees (DBH, SPCD, DIA)

db_subplot_ok_2015 <- db_subplot_sf[db_subplot_sf$INVYR.x==2015,]
db_tree_ok_2015 <- db_tree_ok[db_tree_ok$INVYR==2015,]

db_tree_x_Y <- as.data.frame(st_coordinates(db_tree_ok_2015))
names(db_tree_x_Y)
db_tree_ok_marks_only <- db_tree_ok_2015[,c("SPCD", "DIA", "HT")]

db_tree_ok_marks_only_sp <- as_Spatial(db_tree_ok_marks_only)
head(db_tree_ok_marks_only_sp)
i = 1
db_subplot_sf_single = db_subplot_ok_2015[i,]
plot(db_subplot_sf_single)

db_tree_ok_marks_only_single = db_tree_ok_marks_only[db_subplot_sf_single,]
plot(db_tree_ok_marks_only_single)
print(db_tree_ok_marks_only_single)

db_tree_x_y_single <- st_coordinates(db_tree_ok_marks_only_single)

db_tree_x_y_single <- as.data.frame(db_tree_x_y_single)




colnames(db_tree_x_y_single)

#########make a single point pattern

db_subplot_sf_single_owin = as.owin(db_subplot_sf_single)


db_marks_single <- as.data.frame(db_tree_ok_marks_only_single)[,c("SPCD", "DIA", "HT")]

ppp_single <-ppp(x=db_tree_x_y_single$X, y=db_tree_x_y_single$Y, marks=db_marks_single, window=db_subplot_sf_single_owin)

ppp_single$marks



#i=1 works, 3 trees
ppp_single_1 <- ppp(x=db_tree_x_y_single$X, y=db_tree_x_y_single$Y, marks=db_marks_single, window=db_subplot_sf_single_owin)
ppp_single_1$marks
ppp_single_1$window



#i = 100, second number that does not have NAs, 4 trees
ppp_single_2 <- ppp(x=db_tree_x_y_single$X, y=db_tree_x_y_single$Y, marks=db_marks_single, window=db_subplot_sf_single_owin)
ppp_single_2$marks
ppp_single_2$window

#covariates, choose random numbers (ex:temp,precipitation)
db_tree_x_y_single_ppp_1 <- as.data.frame(st_coordinates(db_tree_ok_marks_only_single))


db_tree_x_y_single_ppp_2 <- as.data.frame(st_coordinates(db_tree_ok_marks_only_single))


db_subplot_ok_2015 <- db_subplot_sf[db_subplot_sf$INVYR.x==2015,]
db_tree_ok_2015 <- db_tree_ok[db_tree_ok$INVYR==2015,]





#####converting sf objects into shapefiles to download into google earth engine

st_write(sf_trees_coords_single1,dsn=file.path(output_folder,"ppp_single_1_trees.shp"),driver="ESRI Shapefile")



st_write(sf_trees_coords_single2,dsn=file.path(output_folder,"ppp_single_2_trees.shp"),driver="ESRI Shapefile")


sf_trees_coords_single1 <- st_as_sf(db_tree_ok_marks_only_single)
sf_trees_coords_single2 <- st_as_sf(db_tree_ok_marks_only_single)


hyperframe_trees <- hyperframe(X=db_tree_ok$TREE_X, Y=db_tree_ok$TREE_Y.DBH=db_tree_ok$DIA,
       
                                            SPCD=db_tree_ok$SPCD, HT=db_tree_ok$HT)
WS <- hyperframe()
WS$larvae <- waterstriders #added a column of ppp objects
WS$experiment <- factor(1:3)
WS
