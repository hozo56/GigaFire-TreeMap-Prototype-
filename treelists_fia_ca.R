### Get packages installed (first time unless you need to do updates)
# spatstat + notes
install.packages("spatstat")
install.packages("devtools")
devtools::install_github('hunter-stanke/rFIA')
library("devtools")
library("spatstat")
### Download FIA data (first time only):
library("rFIA")
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\data\\ca_subplot_fia"
# First time, have R download FIA for CA and adjoining states.  This can take a bit of time.
ca <- getFIA(states="CA",dir=FIA_data_folder,load=FALSE)
# If you look at FIA_data_folder with your file browser you'll see all the files it downloaded.

### To reload after downloaded the first time (memory safe approach):
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\data\\ca_subplot_fia"
db <- readFIA(FIA_data_folder)

# Notes, db has ALL the FIA data in it.  You can see the tables by:
names(db)
# For documentation on the FIA db, see fia.fs.fed.us/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf

### Create stem maps and plot boundaries from data:
library(sf)
library(units)

# Grab the combined plot table out of the main db (this is a convienence):
db_plot <- db$PLOT
#st_write(db_plot,dsn=file.path(output_folder, "db_plot.gpkg"),driver="GPKG")
#st_write(db_subplot,dsn=file.path(output_folder, "db_subplot.gpkg"),driver="GPKG")


# Now we'll do some QA/QC.
# Get rid of plots without coordinates:
db_plot <- db_plot[!is.na(db_plot$LAT) & !is.na(db_plot$LON),]

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

# We are going to only use PLOT.DESIGNCD = 1 for now,
# other plot designs can be incorporated later.
# See https://www.fia.fs.fed.us/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf
#   page 937 for description.
# TODO: many of the other designs are likely useful, so add those in later. https://github.com/gearslaboratory/carb_treelist/issues/11

#db_subplot_merge <- db_subplot_merge[db_subplot_merge$DESIGNCD==1,]

db_subplot_merge <- db_subplot_merge[db_subplot_merge$DESIGNCD==1,]
write.csv(db_subplot_merge, file = "subplotmerge.csv")
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
class(db_tree_sf)
# Finally, we need to clip out any trees falling outside of the subplot:
# Fun with spatial subsetting in sf!
db_tree_ok <- db_tree_sf[db_subplot_sf,,]

### Try making ppp objects:
library(maptools)
library(spatstat)
tree_points <- as.ppp(db_tree_ok) 
as.data.frame(db_tree_ok)
class(tree_points)
summary(tree_points)
marks(tree_points)
#marked planar point pattern
marks(finpines)
as.data.frame(finpines)



#####Observation Window 
coords = as.data.frame(st_coordinates(db_tree_ok))

bb = owin(xrange=c(min(coords$X),max(coords$X)), yrange=c(min(coords$Y),max(coords$Y)))
          
####Original 
marks_tree <- ppp(coords$X,coords$Y, window=bb, marks=db_tree_ok)
marks(marks_tree)
class(marks_tree)
csv_marked_data <- write.csv(marks_tree, file = "marked_fia_data.csv")
marks_tree_as <- as.ppp(db_tree_ok)
marks(marks_tree_as)

###Data Frame for FIA data
df_marks_trees <- as.data.frame(marks_tree)
class(df_marks_trees)


#Removing extra marks #subset
df_rm_marks <- df_marks_trees[, c("x","y","SPCD","DIA")]
ppp_df_treesmarked <- as.ppp(df_rm_marks, W = bb)
marks(ppp_df_treesmarked)

#####Remove all attributes for Google Earth Engine
###Create a shapefile for trees

df_rm_all_marks <- df_marks_trees[, c("x","y")]
coordinates(df_rm_all_marks) <- df_marks_trees[, c("x","y")]
proj4string(df_rm_all_marks) <- working_crs
writeOGR(df_rm_all_marks, dsn = "no_marks_fia.shp", layer = "filename", driver = "ESRI Shapefile")


###Create a shapefile for subplots
dim(db_subplot_sf)
rm_extra_atts <- db_subplot_sf[, -c(1:110)]
final_rm_atts <- rm_extra_atts[, c(2:3)]
st_write(final_rm_atts, dsn = "no_marks_fia_sub_cent.shp", layer = "filename", driver = "ESRI Shapefile")

csv_no_marks <- write.csv(df_rm_all_marks, file = "no_marks_fia.csv")
shp_subplot <- shapefile(db_tree_ok, filename = "no_marks_fia.shp")
class(df_rm_all_marks)


### Save these files to visualize in a GIS:
output_folder="R:\\projects\\gigafire_treelist\\data\\vector\\rFIA\\"
st_write(db_subplot_sf,dsn=file.path(output_folder,"db_subplot_sf.gpkg"),driver="GPKG")
st_write(db_tree_ok,dsn=file.path(output_folder,"db_tree_ok.gpkg"),driver="GPKG")


###Mongo DB 
#write.csv(db_subplot_sf, "Johanson_FIA_subplots_mongo.csv")

#st_write(db_subplot_sf,dsn=file.path(output_folder,"db_subplot_sf.gpkg"),driver="GPKG")





