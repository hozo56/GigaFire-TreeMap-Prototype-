+### Get packages installed (first time unless you need to do updates)
  # spatstat + notes
install.packages("spatstat")
install.packages("devtools")
devtools::install_github('hunter-stanke/rFIA')
# observation window

# Download FIA data:
### Download FIA data (first time only):
library("rFIA")
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\02_28_new_approach\\ca_adjoining_states_data"
# First time:
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\02_28_new_approach\\ca_adjoining_states_data"
# First time, have R download FIA for CA and adjoining states.  This can take a bit of time.
ca_adj_states <- getFIA(states=c("AZ","CA","NV","OR"),dir=FIA_data_folder,load=FALSE)
# If you look at FIA_data_folder with your file browser you'll see all the files it downloaded.

# To reload (memory safe approach:
### To reload after downloaded the first time (memory safe approach):
FIA_data_folder <- "R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\02_28_new_approach\\ca_adjoining_states_data"
db <- readFIA(FIA_data_folder)

# Let's try to do some plot stuff.  
# Notes, db has ALL the FIA data in it.  You can see the tables by:
names(db)
# For documentation on the FIA db, see fia.fs.fed.us/library/database-documentation/current/ver90/FIADB%20User%20Guide%20P2_9-0-1_final.pdf

### Create stem maps and plot boundaries from data:
design_cd_table <- table(db$PLOT$DESIGNCD)
designcd_ca_adj <- write.csv(design_cd_table,"list_of_designcds.csv")
