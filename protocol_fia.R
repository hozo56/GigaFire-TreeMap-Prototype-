#####STEP 1: FIA -> sf

###FIA subplots -> subplot points + attributes
###Temporal filtering -> only subplot + treelist in a given year

summary(db_tree_ok$INVYR) #summary of years FIA plots surveyed
num_surveyed_plots <- table(db_tree_ok$INVYR) #count number of times plots were surveyed that year
plot(num_surveyed_plots) #figure to show occurrences of surveyed #1999 surveyed most
#1999 most occurrences but too old, 2014 has the most in the last decade

####Subsetting subplots and treelist for 2014 
subplots_2014 <- subset(db_subplot_sf, INVYR.x=="2014")
treelist_2014 <- subset(db_tree_ok,INVYR=="2014")
