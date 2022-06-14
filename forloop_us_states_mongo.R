###selecting plot,subplot,tree csvs from folder for each us state

us_plots <- dir(path=".",pattern="_PLOT.csv")

us_subplots <- dir(path=".",pattern="_SUBPLOT.csv")

us_trees <- dir(path=".",pattern="_TREE.csv")

us_conditions_trees <- dir(path=".",pattern="_COND.csv")


us_plots <- us_plots[-c(seq(1,100,2))]

us_subplots <- us_subplots[-c(seq(2,100,2))]

us_conditions_trees <- us_conditions_trees[-c(seq(2,100,2))]


#####how to remove data from mongo
#mongoConn$drop()

###importing data into mongo
library(mongolite)

mongoConn <- mongo("all_states_plots", "states", "mongodb://localhost:27017")
mongoConn1 <- mongo("all_states_subplots", "states", "mongodb://localhost:27017")
mongoConn2 <- mongo("all_states_trees", "states", "mongodb://localhost:27017")
mongoConn3 <- mongo("all_states_conditions", "states", "mongodb://localhost:27017")

for (i in 1:50){
  us_fia_plot <- read.csv(us_plots[i])
  mongoConn$insert(us_fia_plot)

  us_fia_subplot <- read.csv(us_subplots[i])
  mongoConn1$insert(us_fia_subplot)

  us_fia_tree <- read.csv(us_trees[i])
  mongoConn2$insert(us_fia_tree)
  
  us_con_tree <- read.csv(us_conditions_trees[i])
  mongoConn3$insert(us_con_tree)
  print(i)
  
}


#####Joining unique plots of CA with All States Plots and Look up table from TreeMap

tl_look_up <- read.csv("R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\tree_map_tables\\TL_CN_Lookup.csv")
tree_look_up <- read.csv("R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\tree_map_tables\\Tree_table_CONUS.csv")
unique_plots <- read.csv("R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\tree_map_tables\\unique_plots_cali.csv")


state_plots_collection = mongo(collection="all_states_plots", db="states", 
                               url="mongodb://localhost:27017")
state_plots_collection_unique <- state_plots_collection$insert(unique_plots)

head(state_plots_collection_unique$nInserted)

plot_3726 <- mongoConn$find(query = '{"PLOT": 3726}')

existing_plots <- data.frame(plotId=0, MININVYR=0, DESIGNCD=0, MAXINYYR=0, SURVLENGTH=0, Unique_PID=0) # add unique plot column
counter <- 0

df_cn = #queried lookup table for CNs using Unique_PIDs
  
for (i in 1:dim(unique_plots)[1]){
  plotId <- unique_plots[i,1]
  plt_query <- paste0("{\"PLOT\": ", plotId, "}") #replace with CN
  unique_plot_ids <- mongoConn$find(query = plt_query)
  if (dim(unique_plot_ids)[1] < 1){
    print(paste0("Missing Plot id: ", plotId))
  } else {
    print(paste0("Plot id: ", plotId, " exists."))
    counter  <- counter + 1
    invyr <- range(unique_plot_ids$INVYR)
    existing_plots[counter,] <-  c(unique_plot_ids$PLOT[1], invyr[1], unique_plot_ids$DESIGNCD[1], 
                                   invyr[2], diff(invyr))
  }
}

head(existing_plots)


summary(existing_plots)
hist(existing_plots$MEASYEAR)
hist(existing_plots$DESIGNCD)

table_unique_plots <- write.csv(existing_plots, "all_states_unique_plots.csv")
designcd_unique_plots<- table(existing_plots$DESIGNCD)

design_cd_freq <- count(existing_plots, "DESIGNCD")
inv_yr_freq <- count(existing_plots,"INVYR")
plot_id_freq <- count(existing_plots,"plotId")
resampling_period_freq <- count(existing_plots,"SURVLENGTH")
table(existing_plots$SURVLENGTH)

designcd_freq <- write.csv(design_cd_freq, "design_cd_freq.csv")
invyr_freq <- write.csv(inv_yr_freq, "inv_yr_freq.csv")
plotid_freq <- write.csv(plot_id_freq,"plotid_freq.csv")


###############load tree table data and unique plots from Adriano


df_tree_table <- read.csv("R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\tree_map_tables\\Tree_table_CONUS.csv")
df_unique_tl_id <- read.csv("R:\\SCRATCH\\taylorrbrown\\gigafire_treelist\\tree_map_tables\\unique_plots_cali.csv")

unique_id_list <- df_unique_tl_id$Unique_PID
unique_id_list[2]
df_CN_invyr <- data.frame(CN=0, INVYR=0, DESIGNCD=0, SICOND=0, STDAGE=0, Unique_PID=0) #restart forloop need unique PID

for(i in 1:length(unique_id_list)) {
  df_CN_subset <- subset(df_tree_table, tl_id == unique_id_list[i], select=c(CN, INVYR)) #select tl id
  
  plot_query <- paste0('{"CN":', df_CN_subset[1,][1], ',"INVYR":', df_CN_subset[1,][2], '}')
  plot_details <- mongoConn$find(query = plot_query)
  cond_query <- paste0('{"PLT_CN":', df_CN_subset[1,][1], ',"INVYR":',df_CN_subset[1,][2], '}')
  cond_details <- mongoConn3$find(query = cond_query)
  
  df_CN_invyr[i,1:6] <- c(df_CN_subset[1,][1], df_CN_subset[1,][2], null_pass(plot_details$DESIGNCD), 
                          null_pass(cond_details$SICOND), null_pass(cond_details$STDAGE), unique_id_list[i]) #unique plot id
  #print(i)
}

null_pass <- function(value){
  if (is.null(value)){
    result <- NA
  } else{
    result <- value
  }
  return(result)
}


head(df_CN_invyr)

summary(df_CN_invyr)
freq_designcd <- count(df_CN_invyr$DESIGNCD)
freq_invyr <- count(df_CN_invyr$INVYR)

key_file_table <- write.csv(df_CN_invyr, "df_cn_invyr.csv")

#

plot_query <- paste0('{"CN":', df_CN_invyr[1,][1], ',"INVYR":', df_CN_invyr[1,][2], '}')
plot_details <- mongoConn$find(query = plot_query)

cond_query <- paste0('{"PLT_CN":', df_CN_invyr[1,][1], ',"INVYR":', df_CN_invyr[1,][2], '}')
cond_details <- mongoConn3$find(query = cond_query)




###importing data into mongo
library(mongolite)

mongoConn1_apr25 <- mongo("all_states_plots", "states", "mongodb://localhost:27017")
mongoConn2_apr25 <- mongo("all_states_subplots", "states", "mongodb://localhost:27017")
mongoConn3_apr25 <- mongo("all_states_trees", "states", "mongodb://localhost:27017")
mongoConn4_apr25 <- mongo("all_states_conditions", "states", "mongodb://localhost:27017")

for (i in 1:50){
  us_fia_plot <- read.csv(us_plots[i])
  mongoConn1_apr25$insert(us_fia_plot)
  
  us_fia_subplot <- read.csv(us_subplots[i])
  mongoConn2_apr25$insert(us_fia_subplot)
  
  us_fia_tree <- read.csv(us_trees[i])
  mongoConn3_apr25$insert(us_fia_tree)
  
  us_con_tree <- read.csv(us_conditions_trees[i])
  mongoConn4_apr25$insert(us_con_tree)
  print(i)
  
}














