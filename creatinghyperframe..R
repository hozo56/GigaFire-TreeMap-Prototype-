plot(ppp_single_1) #3 trees
plot(ppp_single_2)#4 trees

#summary 
# summmary_ppp_single_1 <- summary(ppp_single_1)
# Marked planar point pattern:  3 points
# Average intensity 0.01785326 points per square unit
# 
# Coordinates are given to 1 decimal place
# i.e. rounded to the nearest multiple of 0.1 units
# 
# Mark variables: SPCD, DIA, HT
# Summary:
#   SPCD            DIA              HT       
# Min.   :108.0   Min.   : 5.60   Min.   :19.00  
# 1st Qu.:112.0   1st Qu.: 7.50   1st Qu.:21.00  
# Median :116.0   Median : 9.40   Median :23.00  
# Mean   :113.3   Mean   :10.77   Mean   :28.67  
# 3rd Qu.:116.0   3rd Qu.:13.35   3rd Qu.:33.50  
# Max.   :116.0   Max.   :17.30   Max.   :44.00  
# 
# Window: polygonal boundary
# single connected closed polygon with 120 vertices
# enclosing rectangle: [-9215316, -9215301] x [8549428, 8549443] units
# (14.63 x 14.63 units)
# Window area = 168.037 square units
# Fraction of frame area: 0.785

summmary_ppp_single_2 <- summary(ppp_single_2)
# 
# Marked planar point pattern:  4 points
# Average intensity 0.02380434 points per square unit
# 
# Coordinates are given to 1 decimal place
# i.e. rounded to the nearest multiple of 0.1 units
# 
# Mark variables: SPCD, DIA, HT
# Summary:
#   SPCD            DIA               HT      
# Min.   : 15.0   Min.   : 1.300   Min.   : 8.0  
# 1st Qu.: 15.0   1st Qu.: 6.100   1st Qu.:38.0  
# Median :108.5   Median :10.300   Median :50.5  
# Mean   :108.5   Mean   : 8.775   Mean   :43.0  
# 3rd Qu.:202.0   3rd Qu.:12.975   3rd Qu.:55.5  
# Max.   :202.0   Max.   :13.200   Max.   :63.0  
# 
# Window: polygonal boundary
# single connected closed polygon with 120 vertices
# enclosing rectangle: [-8942245, -8942230] x [8838323, 8838337] units
# (14.63 x 14.63 units)
# Window area = 168.037 square units
# Fraction of frame area: 0.785
#Intensity is the average density of points in an area

#Density, spatially varying density of points
ppp_single_1_dens <- density(ppp_single_1)
plot(ppp_single_1_dens)

ppp_single_2_dens <- density(ppp_single_2)
plot(ppp_single_2_dens)

#nearest neighbor distance (meters)
ppp_single_1_nn <- nndist(ppp_single_1)
#3.782824 3.782824 6.068328  

ppp_single_2_nn <- nndist(ppp_single_2)
#5.0631461 0.6874201 5.9428309 0.6874201

#covariates
avgtemp_single_ppp_1 <- 
prec_single_ppp_2 <- 

avgtemp_single_ppp_2 <- 70.75141144
prec_single_ppp_2 <- 55.5191299




with(ppp_single_2, avgtemp_single_ppp_2)

simba
simba[1]$Points
simba

pyramidal
class(pyramidal)
names(pyramidal)


pyramidal$Neurons
pyramidal[[1]]
class(pyramidal$group)

class(pyramidal$Neurons)

pyramidal$Neurons[[1]]

length(pyramidal$Neurons)

length(pyramidal$group)


ppp_list <- list(ppp_single_1,ppp_single_2)

temp <- c(50.75,65.75)

prec <- c(85.75,56.42)


myhyperframe <- hyperframe(trees=ppp_list, temp=temp, prec=prec)

myhyperframe$trees$`1`

class(ppp_list)

myhyperframe$trees

plot(myhyperframe)

print.hyperframe(myhyperframe)

#Computing hyperframe 16.5
n_points <- with(myhyperframe, npoints(trees))

D <- with(myhyperframe, distmap(trees))


dist <- with(myhyperframe, nndist(trees))


#Empirical K function, analyzing spatial correlation in point patterns
k_func <- with(myhyperframe, Kest(trees))
plot(k_func)

#intensity
lambda <- with(myhyperframe, intensity(trees))

#trend formula for subset tree patterns
mppm(trees ~ 1, myhyperframe)
# Error in (function (data, dummy, method = c("grid", "dirichlet"), ...)  : 
#             data pattern is not multitype

mppm(Points ~ 1, simba)
# Point process model fitted to 10 point patterns
# Call: mppm(Points ~ 1, simba)
# Log trend formula: ~1
# 
# Fitted trend coefficients:
#   (Intercept) 
# 4.19419 
# 
# Interaction for all patterns:	Poisson process
