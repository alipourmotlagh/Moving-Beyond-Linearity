#GAM
library(MASS)
head(mcycle)

#A data frame giving a series of measurements of head acceleration in a 
#simulated motorcycle accident, used to test crash helmets.

# Examine the mcycle data frame
head(mcycle)
plot(mcycle)

# Fit a linear model
lm_mod <- lm(accel~times, data = mcycle)

# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)


# Load mgcv
library(mgcv)

# Fit the model
gam_mod <- gam(accel ~ s(times), data = mcycle)

# Plot the results
plot(gam_mod, residuals = TRUE, pch = 1)


# Extract the model coefficients
coef(gam_mod)


summary(gam_mod)


# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# Visualize the GAMs
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1,col='darkred')
plot(gam_mod_k20, residuals = TRUE, pch = 1,col='darkgreen')





# Extract the smoothing parameter
gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
gam_mod$sp

# Fix the smoothing parameter at 0.1
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)




# Fit the GAM
gam_mod_sk <- gam(accel~s(times ,k=50),data=mcycle,sp=0.0001)

#Visualize the model
plot(gam_mod_sk, residuals = TRUE, pch = 1)



#----------------------------
#we can use Cars93 dataset instead
rm()
library(ggplot2)
# Examine the data


library(gamair)
# Fit the model
mod_city <- gam(city.mpg ~ s(weight)+s(length)+s(price), 
                data = mpg, method = "REML")

# Plot the model
plot(mod_city, pages = 1)



library(mgcv)

# Fit the model
mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) +fuel+drive+style, 
                 data = mpg, method = "REML")

# Plot the model
plot(mod_city2, all.terms = TRUE, pages = 1)


# Fit the model
mod_city3 <- gam(city.mpg ~ s(weight,by=drive)+s(length,by=drive)+s(price,by=drive)+drive, 
                 data = mpg, method = "REML")

# Plot the model
plot(mod_city3, pages = 1)

#interpreting and visualizing


# Fit the model
mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")

# View the summary
summary(mod_city4)



# Fit the model
mod <- gam(accel ~ s(times), data = mcycle, method = "REML")

# Make the plot with residuals
plot(mod, residuals=TRUE)

# Change shape of residuals
plot(mod, residuals=TRUE,pch=1,cex=1)


# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")

# Plot the price effect
plot(mod,select =  3)

# Plot all effects
plot(mod,pages =  1,all.terms = TRUE)


# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")

# Plot the weight effect with colored shading
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink")

# Make another plot adding the intercept value and uncertainty
plot(mod, select = 1, seWithMean = TRUE, shift = coef(mod)[1])



X1$f0<-X2$f0
X1$f1<-X2$f1
X1$f2<-X2$f2
dat<-X1
library(data.table)
fwrite(dat,'datafortest.csv')
dat<-fread('datafortest.csv')


# Fit the model
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")

# Run the check function
par(no.readonly = TRUE)
par(mfrow = c(2,2))
gam.check(mod)


# Fit the model
mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = dat, method = "REML")

# Check the diagnostics
gam.check(mod)

# Refit to fix issues
mod2 <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 10) + s(x3, k = 3),
            data = dat, method = "REML")

# Check the new model
gam.check(mod2)







# Fit the model
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight), 
           data = mpg, method = "REML")

# Check overall concurvity
concurvity(mod)


# Check pairwise concurvity
concurvity(mod,full=FALSE)


#-------------------------------
# third chapter
library(sp)

data(meuse)


# Fit the 2-D model
mod2d <- gam(cadmium ~ s(x, y), data = meuse, method = "REML")

# Inspect the model
summary(mod2d)
coef(mod2d)


#There are 29 coefficients in s(x, y). 
#Surfaces require more basis coefficients to construct 
#than single variable smooths. That's why the default 
#value for k is high for 2-D smooths.

# Fit the model
mod2da <- gam(cadmium ~ s(x, y) + s(elev)+s(dist), 
              data = meuse, method = "REML")

# Inspect the model
summary(mod2da)


plot(mod2da,pages=1)


# 3D surface plot
plot(mod2da, scheme=1, pages = 1)


# Colored heat map
plot(mod2da, scheme=2, pages=1)



par(mfrow = c(1,1))
# Make the perspective plot with error surfaces
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp")


#with -2 and 2 confifece intervals

vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp",se = 2)


#rotate 135 degree

vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp",se = 2,theta = 135)


# Make plot with 5% extrapolation
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.05)

# Overlay data
points(meuse)

# Make plot with 10% extrapolation
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.25)

# Make plot with 25% extrapolation
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.25)

# Fit a model with separate smooths for each land-use level
mod_sep <- gam(copper~s(dist,by=landuse)+landuse,data=meuse,method='REML')

# Examine the summary
summary(mod_sep)

# Fit a model with a factor-smooth interaction
mod_fs <- gam(copper~s(dist,landuse,bs='fs'),data=meuse,method='REML')
# Examine the summary
summary(mod_fs)

# Plot both the models with plot()
plot(mod_sep,pages=1)
plot(mod_fs,pages=1)


# Plot both the models with vis.gam()
vis.gam(mod_sep, view = c("dist", "landuse"), plot.type = "persp")
vis.gam(mod_fs, view = c("dist", "landuse"), plot.type = "persp")



# Fit the model
tensor_mod <- gam(cadmium~te(x,y,elev),data=meuse,method='REML')

# Summarize and plot
summary(tensor_mod)
plot(tensor_mod,pages=1)


# Fit the model
tensor_mod2 <- gam(cadmium~s(x,y)+s(elev)+ti(x,y,elev),data=meuse,method='REML')


# Summarize and plot
summary(tensor_mod2)
plot(tensor_mod2, pages = 1)

install.packages('Information')
library(Information)
data(csale)

# Fit a logistic model
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) + 
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) +
                  s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# View the summary
summary(log_mod2)
