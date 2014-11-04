######################################
## 3. Plotting & Application        ##
## 3.1 Basic Intro to Graphic Tools ##
######################################

# Recap
# plot() - scatter plot
# boxplot() - side-by-side box plot
# hist() - histogram
# mosaicplot() - mosaic plot

#=====================================================================
#-------------------------------#
# Arguments for plot(x, y, ...) #
#-------------------------------#
# Data for demo: cars (distance taken to stop ~ speed of cars)

# What does it mean by argument "..."?
# It means you can borrow commonly used arguments within certain range.
# Here, "..." could be graphical parameters (the most used method)
# Most popular graphical parameters:
# type / main / sub / xlab / ylab 

plot(cars)

# Default method
plot(cars, type ="h", main = "Distance ~ Speed", sub = "Check It Out!",
     xlab = "Speed (miles/h)", ylab = "Stopping Distance (feet)")
# type - what type of plot should be drawn
#        "h" for "histogram" like vertical lines
# main - overall title for the plot
# sub - sub title for the plot
# xlab / ylab - a title for x / y axis

# Default method for class "formula"
plot(cars$dist~cars$speed, type ="b", main = "Distance ~ Speed",
     xlab = "Speed (miles/h)", ylab = "Stopping Distance (feet)")
# type = "b": both line and points for the plot
# Mind the order of x and y axes labels

abline(lm(cars$dist~cars$speed)) 
# add straight line(s) through the current plot

# Try to do scatter polot for built-in dataset "cats"!

#=====================================================================
#-------------------------------#
# Arguments for boxplot(x, ...) #
#-------------------------------#
# Data for demo: mtcars

# Default method
boxplot(mpg ~ cyl, data = mtcars)
boxplot(mpg ~ cyl, data = mtcars, range = 1.5)
# range: how far the plot whiskers extend out from the box
# Default value of range: 1.5 times interquantile range
# Compare default value (1.5) with range = 0.5
boxplot(mpg ~ cyl, data = mtcars, range = 0.5)

boxplot(mpg ~ cyl, data = mtcars, outline = F,
        names = c("4 cylinders","6 cylinders","8 cylinders"),
        boxwex = 0.5, staplewex = 1, col = c("yellow","orange","brown"))
# ouline: FALSE means removing outliers. Default value is TRUE
# names: add group lables
# boxwex: width of boxes, default value = 0.8
# staplewex: staple line width expansion, proportional to box width;
#            default value = 0.5.
#            i.e. length of boxwex = 0.5, if staplewex = 1
#                 then length of staplewex is 0.5
# col: color of boxes, you can specify one color for all, 
#      or assign each boxes a color

#---------------------------------------------------------------------
boxplot(mpg ~ cyl, data = mtcars, horizontal = T)
# horizontal: TRUE means plotting horizontally. Default value = FALSE

#---------------------------------------------------------------------
# Argument "subset", "add" & "at"
# subsetting the cars the number of cylinders of which are larger than 4

# plot the boxplot for fuel economy per cylinder numbers
# keep only data cars with automatic transmission (am == 0)
boxplot(mpg ~ cyl, data = mtcars, subset = am == 0)

# Plot the cars of automatic transmission 
# at the position 0.2 unit LEFT to where the center of each box supposed to be
# it's obliged to specify "xlim" (limit of x-axis) before using "at"!
boxplot(mpg ~ cyl, data = mtcars, boxwex = 0.25, at = 1:3 - 0.2,
        subset = am == 0, col = "yellow", 
        main = "Fuel Economy per Cylinder Number",
        xlab = "Number of Cylinder", ylab= "Fuel Economy (Miles/Gallon)",
        xlim = c(0.5, 3.5), ylim = c(0, 40))
# xlim: specify the length of x-axis standard unit (not by the value of x)
# ylim: specify the length of y-axis by the value of y

# Add the boxes for cars of manual transmission
# at the position 0.2 unit RIGHT to where the center of each box supposed to be 
# using same "xlim" and "ylim"
boxplot(mpg ~ cyl, data = mtcars, boxwex = 0.25, 
        add = TRUE, at = 1:3 + 0.2,
        subset = am == 1, col = "orange", 
        main = "Fuel Economy per Cylinder Number",
        xlab = "Number of Cylinder", ylab= "Fuel Economy (Miles/Gallon)",
        xlim = c(0.5, 3.5), ylim = c(0, 40))

#---------------------------------------------------------------------
# Add legend to a plot
# legend(x, y, legend, fill = NULL)
legend(2, 5, c("Auto Transmission", "Manual Transmission"),
       fill = c("yellow", "orange"))
# x, y: position (of top-left corner) of the legend box
# x corresponds to the value of xlim
# y corresponds to the value of ylim
# Try to change the coordinates of x and y!

# Try to do boxplot for built-in dataset "Orange"!

#=====================================================================
#----------------------------#
# Arguments for hist(x, ...) #
#----------------------------#
# Data for demo: SP500 (A vector of Standard & Poors 500 index in the 1990's)
#                2780 data points, all the trading days from 1991 to 1999

# Default Method
hist(SP500)

# Specify the breaks
hist(SP500, breaks = 50)
# breaks: no. of cells

hist(SP500, breaks = 50, freq = F)
# freq: Defaults to TRUE if and only if breaks are equidistant 
#       if freq = TRUE: hist() represents frequencies
#       else: hist() represents densities

#---------------------------------------------------------------------
hist(SP500, breaks = 50, freq = F, right = F, density = 12, angle = 30,
     border = "darkblue", axes = F)
# right: Default value is TRUE, if True:
#        histogram cells are right-closed (left open) intervals (i.e. (-5,0])
# desity: Density of shade lines. Default value is NULL, meaning no shading
# angle: Slope of shade lines, given as angle in degrees (counter-colckwise)
#        Default value is 45
# border: color of border of bars
# axes: Default value is TRUE, indicating whether plotting the axes

#---------------------------------------------------------------------
# you could also specify the color of bars/cells
hist(SP500, breaks = 50, col = "orange")
hist(SP500, breaks = 50, col = rainbow(30))

hist(SP500, breaks = 50, plot = F)
# plot: To plot or deliver the statistics of this plot without plotting
#       Default value is TRUE. If plot = F, it returns:
#       break point, counts per bar/cell, density per bar/cell,
#       means of each break interval, label of x-axis, etc.

#---------------------------------------------------------------------
# Optional: breaks
# argument "breaks" is more complicated than this demo, explore it!

# Try to do histogram for any variable in dataset "rock"

#=====================================================================
#----------------------------------#
# Arguments for mosaicplot(x, ...) #
#----------------------------------#
# Data for demo: mtcars (almighty dataset!)
# Data for demo: HairEyeColor 
#                Distribution of hair and eye color and sex 
#                in 592 statistics students

# Default method
# str(mtcars)
car.t <- table(mtcars[,c(10,2)])  # no. of cylinder & no. of forward gears
mosaicplot(car.t)
# A contingency table is very necessary before drawing mosaicplot

# Default method for formula
mosaicplot(gear ~ carb, data = mtcars)
mosaicplot(~ gear + carb, data = mtcars)
# carb: no. of carburetors

# More dimension is even acceptable
mosaicplot(~ vs + am + cyl, data = mtcars)
# vs: V/S - 1: Vehicle; 0: Sport Car 
# try to realize the plot of 3 variables above using table() & mosaicplot()

# What does it mean by the dashed line in some pieces of mosaic?
nrow(subset(mtcars, am==0 & cyl==4 & vs == 0))
# It means there is no observation for the combination

#---------------------------------------------------------------------
# familiar parameters...
mosaicplot(~ vs + am + cyl, data = mtcars, main = "CARS PARAMETERS", sort = NULL,
           sub = NULL, xlab = "Vehicle/Sport Car", ylab = "Transmission Method",
           border = "darkgreen", col = T, cex = 0.5, off = c(10,2,3))
# sort: Sort the category within each variable. Vector ordering of the variables
#       Default value is 1:length(dim(x)), x stands for the data series you plot
# col: T stands for "variety of color turns on". "color =" is also accepted
#      Default value is FALSE. You can also specify the color per dimension
# cex: Font size for axis annotation. "cex.axis = " is also accepted
#      Default value is 0.66.
# off: Stands for "offset". Used to adjust the distance among dimensions
#      i.e. in this case:
#      10: distance between the 2 categories of variable "vs"
#      2: distance between the 2 categories of variable "am"
#      3: distance between the each adjacent pair of 3 categories of "cyl"

#---------------------------------------------------------------------
# specicial parameter
mosaicplot(HairEyeColor, shade = TRUE, cex = 0.5)
# shade: extend the plot comparing different categories with model statistics

# How to read this plot?
# Independence model of hair and eye color and sex.  Indicates that
# there are more blue eyed blonde females than expected in the case
# of independence and too few brown eyed blonde females.
# The model borrowed is logic regression instead of linear regression. FYI.

# Try to do mosaic plot for any variable in dataset "Titanic"/"Shuttle"

#=====================================================================
#-----------------#
# Plot Annotation #
#-----------------#

# title(main, sub, xlab, ylab, line, outer,...)
plot(cars,
     xlab = list("Speed", font = 7), 
     ylab = list("Stopping Distance", font = 7))
title(main = list("Stopping Distance versus Speed", cex = 1.5,
                  col = "purple", font = 3),
      sub = list("Ohlala this is the final plot...", cex = 0.8,
                 col = "darkgrey", font = 2))

# recall legend()

# Many parameters can be borrowed everywhere while plotting
# Try to plot based on any built-in dataset. You can find the list by data()

#---------------------------------------------------------------------
# Session 3.1 Review
# - Arguments for plot(x,...)
#   * Default method
#   * main, sub, xlab, ylab
#   * abline()
# - Arguments for boxplot(x, ...)
#   * Default method
#   * range, outline, boxwex, staplewex, col
#   * horizontal
#   * subset, add, at
#   * xlim, ylim
# - Arguments for hist(x, ...)
#   * Default method
#   * breaks, freq
#   * right, shade, angle, border, axes
#   * col, rainbow(), plot
# - Arguments for mosaicplot(x, ...)
#   * Default method
#   * sort, color/col, cex.axis/cex, off
#   * shade; how to read the plot with shade = T?
# - Plot Annotation
#   * title()
#   * legend()
#---------------------------------------------------------------------

# finished on Tue. 11/4/2014
