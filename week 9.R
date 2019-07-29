#The Base Plotting System
#output simple scatter plot
data(airquality)
with(airquality, {
  plot(Temp, Ozone)
  lines(loess.smooth(Temp, Ozone))
})

#typical base plot is constructed with the following code
#loading car data
data(cars)
cars

# Create the plot / draw canvas
with(cars, plot(speed, dist))

# Add annotation
title("Speed vs. Stopping distance")

#The Lattice System: Implimented in lattice package
#Load the library lattice to use lattice plotting systems
library(lattice)

#example of a lattice plot that looks at the relationship between life expectancy and income and how that relationship varies by region in the United States.
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

# The ggplot2 System
#Installing package
install.packages("ggplot2")
#Load ggplot library
library(ggplot2)

#A sample ggplot package:
data(mpg)
qplot(displ, hwy, data = mpg)

#https://bookdown.org/rdpeng/exdata/graphics-devices.html#
#process of creating a plot using plot() function
# Make plot appear on screen device
with(faithful, plot(eruptions, waiting)) 
# Annotate with a title
title(main = "Old Faithful Geyser data") 

# Here's an example of how to make a plot using this second approach. In this case we make a plot that gets saved in a PDF file
# Open PDF device; create 'myplot.pdf' in my working directory
pdf(file = "myplot.pdf") 

# Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))

# Annotate plot; still nothing on screen
title(main = "Old Faithful Geyser data")

# Close the PDF file device
dev.off()

#Copying a plot
library(datasets)

#Create plot on screen device
with(faithful, plot(eruptions, waiting))  

#Add a main title
title(main = "Old Faithful Geyser data")  

#Copy my plot to a PNG file
dev.copy(png, file = "geyserplot.png")  

#Don't forget to close the PNG device!
dev.off()
