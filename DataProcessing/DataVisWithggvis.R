#Change the sample code on the right to plot the disp variable of mtcars on the x axis. If you are not familiar with the mtcars database, find out by typing 
#?mtcars in the console.
library(ggvis)
#Notice the use of piping operator(%>%) from maggitr package and '~' operator
#1) ~ is used with a variable always for ggvis
#2) := is the settings operator which can be used for purposes like filling color
#3) Hence '=' maps the values, used with variables. ':=' sets the values for color
#4) in ggvis(), first variable is x and second is y
mtcars %>% ggvis(~disp,~mpg) %>% layer_points() #layer_points ensure that scatter plots are given for the graph
# Change the code below to make a graph with red points
mtcars %>% ggvis(~wt, ~mpg, fill := "red") %>% layer_points()

# Change the code below draw smooths instead of points
mtcars %>% ggvis(~wt,~mpg) %>% layer_smooths()

# Change the code below to make a graph containing both points and a smoothed summary line
mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>% layer_smooths()
# Adapt the code: show bars instead of points
pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars()

# Adapt the code: show lines instead of points
pressure %>% ggvis(~temperature, ~pressure) %>% layer_lines()

# Extend the code: map the fill property to the temperature variable
pressure %>% ggvis(~temperature, ~pressure,fill = ~temperature) %>% layer_points()

# Extend the code: map the size property to the pressure variable
pressure %>% ggvis(~temperature, ~pressure, size = ~pressure) %>% layer_points()

# Rewrite the code with the pipe operator
#layer_points(ggvis(faithful, ~waiting, ~eruptions))
faithful %>% ggvis(~waiting,~eruptions) %>% layer_points()

# Modify this graph to map the size property to the pressure variable
pressure %>% ggvis(~temperature, ~pressure, size = ~pressure) %>% layer_points()

# Modify this graph by setting the size property
pressure %>% ggvis(~temperature, ~pressure, size := 100) %>% layer_points()

# Fix this code to set the fill property to red
pressure %>% ggvis(~temperature, ~pressure, fill := "red") %>% layer_points()

#Add code to the first command:
#Map size to the eruptions variable.
#Set the opacity to 0.5 (50%), the fill to "blue", and the stroke to "black".
faithful %>% ggvis(~waiting, ~eruptions, size = ~eruptions, opacity := 0.5, fill := "blue",
stroke := "black") %>% layer_points()

#Add code to the second command:
# Map fillOpacity to the eruptions variable.
# Set size to 100, the fill to "red", the stroke to "red", and the shape to "cross".
faithful %>% ggvis(~waiting, ~eruptions, fillOpacity = ~eruptions,
size := 100, fill := "red", stroke := "red", shape := "cross") %>% layer_points()

#Update the code for the graph on the right:
#Use the lines mark instead of the points mark.
#Set the line stroke to "red"; this controls the color.
# Set the strokeWidth to 2 pixels; this controls the line width.
# Set the strokeDash to 6 pixels; this controls the length of the dashes.
pressure %>% ggvis(~temperature, ~pressure,
stroke := "red",strokeWidth := 2,
strokeDash := 6) %>% layer_lines()

#Change the ggvis command:
#    Use layer_paths() instead of layer_lines(): this will correctly plot the map of Texas.
#    Set the fill property "darkorange". This will color your map!
texas %>% ggvis(~long, ~lat, fill := "darkorange") %>% layer_paths()

#compute_model_prediction() is a useful function to use with line graphs. It takes a data frame as input and returns a new data frame as output. The new data frame will contain the x and y values of a line fitted to the data in the original data frame.
#The code below computes a line that shows the relationship between the eruptions and waiting variables of the faithful data set.

faithful %>% compute_model_prediction(eruptions ~ waiting,
                           model = "lm")
#compute_model_prediction() takes a couple of arguments:
#    faithful, the dataset,
#    an R formula, eruptions ~ waiting, that specifies the relationship to model.
#    a model argument, the name of the R modelling function that is used to calculate the line. "lm" calculates a linear fit, "loess" uses the LOESS method.
#compute_smooth() is a wrapper around compute_model_prediction() that calculates a LOESS smooth line by default.
#Refer for dicumentation: https://rdrr.io/cran/ggvis/man/compute_model_prediction.html

#Use compute_smooth() on mtcars, to generate the x and y coordinates for a LOESS smooth line for mpg ~ wt (predicting mpg with wt).
mtcars %>% compute_smooth(mpg ~ wt)

# Extend with ggvis() and layer_lines()
mtcars %>% compute_smooth(mpg ~ wt) %>% ggvis(~pred_, ~resp_) %>% layer_lines()

# Extend with layer_points() and layer_smooths()
mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>% layer_smooths()

#Finish the code to build a histogram of the waiting variable of the faithful dataset. Inside layer_histograms(), map the width to 5 units.
faithful %>% ggvis(~waiting) %>% layer_histograms(width = 5)

#Just as combining compute_smooth() and layer_lines() does the same as layer_smooths(), combining compute_bin() and layer_rects()
# does the same as layer_histograms()
#  Fill the arguments of the compute_bin() function to calculate the counts for the waiting variable. The bins should have a width of 5. The output is a data frame 
#with the columns count_, x_, xmin_, xmax_ and width_.
#    Use %>% to add a layer_rects() call to the end of the command.
#    To plot the rectangles correctly, you have to specify 4 properties in the ggvis() command: x, x2, y and y2. Fill in the missing mappings: 
#  x2 should be mapped on xmax_, and y2 should be mapped on ~count_.
faithful %>%
  compute_bin(~waiting, width = 5) %>%
  ggvis(x = ~xmin_, x2 = ~xmax_, y = 0, y2 = ~count_) %>% layer_rects()

 #You can build a density plot by combining compute_density() with layer_lines(). compute_density() takes two arguments, 
 #a data set and a variable name. It returns a data frame with two columns: pred_, the x values of the variable's density line, 
 #and resp_, the y values of the variable's density line.
#Just like layer_histograms() combines compute_bin() and layer_rects(), you can use layer_densities() to create density plots easily: it 
#calls compute_density() and layer_lines() in the background.
faithful %>% ggvis(~waiting) %>% layer_densities(fill := "green")

#The plot on the right builds a bar plot of a categorical version of cyl. Simplify the command to use layer_bars() instead 
#of compute_count() and layer_rects().
mtcars %>%
  ggvis(~factor(cyl)) %>%
  layer_bars()

#Have a look at the first line of code below: it uses the group_by() function from the dplyr package. Combined with ggvis commands, 
#this leads to a plot with a separate smooth line for each unique value of the am variable (0 = automatic transmission; 1 = manual transmission).
#group_by() uses a grouping variable to organize a data set into several groups of observations. It places each observation into a group with
# other observations that have the same value of the grouping variable. In other words group_by() will create a separate group for each
# unique value of the grouping variable. When ggvis encounters grouped data, it will draw a separate mark for each group of observations.

library(dplyr)
# Instruction 1
mtcars %>% group_by(am) %>% ggvis(~mpg, ~wt, stroke = ~factor(am)) %>% layer_smooths()

# Instruction 2
mtcars %>% group_by(cyl) %>% ggvis(~mpg) %>% layer_densities()

# Change the first command: plot a unique smooth line for each value of the cyl variable. Note that you will also need to change
# the stroke property from am to cyl. Make sure to use categorical versions of the variables where needed.
# Add code to the second command to map fill to factor(cyl); this will clarify which density corresponds to 
#which group of observations.

# Instruction 1
mtcars %>% group_by(cyl) %>% ggvis(~mpg, ~wt, stroke = ~factor(cyl)) %>% layer_smooths()
# Instruction 2
mtcars %>% group_by(cyl) %>% ggvis(~mpg, fill = ~factor(cyl)) %>% layer_densities()

#group_by() will create a separate group for each distinct combination of values within the grouping variables. 
#However, to also correctly map ggvis properties to unique combinations of multiple variables, you need an additional 
#step: the interaction() function

mtcars %>% group_by(cyl,am) %>% ggvis(~mpg, fill = ~interaction(cyl,am)) %>% layer_densities()


#The first lines of code on the right make a basic interactive plot. It includes a select box that to change the shape of the points in the plot. 
#The command sets shape to a select box created with input_select(): the visualization updates on the fly if you update your selection.
#You can make your plots interactive by setting a property to the output of an input widget. ggvis comes with seven input widgets:
#    input_checkbox(),
#    input_checkboxgroup(),
 #   input_numeric(),
  #  input_radiobuttons(),
  #  input_select(),
  #  input_slider(), and
  #  input_text().
#Inspect the output of the first code chunk on the right. Visit the corresponding shiny app to experiment with the interactive plot by 
#changing the point shape. The command sets shape to a select box created with input_select(). Change the fill property in a similar way:
# set it equal to a select box that has the label "Choose color:" and provides the choices "black", "red", "blue", and "green"
faithful %>% ggvis(~waiting, ~eruptions, fillOpacity := 0.5,
                   shape := input_select(label = "Choose shape:",
                                         choices = c("circle", "square", "cross",
                                                     "diamond", "triangle-up", "triangle-down")),
                   fill := input_select(label = "Choose color:",
                                        choices = c("black", "red", "blue","green"))) %>% 
              layer_points()


#In the second plot, control the fill property with a group of radiobuttons. Give the group the label "Choose color:", and provide
# the choices "black", "red", "blue", and "green", in this order.
mtcars %>%
  ggvis(~mpg, ~wt,
        fill := input_radiobuttons(label = "Choose color:",
                                   choices = c("black", "red", "blue", "green"))) %>%
  layer_points()

#Some input widgets provide choices for the user to select from. Others allow the user to provide their own input. 
#For example, input_text() provides a text field for users to type input into. Instead of assigning input_text() a choices argument,
# you assign it a value argument: a character string to display when the plot first loads
mtcars %>%
  ggvis(~mpg, ~wt,
        fill := input_text(label = "Choose color:",
                           value = "black")) %>%
  layer_points()

#In the command on the right, map fill to a select box that returns variable names. The label of this select box should read 
#"Choose fill variable:" and create the choices with names(mtcars), as shown here. Use map = as.name inside input_select() 
#to map the selection instead of setting it!
 mtcars %>%
  ggvis(~mpg, ~wt,
        fill := input_select(label = "Choose fill variable:",
                           choices = names(mtcars),
                           map = as.name))   %>%
  layer_points()

#Change the first graph coded on the right to map the binwidth to a numeric field that uses the label "Choose a binwidth:" and 
#has a starting value of 1.
mtcars %>%
  ggvis(~mpg,
        input_numeric(label = "Choose a binwidth:",
                      value = 1)) %>%
  layer_histograms(width = 2)

 #Change the second graph to map binwidth to a slider bar that uses the label "Choose a binwidth:" and has a min value of 1 and a max value of 20. 
 #input_slider will place the initial value at (min + max) / 2
 mtcars %>%
  ggvis(~mpg,
        input_slider(min =1, max = 20, label = "Choose a binwidth:")) %>%
  layer_histograms(width = 2)


#Add a layer of points to the first graph on the right.
pressure %>%
  ggvis(~temperature, ~pressure, stroke := "skyblue") %>%
  layer_lines() %>%
  layer_points()

 ## Copy and adapt the solution to the first instruction below so that only the lines layer uses a skyblue stroke.
pressure %>%
  ggvis(~temperature, ~pressure) %>%
  layer_lines(stroke := "skyblue") %>%
  layer_points()

#layer_model_predictions() plots the prediction line of a model fitted to the data. It is similar to layer_smooths() but you can extend it
# to more models than just the "loess" or "gam" model.
#layer_model_predictions() takes a parameter named model; it should be set to a character string that contains the name of an R model function.
# layer_model_predictions() will use this function to generate the model predictions

#Add more layers to the line plot:

  #  layer_points() to also add the actualy data point.
  #  layer_model_predictions() to add a linear model line (model = "lm"). Set the stroke of this line to "navy".
  #  layer_smooths() to add a smooth line with a stroke that is "skyblue"
pressure %>%
  ggvis(~temperature, ~pressure) %>%
  layer_lines(opacity := 0.5) %>%
  layer_points() %>%
  layer_model_predictions(model = "lm", stroke := "navy") %>%
  layer_smooths(stroke := "skyblue")


#Axes help you to customize the plots you create with ggvis. add_axis() allows you to change the titles, tick schemes and 
#positions of your axes. The example code below clarifies:
add_axis("x", 
         title = "axis title", 
         values = c(1, 2, 3), 
         subdivide = 5,
         orient = "top")
#    The first argument specifies which axis to customize.
 #   title - the title of the axis you specified in the first argument.
 #   values - determine where labelled tick marks will appear on each axis.
 #   subdivide - insert unlabelled tick marks between the labelled tick marks on an axis.
 #   orient - control where the axis appears. For the x axis, you can use "top" or "bottom", for the y axis, you can use "left" or "right"

# Change the axes of the plot as instructed
faithful %>% 
  ggvis(~waiting, ~eruptions) %>% 
  layer_points() %>%
  add_axis("x",
            title = "Time since previous eruption (m)",
            values = c(50,60,70,80,90),
            subdivide = 9,
            orient = "top") %>%
  add_axis("y",
            title = "Duration of eruption (m)",
            values = c(2,3,4,5),
            subdivide = 9,
            orient = "right")
#Add a legend to the plot on the right:

   # It should give information on the fill property.
   # Set the title to "~ duration (m)"
   # Specify the orient argument to place the legend on the "left" side.
 # Add a legend
faithful %>% 
  ggvis(~waiting, ~eruptions, opacity := 0.6, 
        fill = ~factor(round(eruptions))) %>% 
  layer_points() %>%
  add_legend("fill",
             title = "~ duration (m)",
             orient = "left")

  # Fix the legend
faithful %>% 
  ggvis(~waiting, ~eruptions, opacity := 0.6, 
        fill = ~factor(round(eruptions)), shape = ~factor(round(eruptions)), 
        size = ~round(eruptions))  %>%
  layer_points() %>%
  add_legend(c("fill","shape","size"),
              title = "~ duration (m)")


#You can change the color scheme of a ggvis plot by adding a new scale to map a data set variable to fill colors. 
#The first chunk of code on the right creates a new scale that will map the numeric disp variable to the fill property.
# The scale will create color output that ranges from red to yellow.

#ggvis provides several different functions for creating scales: scale_datetime(), scale_logical(), scale_nominal(), scale_numeric(), 
#scale_singular(). Each maps a different type of data input to the visual properties that ggvis uses.

# Add a scale_numeric()
mtcars %>% 
  ggvis(~wt, ~mpg, fill = ~disp, stroke = ~disp, strokeWidth := 2) %>%
  layer_points() %>%
  scale_numeric("fill", range = c("red", "yellow")) %>%
  scale_numeric("stroke", range = c("darkred","orange"))


# Add a scale_numeric()
mtcars %>% ggvis(~wt, ~mpg, fill = ~hp) %>%
  layer_points() %>%
  scale_numeric("fill", range = c("green","beige"))


# Add a scale_nominal()
mtcars %>% ggvis(~wt, ~mpg, fill = ~factor(cyl)) %>%
  layer_points() %>%
  scale_nominal("fill", range = c("purple","blue","green"))


 # Add a scale to limit the range of opacity 
mtcars %>% ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl), opacity = ~hp) %>%
  layer_points() %>%
  scale_numeric("opacity", range = c(0.2,1))

# Add a second scale to set domain for x
mtcars %>% ggvis(~wt, ~mpg, fill = ~disp) %>%
  layer_points() %>%
  scale_numeric("y", domain = c(0, NA)) %>%
  scale_numeric("x",
                domain = c(0,6))
# Set the fill instead of mapping it
mtcars %>% 
  ggvis(x = ~wt, y = ~mpg, fill := ~color) %>% 
  layer_points()
# NOTE:  When you use :=, ggvis uses the colors named in mtcars$color to fill the points

