## Assignment 3.2R Solutions - 2024 Programming in Psychological Science (PIPS)
#
# Record of Revisions
#
# Date               Programmer              Descriptions of Change
# ====            ================           ======================
# 27-Jan-24      [Gaayathri Thampi]               Original code

#Q3.2R.1 -----------------------------------------------------------------------
# Answer

#FUNCTION 1

remind_me <- function(){
  reminders<- list(c("Internship Contract Deadline -> 26th Jan", "PIPS Re-take Exam -> May 6th", "Dad's Birthday -> 26th April", "Internship Final Project -> 28th June"))
  return(reminders)
}

remind_me()

#FUNCTION 2 

cheat <- function(exercise_number){
  solutions <- list(
    
    exercise_3.1.2 =  'library(readr)
    library(ggplot2)
    data_url <- "https://bit.ly/3GLVQ86"
    df <- readr::read_csv(data_url)
    ggplot(data = df) + geom_point(mapping = aes(x = DATE, y = TMIN))+labs(title = "Temperature at Schipol",
       x = "Time",
       y = "Temperature"))',
    
    
    exercise_3.1.5 = 'plot(cars$speed, cars$dist, main = "Speed vs. Distance", xlab = "Speed", ylab = "Distance",
                           col = "blue")
    abline(lm(dist ~ speed, data = cars), col = "red")',
    
    
    exercise_3.1.7 = 'ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "loess", formula = "y ~ x", color = "blue") +
  labs(title = "",
       x = "Speed",
       y = "Distance") +
  theme_light()' )
    
 if (exercise_number %in% names(solutions)) {
    return(solutions[[exercise_number]])
  } else {
    return("Exercise not found. Check the exercise number and try again.")
  }
}
  

cheat("exercise_3.1.2") #gives me the solution to 3.1.2 



#Q3.2R.2 -----------------------------------------------------------------------
#Answer

# I am making a function that generates random coloured shapes.
# Art is subjective, I find shapes fascinating :)


make_art <- function(num_shapes = 250, seed=NULL){
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  
 
  plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "", axes= FALSE, main = "Random Art")
  
  # Generate random shapes
  for (i in 1:num_shapes) {
    shape_type <- sample(c("circle", "rectangle", "triangle", "ellipse", "star"), 1)
    x <- runif(1, 0, 10)
    y <- runif(1, 0, 10)
    
    if (shape_type == "circle") {
      radius <- runif(1, 0.5, 2)
      symbols(x, y, circles = radius, add = TRUE, inches = 0.1, col = sample(colors(), 1))
    } else if (shape_type == "rectangle") {
      width <- runif(1, 0.5, 2)
      height <- runif(1, 0.5, 2)
      rect(x - width/2, y - height/2, x + width/2, y + height/2, col = sample(colors(), 1))
    } else if (shape_type == "triangle") {
      size <- runif(1, 0.5, 2)
      x_points <- c(x, x - size/2, x + size/2, x)
      y_points <- c(y - size/2, y + size/2, y + size/2, y - size/2)
      polygon(x_points, y_points, col = sample(colors(), 1))
    } else if (shape_type == "ellipse") {
      major_axis <- runif(1, 0.5, 2)
      minor_axis <- runif(1, 0.5, 2)
      symbols(x, y, circles = major_axis, inches2 = minor_axis, add = TRUE, inches = 0.1, col = sample(colors(), 1))
    } else if (shape_type == "star") {
      size <- runif(1, 0.5, 2)
      x_points <- c(x, x - size/4, x - size/2, x - size/4, x, x + size/4, x + size/2, x + size/4, x)
      y_points <- c(y - size/2, y - size/4, y, y + size/4, y + size/2, y + size/4, y, y - size/4, y - size/2)
      polygon(x_points, y_points, col = sample(colors(), 1))
    }
    }
  }


make_art(seed=12345) #you can set the seed with the "seed" argument in the function 













