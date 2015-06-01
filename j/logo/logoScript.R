# Logo Operations
# ==================================
# Start date: 14th July 2012

# Run j.logo.startup() to load
# dependent packages: 
j.logo.startup=function(showPrints=FALSE){
# dependent packages: none
  if(showPrints){
    print("loading j/logo/logoScript.R");
    print("no dependencies");
  }
}

# 
# Aim: To perform logo operations
# Functions: (all preceeded by j.logo. as a namespace)
#   1. vectorToBoxes   TODO testing
#   2. Square          TODO
#   3. 

j.logo.vectorToBoxes= function(v){
  # vector is boolean to indicate filled boxes
  if(!j.check.isBoolean(v)){ return("input must contain boolean values only")};

  code = c();
  size = 6;

  for(i in 1:length(v)){
  # logo code for drawing a square of 'size' at location of turtle
    square = c("setx xcor - ",size/2,"sety ycor - ",size/2," pendown  setx xcor + ",size," sety ycor + ",size," setx xcor - ",size," sety ycor - ",size," penup  setx xcor + ",size/2," sety ycor + ",size/2," ");
  # turns 'square' as vector into single string
    square = paste(square, collapse = ' ');
  # generate 'code' vector of logo commands
    code = c(code,square);
  # if input vector contains 1 or True, fill the square
    if(j.check.isTrue(v[[i]])){code = c(code,"fill")};
    fwd = c("fd",size);
    fwd = paste(fwd,collapse= ' ');
    code = c(code, fwd);
  }
  # return turtle to one square length south of where it began
  moveTurtle = c("make \"xstart :xstart-",size," setx :xstart sety ycor-",size,"");
  moveTurtle = paste(moveTurtle,collapse = ' ');
  code = c(code, moveTurtle);

  return(code);
}

