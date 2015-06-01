# Checking and validation Operations
# ==================================
# Start date: 28th June 2012
# 1. Load in console as
# source("C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\\j\\check\\checkScript.R")
# or 
# select console window, file->Change dir->C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\\j\
# then you just need > source("check\\checkScript.R")
# 2. Run j.check.startup() to load
# dependent packages: 
j.check.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/check/checkScript.R");
    print("no dependencies");
  }
}

# 
# Aim: To perform data validation operations upon elements, vectors and matrices.
# Functions: (all preceeded by j.check. as a namespace)
#   1. isVector
#   2. isInteger
#   3. indexRange
#   4. isBinary
#   5. isBoolean
#   6. isTrue
#   7. isFalse
#  8. singleElement
#  9. Integer :- object for integer checking
 
j.check.isVector=function(v){
        if(!is.vector(v)){
                warning("v is not a vector");
                return(FALSE);
        }
	else{
		return(TRUE);
	}
}
j.check.isInteger=function(i){
# throws warning for invalid i
	if(!length(i)==1){warning("i should be a single integer value");return();}
	if(is.na(as.integer(i))){
		return(FALSE);
	}		
	if(!isTRUE(all.equal(i, as.integer(i)))){
		return(FALSE);
	}
	else{
		return(TRUE);
	}
}
j.check.indexRange=function(v,p){
# throws warning for invalid p
# checks whether p is a valid index for the vector v.
  # v is the vector
  if(!j.check.isVector(v))return(FALSE);
  # p is the index
  if(!length(p)==1){warning("p should be a single integer value");return(FALSE);}
  if(!j.check.isInteger(p))return(FALSE);
	
  if(p<=0){
    warning("p is less than or equal to zero");
    return(FALSE);
  }
  if(p>length(v)){
    warning("p is greater than length(v)");
    return(FALSE);
  }
return(TRUE);
}
j.check.isBinary = function(b){
# logical to test whether a vector is comprised only of 1, 0, TRUE or FALSE
	result = j.check.isBoolean(b);
	return(result);	
}
j.check.isBoolean = function(v){
# logical to test whether a vector contains only 1, 0, TRUE or FALSE
	for(e in v){
		e = as.numeric(e);#converts "1" into 1
		if(j.check.isTrue(e)|j.check.isFalse(e)){
			#do nothing;
		}
		else return(FALSE);
	}
	return(TRUE);
}
j.check.isTrue = function(v){
# throws warning for invalid v, i.e. zero length
# logical to test whether a vector contains only 1 or TRUE
  if(length(v)==0){
    warning("input has length of zero");
    return(FALSE);
  }
	for(e in v){
		e = as.numeric(e);
		if(isTRUE(e==1)){
			#do nothing;
		}
		else return(FALSE);
	}
	return(TRUE);
}
j.check.isFalse = function(v){
# throws warning for invalid v, i.e. zero length
# logical to test whether a vector contains only 0 or FALSE
  if(length(v)==0){
    warning("input has length of zero");
    return(FALSE);
  }
	for(e in v){
		e = as.numeric(e);
		if(isTRUE(e==0)){
			#do nothing;	
		}
		else return(FALSE);
	}
	return(TRUE);
}
j.check.singleElement = function(i){
# does not throw warning
# logical to test whether input is a single value.
# input: a = 1 	output: TRUE
#        a = c(1) output: TRUE
#        a = c(1,1) output: FALSE
  if(length(i)==1){
    return(TRUE);
  }
  else{
    return(FALSE);
  }
}

j.check.Integer = list();
# Integer :- object for integer checking
# ---------  methods  ------------
# check(int=input)
# input: 3       output: TRUE
# input: 3.0     output: TRUE
# input: "3.0"   output: TRUE

# input: 3.4     output: FALSE
# input: "a"     output: FALSE
# input: TRUE    output: FALSE
# input: FALSE    output: FALSE
# input: c(3,4)  output: warning("input must be a singleElement")
#
# setWarningOn()    default
# setWarningOff()
#
# allowNegative   default TRUE
# allowPositive   default TRUE
# allowZero       default TRUE
#
# allowNegative()   
# allowPositive()  
# allowZero()      
# forbidNegative()   
# forbidPositive()   
# forbidZero()       

  



j.check.Integer$setInteger = function(input){
  j.check.Integer$.integer = input;
}
j.check.Integer$allowStrings = TRUE;
j.check.allowNegative = TRUE;
j.check.allowZERO = TRUE;
j.check.allowPositive = TRUE;

j.check.allowPositive = function(){
  j.check.allowPositive = TRUE;
}
j.check.forbidPositive = function(){
  j.check.allowPositive = FALSE;
}

j.check.Integer$check = function(input){
  j.check.Integer$.integer = input;
# check for boolean TRUE; we do not want to coerce this to 1.  
  if(identical(TRUE,input)) return(FALSE);
# coerce strings into numbers 
  if(j.check.Integer$allowStrings){
    input = as.numeric(input)
  }
    # check valid input
    if(length(input)!=1){
      warning("input must be a singleElement");
      return(FALSE);
    }
    result = j.check.isInteger(int);
    return(result);
 
  return(o);
}
