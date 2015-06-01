# Find and Replace Functions
# ==================================
# Start date: 18th July 2012


j.fr.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/fr/frScript.R");
    print("no dependencies");
  }
}
# 
# Aim: To generate find, match & replace functions
# Functions: (all preceeded by j.fr. as a namespace)
# 1. matchSingleValue
# 2. matchVector
# 3. replace
# 4. removeDuplicates
# 5. removeAll
# 6. addAll
# 7. numberToPlaceValueVector
# 8. placeValueToNumber

j.fr.matchSingleValue = function(v,x){
# input: v = c(2,3,8,15,21)   x = 13
# output: result$boolean = FALSE

# input: v = c(2,3,8,15,21);   x = 15
# output: result$boolean = TRUE
# output: result$indices = c(4)

  # v is a vector in which you are looking for a match
  if(!j.check.isVector(v))return("input1 must be a vector");
  # x is single value which you are matching
  if(!j.check.singleElement(x)) return("input2 must be a single element");

  # boolean result
  oResult = list();
  oResult$boolean = (x %in% v);

  # indices result;
  oResult$indices =grep(x,v);

  # matchedValue result;
  oResult$matchValue = x;

  return(oResult);
}

j.fr.matchVector = function(v,w){
# input: v = c(2,3,8,15,21,34,25,46,13,15,65,47); w = c(13,15);
# output: oResult$indices$`15` = c(4,10)
# output: oResult$indices$`13` = c(9)

  oResult = list();
  # v is a vector in which you are looking for a match
  if(!j.check.isVector(v))return("input1 must be a vector");
  # w is a vector of match values
  if(!j.check.isVector(w))return("input2 must be a vector");
  

# generate keys & indices for which there is a match
  oResult = list();

  length = 0;
  oResult$indices = vector(mode="list");
  keys = vector();
  for(x in w){
    oMatchSingle = j.fr.matchSingleValue(v,x);
    if(oMatchSingle$boolean){
      length = length+1;
      keys[length] = x;
      oResult$indices[[length]]=oMatchSingle$indices;
    }
  }
  names(oResult$indices)<-keys
  return(oResult);
}

j.fr.replace = function(v, match, replaceWith){
# input: v = c(2,3,8,15,21); match = 15; replaceWith= TRUE;
# output: list(2,3,8,TRUE,21);
# note: this has to return a vector so that different data types can be included

  oMatchSingle = j.fr.matchSingleValue(v,match);
  resultAsList =as.list(v);
  resultAsVector =v;
    
  for(index in oMatchSingle$indices){
    resultAsList[[index]] <- replaceWith;
    resultAsVector[[index]] <- replaceWith;
  }
  oResult = list();
  oResult$asList = resultAsList;  # can include more than one data type
  oResult$asVector = resultAsVector;  # all data coerced to same type

  return(oResult);
}

j.fr.removeDuplicates = function(v){
# input:  c(2,3,8,15,12,21,15,12)
# output: c(2,3,8,15,12,21)
  # check v
  if(!j.check.isVector(v))return("input1 must be a vector");

  return(unique(v));
}

j.fr.removeAll = function(v, w){
# input: v = c(1,2,3,4,5,6,7,8); w = c(2,4,6,8);
# output: c(1,3,5,7);
  # check v
  if(!j.check.isVector(v))return("input1 must be a vector");
  # check w
  if(!j.check.isVector(w))return("input2 must be a vector");

  result = v;
  for(x in w){
    result= result[!result==x];
  }
  return(result);
}

j.fr.addAll = function(v1,v2){
#input; v1 = c(1,2,3,4,5); v2 = c(12,15,17);
#output; m = [13,14,15,16,17]
#            [16,17,18,19,20]
#            [18,19,20,21,22]
  m1 = matrix(v1,nrow = length(v2), ncol = length(v1), byrow=TRUE);
  m2 = matrix(v2,nrow = length(v2), ncol = length(v1));

  oResult = list();
  oResult$m1 = m1;
  oResult$m2 = m2;
  oResult$m = m1+m2;
  return(oResult$m);
}

j.fr.numberToPlaceValueVector = function(x){
# input: x = 3794;
# output: c(3,7,9,4);
  # code:  digitsBase(3794,10) (sfsmisc package, returns a matrix)
  if(!j.check.isInteger(x))return("input1 is not an integer");
  m = digitsBase(x,10);
  v = as.vector(m);
  return(v);
}
j.fr.placeValueVectorToNumber = function(v){
# input: c(3,7,9,4);
# output: 3794;
  # check v
  if(!j.check.isVector(v))return("input1 must be a vector");
  for(e in v){
      if(!j.check.isInteger(e))return("input1 includes non-integers");
  }
  length = length(v)
  result=0;
  count = length-1;
  for(e in v){
    result = result + e*(10^count)
    count = count-1;
  }
  return(result);
}
