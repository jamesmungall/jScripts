# Sets Functions
# ==================================
# Start date: 26th July 2012

j.sets.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/sets/setsScript.R");
    print("no dependencies");
  }
}

# 
# Aim: Functions for sets
# Functions: (all preceeded by j.sets. as a namespace)
# 1. union

j.sets.union = function(A,B){
# inputs: two sets, A, B
# output: (A) Union (B)
  return(union(A,B));
}

j.sets.intersect = function(A,B){
# inputs: two sets, A, B
# output: (A) intersect(B)
  return(intersect(A,B));
}

j.sets.setDiff=function(A,B){
# inputs: two sets, A, B
# output: (A) Union (not_B)
# output maybe interpreted as A with elements of B removed.
  return(setdiff(A,B));
}

j.sets.isElement=function(el,set){
# inputs: element, set
# output: boolean
  return(is.element(el,set));
}

