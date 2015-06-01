# Combinatorics
# ==================================
# Start date: 27th July 2012

j.cmb.startup=function(showPrints=FALSE){
# dependent packages: none
  if(showPrints){
    print("loading j/cmb/cmbScript.R");
    print("no dependencies");
  }
}

# 
# Aim: To gather together functions related to combinatorics
#      There will be overlap with other folders, particularly ints.
# Functions: (all preceeded by j.cmb. as a namespace)
#
# parts - generates partitions
# perms - generates permutations


j.cmb.parts = function(n){
#Requires partitions package.
  z=parts(n)
  z=t(parts(n))
  z = z[nrow(z):1,]
  return(z)
}

j.cmb.perms = function(a){
# requires: package(partitions)
# input: 1 1 0 0 
# output: 
#  [1    1    0    0]
#  [1    0    1    0]
#  [1    0    0    1]
#  [0    1    1    0]
#  [0    1    0    1]
#  [0    0    1    1]

  if(!j.check.isVector(a))return();
  l = length(a)
  # generate all permuations, allowing duplicates in input vector
  x = permutations(l,l,a,set=FALSE)
  # remove duplicate row results
  result = matrix(nrow=0,ncol=l)
  duplicates = duplicated(x)
  for(i in 1:length(duplicates)){
    if(duplicates[i]==FALSE){
      result = rbind(result,x[i,])
    }
  }
  return(result)
}
