# Basic Vector and Matrix Operations
# ==================================
# Start date: 28th June 2012
# 1. Load in console as
# source("C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\\j\\vm\\vmScript.R")
# or 
# select console window, file->Change dir->C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\
# then you just need > source("j/vm/vmScript.R")
# 2. Run j.vm.startup() to load
# dependent packages: partitions, gregmisc,sfsmisc
j.vm.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/vm/vmScript.R");
    print("dependent packages: partitions, gtools,sfsmisc");
  }
}
# 
# Aim: To perform simple operations upon vectors and matrices.
# Functions: (all preceeded by j.vm. as a namespace)

#   1. add
#   1b. addAll
#   2. addBeg, addEnd
#   3. and
#   4. 
#   5. 
#   6. crossMultiply
#   7. cumulative
#   7b. divideAll
#   8. indToBool
#   9. insert. Inserts a number into a vector at a given position.
#   10. 
#   11. inverse
#   12. matColSel. Selects columns from a matrix
#   13. matRowSel. Selects columns from a matrix
#   14. move
#   14b. multiplyAll
#   15. neighbours
#   16. or
#   17. 
#   18. 
#   18b. subtractAll
#   19. sumThenAdd
#   20. swap
#   21. vecSel Selects indexed values from a vector
#   22. 2Dlist
#   23. dataFrameToVector
#   24. subMatrix

j.vm.add=function(){ 
# 1. add. Adds each element pairwise across two matrices.
# input: m1 = [1 1]  m2 = [1 0]
#             [0 0]       [1 0]
# output: [2 1]
#         [1 0]
# code: m1+m2
}

j.vm.addAll = function(v1,v2){
# 1b. addAll
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

j.vm.addBeg = function(){
#   2a. addBeg
#   input: a = c(1,1,1), b= c(0,0)
#   output: c(0,0,1,1,1)
#   code: c(b,a)
}

j.vm.addEnd = function(){
#   2b. addEnd
#   input: a = c(1,1,1), b= c(0,0)
#   output: c(1,1,1,0,0)
#   code:  c(a,b)
}

j.vm.and=function(){
# 3. and
# example input:  v1 = c(1,0,0,1), v2 = c(1,1,0,0)
# intended output: c(1,0,0,0)
# code: as.numeric(v1&v2)
} 


j.vm.crossMultiply = function(){
# 6. crossMultiply
# input: m1 = [a b]  m2 = [e f]
#             [c d]       [g h] 
# output: [a*h b*g]
#         [c*f d*e]
# code: v = as.vector(m1) * rev(as.vector(m2))
#       m = matrix(v)
}

j.vm.cumulative = function(){
# 7. Cumulative. 
# Input: a = c(1,7,3,9)
# Ouput: 1,8,11,20
# Code: cumsum(a)
}


j.vm.divideAll = function(v1,v2){
# 7b. divideAll
#input; v1 = c(1,2,3,4,5); v2 = c(12,15,17);
#output; m = [1/12,2/12,3/12,4/12,5/12]
#            [1/15,2/15,3/15,4/15,5/15]
#            [1/17,2/17,3/17,4/17,5/17]
  m1 = matrix(v1,nrow = length(v2), ncol = length(v1), byrow=TRUE);
  m2 = matrix(v2,nrow = length(v2), ncol = length(v1));

  oResult = list();
  oResult$m1 = m1;
  oResult$m2 = m2;
  oResult$m = m1/m2;
  return(oResult$m);
}


j.vm.indToBool = function(l,reqI){
#   8. indToBool
# input: l = 8, reqI = c(1,2,5)
# output: c(T,T,F,F,T,F,F,F)
	# reqI is a vector of indices which you want
	if(!j.check.isVector(reqI))return();
	# l is length of boolean vector result
	if(!j.check.isInteger(l))return('l is not an integer');
	allI = 1:l;
	f = c();
	for(i in allI){
		f = c(f,i %in% reqI);
	}
	result = f;
	return(f);
}

j.vm.insert=function(v,a,p){
# 9. insert. Inserts vector 'a' into a vector 'v' at position 'p'.
# input:  v = c(1,1,1,1), a=0, p=2
# output: c(1,1,0,1,1)
# v is the original vector
# a is the vector (or single element) to add
# p is the position
  if(!j.check.isVector(a))return();
  if(!j.check.indexRange(v,p))return();
  begF = rep("beg",p);
  endF = rep("end",length(v)-p);
  f = c(begF,endF);
  vSplit=split(v,f)
  result = c(vSplit$beg,a,vSplit$end)
  return(result)
}

j.vm.inverse = function(){
# 11. inverse
# input: 1010
# output: 0101
# code: as.numeric(!c(1,0,1,0))
}

j.vm.matColSel = function(m, ci){# m is matrix, ci is column indices
#   12. matColSel. Selects columns from a matrix
# input:	[1 5 9]	output: 	[5] , [1 9]
#		[2 6 10]			[6] , [2 10]
#		[3 7 11]			[7] , [3 11]
#		[4 8 12]			[8] , [4 12]
#
#	To just get first column, use code: m[,ci] 
#
#
# test to check that zeroth column has not been requested
	if(0 %in% ci){
		warning("Cannot select zeroth column: subscript out of bounds");
		return();
	}
	mTrue = m[,ci];
# if just one column, need to convert to a matrix
	if(is.null(dim(mTrue))){
		mTrue = matrix(mTrue, ncol=1);
	}
	mFalse = m[,-ci];
# if just one column, need to convert to a matrix
	if(is.null(dim(mFalse))){
		mFalse = matrix(mFalse, ncol=1);
	}
	result = list(mTrue,mFalse);
	names(result) = c('TRUE','FALSE');
	return(result);		
}


j.vm.matRowSel = function(m, ri){# m is matrix, ri is row indices
#   13. matRowSel. Selects rows from a matrix
# input:	[1 5 9]	output: 		, [1 5 9]
#		[2 6 10]				, [2 6 10]
#		[3 7 11]			[3 7 11] , 
#		[4 8 12]			 	, [4 8 12]
#
#	To just get first row, use code: m[ri,] 
#
#
# test to check that zeroth row has not been requested
	if(0 %in% ri){
		warning("Cannot select zeroth row: subscript out of bounds");
		return();
	}
	mTrue = m[ri,];
# if just one row, need to convert to a matrix
	if(is.null(dim(mTrue))){
		mTrue = matrix(mTrue, nrow=1);
	}
	mFalse = m[-ri,];
# if just one row, need to convert to a matrix
	if(is.null(dim(mFalse))){
		mFalse = matrix(mFalse, nrow=1);
	}
	result = list(mTrue,mFalse);
	names(result) = c('TRUE','FALSE');
	return(result);		
}

j.vm.move = function(a){
#   14. move
# input:  1    0    0    0
# output: 1    0    0    0
#         0    1    0    0
#         0    0    1    0
#         0    0    0    1
  if(!j.check.isVector(a))return("input is not a vector")
  l = length(a)
  m = matrix(nrow=l,ncol=l,byrow=T)
  for(i in 1:l){
    m[i,] = a
    a = c(a[l],a[1:(l-1)])
  }
  return(m)
}
j.vm.multiplyAll = function(v1,v2){
# 14b. multiplyAll
#input; v1 = c(1,2,3,4,5); v2 = c(12,15,17);
#output; m = [12,24,36,48,60]
#            [15,30,45,60,75]
#            [17,34,51,68,85]
  m1 = matrix(v1,nrow = length(v2), ncol = length(v1), byrow=TRUE);
  m2 = matrix(v2,nrow = length(v2), ncol = length(v1));

  oResult = list();
  oResult$m1 = m1;
  oResult$m2 = m2;
  oResult$m = m1*m2;
  return(oResult$m);
}

j.vm.neighbours = function(v,l){
# 15. neighbours 
# input1: v is input vector, e.g. c(1,2,3,4,5)
# input2: l is length of output rows e.g. 3  
# output: [ 1 2 3]
#         [ 2 3 4]
#         [ 3 4 5]
  if(!j.check.isVector(v))return();
  if(!j.check.indexRange(v,l))return();
# factor to indicate which elements to collect
  f = c(rep(TRUE,l),rep(FALSE,(length(v)-l)));
# loop through all possible factors to generate TRUE, FALSE matrix, m
  m= j.vm.move(f);
# required row indices, ri
  n =length(v)+1-l; # n is the number of groupings.
  ri = 1:n;
# cut m to include only required rows
  m = m[ri,];
# split orig vector according to each TRUE / FALSE factor in the matrix
  result = matrix(nrow=0,ncol=0);
  for(i in 1:n){
	result = matrix(c(result,v[m[i,]]));
  }
  result = matrix(result,ncol = n);
  return(result);
}

j.vm.or=function(){
# 16. or
# input:  v1 = c(1,0,0,1), v2 = c(1,1,0,0)
# output: c(1,1,0,1)
# code: as.numeric(v1|v2)
}


j.vm.subtractAll = function(v1,v2){
# 18b. subtractAll
#input; v1 = c(1,2,3,4,5); v2 = c(12,15,17);
#output; m = [1-12,2-12,3-12,4-12,5-12]
#            [1-15,2-15,3-15,4-15,5-15]
#            [1-17,2-17,3-17,4-17,5-17]
  m1 = matrix(v1,nrow = length(v2), ncol = length(v1), byrow=TRUE);
  m2 = matrix(v2,nrow = length(v2), ncol = length(v1));

  oResult = list();
  oResult$m1 = m1;
  oResult$m2 = m2;
  oResult$m = m1-m2;
  return(oResult$m);
}


j.vm.sumThenAdd=function(){
# 19. sumThenAdd. Adds totals of columns and adds as new row to matrix.
# input: m = [1 0]
#            [0 1]
# output: m = [1 0]
#             [0 1]
#             [1 1]
# code:     rbind(m, colSums(m))
}

j.vm.swap = function(v,i,j){
#   20. Swap. Swaps the position of element at position i with that at position 
# input: v = c(5,6,7,8,9), i = 1, j = 3
# output: c(7,6,5,8,9)
  if(!j.check.indexRange(v,i)){warning("i is not a valid index of v.");return();}
  if(!j.check.indexRange(v,j)){warning("j is not a valid index of v.");return();}
  ei = v[i];
  ej = v[j];
  v[i] = ej;
  v[j] = ei;
  return(v);
}

j.vm.vecSel = function(v, reqI){
# 21. vecSel Selects indexed values from a vector
# input: v = c(6,7,8,9,10), i = c(2,3)
# output: c(7,8)
# code: v[i]
# or if you want both matches and non-matches use j.vecSel as below
# output: $TRUE = c(7,8) , $FALSE = c(6,9,10)

	# v is the vector from which you want to select elements
	if(!j.check.isVector(v))return("input 1 is not a vector");
	# reqI is a vector indices which you want
  for(i in reqI){
    if(!j.check.indexRange(v,i)){
      return();
    }
  }
	allI = 1:length(v);
	f = c();
	for(i in allI){
		f = c(f,i %in% reqI);
	}
	result = split(v,f);
	return(result);
}

j.vm.2Dlist = function(n=8, m=8){
  # returns a n x m list of lists
  if(!j.check.isInteger(n))  return("n is not an integer");
  if(!j.check.isInteger(n))  return("m is not an integer");			
  s = list();
  for(i in 1:n){
    s[[i]] = list();
    length(s[[i]]) = m;
  }
  return(s);
}

j.vm.dataFrameToVector = function(df){
  m = as.matrix(df)
  q = c()
  for (i in seq(1:nrow(m))){
    q = c(q, m[i,])
  }
  names(q) = NULL;
  return(q);
}
j.vm.subMatrix = function(matrix, columns=c(1:(ncol(matrix))), rows=c(1:(nrow(matrix)))){
# input: [1,2,3]  , c(2:3), c(2:3)
#        [5,6,7]
#        [4,3,6]
# output: 
#        [6,7]
#        [3,6]

  matrix= matrix[,columns];
  matrix= matrix[rows,];
  return(matrix);
}  
j.vm.subMatrix.test = function(){
  m1 = matrix(c(3,4,2,3,6,2,3,5),ncol=4, byrow=TRUE);
  m2 = matrix(c(3,3,6,5),ncol=2, byrow=TRUE);

  mResult = j.vm.subMatrix(m1,columns=c(1,4));
  result = (mResult==m2);
  return(result);
}

