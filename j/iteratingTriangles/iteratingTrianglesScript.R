# Iterating Triangles
# ==================================
# Start date: 13th July 2012

# Run j.iteratingTriangles.startup() to load
# dependent packages: 
j.iteratingTriangles.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/iteratingTriangles/iteratingTrianglesScript.R");
    print("no dependencies");
  }
}

# 
# Aim: To generate iterating triangles
# Functions: (all preceeded by j.iteratingTriangles. as a namespace)
#   1. n1 - create matrix of neighbours
#   2. n2 - sum each column and generate new vector
#   3. n3 - convert each vector number into new number according to rules
#   4. logo - generate logo code

j.iteratingTriangles.n1 = function(v){
  v = c(0,0,v,0,0);
  result = matrix(nrow=3,ncol=0);
  for(i in 1:(length(v)-2)){
    line = c(v[c(i:(i+2))]);
    result = cbind(result,line);
  }
  return (result);
}

j.iteratingTriangles.n2 = function(v){
  m = j.iteratingTriangles.n1(v);
  result = c();
  for(i in 1:ncol(m)){
    int = j.vm.binToInt(m[,i]);
    result = c(result,int);
  }
  return(result);
}
j.iteratingTriangles.n3 = function(v,rules){
  ints = j.iteratingTriangles.n2(v);
  result = c();
#  rules = c(0,1,0,0,1,0,0,1);
  for(int in ints){
    if(int==0){result = c(result,rules[[1]]);}
    if(int==1){result = c(result,rules[[2]]);}
    if(int==2){result = c(result,rules[[3]]);}
    if(int==3){result = c(result,rules[[4]]);}
    if(int==4){result = c(result,rules[[5]]);}
    if(int==5){result = c(result,rules[[6]]);}
    if(int==6){result = c(result,rules[[7]]);}
    if(int==7){result = c(result,rules[[8]]);}
  }
  return(result);
}

j.iteratingTriangles.logo = function(rules){
  v = c(1);
  code = c("to main");
  code = c(code,"setfloodcolor [56 79 15] penup make \"xstart 0");
  code = c(code,"setx :xstart sety 400");
  code = c(code,"rt 90");
  logoLine = j.logo.vectorToBoxes(v);
  code = c(code,logoLine);
  for(i in 1:80){
    v = j.iteratingTriangles.n3(v,rules);
    logoLine = j.logo.vectorToBoxes(v);
    code = c(code,logoLine);
  }

  code = c(code,"end");
  write(code, file = "C:/Documents and Settings/jms/My Documents/Comput/newLogo/logo.LGO", ncolumns = if(is.character(v)) 1 else length(v), append = FALSE, sep = "\n");
  #return(code);
}
