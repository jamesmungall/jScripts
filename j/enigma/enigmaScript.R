# Enigma 1708 ref: http://www.newscientist.com/article/mg21528751.900-enigma-number-1708.html


# From a point on one side of a rectangular sheet of paper I drew two straight
# lines, one of them to a point on one adjacent side and the other to a point
# on the other adjacent side. My sheet of paper was now divided into two
# triangles and a pentagon. The lengths of the sides of the triangles were all
# integers, the lengths of the sides of the pentagon were, in some order, five
# consecutive integers, each less than 50.
#
# What were the dimensions of the sheet of paper?

# step 1: list all perfect triangle with hypoteneuse  < 50
# Generate pairs of perfect triangles in which the hypoteneuses are 
# consecutive numbers. e.g. (15,20,25) & (10,24,26)
# step 2:
# Call the lower number 'c', the higher number 'd'.
# Then 'b' is one less than 'c', 
# 'e' is one more than 'd', 
# step 2b: calculate the possible left heights and right heights and see if 
# there is a match. In above example, possible left heights are (15+24), (20+24)
# and possible right heights are (10+27), (24+27).


# step 3: Generate paris of perfect triangles in which the hypoteneuses are 
# 4 units apart e.g. (21,28,35) and (15,36,39)
# step 4: For the pairs of triangles in step 2, calculate the two possible
# lengths of the bottom piece. e.g. if hyps are 25 & 26, the bottom piece could
# be 23 or 27.
# step 5: For the pairs of triangles in step 3, calculate the bottom length.
# e.g. if the hyps are 35 & 39, the bottom pieces must be 37.
# step 6: For each of the pairs, calculate the four possible top lengths.
# e.g. (15,20,25) & (10,24,26) gives possible top lengths of (15+10),(15+24),
# (20+10),(20+24)
# step 7: Identify whether the bottom piece is in the set of possible top 
# lengths
# step 8: If

j.enigma.step1 = function(){
  # perfect triangles with hypoteneuse < 50
  allT = j.sq.perfectTriangles(18);
  listOfPairs = j.vm.2Dlist(18,2);
  count=0;
  for(i in 1:nrow(allT)){
    for(j in i:nrow(allT)){
      difference = allT[i,][[3]] - allT[j,][[3]];
      if(difference + 3 ==0){
        count = count+1;
        listOfPairs[[count]][[1]]=allT[i,];
        listOfPairs[[count]][[2]]=allT[j,];
      }
    }
  }
  listOfPairs = listOfPairs[1:count];
  return(listOfPairs);
}
j.enigma.step2 = function(){
  listOfPairs = j.enigma.step1();
  for(i in 1:length(listOfPairs)){
    c = listOfPairs[[i]][[1]][[3]];
    d = listOfPairs[[i]][[2]][[3]];
    b = c+1;
    e = d-1;
    leftHeights = c(b+listOfPairs[[i]][[1]][[1]],b+listOfPairs[[i]][[1]][[2]]);
    rightHeights = c(e+listOfPairs[[i]][[2]][[1]],e+listOfPairs[[i]][[2]][[2]]);
    print(intersect(leftHeights,rightHeights));
  }
}

j.enigma.evaluatePair=function(pair){
  addAll = j.vm.addAll(pair[[1]],pair[[2]]);
  topSet = j.vm.subMatrix(addAll,c(1,2),c(1,2));
  bottomSetMax = min(pair[[1]][[3]],pair[[2]][[3]])+5;
  bottomSetMin = max(pair[[1]][[3]],pair[[2]][[3]])-5;
  bottomSet = c(bottomSetMin:bottomSetMax);
  o = list();
  o$firstT = pair[[1]];
  o$secondT = pair[[2]];
  o$topSet = topSet;
  o$bottomSet = bottomSet;
  o$topBottom = j.sets.intersect(topSet,bottomSet);
  if(length(o$topBottom==1)){
    o$sidesResult = j.enigma.evaluatePair2(pair,); 
  }
  return(o);
}
j.enigma1708.do1 = function(max=18){
  # perfect triangles with hypoteneuse < 50 
  allT = j.sq.perfectTriangles(max);
  bigList = j.vm.extendingMatrix(max);
  for(i in 1:max){
    for(j in i:max){
      bigList[[i]][[j]]=matrix(c(allT[i,],allT[j,]),ncol=3,byrow=TRUE); 
    }
  }
  return(bigList);
}




j.enigma.1706=function(){
# New Scientist, 14th July 2012
# Three positive whole numbers; altogether they use nine digits
# with no digit repeated. One of the numbers is a perfect square
# and the other two are primes. They add up to 1951.
  
  oResult = vector(mode="list");
  count=0;
  # vector of squares, up to about 2000
  squares = j.sq.perfectSquare(44);
  # shortened list for test purposes
#  squares = squares[1:10];
  # the two primes should add up to a number in this diff vector
  diff = 1951-squares;
  # list of primes
  primes = j.io.readCsvToVector("j/data/primes.txt");
  primes = j.vm.dataFrameToVector(primes);
  # shortened list for test purposes
#  primes = primes[1:10];
  # sieve matrix where pairs of primes have unique digit vectors
  
  xyPairs = expand.grid(xPrimes = primes, yPrimes = primes);

  uniqueDigits = apply(xyPairs,1,j.enigma.1706.getSet);
  xyPairs <- cbind(xyPairs,uniqueDigits);
  xyPairs <- xyPairs[xyPairs$uniqueDigits,];
  
  sumPrimes = xyPairs[,1]+xyPairs[,2];
  
  diffPairs = expand.grid(primes = sumPrimes,diff = diff);
  uniqueDigits = apply(diffPairs,1,j.enigma.1706.getSet);
  diffPairs <- cbind(diffPairs,uniqueDigits);
  diffPairs <- diffPairs[diffPairs$uniqueDigits,];
  
  bool2 <- apply(diffPairs,1,function(x)return(x[1]==x[2]));
  diffPairs <- cbind(diffPairs,bool2);
  diffPairs <- diffPairs[diffPairs$bool2,];
  return(diffPairs);
}

j.enigma.1706.getSet = function(x){
  xDigits = j.ints.toDigitVector(x[1]);
  yDigits = j.ints.toDigitVector(x[2]);
  zDigits = c(xDigits,yDigits);
  
  return(length(zDigits)==length(unique(zDigits)));
}

# Enigma 1705, started 27th July 2012
# http://www.newscientist.com/article/mg21528721.700-enigma-number-1705.html
# Harry and Tom have each found a 4-digit perfect square, a 3-digit perfect 
# cube and a 3-digit triangular number that use 10 different digits, but with 
# no leading zero. A triangular number is one that fits the formula n(n+1)/2, 
# such as 1, 3, 6, 10, 15.

# Even if I told you Harry's triangular number you would not be able to deduce 
# with certainty which perfect square he has found. Even if I told you Tom's 
# perfect square you would not be able to deduce with certainty which 
# triangular number he has found.

# What are (a) Harry's triangular number, and (b) Tom's perfect square? 

j.enigma1705.do1 = function(){
  o = list(); 
# Get list of 4 digit perfect squares
  prSq = j.sq.perfectSquare(99);
  o$prSq = prSq[32:99];
# remove those with duplicate digits

  # get list of 3 digit perfect cubes
  o$prCb = c(125,216,343,512,729);

  # list of 3-digit triangular numbers
  trNo = j.sq.triangNumber(44);
  o$trNo = trNo[14:44];

  digits = c(0,1,2,3,4,5,6,7,8,9);
  
#  loop through all cubes
  cube1 = o$prCb[[1]];
  cube1 = j.ints.toDigitVector(cube1);
  remainingDigits = j.sets.setDiff(digits,cube1);

# valid triangle numbers
  for(i in 1:length(o$trNo)){
    triangDigits = j.ints.toDigitVector(o$trNo[[i]]);
    digits = j.sets.setDiff(remainingDigits,triangDigits);
    if(length(digits)==4){
       print(triangDigits);
    }
  }
  return(o);
}


j.enigma1705.giantMatrix=function(o){
  for(i in 1:length(o$prSq)){
    for(j in 1:length(o$prCb)){
      for(k in 1:length(o$trNo)){
        prSqDigits=j.ints.toDigitVector(o$prSq[[i]]);
        prCbDigits=j.ints.toDigitVector(o$prCb[[j]]);
        trNoDigits=j.ints.toDigitVector(o$trNo[[k]]);
        digits = c(prSqDigits,prCbDigits,trNoDigits);

        if(length(sort(unique(digits)))==10){
          print("result...");
          print(prSqDigits);
          print(prCbDigits);
          print(trNoDigits);
        }
      }
    }
  }
}

# Enigma 1703
# http://www.newscientist.com/article/mg21428705.800-enigma-number-1703.html

# I was staying at my sister's house when my niece Amy came home from 
# school feeling special. The class had been shown how to split a whole
# number, T, into whole number parts in such a way that the product of
# the parts was the greatest, G, that could be obtained for that T. For
# instance, she explained, 10 could be split into ten ones, or 2 and 4
# and 4, or 5 and 5, and so on, which would yield products of 1, 32, and
# 25 respectively. But, she warned, G exceeds 32 for T=10.

# Why did she feel special? Well, each pupil in the class had been given
# a different number in the range 20-50 inclusive for their personal T,
# and she had noted that, when she added the digits of her G together,
# the sum was exactly half of her T, and no one else in the class had T
# and G with this property.

# What value of T was Amy given?
j.enigma1703.getMaxProducts = function(startInts = c(20:50)){
  maxProducts = c();
  for(startInt in startInts){
    partSet = j.cmb.parts(startInt);
    productSet = j.enigma1703.getProductSet(partSet);
    maxProducts=c(maxProducts,max(productSet));
  }
  maxProdAsMatrix=matrix(c(startInts,maxProducts),ncol=2,byrow=FALSE);
  return(maxProdAsMatrix);
}
j.enigma1703.getProductSet = function(partSet){
  productSet = c();
  for(i in 1:nrow(partSet)){
    singlePart=partSet[i,];
    singleProduct=j.enigma1703.getSingleProduct(singlePart);
    productSet = c(productSet,singleProduct);
  }
  return(productSet);
}
j.enigma1703.getSingleProduct=function(singlePart){
  singlePartNoZeros=singlePart[singlePart!=0];
  singleProduct=prod(singlePartNoZeros);
  return(singleProduct);
}
j.enigma1703.test1 = function(){
  t1 = c(1,0,2,0,5);
  return(j.enigma1703.getSingleProduct(t1));
}
j.enigma1703.test2 = function(){
  t1 = c(1,0,2,0,5);
  t2 = c(2,0,2,0,4);
  m1 = matrix(c(t1,t2), nrow=2,byrow=TRUE);
  test2result = j.enigma1703.getProductSet(m1);
  return(test2result);
}

# 1701
# The display on my calculator shows 9876543210. As usual, up to seven illuminated strips are used to display each digit - the 8 using all seven, for example. There is just one special 10-figure number with the property that it is a perfect power of the total number of illuminated strips that it uses.
# With a little calculator effort it is possible to answer the following: How many illuminated strips does this special 10-figure number use?
j.enigma1701.getnFtable=function(n=c(20:30),x=5){
  listFs=j.sq.powerVector(n,x);
  listStrips=j.enigma1701.getListStrips(listFs);
  nFtable=data.frame(n,x,listFs,listStrips);
  return(nFtable);
}
j.enigma1701.getListStrips=function(listFs){
  listStrips=sapply(listFs,j.enigma1701.getSumOfStrips);
  return(listStrips);
}
j.enigma1701.getSumOfStrips=function(F){
  Fdigits = j.ints.toDigitVector(F);
  stripsAsVector=sapply(Fdigits,j.ints.digitMap1701);
  return(sum(stripsAsVector));
}
j.enigma1701.test1 = function(){
  v = c(0:9);
  return(sapply(v,j.ints.digitMap1701));
}
j.enigma1701.test2 = function(){
  F = 1079;
  return(j.enigma1701.getSumOfStrips(F));
}
j.enigma1701.test3 = function(){
  listFs=c(7,1079,234981723);
  return(j.enigma1701.getListStrips(listFs));
}
j.enigma1701.test4 = function(){
  n=c(1,2,3,4);
  x=2;
  return(j.sq.powerVector(n,x));
}
j.enigma1701.test5 = function(){
  return(j.enigma1701.getnFtable(c(1:4),2));
}


# Number of throws to complete a loop, X, is a random variable. Find E(X) and 
# Var(X)
#
#
#
j.mono.getDist = function(n=1000){
  allX = c();
  for(i in 1:n){
    x = j.mono.getX();
    allX = c(allX,x);
  }
  hist(allX, breaks = 1:15);
  o = list();
  o$mean = mean(allX);
  o$var = var(allX);
  o$distX = freq(allX);
  return(o);
}
j.mono.getX = function(){
  scores = c();
  while(sum(scores)<41){
    firstDie = sample(1:6,1);
    secondDie = sample(1:6,1);
    score = firstDie+secondDie;
    scores = c(scores, score);
  }
  o = list();
  o$scores = scores;
  o$x = length(scores)-1;
  return(o$x);
}
# Binomial (10,0.5)
#
#
j.bin.getDist = function(count=10000){
  allX = c();
# simulate ten coin tosses, repeated count times
  for(i in 1:count){
# x is the number of successes on tossing a coin 10 times.
    x = sum(rbinom(10,1,0.5));
    allX = c(allX,x);
  }
  hist(allX, breaks = 0:10);
  o = list();
  o$mean = mean(allX);
  o$var = var(allX);
  o$distX = freq(allX);
  myCumsum = cumsum(o$distX[,2])/count;
  o$distX = cbind(o$distX,myCumsum);
  return(o);
}
j.mono.singleTile = function(count = 100){
  # Returns the probability distribution for landing on
  # a single tile on one lap of the monopoly board. For
  # example, this could be interpreted as the probability
  # of landing on Mayfair for someone going once around 
  # the board.
  #   This result might be anticipated to be Poisson, 
  # lambda = 7/40 = 0.175
  runningTotal=0;
  store = c();
  storeRolls = c();
  for(i in 1:count){
    diceRoll = sample(1:6,1)+sample(1:6,1);
    runningTotal = runningTotal + diceRoll;
    if(runningTotal>40){ # once around the board
      runningTotal = runningTotal-40;
    }
    store = c(store,runningTotal);  
    storeRolls = c(storeRolls,diceRoll);
  }
  o=list();
  o$store = store;
  o$storeRolls = storeRolls;
  o$freqDist = freq(store);
  o$overallDist = freq(dist$freqDist[,1])
  return(o);
}
