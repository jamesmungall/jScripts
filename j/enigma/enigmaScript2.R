j.enigma.f1 = function(){
# input is 11531
 input = 11531;
# need to turn this into a character string
 inputAsChar = j.fr.numberToPlaceValueVector(input);
 
  index = 1;
  result = list();
  result$char = inputAsChar[[index]];
  result$myFreq = j.enigma.f3(inputAsChar,index);

  inex

  return(result);
}

# function to count consecutive occurrences of character
# currentIndex is where to start reading for consecutive characters from
j.enigma.f3 = function(inputAsChar, currentIndex){
  # read first character
  firstChar = inputAsChar[[currentIndex]];
  # set counter for frequency of this character
  count = 1;
  # loop while the next characters are equal to the first character
  # next character is calculated from 
  while(firstChar == inputAsChar[[currentIndex+1]]){
    # if so, increase count
    count = count + 1;
    currentIndex = currentIndex + 1;
  }
  return(count);
}