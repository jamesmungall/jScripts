j.sim.1 = function(){
# choose 21 random numbers between 0 and 1 (allE)
# if <0.1 then var = 100, else var = 1

allE = runif(21);
normals = c();
 for(e in allE){
  if (e<0.1){
   sigma = 100;
  }
  else sigma =1;

  x = rnorm(1, mean = 0, sd = sigma);
  normals = rbind(normals,x);
 }
return (normals);

}
