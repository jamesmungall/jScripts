j.stat.oneWayAnova<-function(longDataFrame){
# One Way ANOVA           p24, purple book 31.5.15
#----------------------
# 
# ref: W&W pg 325 and purple book pg22, chickwts
# 
# Aim is to create a general one-way ANOVA calculator
#
# longDataFrame must be data.frame, with as many obervations as you want, and 2 variables.
# $ value: num 23, 43, ...
# $ treatment: Factor w/ as many levels as you want "casein", "horsemeal", ...
# 
# We need computations to complete the following ANOVA  table.
#---------------------------------------------------------------------------
# Source                SS    df    MS    F   p-value
# between treatmentLevels
# within(error) 

ldf <- longDataFrame; 
# TODO Check validity of longDataFrame

totalSS <- j.stat.sumOfSquares(ldf$value);

treatmentLevels <- levels(ldf$treatment); # feed is a factor with 6 levels.

withinSS = 0;
for(i in 1:length(treatmentLevels)){
  treatmentData<- subset(ldf, treatment==treatmentLevels[i]);
  ssi <- j.stat.sumOfSquares(treatmentData$value);
  withinSS <- withinSS+ssi;
}
betweenSS = totalSS - withinSS;
dofBetween = (length(treatmentLevels)-1);
dofWithin = length(ldf$value)-length(treatmentLevels);
MSbetween = betweenSS/dofBetween;
MSwithin = withinSS / dofWithin;
Fresult = MSbetween / MSwithin;
pvalue = df(Fresult,dofBetween, dofWithin);
anovaTable <- list(betweenSS=betweenSS,withinSS=withinSS,totalSS=totalSS,
                   dofBetween = dofBetween,dofWithin=dofWithin,
                   MSbetween = MSbetween, MSwithin=MSwithin,
                   Fresult=Fresult, pvalue=pvalue);

return(anovaTable); 
}