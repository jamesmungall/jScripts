# Object Oriented Exercises
# ==================================
# Start date: 30th June 2012
# 1. Load in console as
# source("C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\\j\\ob\\obScripts.R")
# or 
# select console window, file->Change dir->C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\
# then you just need > source("j/ob/obScript.R")
# 2. Run j.ob.startup() to load
# dependent packages: 
j.ob.startup=function(showPrints=FALSE){
  if(showPrints){
    print("loading j/ob/obScript.R");
    print("no dependencies");
  }
}
# 
# Aim: To learn useful objected oriented programming techniques with R.
# Functions: (all preceeded by j.ob. as a namespace)
#   1. 1
#   2. 2
#   3. duck
#   4. mallardDuck
#   5. redheadDuck
#   6. rubberDuck
#   7. decoyDuck
#   8. flyBehaviour
#   9. quackBehaviour

# 30th June, Object creation
j.ob.1 = function(){
	obj = list();
	obj[["indexRange"]] = j.check.indexRange;
	obj[["author"]] = "jms";#public var
	obj[["realAuthor"]] = function(){return("jms")}; # private var
	

	return(obj);
}
# some sort of inheritance
j.ob.2 = function(){
	obj=j.ob.1();
	obj[["favouriteColour"]]="orange";
	return(obj);
}
# Ducks
j.ob.duck = function(){
	o = list();
# All ducks will have this function.
	o$swim = function(){print("swim")};
# Flying and quacking are optional.
# Flying and quacking are variable.
# Flying and quacking can be changed at runtime.
# Duck superclass uses default fly and quack. Specific ducks will have overidden default behaviours at compile time.
	o = c(o,"oFlyBehaviour"=j.ob.flyBehaviour);
	o = c(o,"oQuackBehaviour"=j.ob.quackBehaviour);
	o$fly = o$oFlyBehaviour()$fly;
	o$quack = o$oQuackBehaviour()$quack;
	return(o);
}
j.ob.mallardDuck = function(){
	o = j.ob.duck();
	o$fly = o$oFlyBehaviour()$fly;
	o$quack = o$oQuackBehaviour()$quack;
#All ducks will have a unique display function.
	o$display = function(){print("looks like a Mallard")};
	return(o);
}
j.ob.redheadDuck = function(){
	o = j.ob.duck();
	o$fly = o$oFlyBehaviour()$fly;
	o$quack = o$oQuackBehaviour()$quack;
	o$display = function(){print("looks like a RedHead")};
	return(o);
}
j.ob.rubberDuck = function(){
	o = j.ob.duck();
	#rubber ducks don't fly
	o$fly = o$oFlyBehaviour()$flyNoWay;
	#rubber ducks don't quack
	o$quack = o$oQuackBehaviour()$squeek;
	o$display = function(){print("looks like a rubber duck")};
	return(o);
}
j.ob.decoyDuck = function(){
	o = j.ob.duck();
	#decoy ducks are silent
	o$quack =  function(){print("-silence-")};
	#and can't fly
	o$fly = o$oFlyBehaviour()$flyNoWay;
	o$display = function(){print("looks like a decoy duck")};
	return(o);
}
j.ob.flyBehaviour= function(){
	o = list();
	o$fly = function(){print("flying")};
	o$flyNoWay = function(){print("I can't fly")};
	return(o);
}
j.ob.quackBehaviour= function(){
	o = list();
	o$quack = function(){print("quack")};
	o$squeek = function(){print("squeek")};
	o$mute = function(){print("--silence--")};
	return(o);
}

#object creation
# e1 <- new.env();
# o$setInput <- function(input) {
#   assign("input", input, env=e1)
# }
# 
# o$getInput <- function() {
#   return(get("input", e1))
# }
		