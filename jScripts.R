# All jScripts
# ==================================
# Start date: 1st July 2012
# 1. Load in console as
# source("C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\\jScripts.R")
# or 
# select console window, file->Change dir->C:\\Documents and Settings\\jms\\My Documents\\Stats\\Rfiles\
# then you just need > source("jScripts.R")
# 2. Run j.startup() to load all jscripts
# 
j.startup=function(){
  library(partitions);
  library(gtools);
  library(sfsmisc);
  library(descr);
source("j/enigma/enigmaScript.R");
	source("j/vm/vmScript.R");
	j.vm.startup();
	source("j/check/checkScript.R");
	j.check.startup();
	source("j/ob/obScript.R");
	j.ob.startup();
	source("j/io/ioScript.R");
      j.io.startup();
	source("j/sq/sqScript.R");
      j.sq.startup();
	source("j/fr/frScript.R");
      j.fr.startup();
	source("j/ints/intsScript.R");
      j.ints.startup();
	source("j/sets/setsScript.R");
      j.sets.startup();
  source("j/cmb/cmbScript.R")
      j.cmb.startup();
}

# 
# Aim: To load all or some of my script files
# Functions: (all preceeded by j.[foldername]. as a namespace)
