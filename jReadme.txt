jReadme
=======

The purpose of this article is to describe how to load, use and write R functions. Some of this is technical use of R, but most is my choice of convention to allow for extensions.

n.b. open program by clicking on .RData icon to load saved functions and variables

Contents: 1. load   2. use   3. write

1. load; source([path/filename.R]); j.startup()

Example: In RGui, choose file->Change dir, then choose MyDocuments->Stats->Rfiles (correct July 2012)
In console, type source("jScripts.R");
In console, type j.startup();

Theory: By loading jScripts.R and running j.startup(), all functions will be loaded. They are viewable using ls().

Details: R scripts can be saved anywhere. During July 2012, the jScripts.R file is stored in MyDocuments->Stats->Rfiles. This file includes a j.startup() function which loads other scripts.

Other scripts are stored in subfolders starting with 'j' folder. Vector and matrix scripts are in 'vm' subfolder, object scripts are in 'ob' subfolder, check functions are in 'check' subfolder. 

Every script includes a [foldername].startup() function. For example vm.startup(). These are all called by the j.startup() function. They are used to load dependencies. For example 'gregmisc' or 'partitions'.

2. use
 j.[foldername].myFun; # prints out the function, including comments, which serve as a help file.
 j.[foldername].myFun(); # executes the function.

3. write
	a) create a folder in the 'j' folder. E.g. 'vm'
	b) create an R file called [foldername]Script.R. E.g. vmScript.R
	c) put a function called j.[foldername].startup() in the new script to put any dependencies in.
	d) Edit jScript such that it now loads the script and executes the startup function. Example of j.startup

function(){
        source("j/vm/vmScript.R");
        j.vm.startup();
        source("j/check/checkScript.R");
        j.check.startup();
        source("j/ob/obScript.R");
        j.ob.startup();
}

Example of j.vm.startup()

function(){
print("loading j/vm/vmScript.R");
print("dependent packages: partitions, gregmisc,sfsmisc");
        library(partitions);
        library(gregmisc);
        library(sfsmisc);
}
