#remove objects
#https://stackoverflow.com/questions/43672774/what-is-the-difference-between-rm-and-rmlist-ls
a <- 1
b <- 2
ls()
rm(a) #specify what you want to remove
ls()

a <- 1
b <- 2
.c <- 3
ls()
rm(list=ls()) #removes all objects from the current workspace (R memory), except objects that start with a .
ls()

a <- 1
b <- 2
.c <- 3
ls()
rm(list = ls(all.names = TRUE)) #removes all objects from the current workspace (R memory)
ls()

#gc() # forces R to free the memory
gcinfo(TRUE)
gcinfo(verbose = FALSE)
