a <- seq(10,20)

b<- letters [4:13]

f <- c(1,1,1,0,0,0,0,0)
ff <- factor(f, levels =c(0,1), labels = c("YES", "NO"))


# define a local variable inside myfunc
myfunc <- function() {y <- 1; ls()}
myfunc()                # shows "y"
