a <- seq(10,20)

b<- letters [4:13]

f <- c(1,1,1,0,0,0,0,0)
ff <- factor(f, levels =c(0,1), labels = c("YES", "NO"))


# define a local variable inside myfunc
myfunc <- function() {y <- 1; ls()}
myfunc()                # shows "y"

movie <- list (m1=list(Rank=1, Peak=1, Title="Avatar", WorldGross = 2787, Year = 2009),
          (m2= list(Rank=2, Peak=1, Title="Titanic", WorldGross = 2187, Year = 1997),
          (m3=(Rank=3, Peak=3, Title="Star wars", WorldGross = 2068, Year = 2015),
          (m4=(Rank=4, Peak=4, Title="avengers", WorldGross = 1844, Year = 2018),
          (m5 (Rank=5, Peak=3, Title="Jurassic World", WorldGross = 1671, Year = 2015)
          )
          
summary( movie)

mapply(rep, LETTERS[26:1],1:1, SIMPLIFY = FALSE)

for (x in sample.int(10)) {
  print(x)
  if(x==8) {
    break
  }
}

x <- sample.int(10)
while(x != 8) {
  x; 
}


library(dplyr)
nasa1<-as.data.frame(nasa)
nasa1 %>% filter(lat > 29.56 & lat < 33.09 & long > -110.93 & long < -90.55) %>%
  mutate(Temp.ratio = temperature/surftemp) %>% 
  group_by(year) %>%
  summarise(pressure_mean=mean(pressure ,na.rm=T),
            ozone_mean=mean(ozone ,na.rm=T),
             Temp.ratio_mean=mean(Temp.ratio ,na.rm=T),
              pressure_sd = sd(pressure),
            ozone_mean = sd(ozone),
            Temp.ratio_sd = sd(Temp.ratio)) %>%
              arrange(desc(ozone_mean))
          

