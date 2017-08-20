library(dplyr)

mtcars %>%
  group_by(cyl) %>%
  summarise(disp = mean(disp), sd = sd(disp))


myfun<- function(x,y,z) {
  return (mean(x)*mean(y) -z)
}

mtcars %>%
  group_by(cyl) %>%
  summarise(disp = myfun(cyl,disp,3))

#That would work for individual variable like efficacy but what if I want to return multiple
#can I do $ ?

myfun<- function(x,y) {
  df<- data.frame( a= mean(x)*mean(y), b= mean(x)-mean(y) )
                   
  return (df)
}


mtcars %>%
  group_by(cyl) %>%
  summarise(a = myfun(cyl,disp)$a, b = myfun(cyl,disp)$b)

#The only hitch is that want to only call my fun once
#https://stackoverflow.com/questions/45778370/split-apply-combine-with-function-that-returns-multiple-variables

mtcars %>% group_by(cyl) %>% do(myfun(.$cyl, .$disp))
