## From R in Action 

## with w/ brackets Page 28
tmp <- mtcars
with(tmp, {
  tmp2 <- tmp[ , c('mpg', 'hp', 'disp')]
  summary(tmp2)
})


## Factors - Page 29
# Nominal vars -> categorical w/o order
# Ordinal vars -> categroical w/ order (be sure to use order = TRUE w/ the factor function)
# There is a handy labels parameter for converting numerical values to labels (ie sex = 1/2 to male/female)


## Data Manipulation Problem - Page 90
x <- data.frame(Student = c('John Davis', 'Angela Williams', 'Bullwinkle Moose', 'David Jones', 'Janice Markhammer',
                            'Cheryl Cushing', 'Reuven Ytzrhak', 'Greg Knox', 'Joel England', 'Mary Rayburn'),
                Math = c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522),
                Science = c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86),
                English = c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18))


## SQLDF Examples (Page 87)
library(sqldf)
sqldf("select * from x order by Student")
sqldf("select * from x order by Math desc, Science desc")
sqldf("select avg(Math) as 'Avg Math', avg(Science) as 'Avg Science', avg(English) as 'Avg English' from x")
apply(x[ , -1], 2, mean)

sqldf("select * from mtcars order by mpg desc limit 10")
sqldf("select cyl, avg(mpg), avg(disp), avg(hp) from mtcars group by cyl")

