# Prep the data
x <- read.csv('~/Downloads/Wyatt School Notes - AZ Merit Results 2016.csv', header = TRUE, comment.char = '#')
x <- x[ , c(-4, -5, -9, -10, -13)]
colnames(x)[7] <- 'Time'
x <- x[x$Type != 'Elementary' & x$Time <= 30, ]
head(x)
nrow(x)
summary(x)

# Adding a color column for the type of school
x$Color <- 'yellow'
x$Color[x$Type == 'High'] <- 'blue'
x$Color[x$Type == 'Middle'] <- 'green'
x$Color[x$Type == 'Middle / High'] <- 'red'
x$Color[x$Title == 'Granite Mountain Middle School'] <- 'black'

# Comparing the ELA and Math scores
plot(x$ELA, x$Math, main = 'Comparing ELA and Math Scores',
     type = 'p', col = x$Color, pch = 19, xlim = c(0, 100), ylim = c(0, 100))
legend(x = 5, y = 95, legend = c('High', 'Middle', 'M/H', 'UNK'), fill = c('blue', 'green', 'red', 'yellow'))
abline(a = 0, b = 1)

# Comparing Time and Mean score
plot(x$Time, x$Mean, main = 'Comparing Time and Mean Test Score',
     type = 'p', col = x$Color, pch = 19, ylim = c(0, 100))
legend(x = 22, y = 97, legend = c('High', 'Middle', 'M/H', 'UNK'), fill = c('blue', 'green', 'red', 'yellow'))


