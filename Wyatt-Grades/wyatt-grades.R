library(googleVis)
library(lubridate)
library(stringr)
library(XML)

# Grades & Attendance
tmp <- readHTMLTable('~/R/Wyatt-Grades/data/grades.html')[[1]]
grades <- tmp[c(4:7, 9:10) , c(12, 17, 20, 21)]
colnames(grades) <- c('Course', 'Grade', 'Absences', 'Tardies')
grades$Course <- c('Science', 'Math', 'Band', 'Enrichment', 'Language Arts', 'Social Studies')
grades$Percent <- as.integer(str_sub(grades$Grade, start = 2))
grades$Grade <- as.character(str_sub(grades$Grade, end = 1))
grades <- grades[ , c(1, 3, 4, 2, 5)]

# Classes
parse_class <- function(filename, dir = '~/R/Wyatt-Grades/data/') {
  x <- readHTMLTable(str_c(dir, filename))[[2]]
  x <- x[2:nrow(x), c(1:3, 9:11)]
  names(x) <- c('Date', 'Category', 'Assignment', 'Score', 'Percent', 'Grade')
  x$Date <- mdy(x$Date)
  x$Score <- as.character(x$Score)
  x$Percent <- as.integer(as.character(x$Percent))
  x$Grade <- factor(x = x$Grade, levels = c('A', 'B', 'C', 'D', 'F'))
  x
}

classes <- lapply(c('science.html', 'math.html', 'band.html', 'ela.html', 'social-studies.html'), parse_class)
names(classes) <- c('Science', 'Math', 'Band', 'ELA', 'Social-Studies')


parse_score <- function(x) {
  tmp <- str_split(x, pattern = "/")
  scored <- sapply(tmp, function(x) x[1])
  scored <- str_replace(scored, "--", "0")
  scored <- sum(as.numeric(scored))
  possible <- sum(as.numeric(sapply(tmp, function(x) x[2])))
  c(scored, possible)
}


# Scores for the past n assignments by class
last_assignments <- function(x, n = 5) tail(x[date(now()) > x$Date, ], n)
last3 <- lapply(classes, last_assignments, 3)
scores <- lapply(lapply(classes, last_assignments, 3), function(x) parse_score(x$Score))
percents <- sapply(scores, function(x) 100 * x[1] / x[2])

# Scores for the past n assignments by class
last_assignments_by_date <- function(x, n.days = 10) {
  x <- x[date(now()) > x$Date, ]
  days <- difftime(date(now()), x$Date, 8)
  x[days <= n.days, ]
}

lapply(classes, last_assignments_by_date)
lapply(lapply(classes, last_assignments_by_date), function(x) parse_score(x$Score))


# Upcoming assignments
upcoming_assignments <- function(x) x[date(now()) < x$Date, ]
lapply(classes, upcoming_assignments)
lapply(lapply(classes, upcoming_assignments), function(x) parse_score(x$Score))



tmp <- data.frame(Course = grades$Course, Percent = grades$Percent)
gauges <- lapply(1:nrow(tmp), function(n) gvisGauge(tmp[n, ], options=list(min=0, max=100, redFrom=0, redTo=69, yellowFrom=70, yellowTo=84, greenFrom=85, greenTo=100)))
plot(gauges[[1]])




