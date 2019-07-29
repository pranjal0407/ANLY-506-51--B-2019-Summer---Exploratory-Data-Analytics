#R Practice 1:
getwd()

#Creating vectors
x <- 1:5
y <- 6:10
z <- 11:15

test <- c(x,y,z)
test

#Using cbind to create matrix by combining x,y,z as columns
cbind(x,y,z)

#Using cbind to create matrix by combining x,y,z as rows
rbind(x,y,z)

#Numbers are turned into characters if the matrix has both.
cbind(c(1, 2, 3, 4, 5), 
      c("a", "b", "c", "d", "e"))

#Matrix with single vector of data can be formed using matrix() function.
#matrix with rows = 5 and columns = 2
matrix(data = 1:10,
       nrow = 5,
       ncol = 2)

#matrix with rows = 2 and columns = 5
matrix(data = 1:10,
       nrow = 2,
       ncol = 5)

#matrix with rows = 2 and columns = 5 but filled by row instead of columns
matrix(data = 1:10,
       nrow = 2,
       ncol = 5, byrow = TRUE)

#dataframe
#similar to cbind but column names can be given.
survey <- data.frame("index"= c(1:5),
                     "sex"= c("m","m","f","f","m"),
                     "age"= c(44,23,45,28,32))
str(survey)

#stringsAsFactors = FALSE is passed so that R donot converts string columns to factors
survey <- data.frame("index"= c(1:5),
                     "sex"= c("m","m","f","f","m"),
                     "age"= c(44,23,45,28,32), stringsAsFactors = FALSE)
str(survey)

library(help ="datasets")
str(ChickWeight)
str(trees)

#head(): To view first few rows of dataframe
head(ChickWeight)

#tail(): To view last few rows of dataframe
tail(ChickWeight)

#View: To view entire dataframe. Opens in new window
View(ChickWeight)
View(trees)

#To get the statstical summary of all column in dataframe
summary(ToothGrowth)
summary(trees)

#To get info on classes of columns in a dataframe
str(trees)

#To get names of columns in a dataframe
names(trees)
names(ToothGrowth)

#Return the len column of ToothGrowth
ToothGrowth$len

#To get the mean of the len column of ToothGrowth
mean(ToothGrowth$len)

#To get the supp column of ToothGrowth
table(ToothGrowth$supp)

#To get the len AND supp columns of ToothGrowth
head(ToothGrowth[c("len", "supp")])

#Working with adding new columns
#Sample data survey2
survey2 <- data.frame("index"= c(1:5), 
                      "name"= c("Jon","Jack","Jasmine","Jacob","Jannet"),
                      "age"=c(45,23,47,34,21), stringsAsFactors = FALSE)
#Adding new column sex
survey2$sex <- c("m", "m", "f", "f", "m")
survey2
str(survey2)

#Changing column name in survey2 dataframe from index to participant.number
names(survey2)[1] <- "participant.number"
survey2

#changing column name using logical indexing
names(survey2)[names(survey2) == "age"] <- "years"
survey2

#slicing using [,]
#To get the rows 1-6 and column 1 of ToothGrowth
ToothGrowth[1:6, 1]

#To get rows 1-3 and columns 1 and 3 of ToothGrowth
ToothGrowth[1:3, c(1,3)]

#To get the 1st row (and all columns) of ToothGrowth
ToothGrowth[1, ]

#To get the 2nd column (and all rows) of ToothGrowth
ToothGrowth[, 2]

#Creating a new df with only the rows of ToothGrowth where supp equals VC
ToothGrowth.VC <- ToothGrowth[ToothGrowth$supp == "VC", ]
ToothGrowth.VC

#Creating a new df with only the rows of ToothGrowth where supp equals OJ and dose < 1
ToothGrowth.OJ.a <- ToothGrowth[ToothGrowth$supp == "OJ" & ToothGrowth$dose < 1, ]
ToothGrowth.OJ.a

#subset() is used to create a new subsetted dataset
#To get rows of ToothGrowth where len < 20 AND supp == "OJ" AND dose >= 1
subset(x = ToothGrowth,
       subset = len < 20 &
         supp == "OJ" &
         dose >= 1)

#To get rows of ToothGrowth where len > 30 AND supp == "VC", but only return the len and dose columns
subset(x = ToothGrowth, subset = len > 30 & 
         supp == "VC", 
       select = c(len, dose))

#What is the mean tooth length of Guinea pigs given OJ?
#step 1 creating subsetted dataframe OJ
oj <- subset(x = ToothGrowth,
             subset = supp == "OJ")
oj
#step 2 caluclating mean
mean(oj$len)

#using logical indexing
oj <- ToothGrowth[ToothGrowth$supp == "OJ",]
mean(oj$len)

#with is used to refer to a dataframe when working with multiple columns.
#sample dataset
health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))
health

#example of use of with()
health$weight / health$height ^ 2
with(health, height / weight ^ 2)

#example of use of with() both provides same output.
health$weight + health$height / health$age + 2 * health$height
with(health, weight + height / age + 2 * height)

#exercise
piratesurvey <- data.frame(
  name = c("Astrid", "Lea", "Sarina", "Remon", "Letizia", "Babice", "Jonas", "Wendy", "Niveditha", "Gioia"),
  sex = c("F", "F", "F", "M", "F", "F", "M", "F", "F", "F"),
  age = c(30, 25, 25, 29, 22, 22, 35, 19, 32, 21),
  superhero = c("Batman", "Superman", "Batman", "Spiderman", "Batman",
                "Antman", "Batman", "Superman", "Maggott", "Superman"),
  tattoos = c(11, 15, 12, 5, 65, 3, 9, 13, 900, 0),
  stringsAsFactors = FALSE
)
piratesurvey
median_age<-median(piratesurvey$age)
mean_female <-mean(piratesurvey$age[piratesurvey$sex == "F"])
mean_male <-mean(piratesurvey$age[piratesurvey$sex == "M"])
median_age 
mean_female
mean_male
max_tattoo <-with(piratesurvey, 
                  max(tattoos[sex == "M"]))
max_tattoo
Q_5 <-with(piratesurvey, mean(sex[age < 25] == "F"))
Q_5
Q_6 <-with(piratesurvey, mean(age[sex == "F"] < 25))
piratesurvey$tattoos.per.year <- with(piratesurvey, tattoos / age) ##Q7
piratesurvey$name[piratesurvey$tattoos.per.year == max(piratesurvey$tattoos.per.year)] ##Q8
piratesurvey$name[with(piratesurvey, sex == "F" & superhero == "Superman")] ##Q9
with(piratesurvey, (tattoos[age > 20 & superhero == "Spiderman"])) ##Q10

#R Practice 2:
library(tidyverse)
#To determine vector type
typeof(letters)
typeof(1:10)

#To determine vector length
x <- list("a", "b", 1:10)
length(x)

#Logical vectors. Three possible value TRUE, FALSE and NA
1:10 %% 3 == 0

#Integer and double vectors are known as numeric vectors
typeof(1)
typeof(1L)
1.5L

#doubles are approx.
x <- sqrt(2) ^ 2
x
x - 2

#Integers have one special value: NA, while doubles have four: NA, NaN, Inf and -Inf.
c(-1, 0, 1) / 0

x <- "This is a reasonably long string."
pryr::object_size(x)

y <- rep(x, 1000)

x <- sample(20, 100, replace = TRUE)
y <- x > 10
#How many are greater than 10?
sum(y)  

#what proportion are greater than 10?
mean(y) 

typeof(c(TRUE, 1L))

typeof(c(1L, 1.5))

typeof(c(1.5, "a"))

sample(10) + 100
runif(10) > 0.5


1:10 + 1:2

1:10 + 1:3
#Warning message:
#In 1:10 + 1:3 :
#longer object length is not a multiple of shorter object length

tibble(x = 1:4, y = 1:2)
#Error: Tibble columns must have consistent lengths, only values of length one are recycled:
#* Length 2: Column `y`
#* Length 4: Column `x`

tibble(x = 1:4, y = rep(1:2, 2))

tibble(x = 1:4, y = rep(1:2, each = 2))

c(x = 1, y = 2, z = 4)

set_names(1:3, c("a", "b", "c"))


x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

x[c(1, 1, 5, 5, 5, 2)]

x[c(-1, -3, -5)]


x[c(1, -1)]
#Error in x[c(1, -1)] : only 0's may be mixed with negative subscripts

x[0]

x <- c(10, 3, NA, 5, 8, 1, NA)

#All non-missing values of x
x[!is.na(x)]

# All even (or missing!) values of x
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

#creating list using list()
x <- list(1, 2, 3)
x

str(x)

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

#list() can have mixed objects.
y <- list("a", 1L, 1.5, TRUE)
str(y)

#list can have multiple lists
z <- list(list(1, 2), list(3, 4))
str(z)


#Visual Listing
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

#Sub-setting
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

#[ extracts a sub-list. The result will always be a list.
str(a[1:2])

str(a[4])

str(a[[1]])

str(a[[4]])

a$a

a[["a"]]

#Attributes
x <- 1:10
attr(x, "greeting")

attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

as.Date

methods("as.Date")

getS3method("as.Date", "default")

getS3method("as.Date", "numeric")

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)

attributes(x)

x <- as.Date("1971-01-01")
unclass(x)

typeof(x)

attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)

typeof(x)

attributes(x)

attr(x, "tzone") <- "US/Pacific"
x

attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
typeof

attributes(y)

tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)

attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)

attributes(df)













































