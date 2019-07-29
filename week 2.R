#To print current working directory
getwd()

#To set new working directory
setwd(dir = "/Users/pranjalkhare4/EDA/Code Portfolio")
getwd()

#To print all objects in current workspace. Gives error if object is not present.
ls()

#Creating sample object
study1.df <- data.frame(id = 1:5, 
                        sex = c("m", "m", "f", "f", "m"), 
                        score = c(51, 20, 67, 52, 42))

score.by.sex <- aggregate(score ~ sex, 
                          FUN = mean, 
                          data = study1.df)

study1.htest <- t.test(score ~ sex, 
                       data = study1.df)

#saving a .RData file. File saved in test folder of current WD.
save(study1.df, score.by.sex, study1.htest,
     file = "data/study1.RData")

#To save all the objects
save.image(file = "data/projectimage.RData")

#Load objects in study1.RData into my workspace
load(file = "data/study1.RData")

# Load objects in projectimage.RData into my workspace
load(file = "data/projectimage.RData")

#rm(): To remove objects from workspace
#removing Q_6 object
rm(Q_6) #If ran again gives error Warning message: In rm(Q_6) : object 'Q_6' not found

#To remove all the objects
rm(list = ls())

#write.table() function to save file as .txt which can be read by other programs.
write.table(x = pirates,
            file = "pirates.txt",  #Save the file as pirates.txt
            sep = "\t")            # Make the columns tab-delimited

#saving outside working directory
write.table(x = pirates,
            file = "/Users/pranjalkhare4/pirates.txt",  # Save the file as pirates.txt to my desktop
            sep = "\t")                                 # Make the columns tab-delim

#reading a mydata.txt file from data folder in my working directory and storing in mydata
mydata <- read.table(file = 'data/mydata.txt',    # file is in a data folder in my working directory
                     sep = '\t',                  # file is tab--delimited
                     header = TRUE,               # the first row of the data is a header row
                     stringsAsFactors = FALSE)    # do NOT convert strings to factors!!

#To read a file from web
fromweb <- read.table(file = 'http://goo.gl/jTNf6P',
                      sep = '\t',
                      header = TRUE)
# Print the result
fromweb

# To read a non-text file into R
install.packages("foreign")

# To read Excel files
install.packages("xlsx")























