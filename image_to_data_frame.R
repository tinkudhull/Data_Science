getwd()
setwd("E:/Files/av/digit/Train")
train = read.csv("train.csv")
str(train)
setwd("E:/Files/av/digit/Train/Images/train")
library(png)
#finding names of all png files 
temp = list.files(pattern="*.png")
#removing .png from the end of names 
for (i in 1:(length(temp))){
  temp[i] = substr(temp[i], 1, nchar(temp[i])-4)
}
#convert to numeric
temp = as.numeric(temp)
#sorting 
temp = (sort(temp, decreasing = FALSE))
#adding .png in the end
temp2 = paste0(temp,".png" )
#reading all png files
myfile = lapply(temp2, readPNG)
newfile = list(NA)
#creating a list of all 2d matrices
for (i in 1:(length(temp))){
  newfile[[i]] = myfile[[i]][,,1]
}
# making each matrix in list as vectors
newnewfile = list(NA)
for (i in 1:(length(temp))) {
  newnewfile[[i]] = as.vector(as.matrix(newfile[[i]]))
}
#converting list into matrix then to data frame and combining with train
mm = do.call(rbind, newnewfile)
ooo = as.data.frame(mm)
full = cbind(train, ooo)
#writing csv of this new train data
write.csv(full, "fulltrain.csv", row.names = FALSE)

# same steps with test data
setwd("E:/Files/av/digit/Train/Images/test")
library(png)
temp = list.files(pattern="*.png")
for (i in 1:length(temp)){
  temp[i] = substr(temp[i], 1, nchar(temp[i])-4)
}

temp = as.numeric(temp)
temp = (sort(temp, decreasing = FALSE))
temp2 = paste0(temp,".png" )
myfile = lapply(temp2, readPNG)
newfile = list(NA)
for (i in 1:(length(temp))){
  newfile[[i]] = myfile[[i]][,,1]
}
newnewfile = list(NA)
for (i in 1:(length(temp))) {
  newnewfile[[i]] = as.vector(as.matrix(newfile[[i]]))
}
mm = do.call(rbind, newnewfile)
ooo = as.data.frame(mm)
test = data.frame(filename = temp2)
fulltest = cbind(test, ooo)
write.csv(fulltest, "fulltest.csv", row.names = FALSE)
