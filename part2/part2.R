#Project Part 2

#------------------------------------------TASK 1-------------------------------------------

install.packages("jpeg")
library(jpeg)

#read the gray scaled version of the original image
img <- readJPEG("C:/Users/Latif/Desktop/ProjectPart2/group_3gray.jpg", native = FALSE)

#Structure of the img variable
str(img)

#Dimensions of the img variable
dim(img)

#Plot the grayscaled image
img <- img[,,1]
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "gray-scale")
rasterImage(img[,],0, 0, 400, 400)

#Plot the histogram of the image
hist(img, nclass = 50)

#Create a x control chart for row1
row1 <- img[1,]
hist(row1)

cl <- mean(row1)
ucl <- mean(row1) + 3*sd(row1)
lcl <- max(0, mean(row1) - 3*sd(row1))

plot(row1, xlim=c(0, 400), ylim=c(0, 1))
abline(h=mean(row1), col="blue")
abline(h=ucl, col="red")
abline(h=lcl, col="red")


#Create a control chart for ROWS
img_new <- img

for (i in 1:400) {
  ucl <- mean(img[i,]) + 3*sd(img[i,])
  lcl <- max(0, mean(img[i,]) - 3*sd(img[i,]))
  
  #Identify and change the values of the pixels which are out of the control limits (detect anomalies in the image)
  for(j in 1:400) {
    if(img[i,j] < lcl | img[i,j] > ucl) {
      img_new[i,j] = 0
    }
    else {img_new[i,j] = img[i,j]}
  }
  
}

#Plot the new anomaly wrt rows detected image with original image
par(mfrow=c(1,2))

plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "Original Image")
rasterImage(img[,],0, 0, 400, 400)

plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "Anomaly-detected Image")
rasterImage(img_new[,],0, 0, 400, 400)


#Create a control charts for COLUMNS
img_new <- img

for (i in 1:400) {
  ucl <- mean(img[,i]) + 3*sd(img[,i])
  lcl <- max(0, mean(img[,i]) - 3*sd(img[,i]))
  
  #Identify and change the values of the pixels which are out of the control limits (detect anomalies in the image)
  for(j in 1:400) {
    if(img[i,j] < lcl | img[i,j] > ucl) {
      img_new[i,j] = 0
    }
    else {img_new[i,j] = img[i,j]}
  }
  
}

#Plot the new anomaly wrt columns detected image with original image
par(mfrow=c(1,2))

plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "Original Image")
rasterImage(img[,],0, 0, 400, 400)

plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "Anomaly-detected Image")
rasterImage(img_new[,],0, 0, 400, 400)



#------------------------------------TASK 2-------------------------------

#read the gray scaled version of the original image
img <- readJPEG("C:/Users/Latif/Desktop/ProjectPart2/group_3gray.jpg", native = FALSE)

#Plot the grayscaled image
img <- img[,,1]
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "gray-scale")
rasterImage(img[,],0, 0, 400, 400)

#Create an empty matrix for regression (for original image --> nrow = 350*350 = 122500, ncol = 51*51 = 2601)
df_allpix <- matrix(0, nrow = 122500, ncol = 2601) 

n <- 51  #Size of the window
k <- 400  #Size of the original image


#Create df for the all windows (This df will be used for prediction)
row_ind <- 1
for (i in (((n+1)/2) : (k - (n-1)/2))){
  for (j in (((n+1)/2) : (k - (n-1)/2))){
    
    df_allpix[row_ind,] <- as.vector(img[(i-25):(i+25),(j-25):(j+25)]) 
    row_ind <- row_ind+1
  }
}
#Convert the data type of df_allpix object from matrix to dataframe
df_allpix <- as.data.frame(df_allpix)


#Crate df by getting windows by 3 steps (This df will be used to construct linear model)
df_trainpix <- matrix(0, nrow = 13689, ncol = 2601) #117*117=13689 windows were used

row_ind <- 1
for (i in seq((n+1)/2, k - (n-1)/2, by = 3)){
  for (j in seq((n+1)/2, k - (n-1)/2, by = 3)){
    
    df_trainpix[row_ind,] <- as.vector(img[(i-25):(i+25),(j-25):(j+25)]) 
    row_ind <- row_ind+1
    
  }
}
#Convert the data type of df_trainpix object from matrix to dataframe
df_trainpix <- as.data.frame(df_trainpix)

#Construct linear model predicting centeral pixel values of each window
linearMod  <- lm(V1301~., data = df_trainpix)

#Predict all center pixel values
predicted <- predict(linearMod, newdata = df_allpix)

#Get the difference(residuals) between actual and predicted pixel values
residual_pixels <- as.vector(predicted - df_allpix$V1301)

#Plot the histogram of the residuals to check normality
hist(residual_pixels, nclass = 100)

#Create limits for control chart for residuals
cl <- mean(residual_pixels)
ucl <- mean(residual_pixels) + 3*sd(residual_pixels)
lcl <- mean(residual_pixels) - 3*sd(residual_pixels)

#Plot the control chart
par(mfrow=c(1,1))
plot(residual_pixels)
abline(h=cl, col="blue")
abline(h=ucl, col="red")
abline(h=lcl, col="red")

#Create a matrix from obtained residual vector
residual_matrix <- matrix(residual_pixels, nrow = 350, ncol = 350, byrow = TRUE)

#Identify and change the values of the pixels which are out of the control limits (detect anomalies in the image)
img_new = img #img is the original image, img_new is the anomaly-detected image

counter = 0
for (i in 1:(k-n+1)) {
  for (j in 1:(k-n+1)) {
    
    if(residual_matrix[i,j] < lcl | residual_matrix[i,j] > ucl) {
      img_new[i+25,j+25] = 0
      counter <- counter+1
    }
  }    
}
#Print the number of pixels which are out of control limits
counter

#Plot the new anomaly detected image along with the original image
par(mfrow=c(1,2))

plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "Original Image")
rasterImage(img[,],0, 0, 400, 400)

plot(1, type="n", xlim=c(0, 400), ylim=c(0, 400), main = "Anomaly-detected Image")
rasterImage(img_new[,],0, 0, 400, 400)





