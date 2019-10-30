install.packages("jpeg")
library(jpeg)


#-------------QUESTION 1------------

#Read the image
img <- readJPEG("C:/Users/Latif/Desktop/IE423_ProjectPart1/kazak.jpg", native = FALSE)

#Structure of the img variable
str(img)

#Dimensions of the img variable
dim(img)

#-------------QUESTION 2------------

#Plot the original image
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Original")
rasterImage(img[,,],0, 0, 512, 512)

#Plot each channel of the image (Original, R, G, B) on a single plot
par(mfrow=c(2,2))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Original")
rasterImage(img[,,],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Red")
rasterImage(img[,,1],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Green")
rasterImage(img[,,2],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Blue")
rasterImage(img[,,3],0, 0, 512, 512)


#-------------QUESTION 3------------

#Get the matrix of each channel and store them in corresponding variables (red, green, blue)
red <- img[,,1]
green <- img[,,2]
blue <- img[,,3]

#Plot the line graph of the average of columns for each channel (red, green, blue) on a single plot
par(mfrow=c(1,1))
x=(1:length(colMeans(red)))

plot(x,colMeans(red), type="l",col="red", ylim=c(0, 1))
lines(x,colMeans(green),col="green")
lines(x,colMeans(blue),col="blue")


#-------------QUESTION 4------------

#Subtract the half of the image from the other half

#RED
red_s <- red

#Do substraction
red_s[,257:512] <- red[,257:512] - red[,1:256]

#Equate the values of the cells to zero whose values are smaller than zero 
for(i in 1:512) {
  for(j in 1:512) {
    if(red_s[i,j] < 0) {
      red_s[i,j] = 0
    }
  }
}

#Plot subtarcted image for red channel
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "original")
rasterImage(red_s,0, 0, 512, 512)


#GREEN
green_s <- green

#Do substraction
green_s[,257:512] <- green[,257:512] - green[,1:256]

#Equate the values of the cells to zero whose values are smaller than zero 
for(i in 1:512) {
  for(j in 1:512) {
    if(green_s[i,j] < 0) {
      green_s[i,j] = 0
    }
  }
}

#Plot subtarcted image for green channel
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "original")
rasterImage(green_s,0, 0, 512, 512)


#BLUE
blue_s <- blue

#Do substraction
blue_s[,257:512] <- blue[,257:512] - blue[,1:256]

#Equate the values of the cells to zero whose values are smaller than zero
for(i in 1:512) {
  for(j in 1:512) {
    if(blue_s[i,j] < 0) {
      blue_s[i,j] = 0
    }
  }
}


#Plot subtarcted image for green channel
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "original")
rasterImage(blue_s,0, 0, 512, 512)


#COLORED IMAGE

#Plot the subtracted image for the original colored image
img_subtracted <- img

img_subtracted[,,1] <- red_s[,]
img_subtracted[,,2] <- green_s[,]
img_subtracted[,,3] <- blue_s[,]

par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "original")
rasterImage(img_subtracted ,0 ,0 ,512 , 512)



#-------------QUESTION 5------------

install.packages("BiocManager")
BiocManager::install("EBImage")
library(EBImage)


a<-medianFilter(img,2) # radius 2, windows size 5 
par(mfrow=c(1,2))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Original")
rasterImage(img[,,],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Smoothed 5x5")
rasterImage(a[,,],0, 0, 512, 512)


a<-medianFilter(img,5) # radius 5, windows size 11 

par(mfrow=c(1,2))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Original")
rasterImage(img[,,],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Smoothed 11x11")
rasterImage(a[,,],0, 0, 512, 512)


a<-medianFilter(img,15) # radius 15, windows size 31 

par(mfrow=c(1,2))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Original")
rasterImage(img[,,],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Smoothed 31x31")
rasterImage(a[,,],0, 0, 512, 512)




###########################################  PART 2   ########################################


#-------------QUESTION 1------------

#read the gray scaled version of the original image
img_gray <- readJPEG("C:/Users/Latif/Desktop/IE423_ProjectPart1/kazak_gray.jpg", native = FALSE)

img_gray <- img_gray[,,1]
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "gray-scale")
rasterImage(img_gray[,],0, 0, 512, 512)

#Plot the histogram of the image
hist(img_gray)


#-------------QUESTION 2------------

mu <- mean(img_gray)
std <- sd(img_gray)
mu
std

#-------------QUESTION 3------------
counter1<-0


#Define upper and lower limits
upper_limit <- qnorm(0.9995, mean = mu, sd = std)
lower_limit <- qnorm(0.0005, mean = mu, sd = std)

img_gray_new <- img_gray

#Identify and change the values of the pixels which are out of boundries (detect anomalies in the image)
for(i in 1:512) {
  for(j in 1:512) {
    if(img_gray[i,j] < lower_limit | img_gray[i,j] > upper_limit) {
      img_gray_new[i,j] = 0
      counter1<-counter1+1
    }
    else {img_gray_new[i,j] = img_gray[i,j]}
  }
}

#Plot anomaly detected image
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "gray-scale")
rasterImage(img_gray_new[,],0, 0, 512, 512)

#Plot the histogram of the anomaly detected image
hist(img_gray_new)

#Plot anomaly detected image and the original gray_scaled image together
par(mfrow=c(1,2))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "original")
rasterImage(img_gray[,],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "anomaly-detected")
rasterImage(img_gray_new[,],0, 0, 512, 512)



#-------------QUESTION 4------------

counter2<-0

img_gray_new2<-img_gray

#Identify and change the values of the pixels which are out of boundries (detect anomalies in the image)
r<-0
for (i in 1:10) {
  for (j in 1:10) {
    indeks<-1
    for (k in (((i-1)*51)+1):(i*51)) {
      for (x in (((j-1)*51)+1):(j*51)) {
        r[indeks]<-img_gray_new2[k,x]
        indeks<-indeks+1
      }
    }
    mu<-mean(r)
    std<-sd(r)
    lower_limit<-qnorm(0.0005,mu,std)
    upper_limit<-qnorm(0.9995,mu,std)
    
    for (a in (((i-1)*51)+1):(i*51)) {
      for (b in (((j-1)*51)+1):(j*51)) {
        if( img_gray_new2[a,b] < lower_limit | img_gray_new2[a,b] > upper_limit) 
        {img_gray_new2[a,b]=0
        counter2<-counter2+1
        }
      }
    }
  }
}


#Plot anomaly detected image
par(mfrow=c(1,1))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "original")
rasterImage(img_gray_new2[,],0, 0, 512, 512)

#Plot anomaly detected image and the original gray_scaled image together
par(mfrow=c(1,2))
plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "original")
rasterImage(img_gray[,],0, 0, 512, 512)

plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "anomaly-detected")
rasterImage(img_gray_new2[,],0, 0, 512, 512)

