#Install required packages
install.packages("jpeg")
install.packages("wvtool")
library(jpeg)
library(wvtool)

#Apply all required steps for each 20 images which are already converted into gray scale format 
for (t in 1:20){
  
  #Read the gray-scaled image
  path = paste("C:/Users/Latif/Desktop/ProjectPart3/Images_gray/", t, ".jpg", sep = "")
  img <- readJPEG(path, native = FALSE)
  
  #Read the colored image
  path2 = paste("C:/Users/Latif/Desktop/ProjectPart3/Images/", "Fabric", t, ".jpg", sep = "")
  colored_img <- readJPEG(path2, native = FALSE)
  
  #Take samples from image as windows sized of 8x8
  
  #Reduce the dimension of the image matrix to two  
  img <- img[,,1]
  
  #Create new matrices for both original gray-scaled image and anomaly detected image
  img_new <- img
  img_original <- img

  #Apply gabor filtering to the image
  test <- gabor.filter(x=img, lamda=2, theta=60, bw=8, phi=0, asp=0.1, disp=FALSE)
  gaborr_filtered<-test[["filtered_img"]]
  img <-gaborr_filtered/max(gaborr_filtered)
  
  #Set size of the squares
  N<-8
  
  #Set the counter to zero which counts the detected problematic points
  counter=0
  
  #Calculate X-bar-bar value
  x_bar_bar<-mean(img[,])
  sd_sum<-0
  
  #Set an array to store x-bar values of windows
  mean_array<-matrix(data=NA,4096,1)
  
  #Set an array to store s values
  sd_array<-matrix(data=NA,4096,1)
  indis<-0
  
  #Calculate sample standard deviations
  for(i in seq(1,512-N+1,N)){
    for(j in seq(1,512-N+1,N)){
      sd_sum<-sd_sum+sd( img[(i:(i+N-1)),(j:(j+N-1))] )
    }
  }
  #Calculate s-bar
  sd_bar<-sd_sum/((512*512)/(N*N))
  
  #Calculate control chart limits for x-bar
  A_3<-(3*(4*N*N-3))/(4*(N*N-1)*sqrt(N*N))
  ucl <- x_bar_bar+A_3*sd_bar*(4-diff(range(img_original))^3)
  lcl <- x_bar_bar-A_3*sd_bar*(4-diff(range(img_original))^3)
    
  #Calculate control chart limits for s chart
  c_4<-4*(N-1)/(4*N-3)
  B_3<-1-(3*sqrt(1-c_4*c_4))/c_4
  B_4<-1+(3*sqrt(1-c_4*c_4))/c_4
  ucl_s <- B_4*sd_bar*3
  lcl_s <- B_3*sd_bar
  #Set lover limit to zero if it is negative
  if(ucl_s<0){
    ucl_s<-0
  }
  
  
  #Make black points when anomaly is detected
  for(i in seq(1,512-N+1,N)){
    for(j in seq(1,512-N+1,N)){
      sample_mean<-mean( img[(i:(i+N-1)),(j:(j+N-1))] )
      sample_sd<-sd( img[(i:(i+N-1)),(j:(j+N-1))] )
      
      mean_array[indis]<-sample_mean
      sd_array[indis]<-sample_sd
      indis <- indis+1
      
      if((sample_mean<lcl | sample_mean>ucl) | (sample_sd<lcl_s | sample_sd>ucl_s)){
        img_new[(i:(i+N-1)),(j:(j+N-1))]<-0
        counter=counter+1
      }
    }
  }
  
  #If there is no point is marked as problematic, control for vertically or horizontally oriented thin problems
  
  #Control the vertical errors
  if(counter == 0){
    img_new <- img_original
    x_bar_bar<-mean(img[,])
    sd_sum<-0
    
    #Calculate s value for each vertical line
    for(j in 1:512){
      sd_sum<-sd_sum+sd(img[,j])
    }
    sd_bar<-sd_sum/512
    
    #Calculate control chart limits for x-bar chart
    A_3<-(3*(4*512-3))/(4*(512-1)*sqrt(512))
    ucl <- x_bar_bar+A_3*sd_bar*4
    lcl <- x_bar_bar-A_3*sd_bar*4
    
    #Plot thin problenmatic vertical lines
    for(j in 1:512){
      colum_mean<-mean(img[,j])
      
      if(colum_mean < lcl | colum_mean > ucl) {
        for(i in 1:512){
          img_new[i,j] = 0
        }
      }
      else {img_new[i,j] = img[i,j]}
    }
  }
  
  
  #Control the horizontal errors
  else if(counter == 0){
    img_new <- img_original
    x_bar_bar<-mean(img[,])
    sd_sum<-0
    
    #Calculate s value for each horizontal line
    for(j in 1:512){
      sd_sum<-sd_sum+sd(img[j,])
    }
    sd_bar<-sd_sum/512
    
    #Calculate control chart limits for x-bar chart
    A_3<-(3*(4*512-3))/(4*(512-1)*sqrt(512))
    ucl <- x_bar_bar+A_3*sd_bar*3
    lcl <- x_bar_bar-A_3*sd_bar*3
    
    #Plot thin problenmatic horizontal lines
    for(i in 1:512){
      row_mean<-mean(img[i,])
      
      if(row_mean < lcl | row_mean > ucl) {
        for(j in 1:512){
          img_new[i,j] = 0
        }
      }
      else {img_new[i,j] = img[i,j]}
    }
  }

  
  #Do not mark black the points which are falsely detected as problematic
  if(counter > 18){
    for(i in seq(N+1,512-N-N+1,N)){
      for(j in seq(N+1,512-N-N+1,N)){
        
        if(img_new[i,j]==0){
          if((img_new[i-N,j]!=0) & (img_new[i,j-N]!=0) & (img_new[i,j+N]!=0) & (img_new[i+N,j]!=0)){
            img_new[(i:(i+N-1)),(j:(j+N-1))]<-img_original[(i:(i+N-1)),(j:(j+N-1))]
            
          }
        }
      }
    }
  }
  
  
  #Plot original and anomaly detected images
  par(mfrow=c(1,2))
  
  #Original Image
  plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = paste("Original Image ", t, sep = ""))
  rasterImage(colored_img[,,],0, 0, 512, 512)
  
  #Anomaly detected Image
  plot(1, type="n", xlim=c(0, 512), ylim=c(0, 512), main = "Anomaly-detected Image")
  rasterImage(img_new[,],0, 0, 512, 512)
  
  
  #Plot x-bar and s-chart for images  
  par(mfrow=c(1,2))
  
  #control chart for x bar
  plot(mean_array, xlim=c(0, 4096), ylim=c(0, 1), main = paste("X-bar Chart of Image ", t, sep = ""))
  abline(h=x_bar_bar, col="blue")
  abline(h=ucl, col="red")
  abline(h=lcl, col="red")
  
  #control chart for s
  plot(sd_array, xlim=c(0, 4096), ylim=c(0, 1), main = paste("S Chart of Image ", t, sep = ""))
  abline(h=sd_bar, col="blue")
  abline(h=ucl_s, col="red")
  abline(h=lcl_s, col="red")
  
}

