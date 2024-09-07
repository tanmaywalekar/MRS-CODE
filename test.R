
data <- read.csv("/home/tanmay/Desktop/MRS/WCM/WCM PYTHON/data.csv", header = TRUE)

top_30_data <- data[2:32, ]  # Select rows 1 to 30 (inclusive)

# Extract x, y, and w from the selected data
X <- top_30_data$RVI  # Assuming "x" is the column name for x-values
Y <- top_30_data$Moisture  # Assuming "y" is the column name for y-values
# w <- top_30_data$VH_dB  # Assuming "w" is the column name for w-values

#theta<-top_30_data$Angle
theta<-mean(data$Angle)
C<--17.083965338986474
D<-0.24711106987968953
nlc<-nls.control(maxiter = 50000, tol = 1e-05, minFactor = 1/100000000000,
 printEval = FALSE, warnOnly = FALSE)
 k<-nls(w~wcm_sim(X,Y,theta,A,B,C,D),control=nlc,
  start=list(A=-1,B=1),trace = T)
y<-predict(k)
# Extract fitted coefficients from the model object
fitted_coefficients <- coef(k)

# Print the fitted coefficients
fitted_coefficients <- c(fitted_coefficients, c = C, d = D)
cat("Fitted coefficients:", fitted_coefficients)





wcm_sim<-function(X,Y,theta,A, B, C, D)
{
  theta<- theta*pi/180.
  return(A*X*cos(theta)*(1- exp(- 2*B*X/cos(theta))) + exp( - 2*B*X/cos(theta))*(C + D*Y))
}

lut1 <- lut_wcm(LAI=seq(.33,1.32,0.01), SM=seq(10,45,.1),fitted_coefficients)

radar1 <- raster::raster(ncol=1, nrow=118)
radar1[]<- data$VH_dB
out_sm <- sm_inversion_lut(img = radar1,lookuptable = lut1)





values_vector <-values(out_sm)  # Replace "column_name" with actual column
# 
# 
estMoist=values_vector


 mst <- data$Moisture
# 
# # # Combine extracted values into a data frame
 combined_data <- data.frame(estMoist = estMoist, mst = mst)
 write.csv( combined_data, file = "est vs ground .csv", row.names = FALSE)

 rmse <- sqrt(mean((combined_data$estMoist - combined_data$mst)^2))
 
print(rmse)


