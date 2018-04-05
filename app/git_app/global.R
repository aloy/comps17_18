## generates two datasets for illustration
I <- 3 # nb tests 
J <- 4 # nb timepoints
dat1 <- data.frame(
  Test=gl(I,J,labels=LETTERS[1:I]),
  timepoint=rep(1:J,I)
)
dat1 <- transform(dat1, y=round(rnorm(I*J,2*timepoint),1))
I <- 5 # nb tests 
J <- 3 # nb timepoints
dat2 <- data.frame(
  Test=gl(I,J,labels=LETTERS[1:I]),
  timepoint=rep(1:J,I)
)
dat2 <- transform(dat2, y=round(rnorm(I*J,2*timepoint),1))
