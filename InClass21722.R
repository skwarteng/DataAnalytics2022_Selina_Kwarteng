#creating a matrix data with random numbers
#and plotting the matrix using the image() function
#you eill see there, it does not have a real pattern in the plot

set.seed(12345)
help(par)

#par can be used to set or query graphical parameters
#parameters can be set by specifying them as arguments
# to par in tag = value from, or by passing them as a list of tagged values

par(mar = rep(0.2,4))
data_Matrix = matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

par(mar = rep(0.2,4))
heatmap(data_Matrix)

#when we run the heatmap() here, we get the dendograms printed on both columns
#and the rows and still there is no real immerging pattern that is interesting to us, it
#is because there s no real interesting pattern underlying in the data we generated

set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}

# Now we will plot the data
# Now we can see that the right hand five columns have more yellow in them,
# which means they have a higher value and the left hand five columns that
# are little bit more in red color which means they have a lower value.
# it is because some of the rows have a mean of three in the right hand side, and
# some of the rows have mean of zero. Now we have introduced some pattern to it

par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

# now we will run the heatmap() function on the data, we can see that, two
#sets of columns are easily separated

par(mar=rep(0.2,4))
heatmap(data_Matrix)

# Let's take a closer look at the patters in rows and columns by
# looking at the marginal
# means of the rows and columns.
# ten different columns mean and forty different rows means

hh = hclust(dist(data_Matrix))
data_Matrix_Ordered = data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered)])
