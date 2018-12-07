library(ggplot2)

head(Algorithm1)
Algorithm1[1,2:50]
class(Algorithm1[1,2:51])

plot(Algorithm1$Size,Algorithm1$Try1)

runningTime <- vector()
runningTime <- c(runningTime,as.numeric(Algorithm1[1,2:51]))
class(runningTime)

index <- 1
while(index <= length(Algorithm1$Size)){
  runningTime <- c(runningTime,as.numeric(Algorithm1[index,2:51]))
  index <- index + 1
}

size <- vector()
index <- 1
while(index <= 500){
  number <- index
  index2 <- 1
  while(index2 <= 50){
    size <- c(size,number)
    index2 <- index2 + 1
  }
  index <- index + 1
}

Algorithm1 <- data.frame(size = size, runningTime = runningTime)

x <- runningTime
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- NA
x[x > (qnt[2] + H)] <- NA

mean(x)
mean(runningTime)

testDataFrame <- data.frame(size = size, runningTime = x)

Algorithm2 <- aggregate(Algorithm2[,2],list(Algorithm2$size), median)

plot(testDataFrame$Group.1,testDataFrame$x, main = "Algorithm 1", ylab = "Running time (in nanoseconds)", xlab = "size of the input")

colnames(algorithmsData) <- c("size","a1time","a2time")

colnames(Algorithm2) <- c("size","time")

plot <- ggplot(df2, aes(x=size,y=value,colour = variable)) + geom_point(colour = "grey") + geom_smooth() +
  labs(x="Input Size", y = "Running Time (microseconds)",
       title = "PrefixAverages1 vs PrefixAverages2",
       subtitle = "O(n^2) vs O(n)         i3-2310M CPU, 2.1 GHz",
       caption = "Made by Maxim Musikhin with R") + 
  theme(legend.title=element_blank()) + 
  scale_color_discrete(labels=c("Algorithm 1", "Algorithm 2"))

library(reshape2)
# original data in a 'wide' format
x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)
df <- data.frame(x, y1, y2)

# melt the data to a long format
df <- melt(data = sortingDfGrouped2, id.vars = "size")

# plot, using the aesthetics argument 'colour'
ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line()
groupA <- aggregate(sortingDf[,2],list(sortingDf$size), median)
groupBm <- aggregate(sortingDf[,3],list(sortingDf$size), meadian)
groupC <- aggregate(sortingDf[,4],list(sortingDf$size), median)
sortingDfGrouped2 <- data.frame(size = groupA$Group.1, sortB = groupB$x, sortC = groupC$x)

plot <- ggplot(df2, aes(x=size,y=value,colour = variable)) + geom_point(colour = "grey") + geom_smooth() +
  labs(x="Input Size", y = "Running Time (milliseconds)",
       title = "Running time of sort Algorithms",
       caption = "Made by Maxim Musikhin with R") + 
  theme(legend.title=element_blank()) + 
  scale_color_discrete(labels=c("SortA - Bubble","SortB - Radix", "SortC - Quick")) + 
  theme(legend.justification=c(0,1), legend.position=c(0.1,0.9))


df2$value <- df2$value/1000000
