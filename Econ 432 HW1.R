#Econ 432 HW1 R Exercises 

# Import the data in the sbuxPrices.csv using read.csv() into
#the data.frame object sbux.df. Explain what you will get from the
#following lines of codes:

sbux.df = read.csv("SBUX-Monthly.csv")
sbux.df$Date = as.Date(sbux.df$Date, format='%Y-%m-%d')
class(sbux.df$Date)

#1.1 
#This command just gives the first few observations for the stock price of starbucks 
#This gives the open price, high, low, closing, adjusted closing, and volume of starbucks stocks traded on this particular day of this month
#Essentially gives the above information for the given month 
head(sbux.df)


#1.2 
tail(sbux.df)
#This command pretty much does the same as the head command, but gives the latter end of observations 
#Alternatively this also tells me that there are 180 total observations in the dataframe 

#1.3
sbuxPrices.df = sbux.df[,"Adj.Close", drop=FALSE]
#This conversion now creates a new variable called sbuxPrices that is solely the Adjusted closing prices for the starbucks stock 

#1.4
rownames(sbuxPrices.df) = sbux.df$Date
#This has attached the dates as the rownames for the respective Adj. Close Prices

#1.5
head(sbuxPrices.df)
#Now we see the first few observations of the new dataframe we created that is solely the dates and corresponding closing prices on those dates

#1.6
sbux.df[101:132,]
#This will give me the 101st to 132nd observation in the starbucks dataframe 

#2 Plot the closing price data using the plot() function with blue line.
#Add a title and a legend to your picture
library(date)
class(sbux.df$Date)
sbux.df$Date = as.Date(sbux.df$Date, format = '%Y-%m-%d')
class(sbux.df$Date)

attach(sbux.df)
plot(Adj.Close~Date, sbux.df, type='l', col = ' blue' , xlab='Date',ylab='Starbucks Adj Closing Prices', main='Daily Prices of Starbucks Shares')
legend("topleft",legend=c("Starbucks"),
       col=c("blue"), lty=1:2, cex=0.8)

#3 Compute monthly simple and continuously compounded returns. Plot
#these returns separately first. Then also plot on the same graph.

#(a) Compute monthly simple returns 

df = read.csv("SBUX-Monthly.csv")
A = length(df$Date)
df$ret[2:A] = df$Adj.Close[2:A]/df$Adj.Close[1:(A-1)]-1
class(df$Date)
df$Date = as.Date(df$Date, format='%Y-%m-%d')


#(b) Plot Monthly Simple returns 
library(ggplot2)
g = ggplot(df, aes(x = Date, y = ret)) + geom_line() + ylab("Starbucks Simple Return")
g

#(c) Compute CC Returns
df$cc.ret[2:A] = log(df$Adj.Close[2:A]/df$Adj.Close[1:(A-1)])
head(df$cc.ret)


#(d) Plot CC Returns
h = ggplot(df, aes(x= Date, y = cc.ret)) + geom_line() + ylab("Starbucks CC Return")
h

#(e) Plot Monthly Simple Returns and CC Returns Together
plot(df$ret[2:A], type ="l", col="blue", lwd=2, ylab="Returns", main="Monthly Returns on SBUX")
abline(h=0, col="green")
legend(x="bottomright", legend = c("Simple", "CC"), lty=1, lwd=2, col=c("blue", "red"))
lines(df$cc.ret[2:A], col="red", lwd=2)


