## Functions

sprintf("%s", "welcome")

check <- function(x)
{
        if(x == 1)
        {
                print("Hello!!")
        }else if(x == 0)
        {
                print("Bye!!")
        }else
        {
                print("Not Sure!!")
        }
}

simplefunc <- function(x,y)
{
        NROW(x) + NROW(y)
}

firstList <- list(A=matrix(1:16,4), B=matrix(1:16,2), C=1:15)

secList <- list(A=matrix(1:16,4), B=matrix(1:16,4), C=15:1)

firstList
secList

mapply(simplefunc, firstList, secList)

require(ggplot2)
data(diamonds)
head(diamonds)
mean(diamonds$price)
aggregate(price ~ cut, diamonds, mean, na.rm = TRUE)

aggregate(price ~cut + color, diamonds, mean, na.rm = TRUE)

aggregate(cbind(price, carat) ~ cut, diamonds, mean)
##############################################################################################
require(plyr)
head(baseball)

baseball$sf[baseball$year < 1954] <- 0
any(is.na(baseball$sf))

baseball$hbp[is.na(baseball$hbp)] <- 0

baseball <- baseball[baseball$ab >= 50, ]

baseball$OBP <- with(baseball, (h + bb + hbp)/ (ab+bb+hbp+sf))
tail(baseball)

obp <- function(data)
{
        c(OBP = with(data, sum(h + bb + hbp)/ sum(ab+bb+hbp+sf)))
}

carrerOBP <- ddply(baseball, .variables="id", obp)

head(carrerOBP)

carrerOBP <- carrerOBP[order(carrerOBP$OBP, decreasing = TRUE), ]

head(carrerOBP)
##########################################################################################

theList <- list(A=matrix(1:9,3), B= 1:5, C= matrix(1:4, 2), D=2)
theList

lapply(theList, sum)
llply(theList, sum)

sapply(theList, sum) # returns vector with header
laply(theList, sum) # returns without element header

head(diamonds)

aggregate(price ~ cut, diamonds, each(mean, median))

numcolwise(sum, na.rm=TRUE)(diamonds)

## OR
sapply(diamonds[,sapply(diamonds, is.numeric)], sum)
