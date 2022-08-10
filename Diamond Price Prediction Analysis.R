---
title: "Diamond Price Prediction Analysis"
output:
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r include=FALSE}
library(mosaic)
library(ggplot2)
diamonds<-diamonds
```

#2 Explore variable


To predict the price of diamonds, several factors that might affect the price of diamonds should be considered. 
For example, the weight and volume should be taken into account. Due to the total depth percentage = z / mean(x, y) = 2 * z / (x + y) ,the volume will be represented by the variable "depth" in the data set(this formula comes from the introduction of the dataset: ?diamonds). It ranges from 43 to 79, and the interquartile is from 61 to 65.

The maximum weight of the diamonds is 5.01 carat, and the minimum is 0.20 carat. The interquartile of the weight is from 0.40 to 1.04. The median is 0.70, and the mean is 0.7979. And the shape of the weight distribution is skew right.

The price interval of the diamonds in this diamonds data set is from \$326 to \$18,823 US dollars. The interquartile of the price is from \$950 to \$5324. The median is \$2401. Mean is \$3933. The histogram shape of the price is a skew right. So, the most expensive diamond is $18,823 and the cheapest diamond is \$326, but the 50% of the diamond's price is between \$950 and \$5324.

The length of the diamonds: x
The mean of the x is 5.731, the median is 5.700, and the max is 10.740. The interquartile is [4.71,6.54]. However, the strange thing is that the minimum of length is 0.

The clarity has many outliers. The relationship between price and clarity(or color) will change, holding the covariates constant.


```{r, echo=FALSE,out.width=c('50%', '50%', '50%', '50%', '50%', '50%', '50%','50%'), fig.show='hold'}
summary(diamonds)
gf_histogram(~price, data=diamonds,color="black",fill="blue")
gf_histogram(~color, data=diamonds,color="black",fill="blue",stat="count") 
gf_histogram(~clarity, data=diamonds,color="black",fill="blue",stat="count") 
gf_histogram(~depth, data=diamonds,color="black",fill="blue")
gf_histogram(~carat, data=diamonds,color="black",fill="blue")
gf_histogram(~x, data=diamonds,color="black",fill="blue")

boxplot(log(price)~clarity, data = diamonds,medcol = "blue", boxfill = "light blue")
boxplot(log(price)~color, data = diamonds,medcol = "blue", boxfill = "light blue")
boxplot(log(price)~cut, data = diamonds,medcol = "blue", boxfill = "light blue")
```

#3 Make a scatterplot of price vs carat.

There is a positive correlation between weight and price, but it is not clear whether there is a linear relationship or curve.
Here are some transformation options: log of price and the square root of price. By comparing both the plot and the $R^2$, the highest is the sqrt transformation. The $R^2$ for the linear, log transformation, and sqrt transformation models are 0.8493, 0.8468, and 0.8959, respectively. They are pretty close. However, when we take a look at the residuals, the log transformation looks most linear by the residuals plot because most of its residuals are between [-2,1], while the sqrt's are in the [-50,50] and original linear model's(price~carat) are in the [-5000,5000]. Therefore, the log transformation looks most linear compared to others.

```{r, echo=FALSE}
xyplot(price~carat,data=diamonds,xlab = "Weight(carat)",ylab="Price",pch=16,cex=0.4)
summary(lm(price~carat,data=diamonds))
```



```{r, echo=FALSE,out.width=c('50%', '50%'), fig.show='hold'}


modlog<-lm(I(log(price))~carat, data = diamonds)
xyplot(log(price)~carat, xlab = "Weight(carat)",pch=16, cex=0.3, data = diamonds)
summary(modlog)


modsqrt<-lm((sqrt(price))~carat, data = diamonds)
xyplot(sqrt(price)~carat, xlab = "Weight(carat)", pch=16, cex=0.3,data = diamonds)
summary(modsqrt)
```

The residuals' plots of different models after transformations:

```{r, echo=FALSE,out.width=c('50%', '50%'), fig.show='hold'}
#plot(resid(lm(price~carat,data=diamonds)),pch=16, cex=0.3)
plot(resid(modlog),pch=16, cex=0.3)
plot(resid(modsqrt),pch=16, cex=0.3)
```



#4 Price vs Weight or Volume


By comparing the correlation coefficients of price vs. weight and price vs. volume(depth), we can find that weight has a more significant influence on the price of diamonds. For example, the correlation coefficient of the sqrt price and the weight is 0.92 while the volume's (measured by "depth" in the dataset) is only -0.00086.
```{r}
cor(log(price)~carat, data = diamonds)
cor(log(price)~depth, data = diamonds)
```
Take a look at more details of these models. First, establish a model of log(price)~depth(Price vs. Volume), and compare it with our log(price)~carat model (Price vs. Weight) in question 3. In the model of Price vs. Volume, the p-value of the depth explanatory term showed 0.324, much larger than 0.05, so that we cannot reject that there is no relationship between the price and the depth. However, when we look at the model of Price vs. Weight, the p-value of the explanatory term(2e-16) is much significant than the volume's(0.842). There is a distinct relationship between carat and the price of diamonds. If the weight of the diamond increase by 1 carat, the price will increase by 1.969757 dollars. F statistics also show that the weight model has much higher power to describe our dataset. The F statistic of the volume model is only 0.0399, while the weight model's is 2.981e+05. There is no doubt that the $R^2$ of these two models tell the same story. The volume model is mostly zero, but the carat one is about 0.84. Therefore, the weight factor as the measurement of big or small diamonds is more important.

```{r}
modlog<-lm((log(price))~carat, data = diamonds)
modlogv<-lm((log(price))~depth, data = diamonds)
summary(modlog)
summary(modlogv)
```
#5 Volume can be approximated by one of x,y, and z? Yes

Due to both the weight and the volume being the physical features of diamonds, we want to find some connection between them. A diamond with a larger volume will have a heavier weight. However, we do not have volume or shape data among diamonds. Fortunately, there is a strong relationship between the volume and the weight if the density of diamonds is held constant. 

So, we can temporarily put the weight in the role of volume as the response variable.

Establish a model for weight in terms of x, y, and z. Then, take a look at the analysis of variance, and we found a very prominent value: Sum of Square of x.

The Sum of Square measures the contribution of a certain term to the model. Sum Sq of x is 11523.2, y is 0.4, and z is 9.4. Clearly, x contributes the most to the model. In other words, x has the most significant effect on weight or volume. y and z are so small that they even can be ignored. Therefore, we can approximate the volume by getting to know the x(length in mm).

We can also check it without using ANOVA. The model formula is y=-1.567+0.359x+0.005y+0.078z. The largest coefficient of x, y, z is the length x, which is 0.359, while the other is only 0.005 or 0.078. So, the volume (response) will depend more on the length of the diamonds, which is consistent with the conclusion by seeing the ANOVA table.

```{r}
modwv<-lm(carat~x+y+z, data = diamonds)
summary(modwv)
anova(modwv)
```
we can also make use of the correlation between the volume and xyz.


#6 Color and Clarity

The boxplot of log(price)~color without any outlier. By contrast, the boxplot of log(price)~clarity has many outliers. By looking at a large number of outliers, we can know that the deviation of the log(price) in terms of clarity is considerable. We can also say this by calculating the variance.

For diamonds, the best color is called D, and the worst is called J. However, it's clear that the mean price of the color J has the highest log(price) in the boxplot.
For clarity, the best is IF, and the worst is I1. But the IF has the lowest mean log(price). IF has many outliers.
The strange thing is that the best diamonds have the lowest price in both clarity and color aspects.

Sometimes the total relationship is the opposite of the partial relationship. The possible reason is that we didn't hold other factors constant, which may affect the price of the diamonds. Once we hold the covariate constant, the conclusion stemming from the data will be closer to the real situation. So, the covariates should be considered to see what the response variables happen when the covariate is held constant.

Firstly, the subset was abstracted from the whole data set and selected only the cases with the color G. Covariate was held constant as G. In the boxplot of the clarity, IF has the highest price, VVS2 has the lowest price. Its price is consistent with the "IF" is the best" condition. Similarly, when investigating the color vs. price, the clarity was held constant as IF and see what happened. The D has the highest price, I and J have the almost lowest price. It meets the market price level here. When the covariates were taken into account, the price relations were different.
Therefore, it's reasonable that contradiction of data and the real condition is caused by did not consider the covariates.

```{r, echo=FALSE,out.width=c('50%', '50%','50%', '50%'), fig.show='hold'}
boxplot(log(price)~color, data = diamonds,medcol = "blue", boxfill = "light blue")
boxplot(log(price)~clarity, data = diamonds,medcol = "blue", boxfill = "light blue")


dataD<-subset(diamonds, color == "D")
boxplot(log(price)~clarity, data = dataD,medcol = "blue", boxfill = "light blue") #IF has highest price, VVS2 has lowest price.

dataIF<-subset(diamonds,clarity=="IF")
boxplot(log(price)~color, data = dataIF,medcol = "blue", boxfill = "light blue") #D has highest price, I and J has almost lowest price.
```

#7 Model for predicting the price

Firstly, include all the variables in the model, then use sum of square in the ANOVA table to "clean" unimportant terms.  In the previous investigation, we found that the weight, volume, color, clarity, and the cut of the diamonds play a role in determining the diamonds' price. By looking at the sum of square of each term in the ANOVA table, we can find which terms are more significant to the model. So, the carat(47022),x(4162),clarity(1733),and color(713) should be remained. The depth percentage and the table will be ignored because its sum of sq is small. 

By looking at the cut vs. price, there is no significant relationship between price and the cut. So I can put it in the last order of the model to make the model more precise. $R^2$ is only 0.01806, consistent with the boxplot(shown in part2).

According to the conclusion we have in the previous sections, the order of significance will be weight, volume, color, clarity, and cut. 

Replace the less significant term with the more significant one. For example, the carat will be the first because it has the highest sum of sq and contributed most to this model. Then, adjust the order of the rest of the terms in the model. For the volume variable, we should have only remained the x variable because including the depth percentage, y, and z will cause redundancy("depth":total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)). For x,y,z, we only need x, according to our previous section conclusion. But in order to predict the price of diamonds, given 0.4 carat diamond with I1 clarity, E color, premium cut, 4.3 mm length, 4.3 mm width, and 2.3mm depth, the model still needs to contain y and z, which are width and depth.

Finally, I got log(price)~carat+x+clarity+color+y+z+cut, named modx. 

And the prediction of the log(price) is in the interval [5.309812,5.916848], and the mean log price is 5.61333. Converted it into the price is that the mean is 274.05, and the 90% interval will be [202.31,371.23]. 

But I am not quite confident about this price. The reason is:
A 0.4-carat diamond ranks in the top 26% of all diamonds, from smallest to largest weight. The top 20 to 30 percent of diamonds from smallest to largest price range is 837 to 1037. If there were a strong correlation between price and weight, the price of a 0.4-carat diamond would fall in the 837 to 1037 (20% to 30% percentile) price range, but our predicted value is only $371.24, which deviates much from the actual data.
```{r}
anova(lm(log(price)~carat+x+y+z+color+clarity+cut+depth+table, data = diamonds))
rsquared(lm(log(price)~cut, data=diamonds))
```


```{r, echo=FALSE,out.width=c('50%'), fig.show='hold'}
anova(lm(log(price)~carat+x+color+clarity+y+z+cut, data = diamonds))

modx<-lm(log(price)~carat+x+color+clarity+y+z+cut, data = diamonds)
summary(modx)
```

```{r}
predict(modx, data.frame(carat=0.4,clarity="I1", color="E", cut="Premium", x=4.3, y=4.3, z=2.3),interval="prediction", level = 0.9)

exp(5.309812)
exp(5.61333)
exp(5.916848)

#something strange. The data are not consistent based on the prediction.
pdata( ~ carat, 0.4, data = diamonds)
qdata(~price, c(0.2,0.3), data = diamonds)
```

#8 How order matters in Anova?

There is no doubt that the weight and the volume contribute the most influence to the diamond price. But the order of the term in the model still gives some more interesting details. For example, in the previous sections, the weight term(carat) was placed in front of the volume term(x), and the sum of sq is 47022 and 4162, which are the highest value compared with all other terms in the model. When placed the x in front of the carat,  where x is the first term of the model, the sum sq of length increased to 50965, and the sum sq of weight decreased dramatically to 219. The reason is that both x and carat play an essential role in impacting the diamonds' price, but the contribution/credit/power of x is shaded by the carat when the weight factor was placed in front of the volume factor. Therefore, the order of the terms of model matters in the Anova and sometimes can help find which factor contributes more or shaded more.

```{r}
anova(lm(log(price)~carat+x+clarity+color, data = diamonds))
anova(lm(log(price)~x+carat+clarity+color, data = diamonds))
```

