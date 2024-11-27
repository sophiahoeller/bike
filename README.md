# GLM Bike Sharing
This dataset contains the daily count of rental bikes between years 2011 and 2012 in Capital bikeshare system, which operates on some of the major cities of the State of Washington D.C, Virginia and Maryland. The dataset is given by the Laboratory of Artificial Intelligence and Decision support of the University of Porto.

The main attributes :\

-   $\textbf{instant}$ record index
-   $\textbf{dteday}$ : date
-   $\textbf{season}$ : 1 = spring, 2 = summer, 3 = fall, 4 = winter
-   $\textbf{yr}$ : Year (0: 2011, 1:2012)
-   $\textbf{mnth}$ : Month ( 1 to 12)
-   $\textbf{holiday}$ : whether the day is considered a holiday or not
-   $\textbf{workingday}$: if day is neither weekend nor holiday is 1, otherwise is 0
-   $\textbf{weekday}$: day of the week
-   $\textbf{weathersit}$: Weather ( 1:Clear,2: Cloudy,3:Rain)
-   $\textbf{temp}$: temperature in Celsius (normalized)
-   $\textbf{atemp}$:"feels like" temperature in Celsius(normalized)
-   $\textbf{hum}$: relative humidity
-   $\textbf{windspeed}$: normalized wind speed.
-   $\textbf{casual}$: count of casual users
-   $\textbf{registered}$: count of registered users
-   $\textbf{cnt}$: count of total rental bikes including both casual and registered

```{r setup, include=FALSE}
library(tidyverse)
library(corrplot)
library('rsample')
library(patchwork)
library(wesanderson)
library(DHARMa)
library(AER)
library('msme')
library(MASS)
library(faraway)
library(MLmetrics)

```

```{r message=FALSE}
day_data=read_csv('/Users/gabrielecola/GLM_BikeSharing/Data/bike_rental.csv')
```

```{r}
head(day_data)
```

```{r}
summary(day_data)
```

#### 2. PRE-PROCESSING

###### 2.1 CHECKING THE MISSING VALUE

```{r}
sum(is.na(day_data))
```

###### 2.2 ENCODING VARIABLE

We encode some variables as factors because initially they are encoded as dbl for practical reason so we reestablish the correct type of them.

```{r warning=FALSE}
cols <- c("season", "yr", "weathersit", "workingday",'holiday','mnth','weekday')


new_data<-day_data %>%
       mutate_each_(funs(factor(.)),cols)

glimpse(new_data)
```

#### 3. EDA

$HeatMap$\
We use a heatmap to see clearly the correlation matrix:\
1. $temp$ and $atemp$ are high correllated , close to 1 so we can remove it one of them.\
2. $Registered$/ $Casual$ and $Count$ are highly correlated which indicates that most of the bikes that are rented are registered , one of them we can eliminate.

```{r}
new_data2<- new_data %>% dplyr:: select(-season,-yr,-weathersit,-workingday,-holiday,-mnth,-weekday,-dteday)
cor_matrix<-cor(new_data2)
corrplot(cor_matrix, method="number",tl.cex=0.5,number.digits = 1)
```

$Scatterplot$\
1. $Temperature$ are positively correlated with $Count$, if $Temperature$ rise also $Count$.\
2. $WindSpeed$ are negatively correlated with $Count$, if $Wind$ rise $Count$ will diminish.\
3. $Temperature \ feeling$ follows the same pattern of $Temperature$ , because they are highly correlated.

```{r warning=FALSE}
temp_scatter<-ggplot(new_data, aes(x=temp, y=cnt)) +
  geom_jitter(aes(colour = temp,width = 0.15))+
  scale_color_gradient(low="blue", high="red")+
  xlab('temperature')+
  ylab('count')

humidity_scatter<-ggplot(new_data, aes(x=hum, y=cnt)) +
  geom_jitter(aes(colour = hum))+
  xlab('humidity')+
  ylab('count')

windspeed_scatter<-ggplot(new_data, aes(x=windspeed, y=cnt)) +
  geom_jitter(aes(colour=windspeed))+
  scale_color_gradient(low="grey", high="black")+
  xlab('windspeed')+
  ylab('count')
  

atemp_scatter<-ggplot(new_data, aes(x=atemp, y=cnt)) +
  geom_jitter(aes(colour=atemp))+
  scale_color_gradient(low="yellow", high="orange")+
  xlab('temperature feeled')+
  ylab('count')
  
  
temp_scatter+humidity_scatter+windspeed_scatter+atemp_scatter
```

Here we create a Dataframe that reports the total count for each $weathersit$.

```{r}
data_weather<-new_data  %>% group_by(weathersit) %>% distinct(weathersit,cnt) %>% summarize(count=sum(cnt)) %>% arrange(desc(count))

glimpse(data_weather)
```

In this two graph we want to analyze the variable $weathersit$, firstly with geom_bar so we look to the frequency of each $weathersit$,and then with geom_col to see the overall count for each $weathersit$.

```{r}
j<-ggplot() + geom_col(data =data_weather, aes(x = weathersit, y = count,fill=weathersit))+
  scale_fill_discrete(name = "weathersit")+
  xlab('Weathersit')+
  labs(title = "The total count for each Weather ", caption = "1: Sunny,2: Cloudy,3:Rainy")
  
m<-ggplot(data = new_data) +
  geom_bar(mapping = aes(x = weathersit,fill=weathersit))+
  scale_fill_discrete(name = "weathersit")+
  xlab('Weathersit')+
  labs(title = "Bar chart ", caption = "1: Sunny,2: Cloudy,3:Rainy")

m+j
```

Now our focus is on variable $Season$,so we decide to choose a boxplot for each $Season$. We have observed that the highest median is registered in Summer and followed by Spring. Furthermore, we spot that there is an outlier in $Winter$ that may be caused by a very hot day where people used bikes, and also another one in Fall, where there is a day with 0 count.

```{r}
ggplot(aes(x=season,y = cnt),data=new_data)+geom_boxplot(aes(fill=season)) + theme_bw()+
  xlab('Season')+
  ylab('Count')+
  scale_fill_discrete(name = "season")+
  labs(title = "Boxplot of Season ", caption = "1: Winter,2: Spring,3:Summer,4:Fall")
```

We want to see the Trends across the $Year$ 2011 and 2012, in order to spot the peaks and downs of each $month$ per $Year$. In 2011 we notice that with the beginning of spring and summer the $count$ of the bike has increased while with the incoming of autumn/winter the trend has decreased, same thing in 2012. Furthermore, we notice the in 2012 the trend has increased so much rather than 2011.

```{r}
ggplot(data = new_data, aes(mnth, cnt)) +
  geom_line(color = "steelblue") +
  geom_point(color="steelblue") +
  xlab('Month')+
  ylab('Count')+
  facet_wrap(~yr)+
    labs(title = "Trend ", caption = "0: 2011,1: 2012")
```

In this graph we analyze in depth the variable $Holiday$, firstly with a bar chart in order to understand if the bike are rented more in holiday or not and we notice that people do not prefer using it on holiday days.

```{r}
ggplot(data = new_data) +
  geom_bar(mapping = aes(x = holiday,fill=holiday))+
  scale_fill_discrete(name = "holiday")+
  xlab('holiday')
```

Furthermore, with the second graph we spot that during the weekend there are the most peaks of renting bikes, while when there is holiday we have few data so we cannot draw a conclusion.

```{r}
ggplot(data = new_data, aes(weekday, cnt)) +
  geom_line(color = "steelblue") +
  geom_point(color="steelblue") +
  xlab('Day')+
  ylab('Count')+
  facet_wrap(~holiday)+
    labs(title = "Trend ", caption = "0: Sunday,1: Monday,2: Tuesday,3:Wednesday,4:Thursday,5:Friday,6:Saturday")
```

To see how our $Y$ is distributed in order to choose which models we have to applied, we notice that the variable $cnt$ is distributed like a Normal, and we know that even if our Y is count but if it is large enough it can be approximately \~ N.

```{r}
ggplot(new_data, aes(x=cnt))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)+
  geom_vline(aes(xintercept=mean(cnt)),
            color="blue", linetype="dashed", size=1)
```

### 4. Feature Engineering

1.  As wee see in Correlation matrix, we can delete $casual$ or $registered$.
2.  We remove $atemp$ because is highly correllated with $temp$
3.  We remove $dteday$ because is not useful
4.  We remove $workingday$ because gives the same information of $weekdays$

```{r}
new_data5<- new_data %>% dplyr:: select(-casual,-atemp,-dteday,-workingday)
glimpse(new_data5)
```

### 5. Model

```{r}
set.seed(123)
split.bike<-initial_split(new_data5,0.7)
train.bike<-training(split.bike)
test.bike<-testing(split.bike)
```

#### 5.1 POISSON MODEL

We adopt for a Poisson model because $Y$ is count, but when we look at the summary we spot that there is big difference between residual deviance and degrees of freedom so probably it is caused by overdispersion. However, count data are often overdispersed.This violates the assumption of a Poisson GLM, so our model has understimated the standard errors, which has resulted in too low p-values.

```{r}
pois_mod<- glm(cnt~.,data=train.bike,family='poisson')
summary(pois_mod)
```

```{r}
exp(coef(pois_mod))
```

#### 5.1.1 DIAGNOSTIC OF THE MODEL

Before we try to spot if there is **overdispersion** or not , we firstly check the goodness of fit and we have to reject the $H_{0}$ , so our model doesn't fit data well.

```{r}
#probability to observed extreme value of deviance that we observed
1-pchisq(pois_mod$deviance,pois_mod$df.residual)
```

There are two ways to check overdispersion:\
1.The Pearson $\chi^2$ dispersion statistic\
2. Plot the approximations of Mean vs Variance\

$The \ Pearson$ $\chi^2$ $dispersion\ statistic$

Firstly, we do an overdispersion test to check if there is any presence of overdispersion and we notice that $\phi$, the dispersion parameter, is far away from 1, so there is a great problem of overdispersion.

```{r}
P__disp(pois_mod)
```

$Plot\ Mean\ vs \ Variance$\
We want to check if the assumption of $\mu$ = $\sigma^2$, so if they are equal they should stand in that following line. We notice that there is a systematic variation that line then it means that there is a hidden relationship between variance and expectation.

```{r}
#Check if mean is equal to variance
#approximate the expected value with your predicted
E_hat=predict(pois_mod,type="response")
# response directly give the lambda
# what you observed and the expected value
V_hat=(train.bike$cnt-E_hat)^2
# the log shift ups because the product of log is the sum of log
plot(log(E_hat),log(V_hat))
abline(0,1)
```

$Outlier$\
Furthermore, we try to detect some $outlier$ to see if it can also cause some troubles to the model and we spot the observation 193 of training set but it don't seem to be a real outlier because it is a low count but in winter so we would expect it, so it not probably the cause.

```{r}
# we make an half normal plot of studentized residuals 
halfnorm(rstudent(pois_mod))
```

```{r}
# we want  see the value of count of that observation
train.bike$cnt[193]
```

```{r}
#inspect the see the feature of that particular observation
train.bike %>% filter(cnt==22)
```

#### 5.1.2 Predictions

```{r}
predictions_pois<- predict(pois_mod,newdata=test.bike,type='response')
MSE_pois<- MSE(predictions_pois,test.bike$cnt)
RMSE_pois<- sqrt(MSE_pois)
RMSE_pois
```

There are two main approaches that we can take to deal with over-dispersed count data in GLMs:\

1.  Fit a quasi-Poisson GLM\
2.  Fit a negative binomial GLM

#### 5.2 Quasi Poisson

We have to deal with $overdispersion$,so we adopted the Quasi Poisson model that will result in the same coefficients as The Poisson model but with different Standard errors and p-values thanks to his dispersion parameter $\phi$, that tells how much the variance linearly changes in relation to the mean. Furthermore, we can’t obtain an AIC value for quasi-Poisson models, because these models use quasi-likelihood rather than true likelihood.

```{r}
quasipois_mod<- glm(cnt~.,data=train.bike,family=quasipoisson)
summary(quasipois_mod)
```

As we notice, there is still a big difference between residual Deviance and degrees of freedom. From the dispersion test we spot that there is a severe dispersion, so probably we have to adopt a Negative Binomial model. Furthermore, we have to guess that there is non linear relationship between the variance and the mean, therefore the quasi Poisson model would not be appropriate because it assumes that the variance increases linearly as a function of the mean.

#### 5.2.1 Predictions

```{r}
predictions_quasipois<- predict(quasipois_mod,newdata=test.bike,type='response')
MSE_quasipois<- MSE(predictions_quasipois,test.bike$cnt)
RMSE_quasipois<- sqrt(MSE_quasipois)
RMSE_quasipois
```

#### 5.3 Negative Binomial

The Negative Binomial is Poisson regression model with an extra parameter $\theta$, which may account for overdispersion.\
$Var=\mu+\frac{\mu^2}{\theta}$\
In comparison to The Quasi-Poisson regression model, the negative binomial models assumes that the variance can increase by the square of the mean.

```{r}
negative_bin <- glm.nb(cnt ~., data=train.bike)
summary(negative_bin)
```

We notice that the distance between Residual Deviance and degrees of freedom has been reduce so much.

#### 5.3.1 MODEL SELECTION

We want to choose the model with lower AIC and possibly less feature in order to have a simpler method so we compute the function step. After computed it, we notice that we can remove variable $holiday$.\
Furthermore, we want to spot if there any differences between the model with holiday and without it so we compute anova and we notice that the models have no differences.

```{r}
final_model<-step(negative_bin)
```

```{r}
final_model
```

```{r}
anova(final_model,negative_bin)
```

$The\ Exploratory \ Power$

When we ran linear models, we used the coefficient of determination, or R2 to assess how much of the variability in our response variable is explained by a given model. R2 is based on the sums of squares of our model, and so cannot be calculated for GLMs. Instead, we can calculate the analogous “deviance explained” by our model:

```{r}
nb_dev.null <- final_model$null.deviance
nb_dev.resid <- final_model$deviance
nb_dev.explained <- (nb_dev.null-nb_dev.resid)/nb_dev.null
# Same formula 1 - residual deviance/null deviance
nb_dev.explained
```

#### 5.3.2 Predictions

```{r}
predictions_nb<- predict(final_model,newdata=test.bike,type='response')
MSE_nb<- MSE(predictions_nb,test.bike$cnt)
RMSE_nb<- sqrt(MSE_nb)
RMSE_nb
```

```{r}
rmse<-round(c(RMSE_pois,RMSE_quasipois,RMSE_nb),2)
model<-c('Poisson Regression','Quasi-Poisson Regression','Negative Binomial Regression')
rmse_data<- cbind(rmse,model)
rmse_data2<- as.data.frame(rmse_data)
rmse_data2<- rmse_data2 %>% mutate(rmse=as.numeric(rmse))
```

RMSE has the same unit as the dependent variable. It means that there is no absolute good or bad threshold, however you can define it based on your DV. Our response variable has range from 22-8714, so this $RMSE_{s}$ indicate a good predictive ability of our model. Furthermore, we can notice despite the $overdispersion$ that is present in $Poisson \ Regression$ has a more predictive power ability rather than $Negative\ Binomial$, probably because $overdispersion$ affects only p-values and not the values of coefficient. Finally, as we can expected the RMSE of $Poisson \ Regression$ and $Quasi\ Poisson \ Regression$ are the same because as we explained before , $Quasi\ Poisson$ only fixed the standard errors but the values of coefficient are the same.

```{r}
ggplot() + geom_col(data =rmse_data2, aes(x=reorder(model,-rmse),y=rmse,fill=model)) +
  xlab('Model') +
   theme(axis.text.x=element_text(size=6.5))+
  labs(title = "RMSE for each Model")
```
