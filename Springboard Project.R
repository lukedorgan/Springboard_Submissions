## Set correct working directory

setwd("C:/Users/LukeDorgan/OneDrive - Procurement Leaders LTD/Springboard Projects")

## Read in data set

memberships = read.csv("Springboard Memberships.csv", header = TRUE)

## Read in packages

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("caTools")
library(caTools)
install.packages("ROCR")
library(ROCR)

## Look at how many FYM we have in the sample

table(memberships$First.Year.Membership)

## Create additional datasets for First Year Members and Non First Year Members

fym = memberships[(memberships$First.Year.Membership == "1"),]
nonfym = memberships[!(memberships$First.Year.Membership == "1"), ]

## Create additional datasets for Drops and Renewed

drops = memberships[(memberships$Renewed == "0"),]
renewed = memberships[(memberships$Renewed == "1"),]

## Explore the dataset

str(memberships)
summary(memberships)

str(fym)
summary(fym)

str(renewed)
summary(renewed)

str(drops)
summary(drops)

## Plot Memberships dataset against various pieces of the data

## First, see if there's a relationship between campaign and renewed

ggplot(memberships, aes(x = Campaign.Response, y = Renewed)) +
  geom_point(position = "jitter") +
  labs(title = "Campaign Response by First Year Membership", x = "Number of Responses", y = "Renewed Yes or No?")

## Looks to be a relationship with accounts renewed having greater number of responses
## Test with histograms again for just accounts that renewed and just accounts that dropped

ggplot(renewed, aes(x = Campaign.Response)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  labs(title = "Number Of Campaign Responses For Accounts That Renewed", x = "Number of Responses", y = "Density") +
  ylim(0, 0.01)

ggplot(drops, aes(x = Campaign.Response)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  labs(title = "Number Of Campaign Responses For Accounts That Dropped", x = "Number of Responses", y = "Density") +
  ylim(0, 0.01)

# Density is greater around 0 for those that dropped, with the renewals having
## a much wider spread, suggesting that accounts that renewed are more engaged.

## Next, we will plot with first year member accounts vs Renewed

ggplot(memberships, aes(x = First.Year.Membership, y = Renewed)) +
  geom_point(position = "jitter") +
  labs(title = "First Year Membership By Renewal Type", x = "First Year Member Yes or No?", y = "Renewed Yes or No?")

## Suggests a relationship between first year membership and renewals
## Going to create a table to see if the point is illustrated

table(memberships$First.Year.Membership, memberships$Renewed)

## Appears that the renewal rate for FYMs is much higher than we would expect - typically operate at about 65% for 
## FYM, however here we are operating at more like 90% - why??

## Follow similar exercise with event attendances

ggplot(memberships, aes(x = Event.Attendances, y = Renewed)) + 
  geom_point(position = "jitter") +
  labs(title = "Number of Event Attendances", x = "Number of Event Attendances", y = "Renewed Yes or No?")

## As expected, suggests that more event attendances means greater renewals
## Let's check by running on drops and renewed

ggplot(drops, aes(x = Event.Attendances)) +
  geom_histogram(aes(y = ..count..), binwidth = 10, position = "dodge") +
  labs(title = "Number of Event Attendanes for Dropped Accounts", x = "Number of Event Attendances", y = "Density")

ggplot(renewed, aes(x = Event.Attendances)) +
  geom_histogram(aes(y = ..count..), binwidth = 10, position = "dodge") +
  labs(title = "Number of Event Attendances for Dropped Accounts", x = "Number of Event Attendances", y = "Count")

## Relatively clear to see that more event attendances mean greater chance of renewal
## greater spread in the data and lower percentage down at zero

## Now we will plot Award Wins against Renewed accounts

ggplot(memberships, aes(x = Awards.Wins, y = Renewed)) +
  geom_point(position = "jitter") +
  labs(title = "Number of Awards Wins", x = "Number of Awards Wins", y = "Renewed Yes or No?")

## Check that the relationship holds by running it on the accounts the dropped and the accounts that renewed

ggplot(renewed, aes(x = Awards.Wins)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, position = "dodge") +
  labs(title = "Number of Awards Wins for Renewed Accounts", x = "Number of Awards Wins", y = "Count")

ggplot(drops, aes(x = Awards.Wins)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, position = "dodge") +
  labs(title = "Number of Awards Wins for Dropped Accounts", x = "Number of Awards Wins", y = "Count")

## Broadly no difference in the awards wins, appears to be very little relationship between renewing and the number
## of awards wins, distribution doesn't appear to change

## Let's look at the relationship between C&A Interactions and Renewing

ggplot(memberships, aes(x = C.A.Interactions, y = Renewed)) +
  geom_point(position = "jitter") +
  labs(title = "Number of C&A Interactions", x = "Number of C&A Interactions", y = "Renewed Yes or No?")

## Looks like there's a relationship betwen the number of C&A interactions and whether or not an account renewed
## Check the distributions

ggplot(renewed, aes(x = C.A.Interactions)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, position = "dodge") +
  labs(title = "Number of C&A Interactions for Renewed Accounts", x = "Number of C&A Interactions", y = "Count")

ggplot(drops, aes(x = C.A.Interactions)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, position = "dodge") +
  labs(title = "Number of C&A Interactions for Dropped Accounts", x = "Number of C&A Interactions", y = "Count")

## Distribution is broadly similar, dropped accounts have a greater concentration at the zero point, more of them
## have had no C&A interactions.

## I expect a similar relationship as displayed for C&A Interactions for Networking CAlls as well, however I am 
## going to plot these anyway to check.

ggplot(memberships, aes(x = Networking.Call.Attendances, y = Renewed)) +
  geom_point(position = "jitter") +
  labs(title = "Number of Networking Call Attendances", x = "Number of Networking Call Attendances", y = "Renewed Yes or No?")

## Looks to be a stronger relationship than expected between networking call attendance and whether the account
## renews or not, let's check

ggplot(renewed, aes(x = Networking.Call.Attendances)) +
  geom_histogram(aes(y = ..count..), binwidth = 2, position = "dodge") +
  labs(title = "Number of Networking Call Attendances for Renewed Accounts", x = "Number of Networking Call Attendances", y = "Count")

ggplot(drops, aes(x = Networking.Call.Attendances)) +
  geom_histogram(aes(y = ..count..), binwidth = 1, position = "dodge") +
  labs(title = "Number of Networking Call Attendances for Dropped Accounts", x = "Number of Networking Call Attendances", y = "Count")

## Distribution is less prevelant than it appeared from the previous graph, but still worth adding into the model

## Plot ACV and Renewals to see if there's a relationship

ggplot(memberships, aes(x = ACV, y = Renewed)) +
  geom_point(position = "jitter") +
  labs(title = "ACV", x = "ACV", y = "Renewed Yes or No?")

## Doesn't appear to be a huge relationship between ACV and Renewals.
## We know internally at PL that there is a relationship between proximity to list price and renewal
## 80% of list price is out sweet spot for renewal
## Below 80% and clients drop as we try to get the closer to list price
## Higher than 90% and clients drop as they feel they are paying too much
## Suggests that our list price is too high

## Let's begin building and testing models

## First piece to do is create a test data set and a training set
## I want to have 75% of the data in the training set and 25% in the test set
## This will mean I will have an good split of the data to build and test my models

set.seed(363)
split = sample.split(memberships$Renewed, SplitRatio = 0.75)
membershipstrain = subset(memberships, split == TRUE)
membershipstest = subset(memberships, split == FALSE)

## First model I'll build contains all the variables I expect to have an impact on renewal.

model = glm(Renewed ~ First.Year.Membership + Campaign.Response + Awards.Wins + Event.Attendances + C.A.Interactions + Networking.Call.Attendances, data = membershipstrain)
summary(model)

## AIC is quite big and there's several variables with a low significance, and therefore we will remove
## C&A Interactions from the model as it is the least significant.

model2 = glm(Renewed ~ First.Year.Membership + Campaign.Response + Awards.Wins + Event.Attendances + Networking.Call.Attendances, data = membershipstrain)
summary(model2)

## AIC didn't change much with that change and we still have low significance on several of the variables
## Remove Awards Wins from the model as it has the highest probability

model3 = glm(Renewed ~ First.Year.Membership + Campaign.Response + Event.Attendances + Networking.Call.Attendances, data = membershipstrain)
summary(model3)

## AIC keeps falling which is good
## Next we will remove networking call attendances from the model, this is the next least significant

model4 = glm(Renewed ~ First.Year.Membership + Campaign.Response + Event.Attendances, data = membershipstrain)
summary(model4)

## AIC keeps falling
## Will remove Campaign Responses to test further as it is the least significant variable left

model5 = glm(Renewed ~ First.Year.Membership + Event.Attendances, data = membershipstrain)
summary(model5)

## AIC fell again
## All variables are highly significant to the model which is good news.
## Let's test just Event Attendances

model6 = glm(Renewed ~ Event.Attendances, data = membershipstrain)
summary(model6)

## Test with Event Attendance and Campaign Response, just to check

model7 = glm(Renewed ~ Event.Attendances + Campaign.Response, data = membershipstrain)
summary(model7)

## The best model to proceed with is model 5.

## Create a table illustrating the outcomes of the model creation

model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7")
AIC = c(1271.5, 1269.5, 1267.5, 1266, 1265, 1276.7, 1276.9)
Variables = c(6,5,4,3,2,1,2)
ModelBuilding = data.frame(model, AIC, Variables)

## Now we need to test the model on the test data set.

predicttest = predict(model5, newdata = membershipstest, type = "response", family = "binomial")
predicttest
summary(predicttest)

## Create ROC curve to help identify where my threshold should be

ROCPred = prediction(predicttest, membershipstest$Renewed)
ROCPerf = performance(ROCPred, "tpr", "fpr")
plot(ROCPerf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

## Threshold should be between 0.69 and 0.87 looking at the graph, likely closer to 0.7
## I will probably need to test a few to finalise the best inflexion point

## Build a classification matrix

## Aim would be to minimise the false positive rate as we don't mind if we predict the account to drop but it renews
## However predicting an account will renew but it drops is much more detrimental

## First test we will run is on 0.7 as my threshold value

table(membershipstest$Renewed, predicttest > 0.7)

## Accuracy = 59%, Error = 41%, False Negative Rate = 40%, False Positive Rate = 43%

table(membershipstest$Renewed, predicttest > 0.75)

## Accuracy = 41%, Error = 59%, False Negative Rate = 75%, False Positive Rate = 12.5%

table(membershipstest$Renewed, predicttest > 0.8)

## Accuracy = 38%, Error = 62%, False Negative Rate = 83%, False Positive Rate = 6.25%

## Seems to be a point between 0.7 and 0.75 where the false positive rate drops, where is that?

table(membershipstest$Renewed, predicttest > 0.725)

## Accuracy = 45%, Error = 55%, False Negative Rate = 68%, False Positive Rate = 20%

## Test 0.72 just to see

table(membershipstest$Renewed, predicttest > 0.72)

## Accuracy = 48%, Error = 52%, False Negative Rate = 62%, False Positive Rate = 23%

## See if 0.71 has a better Accuracy without raising the False Positive Rate too much

table(membershipstest$Renewed, predicttest > 0.71)

## Accuracy = 51%, Error = 49%, False Negative Rate = 57%, False Positive Rate = 26%

## 0.71 looks like the best option for the threshold level.

threshold = c(0.7, 0.75, 0.8, 0.725, 0.72, 0.71)
accuracy = c("59%", "41%", "38%", "45%", "48%", "51%")
error = c("41%", "59%", "62%", "55%", "52%", "49%")
falsenegativerate = c("40%", "75%", "83%", "68%", "62%", "57%")
falsepositiverate = c("43%", "12.5%", "6.25%", "20%", "23%", "26%")
ClassificationTesting = data.frame(threshold, accuracy, error, falsenegativerate, falsepositiverate)
