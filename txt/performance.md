#### How well does the model perform?

Probably you know that R² is often used to assess the performance of a linear model. Unfortunately, it is a bit harder to assess the performance of a logistic regression. There are pseudo R² for logistic regression to compare nested models, but we cannot interpret them as explained variance as we do it with linear model. Instead, you may encounter two terms: sensitivity and specificity.

Sensitivity takes into account whether we classified the true outcome right. How many people who survived (true positives) did our model classify as survivor? The mosaic plot shows you how many passengers did (not) survive; on the x-axis as we have observed and y-axis displays our prediction. We can calculate the sensitivity by dividing the true positives by all the people who survived (207/290). Thus, the sensitivity is 0.71.

How many people did we classify as not-survived, who actually did not survive the Titanic (true negative)? Hence, the specificity does the same job for the negative outcome. If you divide the true negatives by all people who did not survive (356/424), you get the specificity.


A common way to combine both indicators are ROC curves. As the plot below illustrates, a ROC curve displays the sensitivity on the y-axis, and the false positive rate (1-specificity) on the x-axis. What does the ROC curve tells you? By predicting a binary outcome, we want to achieve two things simultaneously: We want to classify the number of people who survived correctly, while we wish that the number of false positives is rather small. Thus, we wish to have a sensitivity of 1 and a false positive rate of zero (highlighted in black in the ROC curve below). However, if the model does not help to predict the outcome, the ROC curve would be a diagonal line, since a fair toss of a coin would have the same predictive power. 50% of the time we identify people correctly, 50% of the time we make a wrong prediction.


Sensitivity and specificity are not the only measures of performance, but in terms of interpretation, we should remember the further the ROC curve is away from the diagonal (and closer to the black line), the greater the explanatory power of the model.
