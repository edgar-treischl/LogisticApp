#### Logistic regression, but why?

There are several reason why logistic regressions were invented to model binary outcomes. You can see the most obvious reason in the figure below. Imagine that we insert a regression line to model a binary outcome. Look how a scatter plot would look like in such a situation.

In a linear regression, we try to fit a line that minimizes the error, but in the case of a binary outcome, the observed error is not homoscedastic. Moreover, the variance of the error term depends on the particular value of X, but we observe only 0 or 1. There are no no observations between zero and one, even though we use a regression line to model between the two outcome values. The next output shows you how the distribution of a logistic and the probit function looks like.

Both distributions are often used to model binary outcomes in the social sciences. Of course, we can adjust the first scatter plot and use a logit function to describe the relationship between X and Y instead of a regression line.

The data for the scatter plot was simulated, that is reason why it looks nice and smooth, but I hope you get a first impression about the difference between linear and logistic regression.