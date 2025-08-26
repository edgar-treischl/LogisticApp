#### Logistic Regression — But Why?

There are several reasons why logistic regression was invented to model binary outcomes. The most obvious one is right there in the figure below. Imagine trying to fit a simple regression line to predict a binary outcome. Now, picture how a scatter plot would look in that situation.

In linear regression, we fit a line that minimizes error assuming the error variance is constant (homoscedastic). But when the outcome is binary, the error variance depends on the value of X — and here’s the catch: the outcome can only be 0 or 1. There are no values in between, even though a regression line models values continuously between 0 and 1. This mismatch causes problems.

Next up, you’ll see how logistic and probit functions distribute probabilities differently.

Both the logistic and probit models are popular choices for binary outcomes in social sciences. Instead of trying to fit a straight line, logistic regression uses a sigmoid (S-shaped) curve — the logit function — to better capture the relationship between X and Y.

Note: The scatter plot data is simulated, which is why it looks nice and smooth. But I hope it gives you a clear first impression of the key differences between linear and logistic regression.
