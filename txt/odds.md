#### Odds Ratio?

What would be the chance to survive for men if they had the same odds (chance) to survive compare to women? The odds would be one, since we expect that the same amount of men and women would survived. You can calculate the odds ratio with the help of the logistic regression. However, let's try to calculate it by hand to get a better intuition what a OR means. In order to do so, the next plot how many men and women have survived.

Look at the bar graph and the numbers of each group. We get the men's odds to survive if we divide the number of survived men (109) by the number of men who did not survive (468). Woman's odds to survive are calculated the very same way (233/81). In the last step, divide men's odds by woman's odds and you get the odds ratio for men to survive.

We don't have to work this out in our own head, just use your statistics software as a calculator, as the next console shows:



#### Remember the interpretation:

- OR > 1: Positive effect
- OR = 1: No effect
- 0 < OR < 1: Negative effect

Thus, men's chance to survive is reduced by the factor 0.08 compared to women. What about age and the other variables in your model? Go back to the Model tab if you did not choose any independent variable for the analysis.


A lot of people argue that OR are also not very intuitive and they provide several good reasons why this might be the case. For instance, include age in your model. What would you say regarding the odds ratio for age? Has age no or at least only a small effect? Nope, age has a substantial effect on the chance to survive! A OR is intuitive if we compare groups, in the case of age it is easier to examine the effect size if we calculate probabilities for the entire range of age. Go and grab your wand, on the next page you can make predictions and see how each variable affects the probability to survive.


