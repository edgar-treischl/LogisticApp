#### Odds Ratio?

What would be the chance for men to survive *if* they had the same odds as women? In that case, the odds ratio would be one, because we would expect the same number of men and women to survive.  

You can calculate the odds ratio directly from the logistic regression, but let’s do it by hand first to build some intuition about what an **OR** really means. The next plot shows how many men and women survived.

Look at the bar graph and the counts for each group. We calculate men’s odds of survival by dividing the number of men who survived (109) by the number of men who did not survive (468). Women’s odds of survival are calculated the same way (233 / 81).  

Finally, divide men’s odds by women’s odds, and you get the odds ratio for men surviving compared to women.

Of course, you don’t have to do this in your head — just let your statistics software handle the calculations, as shown in the next console output:

---

#### Remember the interpretation:

- **OR > 1**: Positive effect (higher odds of surviving)
- **OR = 1**: No effect (equal odds)
- **0 < OR < 1**: Negative effect (lower odds of surviving)

---

In our example, men’s chance to survive is reduced by a factor of 0.08 compared to women. What about age and the other variables in your model? If you haven’t chosen any independent variables yet, go back to the **Model** tab and try them out!

Many people argue that odds ratios can be unintuitive — and they have good reasons. For example, try including age in your model. Does the odds ratio for age look like it has little or no effect? Actually, age has a *substantial* effect on the chance to survive!  

Odds ratios are easiest to interpret when comparing groups. For continuous variables like age, it’s often more helpful to look at predicted probabilities across the full range.  

So grab your wand — on the next page, you can make predictions and see how each variable changes the *probability* of survival in a more intuitive way.
