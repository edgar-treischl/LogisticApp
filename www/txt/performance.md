#### How well does the model perform?

You probably know that R² is often used to assess the performance of linear models. Unfortunately, assessing logistic regression performance is a bit trickier. There are pseudo R² measures for logistic regression to compare nested models, but unlike linear models, these cannot be interpreted as explained variance.

Instead, two important metrics you’ll encounter are **sensitivity** and **specificity**.

---

- **Sensitivity** measures how well the model correctly identifies true positives — that is, how many of the passengers who survived did the model classify correctly? The mosaic plot shows the observed outcomes on the x-axis and the model’s predictions on the y-axis. Sensitivity is calculated by dividing the number of true positives by the total number who survived (207 / 290), which gives us 0.71.

- **Specificity** measures how well the model correctly identifies true negatives — passengers who did not survive and were correctly classified as such. It’s calculated by dividing true negatives by all passengers who did not survive (356 / 424).

---

A common way to combine these two measures is with an **ROC curve**. The plot below shows sensitivity on the y-axis and the false positive rate (1 - specificity) on the x-axis.

What does the ROC curve tell you? When predicting a binary outcome, we want to achieve two things simultaneously:

1. Correctly classify as many survivors as possible (high sensitivity).
2. Minimize false positives (low false positive rate).

Ideally, we want sensitivity = 1 and false positive rate = 0 — shown as the black point in the ROC curve below.  

If the model has no predictive power, the ROC curve will be a diagonal line — like flipping a fair coin, where 50% of the time you classify correctly and 50% of the time you don’t.


Sensitivity and specificity aren’t the only ways to evaluate model performance, but keep this in mind:  
**The further the ROC curve is from the diagonal line and closer to the ideal point, the stronger the model’s explanatory power.**
