#### The Model:

Let’s run a logistic regression. Which of the following independent variables would you like to include to estimate their effect on survival?  

Maybe you picked passenger’s sex. But what does the estimate for “Male” really tell you? For example, an estimate of -2.51 for males compared to females might seem confusing.  

Because of the way logistic regression works, the model gives us the *logarithm* of the odds of survival as the result. When dealing with log(odds), all we can say is whether the effect is positive or negative and whether it’s statistically significant.  

However, interpreting the exact meaning of such an estimate is tricky. That’s why it’s often better to look at **odds ratios** instead — which tell us how the odds change multiplicatively. Even better, we can predict the **probability** of survival, which is easier to understand and communicate. Both odds ratios and predicted probabilities help make the model’s results more intuitive.
