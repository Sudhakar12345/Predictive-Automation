### Accuracy for Regression (Near to zero is best fitted model)
library(hydroGOF)
rmse(test_data$Age,test_data$Predicted_data)

### Accuracy for classification (Near to one is best fitted model)
library(ROCR)











# library(ggplot2)
# library(reshape2)
# 
# dv1 = test_data$Age
# dv2 = test_data$Predicted_data
# plot(range(dv1, dv2), range(dv1, dv2), type = "n", xlab = "NumVar1(red) and NumVar2 (blue)", 
#      ylab = "Density")
# lines(dv1, col = "red")
# lines(dv2, col = "blue")


