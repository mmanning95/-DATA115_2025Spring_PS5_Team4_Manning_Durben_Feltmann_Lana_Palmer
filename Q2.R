library(ggplot2)
library(GGally)


advertising_df <- read.csv('./Advertising.csv')

ggpairs(advertising_df[, c('TV', 'radio', 'newspaper', 'sales')],
        title = 'Scatterplot Matrix of Advertising Data')

model <- lm(sales ~ TV + radio + newspaper, data = advertising_df)

summary(model)
