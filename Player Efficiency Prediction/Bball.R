#----------------------------------------------------#

library(rvest)
library(RSelenium)
library(data.table)
library(ggplot2)
library(plotly)

#----------------------------------------------------#

# player page
lebron <- "https://www.basketball-reference.com/players/j/jamesle01.html"
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(lebron)
lebron_webpage <- read_html(remDr$getPageSource()[[1]])
lebron_table <- html_table(lebron_webpage, fill = TRUE)

for (i in 1:length(lebron_table)) 
  assign(paste0("table_",i),data.frame(lebron_table[i]))

#----------------------------------------------------#

# rename and clean datasets
season_stats <- table_27[1:15, ]
season_totals <- table_29[1:15, ]
per_100 <- table_31[1:15, ]
advanced <- table_32[1:15, ]
playoff <- table_35[1:12, ]

# select dataframes to keep
to_keep <- c("season_stats","season_totals", 'per_100', 'advanced', 'playoff')

# remove all but select datasets
rm(list=ls()[! ls() %in% to_keep])

#----------------------------------------------------#

# begin analytics

# plot PER performance over time
g <- ggplot(advanced, aes(Season)) + 
     geom_point(aes(y = PER, colour = "PER")) + ggtitle('Efficiency Rating') + 
     xlab('Season') + ylab('PER')
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(g)

# functions for performance
advanced$Seasons <- 1:nrow(advanced)

# quadratic fit
quad_fit <- lm(PER~poly(Seasons, 2, raw = TRUE), data = advanced)
quad_val <- data.frame(quad_pred = c(quad_fit$fitted.values))

# poly fit
poly_fit <- lm(PER~poly(Seasons, 3, raw = TRUE), data = advanced)
poly_val <- data.frame(poly_pred = c(poly_fit$fitted.values))

# merge predictions
vals <- cbind(quad_val, poly_val)

# merge to dataframe
advanced <- cbind(advanced, vals)

# plot PER performance over time
g <- ggplot(advanced, aes(Seasons)) + 
     geom_point(aes(y = PER, colour = 'PER')) + ggtitle('Efficiency Rating') +
     geom_line(aes(y = poly_pred, colour = 'poly_pred')) +
     geom_line(aes(y = quad_pred, colour = 'quad_pred')) +
     xlab('Season') + ylab('PER')
g <- g + theme(axis.text.x = element_text(angle = 0, hjust = 1))
ggplotly(g)

#----------------------------------------------------#

# create projections for PER

# projecting PER going forward
pred_season <- data.frame(Seasons = c(16:18))

# predicted performance
data_predictions <- data.frame(Seasons = c(16:18), 
                               quad_prediction = c(predict(quad_fit, newdata = pred_season)), 
                               poly_prediction = c(predict(poly_fit, newdata = pred_season))) 
data_predictions$ensemble <- (data_predictions$quad_prediction + data_predictions$poly_prediction) / 2         

# viz predictions
g <- ggplot(data_predictions, aes(Seasons)) + 
     geom_line(aes(y = quad_prediction, colour = "quad_prediction")) +
     geom_line(aes(y = poly_prediction, colour = "poly_prediction")) +
     geom_point(aes(y = ensemble, colour = "ensemble")) +
     ggtitle("Model PER Predictions") + ylab("Predicted PER") + xlab("Seasons")
ggplotly(g)



# end of script