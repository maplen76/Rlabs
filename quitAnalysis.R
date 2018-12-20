tp1 <- read.csv("D:\\R\\RLabs\\playScore\\tp2.csv", stringsAsFactors = F)

tp_y <- tp1 %>% filter(quit == "Yes")
tp_n <- tp1 %>% filter(quit == "No")

# sample 60% as training set
tp_y_training_row <- sample(1:nrow(tp_y), 0.7*nrow(tp_y))
tp_n_training_row <- sample(1:nrow(tp_n), 0.7*nrow(tp_n))
tp_y_training <- tp_y[tp_y_training_row,]
tp_n_training <- tp_n[tp_n_training_row,]
tp_training <- rbind(tp_y_training, tp_n_training)

tp_y_test <- tp_y[-tp_y_training_row,]
tp_n_test <- tp_n[-tp_n_training_row,]
tp_test <- rbind(tp_y_test, tp_n_test)


tp_training_num <- tp_training %>% 
    mutate(churn = plyr::mapvalues(quit, from = c("Yes", "No"), to = c(1, 0)),
           churn = as.numeric(churn)
    )

IV <- create_infotables(data = tp_training_num, y = "churn", bins = 10, parallel = FALSE)
IV$Summary

tp_glm <- glm(factor(quit) ~ days_notplay + sessionscount + daysplayed + duration + lifetime + nb_mplocal_matches, data = tp_training, family = binomial)
summary(tp_glm)

# remove these not significant indenpendant variables
# days_notplay + sessionscount + duration + lifetime + nb_mplocal_matches
tp_glm_fix <- glm(factor(quit) ~ days_notplay + sessionscount + nb_mplocal_matches, data = tp_training, family = binomial)
summary(tp_glm_fix)

# training set performance
tp_training_v <- tp_training %>% 
    mutate(pred = predict(object = tp_glm_fix, newdata = tp_training, type = "response"))
plotROC(tp_training_v$quit, tp_training_v$pred)


# test set performance
tp_test_v <- tp_test %>% 
    mutate(pred = predict(object = tp_glm_fix, newdata = tp_test, type = "response"),
           pred = as.numeric(pred),
           churn01 = plyr::mapvalues(quit, from = c("Yes","No"), to = c(1,0)),
           churn01 = as.integer(churn01)
    )

plotROC(tp_test_v$churn01, tp_test_v$pred)
