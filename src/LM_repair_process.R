######## preprocessing #########
######## import
queryPuzzleResult <- read_csv("data/queryPuzzleResult.csv", quote = "\'")
repair_attribute_df_ <- repair_attribute_df

####### extract 
tmp <- queryPuzzleResult %>% select(oid,activity_sequence,lead_time,work_time)
names(tmp) <- c("caseID", "process_pattern","lead_time","work_time")

rm(queryPuzzleResult)

######### join 
tmp$caseID <- as.character(tmp$caseID)
repair_attribute_df_$caseID <- as.character(repair_attribute_df_$caseID)
repair_attribute_df_ <- left_join(repair_attribute_df_, tmp, by="caseID")

rm(tmp)

######### transform
repair_attribute_df_$caseID <- as.factor(repair_attribute_df_$caseID)
repair_attribute_df_$process_pattern <- as.factor(repair_attribute_df_$process_pattern)
repair_attribute_df_ <- repair_attribute_df_ %>% mutate(total_time = lead_time + work_time)


########### assumption test ##############
attach(repair_attribute_df_)
plot(repair_attribute_df_[,1:10])

######### normality test
hist(total_time, breaks = 30)
hist(total_time, freq = F,breaks = 30)
lines(density(total_time), col = "red", lwd = 3)
qqnorm(total_time)
qqline(total_time, col = "red")
shapiro.test(total_time)

########## cov
plot(total_time~work_time)
plot(total_time~lead_time)
cov(total_time,work_time)
cov(total_time,lead_time)

########## cor
cor(total_time,work_time,use = "complete.obs", method = "pearson")
cor(total_time,lead_time,use = "complete.obs", method = "pearson")
cor(repair_attribute_df_[,8:10])
cor.test(total_time,lead_time)
detach()

########### total time by pattern boxplot 
ggplot(repair_attribute_df_) + 
    geom_boxplot(aes(x = process_pattern, y = total_time), outlier.color = "red", fill = "grey") + 
    stat_summary(fun.y = mean, geom = "point", size = 1)
    # theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5))


########## linear regression model ############
time_lm_1 = lm(total_time~work_time, data = repair_attribute_df_)

plot(total_time~work_time,data = repair_attribute_df_)
abline(time_lm_1,col="red")
par(mfrow=c(2,2))
plot(time_lm_1)
par(mfrow=c(1,1))
shapiro.test(resid(time_lm_1))
ncvTest(time_lm_1)

summary(time_lm_1)


############ polynomial regression model ################
time_lm_2 = lm(total_time~work_time+work_time^2, data = repair_attribute_df_)

par(mfrow=c(2,2))
plot(time_lm_2)
par(mfrow=c(1,1))
shapiro.test(resid(time_lm_2))
ncvTest(time_lm_2)

summary(time_lm_2)


########## regression model continuos - factor ################
time_lm_3 = lm(total_time~factor(process_pattern), data = repair_attribute_df_)

par(mfrow=c(2,2))
plot(time_lm_3)
par(mfrow=c(1,1))

summary(time_lm_3)


########## multiple regression model ##################
time_lm_4 = lm(total_time~work_time+lead_time+factor(process_pattern), data = repair_attribute_df_)

########## test
par(mfrow=c(2,2))
plot(time_lm_4)
par(mfrow=c(1,1))
shapiro.test(resid(time_lm_4)) # residual normality test
ncvTest(time_lm_4) # residual homoscedasticity test
durbinWatsonTest(time_lm_4) # dependent variable independent test

############ multicolinearity test
vif(time_lm_4)

############ model result
summary(time_lm_4)


########## calibration ##########
######### outlier
outlierTest(time_lm_4)
influencePlot(time_lm_4, id = "identify")

repair_attribute_df_[1,]
fitted(time_lm_4)[1]
residuals(time_lm_4)[1]
rstudent(time_lm_4)[1]

repair_attribute_df_[2,]
fitted(time_lm_4)[2]
residuals(time_lm_4)[2]
rstudent(time_lm_4)[2]

repair_attribute_df_outlier_del <- repair_attribute_df_[-c(1:3),]

######## transform variable
repair_attribute_df_outlier_del <- repair_attribute_df_ %>% filter(total_time != 0)
summary(powerTransform(total_time~work_time+lead_time, data= repair_attribute_df_outlier_del)) # dependent variable => perfect. no need to transform variable
boxTidwell(total_time~work_time+lead_time, data= repair_attribute_df_outlier_del) # independent variable => perfect. no need to transform variable

####### add/remove variable
after <- lm(total_time~work_time+lead_time, data = repair_attribute_df_)

summary(after)

######### partial F test
anova(after, time_lm_4)
AIC(after,time_lm_4)

######### backward stepwise regression
max_lm <- lm(total_time~., data = repair_attribute_df_)
bwd_lm <- step(max_lm, direction = "backward")

summary(bwd_lm)

######### forward stepwise regression
min_lm <- lm(total_time~1, data = repair_attribute_df_)
fwd_lm <- step(min_lm, direction = "forward", scope = (total_time~work_time+lead_time+EstimatedRepairTime),trace = 0)

summary(fwd_lm)


######### validation ###########
######### validation set
smp_size <- floor(0.7 * nrow(repair_attribute_df_))

set.seed(123)
train_idx <- sample(seq_len(nrow(repair_attribute_df_)), size = smp_size)

train <- repair_attribute_df_[train_idx,]
test <- repair_attribute_df_[-train_idx,]

min_lm <- lm(total_time~1, data = train)
fwd_lm <- step(min_lm, direction = "forward", scope = (total_time~work_time+lead_time+EstimatedRepairTime),trace = 0)

mean((repair_attribute_df_$total_time-predict(fwd_lm,repair_attribute_df_))[-train_idx]^2)

######### k-fold cross validation
shrinkage(fwd_lm)
shrinkage(time_lm_1)

######### predict #############
min_lm <- lm(total_time~1, data = train)
fwd_lm <- step(min_lm, direction = "forward", scope = (total_time~work_time+lead_time+EstimatedRepairTime),trace = 0)

predict(fwd_lm, test, interval = "prediction") # example

rm(repair_attribute_df_, time_lm_1, time_lm_2, time_lm_3, time_lm_4,bwd_lm, fwd_lm, max_lm, min_lm, after)
