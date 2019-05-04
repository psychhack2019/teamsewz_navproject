library(readxl)
data <- read.csv(file.choose())

####data set-up####
data_Train <- subset(data, Session=="Train")
##remove Block 1 methods (Blocks 1-3 in data)
data_Train_Block23 <- subset(data_Train, Block>3)
##relabel blocks to match Block 2 methods
data_Train_Block23_relabel <- data_Train_Block23
data_Train_Block23_relabel$Block[data_Train_Block23_relabel$Block<10] <- 2
##relabel blocks to match Block 3 methods
data_Train_Block23_relabel$Block[data_Train_Block23_relabel$Block=="10"] <- 3
##only include invisible trials
data_Train_Block23_invisible <- subset(data_Train_Block23_relabel, Visible==1)

##count instances in levels of a variable
library(plyr)
count(data, "RewardFound")

##main analysis####
block2_invisible <- subset(data_Train_Block23_invisible, Block==2)
##average rewardfound for each participant
library(plyr)
block2_inv_mean <- ddply(block2_invisible, .(Participant, Environment, RewardType), summarize,  RewardFound=mean(RewardFound))
##repeat for block 3
block3_invisible <- subset(data_Train_Block23_invisible, Block==3)
block3_inv_mean <- ddply(block3_invisible, .(Participant, Environment, RewardType), summarize,  RewardFound=mean(RewardFound))
##omnibus model with multiple variables
immediatelearn <- lmer(block3_inv_mean$RewardFound ~ 
                         block2_inv_mean$RewardFound + 
                         block3_inv_mean$Environment + 
                         block3_inv_mean$RewardType + 
                         (1|block3_inv_mean$Participant))
##for model REML criterion
summary(immediatelearn)
##for follow-up Satterthwaite analyses
anova(immediatelearn)

##follow-up analyses####
##separate rewardtype by environment
block2_inv_mean_env2 <- subset(block2_inv_mean, Environment==2)
block2_inv_mean_env3 <- subset(block2_inv_mean, Environment==3)
block3_inv_mean_env2 <- subset(block2_inv_mean, Environment==2)
block3_inv_mean_env3 <- subset(block2_inv_mean, Environment==3)
immediate_learn_env2 <- aov(block3_inv_mean_env2$RewardFound ~ 
                              block2_inv_mean_env2$RewardFound)
immediate_learn_env3 <- aov(block3_inv_mean_env3$RewardFound ~ 
                              block2_inv_mean_env3$RewardFound)

####plot train data####
qplot(x,y, color=factor(block3_inv_mean$Environment), shape=factor(block3_inv_mean$RewardType))
x <- block2_inv_mean$RewardFound
y <- block3_inv_mean$RewardFound
totalplot <- ggplot(block3_inv_mean, aes(x=x, y=y, color=block3_inv_mean$Environment, shape=block3_inv_mean$RewardType)) + geom_point()
totalplot +  labs(x="Block 2 Performance", y="Block 3 Performance", color="Environment", shape="RewardType")
qplot(block2_inv_mean_env2$RewardFound, block3_inv_mean_env2$RewardFound, xlab="Block 2 Performance", ylab="Block 3 Performance")
qplot(block2_inv_mean_env3$RewardFound, block3_inv_mean_env3$RewardFound, xlab="Block 2 Performance", ylab="Block 3 Performance")