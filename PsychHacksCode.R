# PsychHacks2019, Team SEWZ
# Dataset: Navigation

#### clear workspace ####
rm(list=ls(all=TRUE))

#### Setup Working Environment ####
dataDir <- "~/Dropbox/University of Toronto/PhD/Workshops/PsychHacks2019/Navigation-Dataset-master/"
setwd(dataDir)

#### Load Necessary Package ####
library(tidyverse) 
library(ez) # for ezANOVA function
library(gridExtra)
#library(MASS)

#### Functions for Analyses ####
# Compute standard error
se <- function(inputData, na.rm = FALSE) {
  if (na.rm) {inputData <- na.omit(inputData)}
  STE <-  sd(inputData)/sqrt(length(inputData))
  return(STE)
}

# Compute Bhattacharyya coefficient
bhattaCoef <- function(df_train, df_test, bns = 512, xmin = -15, xmax =15, ymin = -15, ymax =15){
  dfprob_train <-MASS::kde2d(df_train$RewardX,df_train$RewardY,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob_train$z <- dfprob_train$z /sum(dfprob_train$z)
  dfprob_test <-kde2d(df_test$X,df_test$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob_test$z <-dfprob_test$z /sum(dfprob_test$z)
  bc <- sum(sqrt(dfprob_train$z * dfprob_test$z))
  return(bc)
}

#### Read Data ####
# Raw Data
dataNav <- read_csv("CleanedParticipantData.csv")
# Extract training data
dataNav_train <- dataNav %>% 
  filter(Session=="Train") 
# Extract testing data
dataNav_test <- dataNav %>% 
  filter(Session=="Test")

#### Clean the Data ####
# Remove the incorrectly-cleaned trial in the testing
dataNav_test <- dataNav_test %>% 
  filter(RewardFound!=1)

# Find out which subject only participated the training but not the testing
subIDs_train <- dataNav_train %>% count(Participant) %>% select(Participant)
subIDs_test <- dataNav_test %>% count(Participant) %>% select(Participant)
indSub <- setdiff(subIDs_train, subIDs_test)
dataNav_train_raw <- dataNav_train
dataNav_train <- dataNav_train %>% 
  filter(Participant!=indSub$Participant)
# update subject IDs after cleaning
subIDs_train <- dataNav_train %>% count(Participant) %>% select(Participant)
subIDs <- subIDs_train # = subIDs_test
nSubs <- dim(subIDs)[1]

#### Check Data Structure ####
# how many times a reward is found for a trial in the training
nRewardFound_train <- dataNav_train %>% 
  filter(RewardType!="NoReward") %>% 
  filter(Block!=3) %>% 
  group_by(Participant,RewardType) %>% 
  summarise(N=n(),nRewardfound=sum(RewardFound==1)) %>% 
  arrange(RewardType, Participant)

# Frequency table of the training
freqTbl_train <- dataNav_train %>% 
  group_by(Participant) %>%
  count(Block,Trial,RewardType,TwoDTrial,Environment,Visible) 

# Frequency table of the testing 
freqTbl_test <- dataNav_test %>% 
  group_by(Participant) %>%
  count(Block,Trial,RewardType,TwoDTrial,Environment,Visible) 


#### Rewards Found by Participants During Training ####
# only take Reward trials and training blocks 4 to 10
dataNav_train_sel <- dataNav_train %>% 
  filter(RewardType!="NoReward") %>% 
  filter(Block!=3) 

rewardFound_train <- data.frame()
for (s in 1:nSubs) {
  currSubID = subIDs[s,1]$Participant
  
  tmpSub_RewFound <- dataNav_train_sel %>%
    filter(Participant==currSubID) %>%
    filter(RewardFound==1) %>% 
    arrange(Block,Trial,RewardType)
  
  rewardFound_train <- rbind(rewardFound_train, tmpSub_RewFound)
  rm(currSubID,tmpSub_RewFound)
}


#### Moving Patterns During Testing ####
movePatt_test <- data.frame()
for (s in 1:nSubs) {
  currSubID = subIDs[s,1]$Participant
  
  tmpSub <- dataNav_test %>%
    filter(Block==3) %>% 
    filter(Participant==currSubID) %>% 
    filter(RewardX==0 & RewardY==0) %>%
    filter(TrialTime > 5) %>% 
    filter(Movement==1) %>% 
    arrange(Block,Trial,RewardType)
  
  movePatt_test <- rbind(movePatt_test, tmpSub)
    
  rm(currSubID,tmpSub)
}

#### Compare Training and Testing Patterns ####
bc_out <- data.frame(Participant=character(), Delay=character(), 
                     Environment=character(), Reward_F=double(),
                     Reward_W=double(), Reward_M=double(),
                     stringsAsFactors = FALSE)
nRewardType <- 3 # number of reward types
for (s in 1:nSubs) {
  currSubID = subIDs[s,1]$Participant
  # data of reward-found trials of 1 subject during training
  pos_reward <- rewardFound_train %>% 
    filter(Participant==currSubID) %>% 
    select(Delay,Environment,RewardType,RewardX,RewardY,Block,Trial,TrialTime)
  # data of moving pattern of 1 subject during training
  pos_move <- movePatt_test %>% 
    filter(Participant==currSubID) %>% 
    select(Delay,Environment,RewardType,X,Y,Block,Trial,TrialTime)
  # compute Bhattacharyya coefficient for each type of reward
  bc_F <- bhattaCoef(pos_reward[pos_reward$RewardType=="F", ], 
                     pos_move[pos_move$RewardType=="F", ])
  bc_W <- bhattaCoef(pos_reward[pos_reward$RewardType=="W", ], 
                     pos_move[pos_move$RewardType=="W", ])
  bc_M <- bhattaCoef(pos_reward[pos_reward$RewardType=="M", ], 
                     pos_move[pos_move$RewardType=="M", ])
  bc_out[s,1] <- currSubID
  bc_out[s,2] <- pos_reward$Delay[1]
  bc_out[s,3] <- pos_reward$Environment[1]
  bc_out[s,4:6] <- c(bc_F, bc_W, bc_M)
  
  print(paste("Participant",currSubID,"done",sep=" "))
  
  rm(pos_reward,pos_move)
  rm(bc_F,bc_W,bc_M)
}
# save the output as a csv file
outputDir <- "~/Dropbox/University of Toronto/PhD/Workshops/PsychHacks2019/teamsewz_navproject/"
outFile <- paste(outputDir,"bc_out.csv",sep="")
write_csv(bc_out,outFile)

#### ANOVA on Bhattacharyya Coefficient #### 
# convert to long form and ensure subject and between-subject variables are factors
bc_out_long <- bc_out %>% 
  gather(key=RewardType, value=BC, Reward_F:Reward_M) %>% 
  mutate(Participant=factor(Participant), Delay=factor(Delay), 
         Environment=factor(Environment), RewardType=factor(RewardType))
levels(bc_out_long$Environment) <- c("Rural","Urban") # rename levels of Environment
mixANOVA <- ezANOVA(data = bc_out_long, dv = BC, 
                    wid = Participant, within = .(RewardType),
                    between = .(Delay,Environment), 
                    type = 3, detailed = TRUE)
print(mixANOVA)


#### Plot Bar Graph #### 
# descriptive stats
dscrpStats <- bc_out_long %>% 
  group_by(Delay,Environment,RewardType) %>% 
  summarise(averBC = mean(BC), N=n())
# bar graph
barGraph_BC <- bc_out_long %>% 
  group_by(Delay, Environment) %>% 
  summarise(averBC=mean(BC), SE = se(BC)) %>% 
  ggplot(aes(fill=Environment, y=averBC, x=Delay)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=averBC-SE, ymax=averBC+SE), width=.2,
                position=position_dodge(.9)) + 
  ylab("Avearage Bhattacharyya Coefficient")
plot(barGraph_BC)

#### Plot Reward Locations and Move Patterns ####
# change the subject ID depending on which subject's data you want to plot
currSubID = 70
pos_reward <- rewardFound_train %>% 
  filter(Participant==currSubID) %>% 
  select(Delay,Environment,RewardType,RewardX,RewardY,Block,Trial,TrialTime) %>% 
  mutate(RewardType=factor(RewardType))
levels(pos_reward$RewardType) <- c("Food","Money","Water")
pos_move <- movePatt_test %>% 
  filter(Participant==currSubID) %>% 
  select(Delay,Environment,RewardType,X,Y,Block,Trial,TrialTime) %>% 
  mutate(RewardType=factor(RewardType))
levels(pos_move$RewardType) <- c("Food","Money","Water")
scatPlot_reward <- pos_reward %>%
  ggplot(aes(x=RewardX,y=RewardY,color=RewardType)) +
  geom_point()
scatPlot_move <- pos_move %>%
  ggplot(aes(x=X,y=Y,color=RewardType)) +
  geom_point()
grid.arrange(scatPlot_reward, scatPlot_move, nrow=1,
             top = paste("Participant",currSubID,sep=" "))


#### Whether Training Data Can Predict Testing Data ####
dv <- bc_out_long$BC # Bhattacharyya Coefficient 
iv <- nRewardFound_train$nRewardfound # number of reward found trials during training
model <- lm(dv ~ iv)
anova(model)

# collapse 3 reward types
nRewardFound_train %>% 
  select(Participant,RewardType,nRewardfound) %>% 
  spread(key=RewardType, value=nRewardfound) %>% 
  mutate(nRewardfound_all=sum(F+M+W))


#### ANOVA on Centroids ####
averPos_reward <- rewardFound_train %>% 
  group_by(Participant, RewardType, Delay, Environment) %>% 
  summarise(averRewardX=mean(RewardX), averRewardY=mean(RewardY))
averPos_move <- movePatt_test %>% 
  group_by(Participant, RewardType, Delay, Environment) %>% 
  summarise(averX=mean(X), averY=mean(Y))
averPos_combine <- data.frame()
for (i in 1:dim(averPos_reward)[1]) {
  averPos_combine[i,1:6] <- averPos_reward[i,]
  averPos_combine[i,7:8] <- averPos_move[i,5:6]
}

dist_averPos_long <- averPos_combine %>% 
  mutate(Dist=sqrt((averX-averRewardX)^2+(averY-averRewardY)^2)) %>% 
  select_at(c(1:4,9)) %>% 
  mutate(Participant=factor(Participant), Delay=factor(Delay), 
         Environment=factor(Environment), RewardType=factor(RewardType))
levels(dist_averPos_long$Environment) <- c("Rural","Urban") # rename levels of Environment
mixANOVA_cent <- ezANOVA(data = dist_averPos_long, dv = Dist, 
                    wid = Participant, within = .(RewardType),
                    between = .(Delay,Environment), 
                    type = 3, detailed = TRUE)
print(mixANOVA_cent)
  
barGraph_dist <- dist_averPos_long %>% 
  group_by(Delay, Environment) %>% 
  summarise(averDist=mean(Dist), SE = se(Dist)) %>% 
  ggplot(aes(fill=Environment, y=averDist, x=Delay)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=averDist-SE, ymax=averDist+SE), width=.2,
                position=position_dodge(.9)) + 
  ylab("Avearage Distance Between Centroids of Training and Testing")
plot(barGraph_dist)



