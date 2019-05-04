#### clear workspace ####
rm(list=ls(all=TRUE))

#### Setup Working Environment ####
rootDir <- "~/Dropbox/University of Toronto/PhD/Workshops/PsychHacks2019/Navigation-Dataset-master/"
setwd(rootDir)

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

#### Read Raw Data ####
dataNav <- read_csv("CleanedParticipantData.csv")

# Extract training data
dataNav_train <- dataNav %>% 
  filter(Session=="Train") 
# %>% 
#   mutate(Participant=factor(Participant), Delay=factor(Delay),
#          Environment=factor(Environment))

# Extract testing data
dataNav_test <- dataNav %>% 
  filter(Session=="Test") %>% 
  filter(RewardFound!=1) # remove the misclean trial

#### Clean the data ####
subIDs_train <- dataNav_train %>% count(Participant) %>% select(Participant)
subIDs_test <- dataNav_test %>% count(Participant) %>% select(Participant)
# find out which subject was in the training but not in the testing
indSub <- setdiff(subIDs_train, subIDs_test)
dataNav_train_raw <- dataNav_train
dataNav_train <- dataNav_train %>% 
  filter(Participant!=indSub$Participant)

subIDs_train <- dataNav_train %>% count(Participant) %>% select(Participant)
subIDs <- subIDs_train # = subIDs_test
nSubs <- dim(subIDs)[1]

#### Check Data Structure ####
# how many times a reward is found for a trial in the training and testing phase
nRewardFound_train <- dataNav_train %>% 
  filter(RewardType!="NoReward") %>% 
  filter(Block!=3) %>% 
  group_by(Participant,RewardType) %>% 
  summarise(N=n(),nRewardfound=sum(RewardFound==1)) %>% 
  arrange(RewardType, Participant)

nRewardFound_test <- dataNav_test %>% 
  group_by(Participant) %>% 
  summarise(N=n(),nRewardfound=sum(RewardFound==1))

# Frequency table of the training
freqTbl_train <- dataNav_train %>% 
  group_by(Participant) %>%
  count(Block,Trial,RewardType,TwoDTrial,Environment,Visible) 

# Frequency table of the testing 
freqTbl_test <- dataNav_test %>% 
  group_by(Participant) %>%
  count(Block,Trial,RewardType,TwoDTrial,Environment,Visible) 

#### Rewards Found during Training ####
# tmp <- dataNav_train %>% 
#   group_by(Participant) %>%
#   count(Block,Trial,RewardType,Environment,Visible)
# rm(tmp)
# only take Reward trials and training blocks 4 to 10
dataNav_train_sel <- dataNav_train %>% 
  filter(RewardType!="NoReward") %>% 
  filter(Block!=3) 

rewardFound_train <- data.frame()
# Participant=character(),Delay=character(),
# Environment=character(),RewardType=character(),
# Block=double(),Trial=double(),Visible=double(),
for (s in 1:nSubs) {
  currSubID = subIDs[s,1]$Participant
  
  tmpSub_RewFound <- dataNav_train_sel %>%
    filter(Participant==currSubID) %>%
    filter(RewardFound==1) %>% 
    arrange(Block,Trial,RewardType)
  # %>%
  #   select(Participant,Session,Delay,Environment,RewardType,Block,Trial,RewardX,RewardY) %>% 
  
  rewardFound_train <- rbind(rewardFound_train, tmpSub_RewFound)
  rm(currSubID,tmpSub_RewFound)
  
  # tmpSub_vis <- dataNav_train_sel %>%
  #   filter(Participant==currSubID) %>% 
  #   filter(Block!=10) %>% 
  #   filter(Visible==1) %>% 
  #   filter(RewardFound==1) %>% 
  #   arrange(Block,Trial) %>% 
  #   select(Participant,Delay,Environment,Block,Trial,RewardType,RewardX,RewardY)
  # 
  # tmpSub_inv <- dataNav_train_sel %>%
  #   filter(Participant==currSubID) %>% 
  #   filter(Block!=10) %>% 
  #   filter(Visible==0) %>% 
  #   filter(RewardFound==1) %>% 
  #   arrange(Block,Trial) %>% 
  #   select(Participant,Delay,Environment,Block,Trial,RewardType,RewardX,RewardY) 
  # 
  # tmpSub_blk10 <- dataNav_train_sel %>% 
  #   filter(Participant==currSubID) %>% 
  #   filter(Block==10) %>%
  #   filter(RewardFound==1) %>% 
  #   arrange(Block,Trial) %>% 
  #   select(Participant,Delay,Environment,Block,Trial,RewardType,RewardX,RewardY) 
}

#### Moving Patterns during Testing ####
movePatt_test <- data.frame()
for (s in 1:nSubs) {
  currSubID = subIDs[s,1]$Participant
  
  tmpSub <- dataNav_test %>%
    filter(Block==3) %>% 
    filter(Participant==currSubID) %>% 
    filter(RewardX==0 & RewardY==0) %>%
    filter(Movement==1) %>% 
    arrange(Block,Trial,RewardType)
  
  movePatt_test <- rbind(movePatt_test, tmpSub)
    
  rm(currSubID,tmpSub)
}

#### Compare training and testing ####
# isPlot <- 1
# if (isPlot==1){
#   scatPlotList_reward <- list()
#   scatPlotList_move <- list()
# }
bc_out <- data.frame(Participant=character(), Delay=character(), 
                     Environment=character(), Reward_F=double(),
                     Reward_W=double(), Reward_M=double(),
                     stringsAsFactors = FALSE)
nRewardType <- 3
for (s in 1:nSubs) {
  currSubID = subIDs[s,1]$Participant
  
  pos_reward <- rewardFound_train %>% 
    filter(Participant==currSubID) %>% 
    select(Delay,Environment,RewardType,RewardX,RewardY,Block,Trial,TrialTime)
  
  pos_move <- movePatt_test %>% 
    filter(Participant==currSubID) %>% 
    select(Delay,Environment,RewardType,X,Y,Block,Trial,TrialTime)
  
  # if (isPlot==1) {
  #   scatPlot_reward <- pos_reward %>%
  #     ggplot(aes(x=RewardX,y=RewardY,color=RewardType)) +
  #     geom_point()
  #   scatPlot_move <- pos_move %>%
  #     ggplot(aes(x=X,y=Y,color=RewardType)) +
  #     geom_point()
  #   scatPlotList_reward[s] <- scatPlot_reward
  #   scatPlotList_move[s] <- scatPlot_move
  # 
  #   # scatPlot_combine <- grid.arrange(scatPlot_reward, scatPlot_move, nrow=1,
  #   #                                  top = paste("Participant",currSubID,sep=" "))
  #   # scatPlotList[s] <- scatPlot_combine
  #   rm(scatPlot_reward,scatPlot_move)
  # }
  
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

outFile <- paste(rootDir,"bc_out.csv",sep="")
write_csv(bc_out,outFile)

#### ANOVA on Bhattacharyya coefficient #### 
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
dscrpStats <- bc_out_long %>% 
  group_by(Delay,Environment,RewardType) %>% 
  summarise(averBC = mean(BC), N=n())

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
# plot the first 3 subjects ?
s <- 2
currSubID = subIDs[s,1]$Participant
pos_reward <- rewardFound_train %>% 
  filter(Participant==currSubID) %>% 
  select(Delay,Environment,RewardType,RewardX,RewardY,Block,Trial,TrialTime)
pos_move <- movePatt_test %>% 
  filter(Participant==currSubID) %>% 
  select(Delay,Environment,RewardType,X,Y,Block,Trial,TrialTime)
scatPlot_reward <- pos_reward %>%
  ggplot(aes(x=RewardX,y=RewardY,color=RewardType)) +
  geom_point()
scatPlot_move <- pos_move %>%
  ggplot(aes(x=X,y=Y,color=RewardType)) +
  geom_point()
grid.arrange(scatPlot_reward, scatPlot_move, nrow=1,
             top = paste("Participant",currSubID,sep=" "))


#### Regression ####
model <- lm(bc_out_long$BC ~ nRewardFound_train$nRewardfound)
anova(model)

# collapse 3 reward types
nRewardFound_train %>% 
  select(Participant,RewardType,nRewardfound) %>% 
  spread(key=RewardType, value=nRewardfound) %>% 
  mutate(nRewardfound_all=sum(F+M+W))


#### ANOVA on Centroids  ####
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



###############################################################################
#### Distance between participants and rewards in testing #### 
#tmp <- dataNav_test %>% filter(Block==1 | Block==2) %>% count(RewardType)

# subIDs <- dataNav_test %>% count(Participant) %>% select(Participant)
# nSubs <- dim(subIDs)[1]
# 
# lastDist_test <- data.frame(Participant=character(), Dist=double(), 
#                        RewardType=character(), Delay=character(), 
#                        Environment=character(), nSteps=double(),
#                        stringsAsFactors = FALSE)
# for (s in 1:nSubs) {
#   currSubID = subIDs[s,1]$Participant
#   
#   tmpSub <- dataNav_test %>%
#     filter(Block==3) %>% 
#     filter(Participant==currSubID) %>% 
#     filter(RewardX==0 & RewardY==0) %>% 
#     mutate(Dist= sqrt((X-RewardX)^2+(Y-RewardY)^2) ) %>% 
#     arrange(Block,Trial)
#   nSteps <- tmpSub %>% group_by(Trial) %>% summarise(N=n())
#   
#   tmpLastDist <- data.frame(Participant=character(), Dist=double(), 
#                             RewardType=character(), Delay=character(), 
#                             Environment=character(), nSteps=double(),
#                             stringsAsFactors = FALSE)
#   for (t in 1:dim(nSteps)[1]) {
#     ind_currTrl <- tmpSub$Trial==t
#     currTrl <- tmpSub[ind_currTrl,]
#     currTrl_lastStep <- nSteps$N[t]
#     tmpLastDist[t,1] <- currTrl[currTrl_lastStep,]$Participant
#     tmpLastDist[t,2] <- currTrl[currTrl_lastStep,]$Dist
#     tmpLastDist[t,3] <- currTrl[currTrl_lastStep,]$RewardType
#     tmpLastDist[t,4] <- currTrl[currTrl_lastStep,]$Delay
#     tmpLastDist[t,5] <- currTrl[currTrl_lastStep,]$Environment
#     tmpLastDist[t,6] <- currTrl_lastStep
#     
#     rm(ind_currTrl, currTrl, currTrl_lastStep)
#   } # end of going through all trials in 1 subject
#   
#   lastDist_test <- rbind(lastDist_test, tmpLastDist)
#   
#   rm(tmpSub,tmpLastDist)
# } # end of going through all subs
# 
# 
# #### Descriptive Stats ####
# nTrials <- lastDist_test %>% count(Participant,RewardType)
# dscrpStats_test <- lastDist_test %>% 
#   group_by(Delay,Environment,RewardType) %>% 
#   summarise(averDist = mean(Dist))
# 
# dscrpStats_test <- lastDist_test %>% 
#   group_by(Delay,Environment) %>% 
#   summarise(averDist = mean(Dist))
# 
# #### ANOVA ####
# # ensure between- and within-subject variables are encoded as factors
# lastDist_test <- lastDist_test %>% 
#   mutate(Participant=factor(Participant), RewardType=factor(RewardType),
#          Delay=factor(Delay), Environment=factor(Environment))
# mixANOVA <- ezANOVA(data = lastDist_test, dv = Dist, 
#                     wid = Participant, within = .(RewardType),
#                     between = .(Delay,Environment), 
#                     type = 3, detailed = TRUE)
# print(mixANOVA)
# 
# #### Plot ####
# barGraph_test <- lastDist_test %>% 
#   group_by(Delay, Environment) %>% 
#   summarise(meanDist=mean(Dist), SE = se(Dist)) %>% 
#   ggplot(aes(fill=Environment, y=meanDist, x=Delay)) +
#   geom_bar(position="dodge", stat="identity") +
#   geom_errorbar(aes(ymin=meanDist-SE, ymax=meanDist+SE), width=.2,
#                 position=position_dodge(.9))
#   
# 
