# moneypuck shots
# data last accessed 2018-07-30, available from:
# http://moneypuck.com/about.htm#data

library(tidyverse)
library(plotly)
library(cluster)
set.seed(2134)

shots<-read.csv("shots_2007-2017.csv")

shots<-shots%>%
  filter(season>=2015)

shooter_chars<-shots%>%
  select(shooterName, teamCode, playerPositionThatDidEvent)%>%
  group_by(shooterName, playerPositionThatDidEvent)%>%
  unique()

shot_type <- shots%>%
  select(shooterName, shotID, shotType)%>%
  filter(shotType!="")

shot_cat <- shot_type%>%
  mutate(defl = ifelse(shotType == "DEFL", 1, 0),
         back = ifelse(shotType == "BACK", 1, 0),
         wrist = ifelse(shotType == "WRIST", 1, 0),
         slap = ifelse(shotType == "SLAP", 1, 0),
         snap = ifelse(shotType == "SNAP", 1, 0),
         tip = ifelse(shotType == "TIP", 1, 0),
         wrap = ifelse(shotType == "WRAP", 1, 0))

row.names(shot_cat)<-shot_type$shotID

shot_tots<-shot_cat%>%
  group_by(shooterName)%>%
  summarize(defl = sum(defl == 1),
            back = sum(back == 1),
            wrist = sum(wrist == 1),
            slap = sum(slap == 1),
            snap = sum(snap = 1),
            tip = sum(tip = 1),
            wrap = sum(wrap = 1))

shot_tots$tot = rowSums(shot_tots[2:8], na.rm = TRUE)

shot_tots<-shot_tots%>%
  group_by(shooterName)%>%
  summarize(defl = round((defl/tot), 1),
            back = round((back/tot), 1),
            wrist = round((wrist/tot), 1),
            slap = round((slap/tot), 1),
            snap = round((snap/tot), 1),
            tip = round((tip/tot), 1),
            wrap = round((wrap/tot), 1),
            tot = tot)

# shot_tots<-shot_tots%>%
#   filter(tot<32)

nope <- c(1,9)
shot_dist <- (dist(shot_tots[, -nope], method = "euclidean"))
shot_dist <- (as.matrix(shot_dist))

# shot_pc <- prcomp(shot_tots[, -nope])
# biplot(shot_pc, scale = 0)
# 
# shot_pc12<-data.frame(shot_pc$x[, 1:2])

tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = shot_dist, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)

plot(elbow_df$k, elbow_df$tot_withinss)

shot_k <- kmeans(shot_dist, centers = 3)
shot_pam <- pam(shot_dist, k = 3)

plot(silhouette(shot_pam), border = NA, col = c("#c51b7d", "#5aae61", "#253494"))

shot_cluster <- shot_k$cluster

shot_totsk <- mutate(shot_tots, cluster = shot_cluster)

# shot_totsk <- mutate(shot_totsk, shooterName = shot_tots$shooterName)
# shot_totsk <- shot_totsk%>%
#   left_join(shot_tots, by = "shooterName")


ggplot(shot_totsk, aes(wrist, slap, color = factor(cluster)))+
  geom_jitter()+
  ggtitle("NHL shot type clusters")+
  labs(x = "Wrist shots proportion", y = "Slap shots proportion", color = "Cluster")

plot_ly (shot_totsk, x = ~wrist , y = ~slap , z = ~back)%>%
  add_markers(color = ~cluster)

shot_exps<-shot_totsk%>%
  left_join(shooter_chars, by = "shooterName")%>%
  group_by(shooterName)%>%
  unique()

shot_exps%>%
  group_by(cluster)%>%
  summarize(defl = mean(defl)*100,
            back = mean(back)*100, 
            wrist = mean(wrist)*100, 
            slap = mean(slap)*100,
            snap = mean(snap)*100,
            tip = mean(tip)*100,
            wrap = mean(wrap)*100,
            D = mean(playerPositionThatDidEvent=="D")*100,
            C = mean(playerPositionThatDidEvent=="C")*100,
            L = mean(playerPositionThatDidEvent=="L")*100,
            R = mean(playerPositionThatDidEvent=="R")*100)


players1<-as.data.frame(shot_exps%>%
  filter(cluster==1)%>%
  arrange(desc(tot))%>%
  select(-c(teamCode, playerPositionThatDidEvent))%>%
  unique())

players2<-as.data.frame(shot_exps%>%
  filter(cluster==2)%>%
  arrange(desc(tot))%>%
  select(-c(teamCode, playerPositionThatDidEvent))%>%
  unique())

def2<-as.data.frame(shot_exps%>%
  filter(cluster==2)%>%
  filter(playerPositionThatDidEvent=="D")%>%
  select(shooterName:cluster)%>%
  arrange(desc(tot))%>%
  unique())

players3<-as.data.frame(shot_exps%>%
  filter(cluster==3)%>%
  arrange(desc(tot))%>%
  select(-c(teamCode, playerPositionThatDidEvent))%>%
  unique())


