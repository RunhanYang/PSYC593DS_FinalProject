setwd("C:/Users/runha/OneDrive - University of Illinois - Urbana/Documents/MSPS/Psyc593DS")
mydata = read.csv("Appendix II.csv")
mydata$PVI = with(mydata, ifelse(item_reported == 999,0,1))
mydata$condition = factor(mydata$condition,levels=c("congruent","incongruent","control"))

library(ggplot2)
library(reshape2)
library(dplyr)
##############################################FIGURES##############################################

## general discription, based on condition 
# Calculate the percentages
temp0 = data.frame(c("congruent","incongruent","control"),
                   c(nrow(mydata[which(mydata$condition=="congruent"&mydata$item_reported==999),])/159,
                     nrow(mydata[which(mydata$condition=="incongruent"&mydata$item_reported==999),])/159,
                     nrow(mydata[which(mydata$condition=="control"&mydata$item_reported==999),])/159),
                   c(nrow(mydata[which(mydata$condition=="congruent"&mydata$item_reported==0),])/159,
                     nrow(mydata[which(mydata$condition=="incongruent"&mydata$item_reported==0),])/159,
                     nrow(mydata[which(mydata$condition=="control"&mydata$item_reported==0),])/159),
                   c(nrow(mydata[which(mydata$condition=="congruent"&mydata$item_reported==1),])/159,
                     nrow(mydata[which(mydata$condition=="incongruent"&mydata$item_reported==1),])/159,
                     NA),
                   c(NA,
                     nrow(mydata[which(mydata$condition=="incongruent"&mydata$item_reported==2),])/159,
                     nrow(mydata[which(mydata$condition=="control"&mydata$item_reported==2),])/159))
colnames(temp0) = c("condition","999","0","1","2")
temp0 = melt(temp0,id.vars = "condition")

figure1 = ggplot(temp0, aes(x=factor(condition,levels=c("congruent","incongruent","control")), y=value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity",width=0.5) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(as.integer(value*100),"%")), 
            position = position_stack(vjust = 0.5), size = 4) + 
  labs(x = "Conditions", y = "Percentage") + 
  scale_fill_discrete(name="Q1 Responses",
                      labels=c("no PVI", "PVI, no specific item", "PVI, v1/2 item", "PVI, v3 item")) +
  theme(text = element_text(size = 15))
figure1 

## Q2-4 for v1/v2
temp <- mydata %>%
  group_by(v1_item) %>% #change to v2_item for v2 responses
  summarise( 
    n=159,
    mean1=mean(v1_Q2),
    mean2=mean(v1_Q3),
    mean3=mean(v1_Q4),
    sd1=sd(v1_Q2),
    sd2=sd(v1_Q3),
    sd3=sd(v1_Q4),
  ) %>%
  mutate( se1=sd1/sqrt(n))  %>%
  mutate( se2=sd2/sqrt(n))  %>%
  mutate( se3=sd3/sqrt(n))  %>%
  mutate( ci1=se1 * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( ci2=se2 * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( ci3=se3 * qt((1-0.05)/2 + .5, n-1))

temp_long = melt(temp,id.vars = "v1_item")
a1 = temp_long[5:16,]
a2 = temp_long[41:52,]
temp_long = cbind(a1,a2)
temp_long = temp_long[,c(1,2,3,6)]
colnames(temp_long) = c("v1_item","variable","m","ci")
figure2 = ggplot(temp_long, aes(x = factor(v1_item,levels=c("coin","card","handkerchief","control")), y = m, fill = factor(variable))) + 
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=m-ci, ymax=m+ci), width=.3,
                position=position_dodge(.9)) +
  scale_fill_discrete(name="Questions",
                      labels=c("Q2", "Q3", "Q4"))+ 
  theme(text = element_text(size = 20)) + 
  xlab("Video 1 items") + ylab("Ratings") + ylim(0,100)
figure2

temp <- mydata %>%
  group_by(v2_item) %>% #change to v2_item for v2 responses
  summarise( 
    n=159,
    mean1=mean(v2_Q2),
    mean2=mean(v2_Q3),
    mean3=mean(v2_Q4),
    sd1=sd(v2_Q2),
    sd2=sd(v2_Q3),
    sd3=sd(v2_Q4),
  ) %>%
  mutate( se1=sd1/sqrt(n))  %>%
  mutate( se2=sd2/sqrt(n))  %>%
  mutate( se3=sd3/sqrt(n))  %>%
  mutate( ci1=se1 * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( ci2=se2 * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( ci3=se3 * qt((1-0.05)/2 + .5, n-1))

temp_long = melt(temp,id.vars = "v2_item")
a1 = temp_long[5:16,]
a2 = temp_long[41:52,]
temp_long = cbind(a1,a2)
temp_long = temp_long[,c(1,2,3,6)]
colnames(temp_long) = c("v2_item","variable","m","ci")
figure2 = ggplot(temp_long, aes(x = factor(v2_item,levels=c("coin","card","handkerchief","control")), y = m, fill = factor(variable))) + 
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=m-ci, ymax=m+ci), width=.3,
                position=position_dodge(.9)) +
  scale_fill_discrete(name="Questions",
                      labels=c("Q2", "Q3", "Q4"))+ 
  theme(text = element_text(size = 20)) + 
  xlab("Video 2 items") + ylab("Ratings") + ylim(0,100)
figure2

## Q2-4 for v3 
temp2 = aggregate(mydata, by=list(mydata$v3_item,mydata$PVI), mean)
temp2 <- mydata %>%
  group_by(v3_item,PVI) %>% 
  summarise( 
    n=159,
    mean1=mean(v3_Q2),
    mean2=mean(v3_Q3),
    mean3=mean(v3_Q4),
    sd1=sd(v3_Q2),
    sd2=sd(v3_Q3),
    sd3=sd(v3_Q4),
  ) %>%
  mutate( se1=sd1/sqrt(n))  %>%
  mutate( se2=sd2/sqrt(n))  %>%
  mutate( se3=sd3/sqrt(n))  %>%
  mutate( ci1=se1 * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( ci2=se2 * qt((1-0.05)/2 + .5, n-1))  %>%
  mutate( ci3=se3 * qt((1-0.05)/2 + .5, n-1))
temp2_long = melt(temp2, id.vars = c("v3_item", "PVI"))
temp2_long = cbind(temp2_long[7:24,],temp2_long[61:78,])
temp2_long = temp2_long[,c(1,2,3,4,8)]
colnames(temp2_long) = c("v3_item","PVI","variable","m","ci")

PVI_labs = c("no PVI","PVI")
names(PVI_labs) = c("0","1")

figure3 = ggplot(temp2_long, aes(x = factor(v3_item,levels=c("coin","card","handkerchief")), y = m, group = variable, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.5, position = "dodge") + 
  geom_errorbar(aes(ymin=m-ci, ymax=m+ci), width=.3,
                position=position_dodge(.5)) +
  facet_grid(. ~ PVI, labeller = labeller(PVI = PVI_labs)) + ylim(0,100) + ylab("Ratings") + xlab("Condition") + 
  theme(text = element_text(size = 15)) + 
  scale_fill_discrete(name="Question", labels=c("Q2","Q3","Q4"))
figure3

## responses from PVI 
temp3 = mydata[which(mydata$PVI == 1),]
temp3 = temp3 %>% count(item_reported, condition)
figure4 = ggplot(temp3, aes(x = factor(condition,levels=c("congruent","incongruent","control")), y = n, group=factor(item_reported), fill=factor(item_reported))) + 
  geom_bar(stat = "identity", width = 0.5, position = "dodge") + 
  ylab("Number of Subjects") + xlab("Conditions") + 
  geom_text(aes(label=n), position=position_dodge(0.5), vjust=-0.25,size=5) + 
  theme(text = element_text(size = 20)) + 
  scale_fill_discrete(name="items", labels=c("no specific item", "v1 item", "v2 item", "other items"))
figure4

##############################################TESTS##############################################
#PVI proportion test between conditions
temp4 = mydata %>% count(PVI,condition)
temp4 = temp4[which(temp4$PVI == 1),]
test1_1 = with(temp4, prop.test(x=n[c(1,2)],n=c(159,159),alternative="two.sided"))
test1_2 = with(temp4, prop.test(x=n[c(1,3)],n=c(159,159),alternative="two.sided"))
test1_3 = with(temp4, prop.test(x=n[c(2,3)],n=c(159,159),alternative="two.sided"))
test1_1
test1_2
test1_3

#Q2-4 between PVI and no PVI
test2_1 = with(mydata, t.test(v3_Q2[which(v3_item=="coin"&PVI==1)],v3_Q2[which(v3_item=="coin"&PVI==0)],alternative="greater"))
test2_2 = with(mydata, t.test(v3_Q2[which(v3_item=="card"&PVI==1)],v3_Q2[which(v3_item=="card"&PVI==0)],alternative="greater"))
test2_3 = with(mydata, t.test(v3_Q2[which(v3_item=="handkerchief"&PVI==1)],v3_Q2[which(v3_item=="handkerchief"&PVI==0)],alternative="greater"))

test2_4 = with(mydata, t.test(v3_Q3[which(v3_item=="coin"&PVI==1)],v3_Q3[which(v3_item=="coin"&PVI==0)],alternative="greater"))
test2_5 = with(mydata, t.test(v3_Q3[which(v3_item=="card"&PVI==1)],v3_Q3[which(v3_item=="card"&PVI==0)],alternative="greater"))
test2_6 = with(mydata, t.test(v3_Q3[which(v3_item=="handkerchief"&PVI==1)],v3_Q3[which(v3_item=="handkerchief"&PVI==0)],alternative="greater"))

test2_7 = with(mydata, t.test(v3_Q4[which(v3_item=="coin"&PVI==1)],v3_Q4[which(v3_item=="coin"&PVI==0)],alternative="greater"))
test2_8 = with(mydata, t.test(v3_Q4[which(v3_item=="card"&PVI==1)],v3_Q4[which(v3_item=="card"&PVI==0)],alternative="greater"))
test2_9 = with(mydata, t.test(v3_Q4[which(v3_item=="handkerchief"&PVI==1)],v3_Q4[which(v3_item=="handkerchief"&PVI==0)],alternative="greater"))

test2_1
test2_2
test2_3

test2_4
test2_5
test2_6

test2_7
test2_8
test2_9


#Q2-4 between no specific item and specific item
test3_1 = with(mydata, t.test(v3_Q2[which(item_reported==1|item_reported==2)],v3_Q2[which(item_reported==0)],alternative="greater"))
test3_2 = with(mydata, t.test(v3_Q3[which(item_reported==1|item_reported==2)],v3_Q3[which(item_reported==0)],alternative="greater"))
test3_3 = with(mydata, t.test(v3_Q4[which(item_reported==1|item_reported==2)],v3_Q4[which(item_reported==0)],alternative="greater"))

test3_1
test3_2
test3_3

#all PVI participants. how item reported is affected by condition and v3_item
temp5 = mydata[which(mydata$PVI==1),]
test4_1 = with(temp5,binom.test(x=nrow(temp5[which(condition=="incongruent"&item_reported==2),]),n=nrow(temp5[which(condition=="incongruent"),]),p=0,alternative="two.sided"))
test4_2 = with(temp5,binom.test(x=nrow(temp5[which(condition=="control"&item_reported==2),]),n=nrow(temp5[which(condition=="control"),]),p=0,alternative="two.sided"))
test4_3 = with(temp5,prop.test(x=c(nrow(temp5[which(condition=="incongruent"&item_reported==2),]),nrow(temp5[which(condition=="control"&item_reported==2),])),
                               n=c(nrow(temp5[which(condition=="incongruent"),]),nrow(temp5[which(condition=="control"),]))
                               ,alternative="two.sided"))
test4_1
test4_2
test4_3
