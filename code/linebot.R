library(tidyverse)
#library(olsrr)
#library(MASS)

getwd()
setwd("/Users/liuchenli/Desktop/NCCU/1111_Quality_Control_System/final-linebot")
df = read.csv("survey.csv")

colnames(df)

# delete 3 rows
df = df[-c(52:56),]
df = df[-c(20,24,41,52:56),]
dim(df)
attach(df)

# H1: old < new, alpha = 0.05
dat = data.frame(oldmean=rep(NA,6),newmean=rep(NA,6),diff=rep(NA,6),p.value=rep(NA,6))
for (i in 1:6){
  dat$oldmean[i] = mean(df[,6+i])
  dat$newmean[i] = mean(df[,23+i])
  dat$diff[i] = mean(df[,23+i])-mean(df[,6+i])
  dat$p.value[i] = t.test(x = df[,23+i], y = df[,6+i], alternative = "greater", paired = T)$p.value
}
dat2 = round(dat,4)
dat3 = round(dat,2)

# plot
p = df %>%
  select(7,24) %>%
  ggplot(aes(x=7))+
  geom_histogram(bins=5)+
  geom_text(aes(x=x1, y=x2+0.2, label = paste(obs_i)))+
  theme(plot.background = element_rect(fill='transparent'))
x1x2

ggsave('x1x2.png', x1x2, 
       width = 1200, height = 1200, units = "px",
       bg='transparent')

par(bg = "transparent") #fbf9ea

par(mfrow=c(2,1), mar=c(4,4,4,0), oma=c(0,0,0,0), family="STHeiti")
hist(old_use, main = "針對操作流暢之滿意度",
     col = "#ecbd2a", xlab = "改善前滿意度", breaks = c(0,2,4,6,8,10), labels = T)
hist(new_use, main = "",
     col = "#ecbd2a", xlab = "改善後滿意度", breaks = c(0,2,4,6,8,10), labels = T)

# linear model
colnames(df)
fit = lm(old_satisfaction~good_cate+good_go+good_handy+good_menu+good_pic+good_score+good_tasty+bad_mess+bad_repeat)
t.test(resid(fit))

fit = lm(old_satisfaction~good_cate+good_handy+good_menu+good_score+good_tasty)
t.test(resid(fit))

summary(fit)
anova(fit)

ols_step_both_aic(fit)
ols_step_both_p(fit, pent = 0.35, prem = 0.5)
ols_step_forward_aic(fit)
ols_step_forward_p(fit, pent = 0.2, prem = 0.5)
names(ols_step_backward_aic(fit))
ols_step_backward_aic(fit)$rsq
ols_step_backward_p(fit, pent = 0.2, prem = 0.5)

fit_info = lm(info~good_menu+good_score)
anova(fit_info)

# sex and major
colnames(df)

sex = df %>%
  select(3,7:12,24:29) %>%
  mutate(use = new_use-old_use,
         design = new_design-old_design,
         cate = new_cate-old_cate,
         res = new_res-old_res,
         info = new_info-old_info,
         satisfaction = new_satisfaction-old_satisfaction) %>%
  group_by(性別) %>%
  summarise_all(mean)
sex1 = cbind(as.data.frame(sex)[,1],round(as.data.frame(sex)[,2:19],2))
write.csv(sex1,"sex.csv")

df_temp = df %>% filter(性別=="女")
dat= data.frame(oldmean=rep(NA,6),newmean=rep(NA,6),diff=rep(NA,6),p.value=rep(NA,6))
for (i in 1:6){
  dat$oldmean[i] = mean(df_temp[,6+i])
  dat$newmean[i] = mean(df_temp[,23+i])
  dat$diff[i] = mean(df_temp[,23+i])-mean(df_temp[,6+i])
  dat$p.value[i] = t.test(x = df_temp[,23+i], y = df_temp[,6+i], alternative = "greater", paired = T)$p.value
}
write.csv(dat,"female.csv")

major = df %>%
  select(4,7:12,24:29) %>%
  mutate(use = new_use-old_use,
         design = new_design-old_design,
         cate = new_cate-old_cate,
         res = new_res-old_res,
         info = new_info-old_info,
         satisfaction = new_satisfaction-old_satisfaction) %>%
  group_by(就讀院所) %>%
  summarise_all(mean)
major1 = cbind(as.data.frame(major)[,1],round(as.data.frame(major)[,2:19],2))
write.csv(major1,"major.csv")

df_temp = df %>% filter(就讀院所=="社會科學學院")
dat= data.frame(oldmean=rep(NA,6),newmean=rep(NA,6),diff=rep(NA,6),p.value=rep(NA,6))
for (i in 1:6){
  dat$oldmean[i] = mean(df_temp[,6+i])
  dat$newmean[i] = mean(df_temp[,23+i])
  dat$diff[i] = mean(df_temp[,23+i])-mean(df_temp[,6+i])
  dat$p.value[i] = t.test(x = df_temp[,23+i], y = df_temp[,6+i], alternative = "greater", paired = T)$p.value
}
write.csv(dat,"cs.csv")
