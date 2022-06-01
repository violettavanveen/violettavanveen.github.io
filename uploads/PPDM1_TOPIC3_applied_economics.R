library(tidyverse)
library(sf)
library(foreign)
library(RStata)
library(haven)
library(stargazer)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lmtest)
library(sandwich)
library(modelsummary)


rm(list=ls())

d <- read.dta("C:/Users/hp/Desktop/Parigi/Metrics_Project/data_restud.dta")


######################### DESCRIPTIVE STATISTICS ########################


summary(d$retard) # 27% of students repeated a grade

#how many repeaters in each class?
repeaters.per.class <- aggregate(d$retard, list(d$id_colcla), FUN=sum) 

summary(repeaters.per.class)
hist(repeaters.per.class$x, breaks = 24)

# standardize all scores

d.std <- d %>% mutate_at(c("note_FRA_t1", "note_FRA_t2", "note_FRA_t3", "note_MAT_t1", "note_MAT_t2","note_MAT_t3",
                           "note_FRA_t6", "note_MAT_t6", "evfra3zscore", "evmat3zscore", "evfra1zscore", "evmat1zscore"), ~(scale(.) %>% as.vector))


# create subset of variables relevant for summary statistics

d.sub <- subset(d.std, select = c('note_FRA_t1', 'note_FRA_t2', 'note_FRA_t3', 'note_MAT_t1', 'note_MAT_t2','note_MAT_t3',
                                   'note_FRA_t6', 'note_MAT_t6', 'evfra1zscore', 'evmat1zscore', 'evfra3zscore', 'evmat3zscore', 
                                   'sanc_xtavc1', 'sanc_xtavc2',  'sanc_xtavc3', 'av_comp_t6', 'emploi', 'whitecollar', "retard",
                                  "redoublement6eme", "fille", "biparental", "boursier", "aine", "note_mean_pond_t3",
                                  "mention_t3", "dj_abs_nj3"))

tapply(d.sub$note_FRA_t1, d.sub$retard, summary)

# Summary by group using purrr
d.sub %>%                     
  split(.$retard) %>%
  map(summary)


# to have the latex code of the summary statistics:
datasummary_balance(~retard, d.sub, output = "latex_tabular", 
                    fmt = 2, notes = "bla", title = "Descriptive statistics 
                    for late and not students", dinm = TRUE)


###################### DENSITY PLOTS #####################
# Third term

# non blind fra
ggplot(d.sub, aes(color=as.factor(retard), x=note_FRA_t3)) +
  geom_density(alpha=.25) + labs(color = "Repetition") + 
  xlab("Non-Blind French scores") + theme_light()

#blind fra
ggplot(d.sub, aes(color=as.factor(retard), x=evfra3zscore)) +
  geom_density(alpha=.25) + labs(color = "Repetition") + 
  xlab("Blind French scores") + theme_light()


# nb maths
ggplot(d.sub, aes(color=as.factor(retard), x=note_MAT_t3)) +
  geom_density(alpha=.25) + labs(color = "Repetition") + 
  xlab("Non-Blind Maths scores") + theme_light()

# b maths
ggplot(d.sub, aes(color=as.factor(retard), x=evmat3zscore)) +
  geom_density(alpha=.25) + labs(color = "Repetition") + 
  xlab("Blind Maths scores") + theme_light()



######################### DIFFERENCE IN DIFFERENCE ###########################

# create dataset with each row 1 score (blind or non blind)
scores <- numeric(nrow(d) * 4)
subject <- numeric(nrow(d) * 4)
girls <- numeric(nrow(d) * 4)
nb <- numeric(nrow(d) * 4)
repetition <- numeric(nrow(d) * 4)
sanction <- numeric(nrow(d) * 4)
employed <- numeric(nrow(d) * 4)
white.collar <- numeric(nrow(d) * 4)
biparental <- numeric(nrow(d) * 4)
mention <- numeric(nrow(d) * 4)
goodconduct <- numeric(nrow(d) * 4)
scholarship <- numeric(nrow(d) * 4)
teacher_eval <- numeric(nrow(d) * 4)
id_classe <- numeric(nrow(d) * 4)
id_college <- numeric(nrow(d) * 4)

d.std$mention_t1 <- ifelse(d.std$mention_t1 ==1 | d.std$mention_t1 ==2 | d.std$mention_t1 ==3, 1, 0)
d.std$mention_t2 <- ifelse(d.std$mention_t2 ==1 | d.std$mention_t2 ==2 | d.std$mention_t2 ==3, 1, 0)
d.std$mention_t3 <- ifelse(d.std$mention_t3 ==1 | d.std$mention_t3 ==2 | d.std$mention_t3 ==3, 1, 0)



for (j in 1:nrow(d.std)) {
  i <- d.std[j,]
  k <- j * 4 - 3
  scores[k] <- i$note_FRA_t3
  subject[k] <- "French"
  nb[k] <-  1 
  girls[k] <-  i$fille 
  repetition[k] <-  i$retard 
  sanction[k] <-  i$sanc_xtavc1 | i$sanc_xtavc2 | i$sanc_xtavc3
  employed[k] <-  i$emploi 
  white.collar[k] <-  i$whitecollar  
  biparental[k] <- i$biparental
  mention[k] <-  i$mention_t1 | i$mention_t2 | i$mention_t3
  goodconduct[k] <- i$note_VIEmax_t1 | i$note_VIEmax_t2 | i$note_VIEmax_t3
  scholarship[k] <-  i$boursier
  teacher_eval[k] <- sum(i$qprof_conduct, i$qprof_works, i$qprof_progress, i$qprof_dialogue, i$qprof_homeinv, na.rm = TRUE)
  id_college[k] <- i$id_college
  id_classe[k] <- i$id_colcla
 
  
  scores[k+1] <-  i$note_MAT_t3 
  subject[k+1] <- "Math"
  nb[k+1] <-  1 
  girls[k+1] <-  i$fille 
  repetition[k+1] <-  i$retard 
  sanction[k+1] <-  i$sanc_xtavc1 | i$sanc_xtavc2 | i$sanc_xtavc3 
  employed[k+1] <-  i$emploi 
  white.collar[k+1] <-  i$whitecollar 
  biparental[k+1] <- i$biparental
  mention[k+1] <-  i$mention_t1 | i$mention_t2 | i$mention_t3
  goodconduct[k+1] <- i$note_VIEmax_t1 | i$note_VIEmax_t2 | i$note_VIEmax_t3
  scholarship[k+1] <-  i$boursier 
  teacher_eval[k+1] <- sum(i$qprof_conduct, i$qprof_works, i$qprof_progress, i$qprof_dialogue, i$qprof_homeinv, na.rm = TRUE)
  id_college[k+1] <- i$id_college
  id_classe[k+1] <- i$id_colcla

  
  scores[k+2] <-  i$evfra3zscore 
  subject[k+2] <- "French"
  nb[k+2] <-  0 
  girls[k+2] <-  i$fille 
  repetition[k+2] <-  i$retard 
  sanction[k+2] <-  i$sanc_xtavc1 | i$sanc_xtavc2 | i$sanc_xtavc3 
  employed[k+2] <-  i$emploi 
  white.collar[k+2] <-  i$whitecollar 
  biparental[k+2] <- i$biparental
  mention[k+2] <-  i$mention_t1 | i$mention_t2 | i$mention_t3
  goodconduct[k+2] <- i$note_VIEmax_t1 | i$note_VIEmax_t2 | i$note_VIEmax_t3
  scholarship[k+2] <-  i$boursier
  teacher_eval[k+2] <- sum(i$qprof_conduct, i$qprof_works, i$qprof_progress, i$qprof_dialogue, i$qprof_homeinv, na.rm = TRUE)
  id_college[k+2] <- i$id_college
  id_classe[k+2] <- i$id_colcla

  
  scores[k+3] <-  i$evmat3zscore 
  subject[k+3] <- "Math"
  nb[k+3] <-  0 
  girls[k+3] <-  i$fille 
  repetition[k+3] <-  i$retard 
  sanction[k+3] <- i$sanc_xtavc1 | i$sanc_xtavc2 | i$sanc_xtavc3
  employed[k+3] <-  i$emploi 
  white.collar[k+3] <-  i$whitecollar 
  biparental[k+3] <- i$biparental
  mention[k+3] <-  i$mention_t1 | i$mention_t2 | i$mention_t3
  goodconduct[k+3] <- i$note_VIEmax_t1 | i$note_VIEmax_t2 | i$note_VIEmax_t3
  scholarship[k+3] <-  i$boursier 
  teacher_eval[k+3] <- sum(i$qprof_conduct, i$qprof_works, i$qprof_progress, i$qprof_dialogue, i$qprof_homeinv, na.rm = TRUE)
  id_college[k+3] <- i$id_college
  id_classe[k+3] <- i$id_colcla
} 

df.clean <- as.data.frame(cbind(id_college, id_classe, scores, subject, girls, nb, repetition, sanction, employed, white.collar, biparental,
                                mention, goodconduct, scholarship, teacher_eval))
sum(is.na(df.clean))

### actual regressions:

# only on math

math <- lm(scores ~ repetition*nb + repetition + nb + factor(id_classe) -1, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])
summary(math)


cl_vcov_math <- vcovCL(math, cluster = ~id_college)

math.coeffs_cl <- coeftest(math, vcov = cl_vcov_math, cluster = ~id_college)
math.coeffs_cl # bias in math (repetition1:nb1 ) no stat significant 


# i exclude the outlier class in which there are 
# 24 repeaters and the other outlier of 0 repeaters

no.outlier <- subset(df.clean, id_classe != c("93648", "94453"))

math.no.out <- lm(scores ~ repetition*nb + repetition + nb + factor(id_classe) -1, data =  no.outlier[no.outlier$subject == "Math", colnames(no.outlier)])
summary(math.no.out)

cl_vcov_math_no_out <- vcovCL(math.no.out, cluster = ~id_college)

math.no.out.coeffs_cl <- coeftest(math.no.out, vcov = cl_vcov_math_no_out, cluster = ~id_college)
math.no.out.coeffs_cl 
# if get rid of a single class (the class with 24 repeaters, an outlier) 
# the bias in math (repetition1:nb1) is still not stat sign

# now in french

french <- lm(scores ~ repetition*nb + repetition + nb + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french)

cl_vcov_french <- vcovCL(french, cluster = ~id_college)

french.coeffs_cl <- coeftest(french, vcov = cl_vcov_french, cluster = ~id_college)
french.coeffs_cl 


# no outliers

french_no_out <- lm(scores ~ repetition*nb + repetition + nb + factor(id_classe) -1, data= no.outlier[no.outlier$subject == "French", colnames(no.outlier)])
summary(french_no_out)

cl_vcov_french_no_out <- vcovCL(french_no_out, cluster = ~id_college)

french.coeffs_cl_no_out <- coeftest(french_no_out, vcov = cl_vcov_french_no_out, cluster = ~id_college)
french.coeffs_cl_no_out # this bias (repetition1:nb1) is statistically significant at 5% also if exclude outlier



#################### DDD ###########################

# add control for girls

# how many girls and boys in repeaters?

summary(d$fille[d$retard == 1]) # among repeater, 41% are girls, 59% boys (relatively balanced)

# math 

math_girls <- lm(scores ~ repetition*nb + repetition + nb + girls + girls*nb + girls*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])
summary(math_girls)

cl_vcov_math_girls <- vcovCL(math_girls, cluster = ~id_college)

math.coeffs_cl_girls <- coeftest(math_girls, vcov = cl_vcov_math_girls, cluster = ~id_college)
math.coeffs_cl_girls #clearly still no stat sign at all

# french

french_girls <- lm(scores ~ repetition*nb + repetition + nb + girls + girls*nb + girls*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_girls)

cl_vcov_french_girls <- vcovCL(french_girls, cluster = ~id_college)

french.coeffs_cl_girls <- coeftest(french_girls, vcov = cl_vcov_french_girls, cluster = ~id_college)
french.coeffs_cl_girls # when i control for the sex, bias for repeaters is no stat sign


#### what happens to girls instead?

df.clean$boys <- ifelse(df.clean$girls == 1, 0, 1)
options(max.print = 10000)
french_boys <- lm(scores ~ repetition*nb + repetition + nb + boys + boys*nb + boys*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_boys)


# now i control for the punishment dummy

# i do it only on french since clearly in math will be non significant always

french_punishm <- lm(scores ~ repetition*nb + repetition + nb + sanction + sanction*nb + sanction*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_punishm)

cl_vcov_french_punishm <- vcovCL(french_punishm, cluster = ~id_college)

french.coeffs_cl_punishm <- coeftest(french_punishm, vcov = cl_vcov_french_punishm, cluster = ~id_college)
french.coeffs_cl_punishm # when i control for punishment, again bias for repeaters is no stat sign

df.clean$no_punish <- ifelse(df.clean$sanction ==1,0,1)

french_no_punishm <- lm(scores ~ repetition*nb + repetition + nb + no_punish + no_punish*nb + no_punish*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_no_punishm)

# control for teacher evaluation

french_teacher_eval <- lm(scores ~ repetition*nb + repetition + nb + teacher_eval + teacher_eval*nb + teacher_eval*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_teacher_eval)

cl_vcov_french_teacher_eval <- vcovCL(french_teacher_eval, cluster = ~id_college)

french.coeffs_cl_teacher_eval <- coeftest(french_teacher_eval, vcov = cl_vcov_french_teacher_eval, cluster = ~id_college)
french.coeffs_cl_teacher_eval  

tail(french.coeffs_cl_teacher_eval, n= 16)# (i use this because i cant see the last 16 rows) when i control for teacher evaluation, bias repetition1:nb1 is stat sign at 10% level!! 

# now i include both controls for teacher evaluation and sex

french_teacher_eval_girls <- lm(scores ~ repetition*nb + repetition + nb + teacher_eval + teacher_eval*nb + teacher_eval*nb*repetition + girls + girls*nb + girls*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_teacher_eval_girls)

cl_vcov_french_teacher_eval_girls <- vcovCL(french_teacher_eval_girls, cluster = ~id_college)

french.coeffs_cl_teacher_eval_girls <- coeftest(french_teacher_eval_girls, vcov = cl_vcov_french_teacher_eval_girls, cluster = ~id_college)
french.coeffs_cl_teacher_eval_girls  
tail(french.coeffs_cl_teacher_eval, n= 20) # still stat sign at 10% level


# controls for teacher evaluation, punishment and sex

french_teacher_eval_punishm_girls <- lm(scores ~ repetition*nb + repetition + nb + teacher_eval + teacher_eval*nb + teacher_eval*nb*repetition + sanction + sanction*nb + sanction*nb*repetition + girls + girls*nb + girls*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_teacher_eval_punishm_girls)

cl_vcov_french_teacher_eval_punishm_girls <- vcovCL(french_teacher_eval_punishm_girls, cluster = ~id_college)

french.coeffs_cl_teacher_eval_punishm_girls <- coeftest(french_teacher_eval_punishm_girls, vcov = cl_vcov_french_teacher_eval_punishm_girls, cluster = ~id_college)
french.coeffs_cl_teacher_eval_punishm_girls  # bias not stat sign

french_teacher_eval_no_punishm_girls <- lm(scores ~ repetition*nb + repetition + nb + teacher_eval + 
                                             teacher_eval*nb + teacher_eval*nb*repetition + no_punish
                                           + no_punish*nb + no_punish*nb*repetition + girls + girls*nb + girls*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])

summary(french_teacher_eval_no_punishm_girls)

# control for biparental

# french
french_biparental <- lm(scores ~ repetition*nb + repetition + nb + biparental +
                                          biparental*nb + biparental*repetition + biparental*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_biparental)
french_biparental.cl <- coeftest(french_biparental, vcov =  vcovCL(french_biparental, cluster = ~id_college), cluster = ~id_college)


df.clean$monoparental <- ifelse(df.clean$biparental == 1, 0, 1)

french_monoparental <- lm(scores ~ repetition*nb + repetition + nb + monoparental +
                          monoparental*nb + monoparental*repetition + monoparental*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_monoparental)
french_monoparental.cl <- coeftest(french_monoparental, vcov =  vcovCL(french_monoparental, cluster = ~id_college), cluster = ~id_college)

#stargazer(french_biparental, french_monoparental)

summary(lm(scores ~   nb + monoparental +
     monoparental*nb + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)]))
summary(french_monoparental)

# maths
maths_monoparental <- lm(scores ~ repetition*nb + repetition + nb + monoparental +
                            monoparental*nb + monoparental*repetition + monoparental*nb*repetition + factor(id_classe) -1,
                          data= df.clean[df.clean$subject == "Math", colnames(df.clean)])
summary(maths_monoparental)
maths_monoparental.cl <- coeftest(french_monoparental, vcov =  vcovCL(achieve, cluster = ~id_college), cluster = ~id_college)
maths_monoparental.cl

# control for whitecollar

# french
french_white.collar <- lm(scores ~ repetition*nb + repetition + nb + white.collar +
                          white.collar*nb + white.collar*repetition + white.collar*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_white.collar)
french_white.collar.cl <- coeftest(french_white.collar, vcov =  vcovCL(french_white.collar, cluster = ~id_college), cluster = ~id_college)


# maths 
math_white.collar <- lm(scores ~ repetition*nb + repetition + nb + white.collar +
                            white.collar*nb + white.collar*repetition + white.collar*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])
summary(math_white.collar)


# control for employed
french_employed <- lm(scores ~ repetition*nb + repetition + nb + employed +
                            employed*nb + employed*repetition + employed*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_employed) # no effect

df.clean$unemployed <- ifelse(df.clean$employed == 1, 0, 1)

# same for unemployed
french_unemployed <- lm(scores ~ repetition*nb + repetition + nb + unemployed +
                        unemployed*nb + unemployed*repetition + unemployed*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_unemployed) # yes effect
french_unemployed.cl <- coeftest(french_unemployed, vcov =  vcovCL(french_unemployed, cluster = ~id_college), cluster = ~id_college)

#stargazer(french_white.collar.cl, french_unemployed.cl)

# maths
math_unemployed <- lm(scores ~ repetition*nb + repetition + nb + unemployed +
                          unemployed*nb + unemployed*repetition + unemployed*nb*repetition + factor(id_classe) -1,
                      data= df.clean[df.clean$subject == "Math", colnames(df.clean)])
summary(math_unemployed) 

# control for mention 

french_mention <- lm(scores ~ repetition*nb + repetition + nb + mention +
                          mention*nb + mention*repetition + mention*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_mention) 

french_mention.cl <- coeftest(french_mention, vcov =  vcovCL(french_mention, cluster = ~id_college), cluster = ~id_college)
french_mention.cl


df.clean$no_mention <- ifelse(df.clean$mention ==1, 0,1)

french_no_mention <- lm(scores ~ repetition*nb + repetition + nb + no_mention +
                          no_mention*nb + no_mention*repetition + no_mention*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_no_mention) 



# control for good grade

french_goodconduct <- lm(scores ~ repetition*nb + repetition + nb + goodconduct +
                       goodconduct*nb + goodconduct*repetition + goodconduct*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_goodconduct) 

df.clean$no_goodconduct <- ifelse(df.clean$goodconduct ==1, 0,1)

french_no_goodconduct <- lm(scores ~ repetition*nb + repetition + nb + no_goodconduct +
                          no_goodconduct*nb + no_goodconduct*repetition + no_goodconduct*nb*repetition + factor(id_classe) -1, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
summary(french_no_goodconduct) 



# quantile regression

library(quantreg)

# maths

df.clean$repetition <- as.numeric(df.clean$repetition)
df.clean$nb <- as.numeric(df.clean$nb)
df.clean$scores <- as.numeric(df.clean$scores)

rq_05 <- rq(scores ~ repetition*nb + repetition + nb , 
         tau=.05, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])
rq_25 <- rq(scores ~ repetition*nb + repetition + nb, 
            tau=.25, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])

rq_50<- rq(scores ~ repetition*nb + repetition + nb, 
           tau=.5, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])

rq_75<- rq(scores ~ repetition*nb + repetition + nb, 
           tau=.75, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])

rq_95<- rq(scores ~ repetition*nb + repetition + nb, 
           tau=.95, data= df.clean[df.clean$subject == "Math", colnames(df.clean)])

summary(rq_95)

#stargazer(rq_05, rq_25, rq_50, rq_75, rq_95)

# french

rq_05 <- rq(scores ~ repetition*nb + repetition + nb, 
            tau=.05, data= df.clean[df.clean$subject == "French", colnames(df.clean)])
rq_25 <- rq(scores ~ repetition*nb + repetition + nb, 
            tau=.25, data= df.clean[df.clean$subject == "French", colnames(df.clean)])

rq_50<- rq(scores ~ repetition*nb + repetition + nb, 
           tau=.5, data= df.clean[df.clean$subject == "French", colnames(df.clean)])

rq_75<- rq(scores ~ repetition*nb + repetition + nb, 
           tau=.75, data= df.clean[df.clean$subject == "French", colnames(df.clean)])

rq_95<- rq(scores ~ repetition*nb + repetition + nb, 
           tau=.95, data= df.clean[df.clean$subject == "French", colnames(df.clean)])

#stargazer(rq_05, rq_25, rq_50, rq_75, rq_95)
summary(rq_25)


######################### EFFECT OF BIAS ON PROGRESS #########################

# B2i - B1i --> progress
# let's take blind in 3rd term and lind in 1st term

################## french
d.std$progress.fra <- d.std$evfra3zscore - d.std$evfra1zscore 
sum(is.na(d.std$progress.fra))
# 1324 NA: we dont have one or the other or both standardized scores: 1054 we 
# dont have score in t1, 619 we dont have score in t3, 349 neither, so 1054 +619 -349 = 1324

# NB1i - B1i --> bias

d.std$bias.fra <- d.std$note_FRA_t1 - d.std$evfra1zscore
sum(is.na(d.std$bias.fra))
# 843 NA. for 619 no evfra1zscore, 
# for 514 no note_FRA_t1, for  290 neither. 619 + 514 -290 = 843


# retard bias: (NB1retard - B1iretard) - (NB1not - B1inot)
# defined as the class average difference between the non-blind and the blind
# scores for retard, minus this same difference for not

d.std$bias.repeaters <- ifelse(d.std$retard == 1, d.std$bias.fra, NA) 
sum(is.na(d.std$bias.repeaters))
# 355 NA

d.std$bias.not.repeaters <- 
  ifelse(d.std$retard == 0, d.std$bias.fra, NA) 
 sum(is.na(d.std$bias.not.repeaters))# 488 NA


mean.bias.repeaters.class <- aggregate(d.std$bias.repeaters, 
                                       list(d.std$id_colcla), FUN=mean, na.rm = T)  


mean.bias.not.repeaters.class <- aggregate(d.std$bias.not.repeaters, list(d.std$id_colcla), FUN=mean, na.rm = T)
sum(is.na(mean.bias.not.repeaters.class)) # 1 NA

repeaters.bias <- mean.bias.repeaters.class$x - mean.bias.not.repeaters.class$x
sum(is.na(repeaters.bias)) # 29 NA

repeaters.bias <- cbind.data.frame(repeaters.bias, mean.bias.not.repeaters.class$Group.1)
sum(is.na(repeaters.bias))
# 29 NA

names(repeaters.bias)[names(repeaters.bias) == 'mean.bias.not.repeaters.class$Group.1'] <- 'id_colcla'
names(repeaters.bias)
# repeaters progress (B2R - B1R) - (B2NR -B1NR)
# the difference between repeaters blind score
# at the end of grade 9 and this blind score at the beginning of grade 6,
# minus the same difference for non repeaters.

#  now only french

d.std$progress.repeaters <- ifelse(d.std$retard == 1, d.std$progress.fra, NA) 
sum(is.na(d.std$progress.repeaters))

d.std$progress.not.repeaters <- 
  ifelse(d.std$retard ==0, d.std$progress.fra, NA) 
sum(is.na(d.std$progress.not.repeaters)) #785 na

mean.progress.repeaters.class <- aggregate(d.std$progress.repeaters, list(d.std$id_colcla), FUN=mean, na.rm = T)  
sum(is.na(mean.progress.repeaters.class)) # 6 na

mean.progress.not.repeaters.class <- aggregate(d.std$progress.not.repeaters, list(d.std$id_colcla), FUN=mean, na.rm = T) 
sum(is.na(mean.progress.not.repeaters.class)) # 0 na!


repeaters.relative.progress <- mean.progress.repeaters.class$x - mean.progress.not.repeaters.class$x 
sum(is.na(repeaters.relative.progress)) # 6 na

repeaters.relative.progress <- cbind.data.frame(repeaters.relative.progress, mean.progress.repeaters.class$Group.1) # 0 na
sum(is.na(repeaters.relative.progress)) # 0

bias.progress.fra <- cbind.data.frame(repeaters.relative.progress, repeaters.bias)
names(bias.progress.fra)[names(bias.progress.fra) == "mean.progress.repeaters.class$Group.1"] <- "V2"
bias.progress.fra <- subset(bias.progress.fra, select = - V2)
names(bias.progress.fra)

##### scatterplot of the bias against progress
plot(bias.progress.fra$repeaters.bias, bias.progress.fra$repeaters.relative.progress
     , 
     xlab = "French teacher bias against repeaters", ylab = "Relative progress of repeaters in French")

#ggplot(bias.all, aes(x=repeaters.bias.fra)) +  geom_density() +
#  xlab("French teachers' bias") + ggplot(bias.all, aes(x=repeaters.bias.mat)) 


plot(density(bias.all$repeaters.bias.mat), col = "red")
lines(density(bias.all$repeaters.bias.fra))

# achievement gap B1R -B1NR

repeaters <- subset(d.std, d.std$retard==1)

non.repeaters <- subset(d.std, d.std$retard==0)

sum(is.na(d.std$id_colcla)) # 0
sum(is.na(d.std$evfra1zscore)) #619



achieve.non.repeaters <- aggregate(non.repeaters$evfra1zscore, list(non.repeaters$id_colcla), FUN=mean, na.rm = TRUE)
sum(is.na(achieve.non.repeaters)) # 12 na

achieve.repeaters <- aggregate(repeaters$evfra1zscore, list(repeaters$id_colcla), FUN=mean, na.rm = TRUE)
sum(is.na(achieve.repeaters)) # 16 na's

achieve.all <- merge(achieve.non.repeaters, achieve.repeaters, by = "Group.1")
sum(is.na(achieve.all)) # 25 na

colnames(achieve.all) <- c("id_colcla", "mean_t1_non_repeaters", "mean_t1_repeaters")

achieve.all$achievement_gap_fra <- achieve.all$mean_t1_repeaters - achieve.all$mean_t1_non_repeaters
sum(is.na(achieve.all$achievement_gap_fra)) # 13 na
### TABLE 5

# merge together all
bias.progress.fra <-  merge(achieve.all, bias.progress.fra, by = "id_colcla" )
sum(is.na(bias.progress.fra$mean_t1_non_repeaters)) #12
sum(is.na(bias.progress.fra$mean_t1_repeaters)) #13
sum(is.na(bias.progress.fra$achievement_gap_fra)) # 13
sum(is.na(bias.progress.fra$repeaters.relative.progress)) #0
sum(is.na(bias.progress.fra$repeaters.bias)) #0


lm.progress.fra <- lm(repeaters.relative.progress ~ repeaters.bias + 
                        achievement_gap_fra, data = bias.progress.fra)

summary(lm.progress.fra, diagnostics = TRUE)

############################ repeat everything for math

# B2i - B1i --> progress
d.std$progress.mat <- d.std$evmat3zscore - d.std$evmat1zscore
sum(is.na(d.std$progress.fra)) #1324


# NB1i - B1i --> bias
d.std$bias.mat <- d.std$note_MAT_t1 - d.std$evmat1zscore
sum(is.na(d.std$bias.mat)) #801


# retard bias: (NB1retard - B1iretard) - (NB1not - B1inot)

d.std$bias.repeaters.mat <- ifelse(d.std$retard == 1, d.std$bias.mat, NA)

d.std$bias.not.repeaters.mat <- 
  ifelse(d.std$retard == 0, d.std$progress.mat, NA)

mean.bias.repeaters.class.mat <- aggregate(d.std$bias.repeaters.mat, 
                                           list(d.std$id_colcla), FUN=mean, na.rm = T) 

mean.bias.not.repeaters.class.mat <- aggregate(d.std$bias.not.repeaters.mat
                                               , list(d.std$id_colcla), FUN=mean, na.rm = T) 

repeaters.bias.mat <- mean.bias.repeaters.class.mat$x - mean.bias.not.repeaters.class.mat$x 
repeaters.bias.mat <- cbind.data.frame(repeaters.bias.mat, mean.bias.not.repeaters.class.mat$Group.1)
names(repeaters.bias.mat)[names(repeaters.bias.mat) == 'mean.bias.not.repeaters.class.mat$Group.1'] <- 'id_colcla'

# repeaters progress (B2R - B1R) - (B2NR -B1NR)

d.std$progress.repeaters.mat <- ifelse(d.std$retard == 1, d.std$progress.mat, NA)

d.std$progress.not.repeaters.mat <- 
  ifelse(d.std$retard == 0, d.std$progress.mat, NA)

mean.progress.repeaters.class.mat <- aggregate(d.std$progress.repeaters.mat, list(d.std$id_colcla), FUN=mean, na.rm = T) 

mean.progress.not.repeaters.class.mat <- aggregate(d.std$progress.not.repeaters.mat, list(d.std$id_colcla), FUN=mean, na.rm = T) 

repeaters.relative.progress.mat <- mean.progress.repeaters.class.mat$x - mean.progress.not.repeaters.class.mat$x
repeaters.relative.progress.mat <- cbind.data.frame(repeaters.relative.progress.mat, mean.progress.repeaters.class.mat$Group.1)

bias.progress.mat <- cbind.data.frame(repeaters.relative.progress.mat, repeaters.bias.mat)
names(bias.progress.mat)[names(bias.progress.mat)== "mean.progress.repeaters.class.mat$Group.1"] <- "V2"
bias.progress.mat <- subset(bias.progress.mat, select = - V2)

### plot of bias against progress

plot(bias.progress.mat$repeaters.bias.mat, bias.progress.mat$repeaters.relative.progress.mat, 
     xlab = "Math teacher bias against repeaters", ylab = "Relative progress of repeaters in Math")


# achievement gap B1R -B1NR
achieve.non.repeaters.mat <- aggregate(non.repeaters$evmat1zscore, list(non.repeaters$id_colcla), FUN=mean, na.rm = TRUE)

achieve.repeaters.mat <- aggregate(repeaters$evmat1zscore, list(repeaters$id_colcla), FUN=mean, na.rm = TRUE)

achieve.all.mat <- merge(achieve.non.repeaters.mat, achieve.repeaters.mat, by = "Group.1")

colnames(achieve.all.mat) <- c("id_colcla", "mean_t1_non_repeaters", "mean_t1_repeaters")

achieve.all.mat$achievement_gap_mat <- achieve.all.mat$mean_t1_repeaters - achieve.all.mat$mean_t1_non_repeaters

### actually run regressions

# merge together all
bias.progress.mat <-  merge(achieve.all.mat, bias.progress.mat, by = "id_colcla" )


lm.progress.mat <- lm(repeaters.relative.progress.mat ~ repeaters.bias.mat + 
                        achievement_gap_mat, data = bias.progress.mat)

summary(lm.progress.mat)
plot(lm.progress.mat)

## see if spillover of teacher bias

bias.all <- merge(bias.progress.fra, bias.progress.mat, by = "id_colcla")
bias.all <- subset(bias.all, select = -c(mean_t1_non_repeaters.x, mean_t1_repeaters.x, mean_t1_non_repeaters.y, mean_t1_repeaters.y))


# effect of bias in french on progress in math
lm.progress.bias.fra.on.math <- lm(repeaters.relative.progress.mat ~ repeaters.bias.mat + repeaters.bias +
                                     achievement_gap_mat, data = bias.all)

summary(lm.progress.bias.fra.on.math) # yes spillovers


# effect of bias in math on progress in french
lm.progress.bias.math.on.french <- lm(repeaters.relative.progress ~ repeaters.bias.mat + repeaters.bias +
                                        achievement_gap_fra, data = bias.all)

summary(lm.progress.bias.math.on.french) # yes spillovers again



stargazer(lm.progress.fra, lm.progress.bias.math.on.french, lm.progress.mat, lm.progress.bias.fra.on.math)

######### bootstrap errors
library('boot')

bias.all %>%
  bootstrap(1000) -> bootCrime

# regression: french

bootCrime %>% 
  mutate(lm = map(strap, ~lm(repeaters.relative.progress ~ as.numeric(repeaters.bias) + 
                               achievement_gap_fra, 
                             data = .)),
         tidy = map(lm, broom::tidy)) -> bootCrime


bootCrime %>%
  pull(tidy) %>%
  map2_df(., # map to return a data frame
          seq(1, 1000), # make sure to get this seq right. We did this 1000 times.
          ~mutate(.x, resample = .y)) -> tidybootCrime

tidybootCrime %>%
  # group by term, naturally
  group_by(term) %>%
  # This is the actual bootstrapped standard error you want
  summarize(bse = sd(estimate)) -> bseM1

bseM1

# regression: math
bootCrime %>% 
  mutate(lm = map(strap, ~lm(repeaters.relative.progress.mat ~ as.numeric(repeaters.bias.mat) + 
                               achievement_gap_mat, 
                             data = .)),
         tidy = map(lm, broom::tidy)) -> bootCrime


bootCrime %>%
  pull(tidy) %>%
  map2_df(., # map to return a data frame
          seq(1, 1000), # make sure to get this seq right. We did this 1000 times.
          ~mutate(.x, resample = .y)) -> tidybootCrime

tidybootCrime %>%
  # group by term, naturally
  group_by(term) %>%
  # This is the actual bootstrapped standard error you want
  summarize(bse = sd(estimate)) -> bseM1

bseM1

# progress in french including maths
bootCrime %>% 
  mutate(lm = map(strap, ~lm(repeaters.relative.progress ~ as.numeric(repeaters.bias.mat) + as.numeric(repeaters.bias) +
                               achievement_gap_fra, 
                             data = .)),
         tidy = map(lm, broom::tidy)) -> bootCrime


bootCrime %>%
  pull(tidy) %>%
  map2_df(., # map to return a data frame
          seq(1, 1000), # make sure to get this seq right. We did this 1000 times.
          ~mutate(.x, resample = .y)) -> tidybootCrime

tidybootCrime %>%
  # group by term, naturally
  group_by(term) %>%
  # This is the actual bootstrapped standard error you want
  summarize(bse = sd(estimate)) -> bseM1

bseM1

# progress in maths including french
bootCrime %>% 
  mutate(lm = map(strap, ~lm(repeaters.relative.progress.mat ~ as.numeric(repeaters.bias.mat) + as.numeric(repeaters.bias) +
                               achievement_gap_mat, 
                             data = .)),
         tidy = map(lm, broom::tidy)) -> bootCrime


bootCrime %>%
  pull(tidy) %>%
  map2_df(., # map to return a data frame
          seq(1, 1000), # make sure to get this seq right. We did this 1000 times.
          ~mutate(.x, resample = .y)) -> tidybootCrime

tidybootCrime %>%
  group_by(term) %>%
  summarize(bse = sd(estimate)) -> bseM1

bseM1

############################# ROBUSTNESS CHECKS ####################


## replication with first term

scores <- numeric(nrow(d) * 4)
subject <- numeric(nrow(d) * 4)
nb <- numeric(nrow(d) * 4)
repetition <- numeric(nrow(d) * 4)
id_classe <- numeric(nrow(d) * 4)
id_college <- numeric(nrow(d) * 4)

for (j in 1:nrow(d.std)) {
  i <- d.std[j,]
  k <- j * 4 - 3
  scores[k] <- i$note_FRA_t1
  subject[k] <- "French"
  nb[k] <-  1 
  repetition[k] <-  i$retard 
  id_college[k] <- i$id_college
  id_classe[k] <- i$id_colcla
  
  
  scores[k+1] <-  i$note_MAT_t1
  subject[k+1] <- "Math"
  nb[k+1] <-  1 
  repetition[k+1] <-  i$retard 
  id_college[k+1] <- i$id_college
  id_classe[k+1] <- i$id_colcla
  
  
  scores[k+2] <-  i$evfra1zscore 
  subject[k+2] <- "French"
  nb[k+2] <-  0 
  repetition[k+2] <-  i$retard 
  id_college[k+2] <- i$id_college
  id_classe[k+2] <- i$id_colcla
  
  
  scores[k+3] <-  i$evmat1zscore 
  subject[k+3] <- "Math"
  nb[k+3] <-  0 
  repetition[k+3] <-  i$retard 
  id_college[k+3] <- i$id_college
  id_classe[k+3] <- i$id_colcla
} 

df.clean.first <- as.data.frame(cbind(id_college, id_classe, scores, subject, nb, repetition))

# french

french_first <- lm(scores ~ repetition*nb + repetition + nb + factor(id_classe) -1, data
             = df.clean.first[df.clean.first$subject == "French", colnames(df.clean.first)])
summary(french_first)

cl_vcov_french_first <- vcovCL(french_first, cluster = ~id_college)

french_first.coeffs_cl <- coeftest(french_first, vcov = cl_vcov_french_first, cluster = ~id_college)
french_first.coeffs_cl 

# maths
math_first <- lm(scores ~ repetition*nb + repetition + nb + factor(id_classe) -1, data
                   = df.clean.first[df.clean.first$subject == "Math", colnames(df.clean.first)])
summary(math_first)

cl_vcov_math_first <- vcovCL(math_first, cluster = ~id_college)

math_first.coeffs_cl <- coeftest(math_first, vcov = cl_vcov_math_first, cluster = ~id_college)
math_first.coeffs_cl 

#stargazer(french_first.coeffs_cl , math_first.coeffs_cl)


########## Right hand side balancing test

# school fixed effects
typeof(d.std$id_college)
d.std$id_college <- as.numeric(d.std$id_college)

id_college <- aggregate(d.std$id_college, list(d.std$id_colcla), FUN=mean, na.rm = TRUE)
colnames(id_college) <- c("id_colcla", "id_college")
bias.all <- merge(bias.all, id_college, by = "id_colcla")

#: blind score for math


achieve <- lm(repeaters.bias.mat ~ achievement_gap_mat+ factor(id_college) -1, data = bias.all)

# high ses
whitecollar.non.repeaters <- aggregate(non.repeaters$whitecollar, list(non.repeaters$id_colcla), FUN=sum, na.rm = TRUE)

whitecollar.repeaters <- aggregate(repeaters$whitecollar, list(repeaters$id_colcla), FUN=sum, na.rm = TRUE)

whitecollar.all <- merge(whitecollar.non.repeaters, whitecollar.repeaters, by = "Group.1")

colnames(whitecollar.all) <- c("id_colcla", "white_non_repeaters", "white_repeaters")

whitecollar.all$whitecollar_gap <- whitecollar.all$white_repeaters - whitecollar.all$white_non_repeaters

bias.all <- merge(bias.all, whitecollar.all, by = "id_colcla")

achieve.white <- lm(repeaters.bias.mat ~ achievement_gap_mat + whitecollar_gap+ factor(id_college) -1, data = bias.all)

# 2 parents

biparental.non.repeaters <- aggregate(non.repeaters$biparental, list(non.repeaters$id_colcla), FUN=sum, na.rm = TRUE)

biparental.repeaters <- aggregate(repeaters$biparental, list(repeaters$id_colcla), FUN=sum, na.rm = TRUE)

biparental.all <- merge(biparental.non.repeaters, biparental.repeaters, by = "Group.1")

colnames(biparental.all) <- c("id_colcla", "biparental_non_repeaters", "biparental_repeaters")

biparental.all$biparental_gap <- biparental.all$biparental_repeaters - biparental.all$biparental_non_repeaters

bias.all <- merge(bias.all, biparental.all, by = "id_colcla")

achieve.white.biparental <- lm(repeaters.bias.mat ~ achievement_gap_mat + whitecollar_gap + biparental_gap + factor(id_college) -1, data = bias.all)

# need based scholarship

boursier.non.repeaters <- aggregate(non.repeaters$boursier, list(non.repeaters$id_colcla), FUN=sum, na.rm = TRUE)

boursier.repeaters <- aggregate(repeaters$boursier, list(repeaters$id_colcla), FUN=sum, na.rm = TRUE)

boursier.all <- merge(boursier.non.repeaters, boursier.repeaters, by = "Group.1")

colnames(boursier.all) <- c("id_colcla", "boursier_non_repeaters", "boursier_repeaters")

boursier.all$boursier_gap <- boursier.all$boursier_repeaters - boursier.all$boursier_non_repeaters

bias.all <- merge(bias.all, boursier.all, by = "id_colcla")

achieve.white.biparental.boursier <- lm(repeaters.bias.mat ~ achievement_gap_mat + whitecollar_gap + biparental_gap + boursier_gap + factor(id_college) -1, data = bias.all)
summary(achieve.white.biparental.boursier)

# 1st kid

aine.non.repeaters <- aggregate(non.repeaters$aine, list(non.repeaters$id_colcla), FUN=sum, na.rm = TRUE)

aine.repeaters <- aggregate(repeaters$aine, list(repeaters$id_colcla), FUN=sum, na.rm = TRUE)

aine.all <- merge(aine.non.repeaters, aine.repeaters, by = "Group.1")

colnames(aine.all) <- c("id_colcla", "aine_non_repeaters", "aine_repeaters")

aine.all$aine_gap <- aine.all$aine_repeaters - aine.all$aine_non_repeaters

bias.all <- merge(bias.all, aine.all, by = "id_colcla")

achieve.white.biparental.boursier.aine <- lm(repeaters.bias.mat ~ achievement_gap_mat + whitecollar_gap +   factor(id_college) -1 + biparental_gap + boursier_gap + aine_gap, data = bias.all)
summary(achieve.white.biparental.boursier.aine)


# employed

emploi.non.repeaters <- aggregate(non.repeaters$emploi, list(non.repeaters$id_colcla), FUN=sum, na.rm = TRUE)

emploi.repeaters <- aggregate(repeaters$emploi, list(repeaters$id_colcla), FUN=sum, na.rm = TRUE)

emploi.all <- merge(emploi.non.repeaters, emploi.repeaters, by = "Group.1")

colnames(emploi.all) <- c("id_colcla", "emploi_non_repeaters", "emploi_repeaters")

emploi.all$emploi_gap <- emploi.all$emploi_repeaters - emploi.all$emploi_non_repeaters

bias.all <- merge(bias.all, emploi.all, by = "id_colcla")

achieve.white.biparental.boursier.aine.emploi <- lm(repeaters.bias.mat ~ achievement_gap_mat + whitecollar_gap + biparental_gap + boursier_gap + aine_gap
                                                    + emploi_gap+ factor(id_college) -1, data = bias.all)
summary(achieve.white.biparental.boursier.aine)

# cluster standard errors 
achieve.cl <- coeftest(achieve, vcov =  vcovCL(achieve, cluster = ~id_college), cluster = ~id_college)
achieve.white.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.boursier.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental.boursier, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.boursier.aine.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental.boursier.aine, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.boursier.aine.emploi.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental.boursier.aine.emploi, cluster = ~id_college), cluster = ~id_college)

library(#stargazer)
#stargazer(achieve.cl, achieve.white.cl, achieve.white.biparental.cl, achieve.white.biparental.boursier.cl, 
          achieve.white.biparental.boursier.aine.cl, achieve.white.biparental.boursier.aine.emploi.cl, omit = c("factor(id_college)"))



############## left hand side balancing test 

white <- lm(whitecollar_gap ~ as.numeric(repeaters.bias.mat) + achievement_gap_mat 
            + factor(id_college), data= bias.all)
summary(white)
white.cl <- coeftest(white, vcov =  vcovCL(white, cluster = ~id_college), cluster = ~id_college)
white.cl

biparental <- lm(biparental_gap ~ as.numeric(repeaters.bias.mat) + achievement_gap_mat + factor(id_college), data= bias.all)
biparental.cl <- coeftest(biparental, vcov =  vcovCL(biparental, cluster = ~id_college), cluster = ~id_college)

boursier <- lm(boursier_gap ~ as.numeric(repeaters.bias.mat) + achievement_gap_mat + factor(id_college), data= bias.all)
boursier.cl <- coeftest(boursier, vcov =  vcovCL(boursier, cluster = ~id_college), cluster = ~id_college)

aine <- lm(aine_gap ~ as.numeric(repeaters.bias.mat) + achievement_gap_mat + factor(id_college), data= bias.all)
aine.cl <- coeftest(aine, vcov =  vcovCL(aine, cluster = ~id_college), cluster = ~id_college)

emploi <- lm(emploi_gap ~ as.numeric(repeaters.bias.mat) + achievement_gap_mat + factor(id_college), data= bias.all)
emploi.cl <- coeftest(emploi, vcov =  vcovCL(emploi, cluster = ~id_college), cluster = ~id_college)

#stargazer(white.cl, biparental.cl, boursier.cl, aine.cl, emploi.cl)


############ redo right for french 
names(bias.all)[names(bias.all) == 'repeaters.bias'] <- "repeaters.bias.fra"

achieve <- lm(repeaters.bias.fra ~ achievement_gap_fra + factor(id_college) -1, data = bias.all)

achieve.white <- lm(repeaters.bias.fra ~ achievement_gap_fra + whitecollar_gap + factor(id_college) -1, data = bias.all)

achieve.white.biparental <- lm(repeaters.bias.fra ~ achievement_gap_fra + whitecollar_gap + biparental_gap + factor(id_college) -1, data = bias.all)

achieve.white.biparental.boursier <- lm(repeaters.bias.fra ~ achievement_gap_fra + whitecollar_gap + biparental_gap + boursier_gap + factor(id_college) -1, data = bias.all)

achieve.white.biparental.boursier.aine <- lm(repeaters.bias.fra ~ achievement_gap_fra + whitecollar_gap + biparental_gap + boursier_gap + aine_gap
                                                   + factor(id_college) -1, data = bias.all)

achieve.white.biparental.boursier.aine.emploi <- lm(repeaters.bias.fra ~ achievement_gap_fra + whitecollar_gap + biparental_gap + boursier_gap + aine_gap
                                                    + emploi_gap+ factor(id_college) -1, data = bias.all)

achieve.cl <- coeftest(achieve, vcov =  vcovCL(achieve, cluster = ~id_college), cluster = ~id_college)
achieve.white.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.boursier.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental.boursier, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.boursier.aine.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental.boursier.aine, cluster = ~id_college), cluster = ~id_college)
achieve.white.biparental.boursier.aine.emploi.cl <- coeftest(achieve, vcov =  vcovCL(achieve.white.biparental.boursier.aine.emploi, cluster = ~id_college), cluster = ~id_college)

#stargazer(achieve.cl, achieve.white.cl, achieve.white.biparental.cl, achieve.white.biparental.boursier.cl, 
          achieve.white.biparental.boursier.aine.cl, achieve.white.biparental.boursier.aine.emploi.cl, omit = c("factor(id_college)"))


#stargazer(achieve , achieve.white , achieve.white.biparental , achieve.white.biparental.boursier , 
          achieve.white.biparental.boursier.aine , achieve.white.biparental.boursier.aine.emploi , omit = c("factor(id_college)"))


############### redo left for french
white <- lm(whitecollar_gap ~ as.numeric(repeaters.bias.fra) + achievement_gap_fra 
            + factor(id_college), data= bias.all)
summary(white)
white.cl <- coeftest(white, vcov =  vcovCL(white, cluster = ~id_college), cluster = ~id_college)
white.cl

biparental <- lm(biparental_gap ~ as.numeric(repeaters.bias.fra) + achievement_gap_fra + factor(id_college), data= bias.all)
biparental.cl <- coeftest(biparental, vcov =  vcovCL(biparental, cluster = ~id_college), cluster = ~id_college)

boursier <- lm(boursier_gap ~ as.numeric(repeaters.bias.fra) + achievement_gap_fra + factor(id_college), data= bias.all)
boursier.cl <- coeftest(boursier, vcov =  vcovCL(boursier, cluster = ~id_college), cluster = ~id_college)

aine <- lm(aine_gap ~ as.numeric(repeaters.bias.fra) + achievement_gap_fra + factor(id_college), data= bias.all)
aine.cl <- coeftest(aine, vcov =  vcovCL(aine, cluster = ~id_college), cluster = ~id_college)

emploi <- lm(emploi_gap ~ as.numeric(repeaters.bias.fra) + achievement_gap_fra + factor(id_college), data= bias.all)
emploi.cl <- coeftest(emploi, vcov =  vcovCL(emploi, cluster = ~id_college), cluster = ~id_college)

#stargazer(white.cl, biparental.cl, boursier.cl, aine.cl, emploi.cl)


########### balanced checks of attrition

d.std$perc_miss_t2_fra <- NA
d.std$perc_miss_t2_mat <- NA
d.std$perc_miss_t3_fra <- NA
d.std$perc_miss_t3_mat <- NA

d.std$perc_miss_t2_fra_nr <- NA
d.std$perc_miss_t2_mat_nr <- NA
d.std$perc_miss_t3_fra_nr <- NA
d.std$perc_miss_t3_mat_nr <- NA

for (c in unique(d.std$id_classe)) {
  d.std$perc_miss_t2_fra[d.std$id_classe == c & d.std$retard == 1] <- (length(d.std$id_eleve[!is.na(d.std$note_FRA_t1) & is.na(d.std$note_FRA_t2) & d.std$retard == 1 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 1 & d.std$id_classe == c]))*100
  d.std$perc_miss_t2_mat[d.std$id_classe == c & d.std$retard == 1] <- (length(d.std$id_eleve[!is.na(d.std$note_MAT_t1) & is.na(d.std$note_MAT_t2) & d.std$retard == 1 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 1 & d.std$id_classe == c]))*100
  d.std$perc_miss_t3_fra[d.std$id_classe == c & d.std$retard == 1] <- (length(d.std$id_eleve[!is.na(d.std$note_FRA_t1) & !is.na(d.std$note_FRA_t2) & is.na(d.std$note_FRA_t3) & d.std$retard == 1 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 1 & d.std$id_classe == c]))*100
  d.std$perc_miss_t3_mat[d.std$id_classe == c & d.std$retard == 1] <- (length(d.std$id_eleve[!is.na(d.std$note_MAT_t1) & !is.na(d.std$note_MAT_t2) & is.na(d.std$note_MAT_t3) & d.std$retard == 1 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 1 & d.std$id_classe == c]))*100
  
  d.std$perc_miss_t2_fra_nr[d.std$id_classe == c & d.std$retard == 0] <- (length(d.std$id_eleve[!is.na(d.std$note_FRA_t1) & is.na(d.std$note_FRA_t2) & d.std$retard == 0 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 0 & d.std$id_classe == c]))*100
  d.std$perc_miss_t2_mat_nr[d.std$id_classe == c & d.std$retard == 0] <- (length(d.std$id_eleve[!is.na(d.std$note_MAT_t1) & is.na(d.std$note_MAT_t2) & d.std$retard == 0 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 0 & d.std$id_classe == c]))*100
  d.std$perc_miss_t3_fra_nr[d.std$id_classe == c & d.std$retard == 0] <- (length(d.std$id_eleve[!is.na(d.std$note_FRA_t1) & !is.na(d.std$note_FRA_t2) & is.na(d.std$note_FRA_t3) & d.std$retard == 0 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 0 & d.std$id_classe == c]))*100
  d.std$perc_miss_t3_mat_nr[d.std$id_classe == c & d.std$retard == 0] <- (length(d.std$id_eleve[!is.na(d.std$note_MAT_t1) & !is.na(d.std$note_MAT_t2) & is.na(d.std$note_MAT_t3) & d.std$retard == 0 & d.std$id_classe == c])/length(d.std$id_eleve[d.std$retard == 0 & d.std$id_classe == c]))*100
  
}

t2_fr_rep <- lm(bias.repeaters ~ perc_miss_t2_fra + factor(id_classe) -1, data= d.std[d.std$retard ==1, colnames(d.std)])
summary(t2_fr_rep)

t2_fr_nonrep <- lm(bias.not.repeaters ~ perc_miss_t2_fra_nr + factor(id_classe) -1, data= d.std[d.std$retard ==0, colnames(d.std)])
summary(t2_fr_nonrep)

t2_mt_rep <- lm(bias.repeaters.mat ~ perc_miss_t2_mat + factor(id_classe) -1, data= d.std[d.std$retard ==1, colnames(d.std)])
summary(t2_mt_rep)

t2_mt_nonrep <- lm(bias.not.repeaters.mat ~ perc_miss_t2_mat_nr + factor(id_classe) -1, data= d.std[d.std$retard ==0, colnames(d.std)])
summary(t2_mt_nonrep)

t3_fr_rep <- lm(bias.repeaters ~ perc_miss_t3_fra + factor(id_classe) -1, data= d.std[d.std$retard ==1, colnames(d.std)])
summary(t3_fr_rep)

t3_fr_nonrep <- lm(bias.not.repeaters ~ perc_miss_t3_fra_nr + factor(id_classe) -1, data= d.std[d.std$retard ==0, colnames(d.std)])
summary(t3_fr_nonrep)

t3_mt_rep <- lm(bias.repeaters.mat ~ perc_miss_t3_mat + factor(id_classe) -1, data= d.std[d.std$retard ==1, colnames(d.std)])
summary(t3_mt_rep)

t3_mt_nonrep <- lm(bias.not.repeaters.mat ~ perc_miss_t3_mat_nr + factor(id_classe) -1, data= d.std[d.std$retard ==0, colnames(d.std)])
summary(t3_mt_nonrep)

