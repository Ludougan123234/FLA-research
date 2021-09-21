##### setup (wd, packages) #####
getwd()
setwd("#MY/WORKING/DIRECTORY#")

##### Install necessary packages#####
packages <- c("ltm", "lavaan", "tidySEM", "psych", "pastecs", "ggplot2", "ggpubr", "GGally", "rstatix", "corrplot", "PerframnceAnalytics",
              "Hmisc", "nFactors", "paran", "dplyr", "DAAG", "ggiraphExtra")
installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages = FALSE)){
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

#####Clean data#####
surv_dat <- read.csv("surv_dat.csv")
#reverse coding keys
#flcas_rev_key <- c(1,1,1,1,-1,1,1,1,1,-1,1,1,1,1,1,1,1,1,1,1) #5,10 reverse coded
ars_rev_key <- c(1,-1,1,-1,1,-1,1,-1,1,1,1,1,1,1,1,1,-1) #2,4,6,8,17 reverse coded

#raw responses
demographic <- surv_dat[, c(1:2)]
flcas_rep <- surv_dat[, c(3:22)]
ars_rep <- reverse.code(ars_rev_key, surv_dat[, c(23:39)]) #reverse.code(key, data) to reverse code ARS items
btps_rep <- surv_dat[, c(40:52)]

#####Cronbach's alpha#####
# calculate cronbach's alpha of the three questionnaires
cronbach.alpha(flcas_rep) # = 0.955
cronbach.alpha(ars_rep)  # = 0.876
cronbach.alpha(btps_rep) # = 0.827

#####factor analysis prep#####
# Preparation of factor analysis - calculate KMO index of sampling adequacy, bartlett's sphericity
KMO(ars_rep)
KMO(btps_rep)
KMO(flcas_rep)
antimg(ars_rep)
antimg(btps_rep)
antimg(flcas_rep)


#####factor analysis flcas#####
# all three questionnaires go through eigenvalue calculation and parallel analysis to determine the number of factors to be retained, 
ev <- eigen(cor(flcas_rep))
ap <- parallel(subject = nrow(flcas_rep), var = ncol(flcas_rep), rep = 100, cent = 0.05)
ns <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
paran(flcas_rep, iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0) +Ttitle("FLCAS")
plotnScree(ns)

factanal(flcas_rep, factors = 3)

#####factor analysis ars_30#####
ev <- eigen(cor(ars_rep))
ap <- parallel(subject = nrow(ars_rep), var = ncol(ars_rep), rep = 100, cent = 0.05)
ns <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(ns)
paran(ars_rep, iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0) + title("ARS-30")
factanal(ars_rep, factors = 3)
fa(ars_rep, nfactors = 3, rotate = "oblimin")

#####factor analysis perfectionism#####
ev <- eigen(cor(btps_rep))
ap <- parallel(subject = nrow(btps_rep), var = ncol(btps_rep), rep = 100, cent = 0.05)
ns <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(ns)
paran(btps_rep, iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0) + title("BTPS")
factanal(btps_rep, factors = 3)
fa(btps_rep, nfactors = 3, rotate = "oblimin")

factanal(btps_rep, factors = 2) 
factanal(ars_rep, factors = 3)
factanal(flcas_rep, factors = 3)


# binding factors into their respective dataframes with rowSums()
flcas.com_app.low_est <- surv_dat[, c(3,4,5,6,8,9,11,12,13,14,17,18,20,21,22)]
flcas.com_app.low_est.total <- rowSums(flcas.com_app.low_est)
flcas.neg_att <- surv_dat[, c(7,10,15,16,19)]
flcas.neg_att.total <- rowSums(flcas.neg_att)

ars.perseverance <- surv_dat[, c(23,24,25,27,29,31)]
ars.perseverance.total <- rowSums(ars.perseverance)
ars.adaptive <- surv_dat[, c(32:38)]
ars.adaptive.total <- rowSums(ars.adaptive)
ars.neg_emo <- surv_dat[, c(26,28,30,39)]
ars.neg_emo.total <- rowSums(ars.neg_emo)

btps.self_critical <- surv_dat[, c(44:50)]
btps.self_critical.total <- rowSums(btps.self_critical)
btps.rigid <- surv_dat[, c(40:43)]
btps.rigid.total <- rowSums(btps.rigid)
btps.narcissistic <- surv_dat[, c(51:52)]
btps.narcissistic.total <- rowSums(btps.narcissistic)

factored_total <- data.frame(flcas.neg_att.total, flcas.com_app.low_est.total, 
                             ars.adaptive.total, ars.neg_emo.total, ars.perseverance.total,
                             btps.narcissistic.total, btps.rigid.total, btps.self_critical.total)


#sum of responses
flcas_total <- rowSums(flcas_rep)
ars_total <- rowSums(ars_rep)
btps_total <- rowSums(btps_rep)
total_scores <- data.frame(flcas_total, ars_total, btps_total)
dem_total <- data.frame(demographic, flcas_total, ars_total, btps_total)
head(surv_dat)

#####Descriptive statistics#####
describe(surv_dat[, c(1:2)]) #demographic
describeBy(flcas_total)
describeBy(ars_total)
describeBy(btps_total)

#descriptive statistics by age group and gender group
dem_total %>% group_by(gender) %>% summarize(count = n(),mean = mean(ars_total, na.rm = TRUE),sd = sd(ars_total, na.rm = TRUE),var = var(ars_total, na.rm = TRUE))
dem_total %>% group_by(gender) %>% summarize(count = n(),mean = mean(btps_total, na.rm = TRUE),sd = sd(btps_total, na.rm = TRUE), var = var(btps_total, na.rm = TRUE),
                                             qs = quantile(btps_total, c(0.25, 0.75)), min = min(btps_total), max = max(btps_total))
dem_total %>% group_by(gender) %>% summarize(count = n(),mean = mean(flcas_total, na.rm = TRUE),sd = sd(flcas_total, na.rm = TRUE), var = var(flcas_total, na.rm = TRUE), 
                                             qs = quantile(flcas_total, c(0.25, 0.75)), min = min(flcas_total), max = max(flcas_total))
dem_total %>% group_by(age) %>% summarize(count = n(),mean = mean(flcas_total, na.rm = TRUE),sd = sd(flcas_total, na.rm = TRUE), var = var(flcas_total, na.rm = TRUE), 
                                             qs = quantile(flcas_total, c(0.25, 0.75)), min = min(flcas_total), max = max(flcas_total))
dem_total %>% group_by(age) %>% summarize(count = n(),mean = mean(btps_total, na.rm = TRUE),sd = sd(btps_total, na.rm = TRUE), var = var(btps_total, na.rm = TRUE),
                                             qs = quantile(btps_total, c(0.25, 0.75)), min = min(btps_total), max = max(btps_total))
dem_total %>% group_by(age) %>% summarize(count = n(),mean = mean(ars_total, na.rm = TRUE),sd = sd(ars_total, na.rm = TRUE),var = var(ars_total, na.rm = TRUE))

#t-tests and anova tests
dem_total %>% welch_anova_test(flcas_total ~ gender)
dem_total %>% welch_anova_test(btps_total ~ gender)
dem_total %>% kruskal_test(ars_total ~ gender) #similar to t-test 
dem_total %>% levene_test(ars_total ~ gender) #variance
dem_total %>% t_test(flcas_total ~ gender, var.equal = TRUE, detailed = TRUE)
dem_total %>% t_test(btps_total ~ gender, var.equal = TRUE, detailed = TRUE)
dem_total %>% kruskal_test(ars_total ~ age) #similar to t-test 
dem_total %>% anova_test(btps_total ~ age, detailed = TRUE)
dem_total %>% kruskal_test(ars_total ~ age)

#####correlation#####

#Correlation of FLCAS and Academic resilience
cor.test(flcas_total, ars_total)

#Correlation of FLCAS and perfectionism
cor.test(flcas_total, btps_total)

#Correlation of Perfectionism and Academic resilience
cor.test(btps_total, ars_total)

cor.wt(total_scores)
cor.wt(factored_total)
cor_mat(factored_total)

flcas_cor <- cor_mat(flcas_rep)
cor_mark_significant(flcas_cor)
cor_mat(btps_rep)
cor_mat(ars_rep)
#####correlation plots#####
#plot with p-value
chart.Correlation(total_scores)
#plot with no p-value
cor.plot(total_scores)
#ggpairs(total_scores)

# plot correlation plots by group
# use grab_legend() to grab legend from a different ggplot() plot to yield more readable legend
legend_age <- grab_legend(ggplot(total_scores, aes(x = "flcas_total", y = "btps_total", color = demographic[, 2])) + geom_point() + scale_color_discrete("Age groups"))
ggpairs(total_scores, 
        ggplot2::aes_string(colour = demographic[, 2]), 
        upper = list(continuous = my_fn),
        lower = list(continuous = wrap("smooth", alpha = 0.7, size=0.7)),
        title = "Correlation of FLCAS, ARS-30, and BTPS by age group",
        legend = legend_age) + theme(legend.position = "bottom")
legend_gender <- grab_legend(ggplot(total_scores, aes(x = "flcas_total", y = "btps_total", color = demographic[, 1])) + geom_point() + scale_color_discrete("Gender"))
ggpairs(total_scores, 
        ggplot2::aes_string(colour = demographic[, 1]), 
        upper = list(continuous = my_fn),
        lower = list(continuous = wrap("smooth", alpha = 0.7, size=0.7)),
        title = "Correlation of FLCAS, ARS-30, and BTPS by gender",
        legend = legend1) + theme(legend.position = "bottom")

ggpairs(factored_total, 
        ggplot2::aes_string(colour = demographic[, 1]), 
        upper = list(continuous = my_fn),
        lower = list(continuous = wrap("smooth", alpha = 0.7, size=0.7)),
        title = "Correlation of FLCAS, ARS-30, and BTPS by gender",
        legend = legend_gender) + theme(legend.position = "bottom")

ggpairs(factored_total, 
        ggplot2::aes_string(colour = demographic[, 2]), 
        upper = list(continuous = my_fn),
        lower = list(continuous = wrap("smooth", alpha = 0.7, size=0.7)),
        title = "Correlation of FLCAS, ARS-30, and BTPS by age group",
        legend = legend_age) + theme(legend.position = "bottom")

#####CFL and structural regression#####
flcas_factor <- "CA_LSC =~ FLCAS_1 + FLCAS_2 + FLCAS_3 + FLCAS_4+ FLCAS_6+ FLCAS_7+ FLCAS_9+ FLCAS_10+ FLCAS_11 + FLCAS_12+ FLCAS_15+ FLCAS_16+ FLCAS_18+ FLCAS_19+ FLCAS_20
NATEC =~ FLCAS_5+ FLCAS_8+ FLCAS_13+ FLCAS_14+ FLCAS_17"
flcas_cfa <- cfa(flcas_factor, data = flcas_rep)
summary(flcas_cfa, fit.measures = TRUE, standardized = TRUE)
flcas_layout <- get_layout("", "","","","","","","","CA_LSC","","","","","","","","","","","NATEC","","",
                           "FLCAS_1","FLCAS_2","FLCAS_3","FLCAS_4","FLCAS_6","FLCAS_7","FLCAS_9","FLCAS_10","FLCAS_11","FLCAS_12","FLCAS_15","FLCAS_16","FLCAS_18","FLCAS_19","FLCAS_20","FLCAS_5","FLCAS_8","FLCAS_13","FLCAS_14","FLCAS_17"
                           ,rows = 2)

#graph_sem(model = flcas_cfa, layout = flcas_layout)
flcas_second_order <- "CA_LSC =~ FLCAS_1 + FLCAS_2 + FLCAS_3 + FLCAS_4+ FLCAS_6+ FLCAS_7+ FLCAS_9+ FLCAS_10+ FLCAS_11++ FLCAS_12+ FLCAS_15+ FLCAS_16+ FLCAS_18+ FLCAS_19+ FLCAS_20
NATEC =~ FLCAS_5+ FLCAS_8+ FLCAS_13+ FLCAS_14+ FLCAS_17
FLCAS =~ 1*CA_LSC + 1*NATEC
FLCAS~~FLCAS"
flcas_2_order <- cfa(flcas_second_order, data = flcas_rep)
summary(flcas_2_order, fit.measures = TRUE)
#graph_sem(model = flcas_2_order)

colnames(ars_rep) <- c("ARS_1","ARS_2","ARS_3","ARS_4","ARS_5","ARS_6","ARS_7","ARS_8","ARS_9","ARS_10","ARS_11","ARS_12","ARS_13","ARS_14","ARS_15","ARS_16","ARS_17")
ars_factor <- "Pers =~ ARS_1 + ARS_2 + ARS_3 + ARS_5 + ARS_7 + ARS_9
AHS =~ ARS_10 + ARS_11 + ARS_12 + ARS_13 + ARS_14 + ARS_15 + ARS_16
NEA =~ ARS_4 + ARS_6 + ARS_8 + ARS_17
ARS =~ 1*Pers + 1*AHS + 1*NEA
ARS~~ARS"
ars_2_order <- cfa(ars_factor, data = ars_rep)
summary(ars_2_order, fit.measures = TRUE, standardized = TRUE)
#graph_sem(model = ars_2_order)

btps_factor <- "RP =~ BTPS_1 + BTPS_2 + BTPS_3 + BTPS_4
SCP =~ BTPS_5 + BTPS_6 + BTPS_7 + BTPS_8 + BTPS_9 + BTPS_10 + BTPS_11
NP =~ BTPS_12 + BTPS_13
BTPS =~ 1*RP + 1*SCP + 1*NP
BTPS ~~ BTPS"
btps_2_order <- cfa(btps_factor, data = btps_rep)
summary(btps_2_order, fit.measures = TRUE)
#graph_sem(model = btps_2_order)

surv_rep <- data.frame(flcas_rep, ars_rep, btps_rep)
surv_rep_mat <- data.matrix(surv_rep)
reg_mod_stx <- "FLCAS =~ FLCAS_1+FLCAS_2 + FLCAS_3 + FLCAS_4 + FLCAS_5+FLCAS_6+FLCAS_7+FLCAS_8+FLCAS_9+FLCAS_10+FLCAS_11+FLCAS_12+FLCAS_13+FLCAS_14+FLCAS_15+FLCAS_16+FLCAS_17+FLCAS_18+FLCAS_19+FLCAS_20
ARS =~ ARS_1+ARS_2 + ARS_3 + ARS_4 + ARS_5+ARS_6+ARS_7+ARS_8+ARS_9+ARS_10+ARS_11+ARS_12+ARS_13+ARS_14+ARS_15+ARS_16+ARS_17
BTPS =~ BTPS_1+BTPS_2 + BTPS_3 + BTPS_4 + BTPS_5+BTPS_6+BTPS_7+BTPS_8+BTPS_9+BTPS_10+BTPS_11+BTPS_12+BTPS_13
FLCAS ~ 1 + ARS + BTPS
ARS ~~ BTPS"

#cov_mat <- cov(factored_total)
reg_mod_ver2 <- "FLCAS =~ flcas.neg_att.total + flcas.com_app.low_est.total
ARS =~ ars.adaptive.total + ars.neg_emo.total + ars.perseverance.total
BTPS =~ btps.narcissistic.total + btps.rigid.total + btps.self_critical.total
FLCAS ~ ARS + BTPS
ARS ~~ BTPS"

reg_mod_ver3 <- "FLCAS =~ flcas_total
ARS =~ ars_total
BTPS =~ btps_total
FLCAS ~ ARS + BTPS
ARS ~~ BTPS"


#reg_mod <- sem(reg_mod_stx, sample.cov = reg_mod_ver2, data = cov_mat)
reg_mod <- sem(reg_mod_ver2, data = factored_total)
summary(reg_mod)

regression <- lm(flcas_total ~ ars_total + btps_total, data = total_scores)
layout(matrix(c(1,2,3,4),2,2))
plot(regression)
anova(regression)

struct_mod <- "
FLCAS =~ flcas.com_app.low_est.total + flcas.neg_att.total
ARS =~ ars.adaptive.total + ars.neg_emo.total + ars.perseverance.total
BTPS =~ btps.narcissistic.total + btps.rigid.total + btps.self_critical.total
FLCAS ~ BTPS + ARS"

struct_mod_res <- sem(struct_mod, data = factored_total)
summary(struct_mod_res, standardized = TRUE, fit.measures = TRUE)
graph_sem(struct_mod_res)

#validate model 
cv.lm(regression, m=3)


######plot function#####
# these functions are referenced from online sources
my_fn <- function(data, mapping, ...){
  # takes in x and y for each panel
  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)
  colorData <- eval_data_col(data, mapping$colour)
  
  byGroup =by(data.frame(xData,yData),colorData,function(i)printVar(i[,1],i[,2]))
  byGroup = data.frame(col=names(byGroup),label=as.character(byGroup))
  byGroup$x = 0.5
  byGroup$y = seq(0.8-0.3,0.2,length.out=nrow(byGroup))
  
  #main correlation
  mainCor = printVar(xData,yData)
  
  p <- ggplot(data = data, mapping = mapping) +
    annotate(x=0.5,y=0.8,label=mainCor,geom="text",size=4) +
    geom_text(data=byGroup,inherit.aes=FALSE,
              aes(x=x,y=y,col=col,label=label),size=4)+ 
    theme_void() + ylim(c(0,1))
  p
}

printVar = function(x,y){
  vals = cor.test(x,y,
                  method="pearson")[c("estimate","p.value")] # select pearson cor estimate and p-value to put in graph
  names(vals) = c("r = ","p = ")
  paste(names(vals),signif(unlist(vals),2), collapse = "\n")
}


# Anti-image correlation matrix diagonals - should be > 0.5
antimg <- function(x){
  X <- cor(x) 
  iX <- ginv(X) # generalized inverse of a matrix
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  print(diag(AIR), row.names = FALSE)
}

totcor <- cor_mat(factored_total)
