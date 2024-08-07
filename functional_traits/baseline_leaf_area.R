library(tidyverse)
library(lme4)
library(DHARMa)
library(emmeans)
library(car)
library(glmmTMB)
getwd()
setwd("C:/Users/kennad/Box/Kenna Working Directory/MS_Thesis")

df0 = read.csv("raw_data/Functional_traits/Defoliation/leaf_area_scans_updated_datasheet.csv")
#Select only collumns we need for leaf area
#Basline SLA = leaf area 1 
names(df0)
df1 = df0  %>%
  select(Flat.., DNA_ENTRY, DNA_ID, 
          X1st.cut.leaf.area..cm2.)

names(df1)


#Data is in long format already

#data coercion 
df1$Flat.. = as.factor(df1$Flat..)
df1$DNA_ID = as.factor(df1$DNA_ID)
df1$DNA_ENTRY = factor(df1$DNA_ENTRY, levels = c("Wasatch" ,
                                                         "Basin",
                                                         "Columbia","P7","Anatone"))
levels(df1$DNA_ENTRY) <- c("BasinSTZ3a","BasinSTZ4", "Columbia", "P7", "Anatone")

#visualizeing data================================================================


ggplot(df1,
       aes(x = DNA_ENTRY,
           y = X1st.cut.leaf.area..cm2.,
           fill = DNA_ENTRY,)) + 
  geom_boxplot() 


##Examining Nas and Outliers===================================
which(is.na(df1$X1st.cut.leaf.area..cm2.))



#which values are considered outliers based on IQRs
boxplot.stats(df1$X1st.cut.leaf.area..cm2.)$out


#find row names of these outliers

out <- boxplot.stats(df1$X1st.cut.leaf.area..cm2.)$out
out_row_num <- which(df1$X1st.cut.leaf.area..cm2. %in% c(out))
out_row_num
df1[df1$DNA_ID == 725,]

##remove 1314, 1337, 1338
df1 = df1[!df1$DNA_ID == 1337,]
df1 = df1[!df1$DNA_ID == 1338,]
df1 = df1[!df1$DNA_ID == 1314,]

###=========fit a linear mixed model==============================

#Linear model==== 
mod = lmer(X1st.cut.leaf.area..cm2. ~ DNA_ENTRY + (1|Flat..),
           data = df1)
summary(mod)

#Model checking
mod.res = simulateResiduals(mod, plot = T)

##SQrt transform 
mod1 = lmer(sqrt(X1st.cut.leaf.area..cm2.) ~ DNA_ENTRY + (1|Flat..),
           data = df1)
summary(mod1)

#Model checking
mod1.res = simulateResiduals(mod1, plot = T)

# #Does not really imrpove variance=====
# #Try adding 0.05 to each value so I can log transform to see if that helps
# df1$X1st.cut.leaf.area..cm2. = df1$X1st.cut.leaf.area..cm2. + 0.5
# 
# 
# mod2 = lmer(log(X1st.cut.leaf.area..cm2.) ~ DNA_ENTRY + (1|Flat..),
#            data = df1)
# summary(mod2)
# 
# #Model checking
# mod.res = simulateResiduals(mod2, plot = T)
# #nope


#Run an anova on the model= 
anova(mod)
car::Anova(mod)

#DNA_entry is significant 

#look at contrasts 

emmeans(mod, pairwise ~ DNA_ENTRY)
emmeans(mod, pairwise ~ DNA_ENTRY, type = "response")

#.05 significance level 


# Basin3a = basin4 
# Basin3a < columbia, P7
# Basin3a = anatone 
# Basin4 is less than columbia, p7 
# Basin4 = anatone 
# Columbia = P7 
# Columbia = anatone 
# P7 = Anatone 



#Columbia and P7: A 
#Anatone: AB not different from Basin3a and Basin4, 
#also similar to columbia and P7
#Basin3a and Basin4 = B


cbbPalette <- c("#56B4E9", "#56B4E9", "#E69F00", 
                "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7")



ggplot(df1,
       aes(x = DNA_ENTRY,
           y = X1st.cut.leaf.area..cm2.,
           colour = DNA_ENTRY)) + 
  geom_boxplot() +
  labs(title = 'Baseline Leaf Area', 
       x = "Plant Material", y = "Leaf Area (cm^2)") +
  scale_fill_manual(values=cbbPalette) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.title = element_text(colour="black", size=16, face="bold"),
        axis.title=element_text(size=14,face="bold") )+
  scale_color_discrete(name="DNA_ENTRY")+
  scale_colour_manual(values=cbbPalette) +
  theme(axis.text.x = element_text(angle = 45, size = 12,
                                   hjust=0.95,vjust=0.9)) +   
  annotate("text", x = "BasinSTZ4", y = 37, label = "B",size = 9)+ 
  annotate("text", x = "BasinSTZ3a", y = 37, label = "B",size = 9) +
  annotate("text", x = "Columbia", y = 46, label = "A",size = 9) + 
  annotate("text", x = "Anatone", y = 42, label = "AB",size = 9) + 
  annotate("text", x = "P7", y = 46, label = "A",size = 9)



#setwd(("C:/Users/kennad/Box/Kenna Working Directory/MS_Thesis/code/funct_traits/harvest_1"))
#save.image(file = "base_leaf_area.RData")

