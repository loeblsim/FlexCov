
#clear workspace
rm(list = ls())

setwd("C:/Users/Simon/Nextcloud/Shared/COVID-19 Shared Folder/analysis/FlexCov")

# setdwd("YOURPATH")

# load packages
library(haven)
library(estimatr)
library(tidyverse)
library(ggrepel)
library(sandwich)
library(magrittr)
library(extrafont)
library(ggstatsplot)
library(patchwork)

#import fonts
#remotes::install_version("Rttf2pt1", version = "1.3.8")
#extrafont::font_import()
loadfonts() 
windowsFonts(TNR=windowsFont("Times New Roman"))

FlexCoV <- read_dta("./data_out/FlexCov.dta") 

FlexCoV_oct <- FlexCoV[which(FlexCoV$date==20201031),]

######################
# Sample definitions #
######################

# Sample 1, Main relation
m1 <- lm(ln_daily_cum_deaths_million ~ FLXMON_std, data= FlexCoV_oct)

FlexCoV_oct$used1 <- !seq_len(nrow(FlexCoV_oct))%in%na.action(m1)


# Sample 2, Main model
m2 <- lm(ln_daily_cum_deaths_million ~ FLXMON_std + Tightness + IDVCOLL
         + v2x_libdem + ln_gdp + ln_PopDensity + hospital_beds_per_thousand
         + median_age, data= FlexCoV_oct)

FlexCoV_oct$used2 <- !seq_len(nrow(FlexCoV_oct))%in%na.action(m2)


# Sample 3, Mediator model I
m3 <- lm(ln_daily_cum_deaths_million ~ FLXMON_std + ln_gdp + MaskWearing_whole
         + Fear_whole, data= FlexCoV_oct)

FlexCoV_oct$used3 <- !seq_len(nrow(FlexCoV_oct))%in%na.action(m3)


# Sample 4, Mediator model II
m4 <- lm(ln_daily_cum_deaths_million ~ FLXMON_std + ln_gdp + MaskWearing_mar
         + Fear_mar, data= FlexCoV_oct)

FlexCoV_oct$used4 <- !seq_len(nrow(FlexCoV_oct))%in%na.action(m4)

##############
#  Appendix  #
##############

#
#Appendix: Figure S1: Log. number of deaths per million until October 31, 2020
#

# Subsetting dataset
FlexCoV_oct_2 <-
  FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, v2x_libdem, country_iso3, used1, used2) %>%
  drop_na() 

FlexCoV_oct_2_true <- FlexCoV_oct_2[which(FlexCoV_oct_2$used2==TRUE),]

# Model specification
lm1 <- lm_robust(ln_daily_cum_deaths_million ~ v2x_libdem , data= FlexCoV_oct_2, se_type = "HC3")

# Creating 95% C.I.s
fit1 <- summary(lm1)
fit1$coefficients[2,1]

lm1_dat <- data.frame(lm1$fitted.values, 
                      predict(lm1, newdata = FlexCoV_oct_2,se.fit = TRUE, interval = "confidence"))

# Plot
appendix1 <- FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, v2x_libdem, country_iso3, used2, used1) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = v2x_libdem, y = ln_daily_cum_deaths_million, 
                       label = country_iso3))

appendix1 +  
  geom_segment(
    aes(
      x = min(FlexCoV_oct_2$v2x_libdem),
      xend = max(FlexCoV_oct_2$v2x_libdem),
      y = fit1$coefficients[1,1] +  (min(FlexCoV_oct_2$v2x_libdem)* fit1$coefficients[2,1]),
      yend = fit1$coefficients[1,1] +  (max(FlexCoV_oct_2$v2x_libdem)* fit1$coefficients[2,1]),
      linetype = "Fitted Values for 160 countries with available data\non death rate and Liberal Democracy Index"), colour = "#FF0099") +
  geom_ribbon(
    data = FlexCoV_oct_2, 
    aes(ymin = lm1_dat$fit.lwr, 
        ymax = lm1_dat$fit.upr,
        fill = ""), 
    alpha = 0.25) +
  geom_point(aes(shape = used2, color = used2),size = 2, alpha = .5) + 
  geom_text_repel(show.legend = FALSE, box.padding = 0.3, point.padding = 0, min.segment.length = 0.5, 
                  nudge_x = .02, nudge_y = .04, size = 4, segment.alpha = 0.5, family = "TNR", max.overlaps  = Inf) +
  scale_linetype_manual(name = NULL,  values = c(1,1)) +
  scale_shape_manual(values = c(16,17),
                     labels = c("Not included in Tables 1 to 4\ndue to missing on flexibility and covariates",
                                "Included in Tables 1 to 4 in main article")) +
  scale_colour_manual(values = c('#FF0099','#0066FF')) +
  scale_fill_manual(
    values=c('#FF0099'),
    labels=c("95% C.I. for 160 countries with available data\non death rate and liberal democracy index")) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(limits = c(-3.1, 8.1), breaks = -3:8) +
  theme_bw() +
  theme(axis.text.x   = element_text(family = "TNR", size = 14),
        axis.text.y   = element_text(family = "TNR", size = 14)) +
  labs(x = "Liberal Democracy Index",
       y = "Log. number of deaths per million until October 31, 2020",
       shape = "", color = "") +
  theme(axis.title.x = element_text(hjust = 0.47, size = 19, family = "TNR"),
        axis.title.y = element_text(hjust = 0.5, size = 19, family = "TNR"), legend.position = "bottom") +
  theme(legend.text=element_text(size=17, family = "TNR", margin = margin(t = 5)),
        plot.caption = element_text(family = "TNR", size = 14, hjust = 0)) +
  guides(shape = guide_legend(nrow = 2, override.aes=list(color = c('#FF0099','#0066FF'))),
         fill = guide_legend(nrow = 2, title = ""),
         color = "none")


ggsave(file="./graphs/appendix1.png", width=15, height=10)    

#
#Appendix: Figure S2: Log. number of infections per million until October 31, 2020
#

# Subsetting dataset
FlexCoV_oct_3 <-
  FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_cases_million, v2x_libdem, country_iso3, used1, used2) %>%
  drop_na() 

FlexCoV_oct_3_true <- FlexCoV_oct_3[which(FlexCoV_oct_3$used2==TRUE),]

# Model specification
lm2 <- lm_robust(ln_daily_cum_cases_million ~ v2x_libdem , data= FlexCoV_oct_3, se_type = "HC3")

# Creating 95% C.I.s
fit2 <- summary(lm2)
fit2$coefficients[2,1]

lm2_dat <- data.frame(lm2$fitted.values, 
                      predict(lm2, newdata = FlexCoV_oct_3,se.fit = TRUE, interval = "confidence"))

# Plot
appendix2 <- FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_cases_million, v2x_libdem, country_iso3, used2, used1) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = v2x_libdem, y = ln_daily_cum_cases_million, 
                       label = country_iso3))

appendix2 +  
  geom_segment(
    aes(
      x = min(FlexCoV_oct_3$v2x_libdem),
      xend = max(FlexCoV_oct_3$v2x_libdem),
      y = fit2$coefficients[1,1] +  (min(FlexCoV_oct_3$v2x_libdem)* fit2$coefficients[2,1]),
      yend = fit2$coefficients[1,1] +  (max(FlexCoV_oct_3$v2x_libdem)* fit2$coefficients[2,1]),
      linetype = "Fitted Values for 168 countries with available data\non infection rate and Liberal Democracy Index"), colour = "#FF0099") +
  geom_ribbon(
    data = FlexCoV_oct_3, 
    aes(ymin = lm2_dat$fit.lwr, 
        ymax = lm2_dat$fit.upr,
        fill = ""), 
    alpha = 0.25) +
  geom_point(aes(shape = used2, color = used2),size = 2, alpha = .5) + 
  geom_text_repel(show.legend = FALSE, box.padding = 0.3, point.padding = 0, min.segment.length = 0.5, 
                  nudge_x = .02, nudge_y = .04, size = 4, segment.alpha = 0.5, family = "TNR", max.overlaps  = Inf) +
  scale_linetype_manual(name = NULL,  values = c(1,1)) +
  scale_shape_manual(values = c(16,17),
                     labels = c("Not included in Tables 1 to 4\ndue to missing on flexibility and covariates",
                                "Included in Tables 1 to 4 in main article")) +
  scale_colour_manual(values = c('#FF0099','#0066FF')) +
  scale_fill_manual(
    values=c('#FF0099'),
    labels=c("95% C.I. for 168 countries with available data\non death rate and liberal democracy index")) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(limits = c(1, 11), breaks = 1:11) +
  theme_bw() +
  theme(axis.text.x   = element_text(family = "TNR", size = 14),
        axis.text.y   = element_text(family = "TNR", size = 14)) +
  labs(x = "Liberal Democracy Index",
       y = "Log. number of infections per million until October 31, 2020",
       shape = "", color = "") +
  theme(axis.title.x = element_text(hjust = 0.47, size = 19, family = "TNR"),
        axis.title.y = element_text(hjust = 0.5, size = 19, family = "TNR"), legend.position = "bottom") +
  theme(legend.text=element_text(size=17, family = "TNR", margin = margin(t = 5)),
        plot.caption = element_text(family = "TNR", size = 14, hjust = 0)) +
  guides(shape = guide_legend(nrow = 2, override.aes=list(color = c('#FF0099','#0066FF'))),
         fill = guide_legend(nrow = 2, title = ""),
         color = "none")


ggsave(file="./graphs/appendix2.png", width=15, height=10)    


#
# Appendix: Figure S3: Log. number of deaths per million until October 31, 2020
#

# Subsetting dataset
FlexCoV_oct_4 <-
  FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, ln_gdp, country_iso3, used1, used2) %>%
  drop_na() 

FlexCoV_oct_4_true <- FlexCoV_oct_4[which(FlexCoV_oct_4$used2==TRUE),]

# Model specification
lm3 <- lm_robust(ln_daily_cum_deaths_million ~ ln_gdp , data= FlexCoV_oct_4, se_type = "HC3")

# Creating 95% C.I.s
fit3 <- summary(lm3)
fit3$coefficients[2,1]

lm3_dat <- data.frame(lm3$fitted.values, 
                      predict(lm3, newdata = FlexCoV_oct_4,se.fit = TRUE, interval = "confidence"))

# Plot
appendix3 <- FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, ln_gdp, country_iso3, used2, used1) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = ln_gdp, y = ln_daily_cum_deaths_million, 
                       label = country_iso3))

appendix3 +  
  geom_segment(
    aes(
      x = min(FlexCoV_oct_4$ln_gdp),
      xend = max(FlexCoV_oct_4$ln_gdp),
      y = fit3$coefficients[1,1] +  (min(FlexCoV_oct_4$ln_gdp)* fit3$coefficients[2,1]),
      yend = fit3$coefficients[1,1] +  (max(FlexCoV_oct_4$ln_gdp)* fit3$coefficients[2,1]),
      linetype = "Fitted Values for 159 countries with available data\non death rate and Log. GDP per capita"), colour = "#FF0099") +
  geom_ribbon(
    data = FlexCoV_oct_4, 
    aes(ymin = lm3_dat$fit.lwr, 
        ymax = lm3_dat$fit.upr,
        fill = ""), 
    alpha = 0.25) +
  geom_point(aes(shape = used2, color = used2),size = 2, alpha = .5) + 
  geom_text_repel(show.legend = FALSE, box.padding = 0.3, point.padding = 0, min.segment.length = 0.5, 
                  nudge_x = .02, nudge_y = .04, size = 4, segment.alpha = 0.5, family = "TNR", max.overlaps  = Inf) +
  scale_linetype_manual(name = NULL,  values = c(1,1)) +
  scale_shape_manual(values = c(16,17),
                     labels = c("Not included in Tables 1 to 4\ndue to missing on flexibility and covariates",
                                "Included in Tables 1 to 4 in main analysis")) +
  scale_colour_manual(values = c('#FF0099','#0066FF')) +
  scale_fill_manual(
    values=c('#FF0099'),
    labels=c("95% C.I. for 168 countries with available data\non death rate and Log. GDP per capita")) +
  scale_x_continuous(limits = c(5, 12), breaks = 5:12) +
  scale_y_continuous(limits = c(-3, 8), breaks = -3:8) +
  theme_bw() +
  theme(axis.text.x   = element_text(family = "TNR", size = 14),
        axis.text.y   = element_text(family = "TNR", size = 14)) +
  labs(x = "Log. GDP per capita",
       y = "Log. number of deaths per million until October 31, 2020",
       shape = "", color = "") +
  theme(axis.title.x = element_text(hjust = 0.47, size = 19, family = "TNR"),
        axis.title.y = element_text(hjust = 0.5, size = 19, family = "TNR"), legend.position = "bottom") +
  theme(legend.text=element_text(size=17, family = "TNR", margin = margin(t = 5)),
        plot.caption = element_text(family = "TNR", size = 14, hjust = 0)) +
  guides(shape = guide_legend(nrow = 2, override.aes=list(color = c('#FF0099','#0066FF'))),
         fill = guide_legend(nrow = 2, title = ""),
         color = "none")


ggsave(file="./graphs/appendix3.png", width=15, height=10)  


#
# Appendix: Figure S4: Log. number of deaths per million until October 31, 2020
#

# Subsetting dataset
FlexCoV_oct_5 <-
  FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_cases_million, ln_gdp, country_iso3, used1, used2) %>%
  drop_na() 

FlexCoV_oct_5_true <- FlexCoV_oct_5[which(FlexCoV_oct_5$used2==TRUE),]

# Model specification
lm4 <- lm_robust(ln_daily_cum_cases_million ~ ln_gdp , data= FlexCoV_oct_5, se_type = "HC3")

# Creating 95% C.I.s
fit4 <- summary(lm4)
fit4$coefficients[2,1]

lm4_dat <- data.frame(lm4$fitted.values, 
                      predict(lm4, newdata = FlexCoV_oct_5,se.fit = TRUE, interval = "confidence"))

# Plot
appendix4 <- FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_cases_million, ln_gdp, country_iso3, used2, used1) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = ln_gdp, y = ln_daily_cum_cases_million, 
                       label = country_iso3))

appendix4 +  
  geom_segment(
    aes(
      x = min(FlexCoV_oct_5$ln_gdp),
      xend = max(FlexCoV_oct_5$ln_gdp),
      y = fit4$coefficients[1,1] +  (min(FlexCoV_oct_5$ln_gdp)* fit4$coefficients[2,1]),
      yend = fit4$coefficients[1,1] +  (max(FlexCoV_oct_5$ln_gdp)* fit4$coefficients[2,1]),
      linetype = "Fitted Values for 171 countries with available data\non infection rate and Liberal Democracy Index"), colour = "#FF0099") +
  geom_ribbon(
    data = FlexCoV_oct_5, 
    aes(ymin = lm4_dat$fit.lwr, 
        ymax = lm4_dat$fit.upr,
        fill = ""), 
    alpha = 0.25) +
  geom_point(aes(shape = used2, color = used2),size = 2, alpha = .5) + 
  geom_text_repel(show.legend = FALSE, box.padding = 0.3, point.padding = 0, min.segment.length = 0.5, 
                  nudge_x = .02, nudge_y = .04, size = 4, segment.alpha = 0.5, family = "TNR", max.overlaps  = Inf) +
  scale_linetype_manual(name = NULL,  values = c(1,1)) +
  scale_shape_manual(values = c(16,17),
                     labels = c("Not included in Tables 1 to 4\ndue to missing on flexibility and covariates",
                                "Included in Tables 1 to 4 in main analysis")) +
  scale_colour_manual(values = c('#FF0099','#0066FF')) +
  scale_fill_manual(
    values=c('#FF0099'),
    labels=c("95% C.I. for 171 countries with available data\non death rate and Log. GDP per capita")) +
  scale_x_continuous(limits = c(5, 12), breaks = 5:12) +
  scale_y_continuous(limits = c(1, 11.5), breaks = 1:11) +
  theme_bw() +
  theme(axis.text.x   = element_text(family = "TNR", size = 14),
        axis.text.y   = element_text(family = "TNR", size = 14)) +
  labs(x = "Log. GDP per capita",
       y = "Log. number of infections per million until October 31, 2020",
       shape = "", color = "") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 19, family = "TNR"),
        axis.title.y = element_text(hjust = 0.5, size = 19, family = "TNR"), legend.position = "bottom") +
  theme(legend.text=element_text(size=17, family = "TNR", margin = margin(t = 5)),
        plot.caption = element_text(family = "TNR", size = 14, hjust = 0)) +
  guides(fill = guide_legend(title = ""),
         shape = guide_legend(nrow = 2, override.aes=list(color = c('#FF0099','#0066FF')), order = 3),
         color = "none")


ggsave(file="./graphs/appendix4.png", width=15, height=10)    


#
# Appendix: Figure S5: Robustness check for main models
#
options(ggrepel.max.overlaps = Inf)

# Subsetting dataset
FlexCoV_oct_6 <-
  FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, FLXMON_std, country_iso3, used1, used2) %>%
  drop_na() 

FlexCoV_oct_6_true <- FlexCoV_oct_6[which(FlexCoV_oct_6$used2==TRUE),]

# Model specification
lm5 <- lm_robust(ln_daily_cum_deaths_million ~ FLXMON_std , data= FlexCoV_oct_6, se_type = "HC3")

lm6 <- lm_robust(ln_daily_cum_deaths_million ~ FLXMON_std , data= FlexCoV_oct_6_true, se_type = "HC3")

# Creating 95% C.I.s
fit5 <- summary(lm5)
fit5$coefficients[2,1]

lm5_dat <- data.frame(lm5$fitted.values, 
                          predict(lm5, newdata = FlexCoV_oct_6,se.fit = TRUE, interval = "confidence"))

fit6 <- summary(lm6)
fit6$coefficients[2,1]

lm6_dat <- data.frame(lm6$fitted.values, 
                      predict(lm6, newdata = FlexCoV_oct_6_true,se.fit = TRUE, interval = "confidence"))

# Plot
appendix5 <- FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, FLXMON_std, country_iso3, used2, used1) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = FLXMON_std, y = ln_daily_cum_deaths_million, 
                       label = country_iso3))
appendix5 +  
  geom_segment(
    aes(
      x = min(FlexCoV_oct_6$FLXMON_std),
      xend = max(FlexCoV_oct_6$FLXMON_std),
      y = fit5$coefficients[1,1] +  (min(FlexCoV_oct_6$FLXMON_std)* fit5$coefficients[2,1]),
      yend = fit5$coefficients[1,1] +  (max(FlexCoV_oct_6$FLXMON_std)* fit5$coefficients[2,1]),
      linetype = "Fitted Values for 37 countries included Tables 1 \nand 3 in the main article"), colour = "#FF0099") +
  geom_ribbon(
    data = FlexCoV_oct_6, 
    aes(ymin = lm5_dat$fit.lwr, 
        ymax = lm5_dat$fit.upr), 
    alpha = 0.2,  fill = "#FF0099") +
  geom_segment(
    aes(
      x = min(FlexCoV_oct_6_true$FLXMON_std),
      xend = max(FlexCoV_oct_6_true$FLXMON_std),
      y = fit6$coefficients[1,1] +  (min(FlexCoV_oct_6_true$FLXMON_std)* fit6$coefficients[2,1]),
     yend = fit6$coefficients[1,1] +  (max(FlexCoV_oct_6_true$FLXMON_std)* fit6$coefficients[2,1]),
     linetype = "Fitted Values for 50 countries\nwith data on flexibility-monumentalism"),  colour = "#0066FF") +
  geom_ribbon(data = FlexCoV_oct_6_true, 
              aes(ymin = lm6_dat$fit.lwr, 
                  ymax = lm6_dat$fit.upr), 
    alpha = 0.2, fill = "#0066FF") +
  geom_point(aes(shape = used2, color = used2),size = 5, alpha = .5) + 
  geom_text_repel(show.legend = FALSE, box.padding = 0.2, point.padding = 0.2, 
                  size = 6, segment.alpha = 0.5, family = "TNR") +
  scale_shape_manual(values=c(16, 17),
    labels = c("Not included in Tables 1\nand 3 in main article due to missing data on covariates",
               "Included in Tables 1 and 3 in main article")) + 
  scale_linetype_manual(name = NULL,  values = c(1,1)) +
  scale_color_manual(
      values=c('#FF0099','#0066FF'),
     labels = c("95% C.I. for 37 countries included in Tables 1\nand 3 in the main article",
               "95% C.I. for 50 countries with\ndata on flexibility-monumentalism")) +
  scale_fill_manual(
    values=c('#FF0099','#0066FF')) +
  scale_x_continuous(limits = c(-2.5, 2.8), breaks = c(-2,-1,0,1,2,3)) +
  scale_y_continuous(limits = c(-2, 8.4), breaks = -2:8) +
 theme_bw() +
  theme(axis.text.x   = element_text(family = "TNR", size = 16),
        axis.text.y   = element_text(family = "TNR", size = 16)) +
  labs(x = "Monumentalism <<<   >>> Flexibility     ",
       y = "Log. number of deaths per million until October 31, 2020",
       shape = "", color = "") +
  theme(axis.title.x = element_text(hjust = 0.45, size = 19, family = "TNR"),
        axis.title.y = element_text(hjust = 0.5, size = 19, family = "TNR"), 
        legend.position = "bottom",
        plot.caption = element_text(family = "TNR", size = 14, hjust = 0)) +
  theme(legend.text=element_text(size=17, family = "TNR", margin = margin(t = 5))) +
  guides(shape = guide_legend(nrow = 2, override.aes=list(color = c('#FF0099','#0066FF'))),
         color = guide_legend(nrow = 2, override.aes=list(shape = 15, alpha = .5, size = 5)),
         linetype = guide_legend(nrow = 2, override.aes=list(color = c('#0066FF', '#FF0099'))))



ggsave(file="./graphs/appendix5.png", width=17, height=13)


#
# Appendix: Figure 6: Robustness check for mediator model II
#

# Subsetting dataset
FlexCoV_oct_7 <-
  FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, FLXMON_std, country_iso3, used1, used2) %>%
  drop_na() 

FlexCoV_oct_7_true <- FlexCoV_oct_7[which(FlexCoV_oct_7$used2==TRUE),]

# Model specification
lm7 <- lm_robust(ln_daily_cum_deaths_million ~ FLXMON_std , data= FlexCoV_oct_7, se_type = "HC3")

lm8 <- lm_robust(ln_daily_cum_deaths_million ~ FLXMON_std , data= FlexCoV_oct_7_true, se_type = "HC3")

# Creating 95% C.I.s
fit7 <- summary(lm7)
fit7$coefficients[2,1]

lm7_dat <- data.frame(lm7$fitted.values, 
                      predict(lm7, newdata = FlexCoV_oct_7,se.fit = TRUE, interval = "confidence"))

fit8 <- summary(lm8)
fit8$coefficients[2,1]

lm8_dat <- data.frame(lm8$fitted.values, 
                      predict(lm8, newdata = FlexCoV_oct_7_true,se.fit = TRUE, interval = "confidence"))

# Plot
options(ggrepel.max.overlaps = Inf)

appendix7 <- FlexCoV_oct %>% 
  dplyr::select(ln_daily_cum_deaths_million, FLXMON_std, country_iso3, used4, used1) %>%
  drop_na() %>% 
  ggplot(mapping = aes(x = FLXMON_std, y = ln_daily_cum_deaths_million, 
                       label = country_iso3))
appendix7 +  
  geom_segment(
    aes(
      x = min(FlexCoV_oct_7$FLXMON_std),
      xend = max(FlexCoV_oct_7$FLXMON_std),
      y = fit7$coefficients[1,1] +  (min(FlexCoV_oct_7$FLXMON_std)* fit7$coefficients[2,1]),
      yend = fit7$coefficients[1,1] +  (max(FlexCoV_oct_7$FLXMON_std)* fit7$coefficients[2,1]),
      linetype = "Fitted Values for 23 countries included in Tables 2 and 4 in the\nmain article (valid data on all covariates, incl. mediators)"), colour = "#FF0099") +
  geom_ribbon(
    data = FlexCoV_oct_7, 
    aes(ymin = lm7_dat$fit.lwr, 
        ymax = lm7_dat$fit.upr), 
    alpha = 0.2,  fill = "#FF0099") +
  geom_segment(
    aes(
      x = min(FlexCoV_oct_7_true$FLXMON_std),
      xend = max(FlexCoV_oct_7_true$FLXMON_std),
      y = fit8$coefficients[1,1] +  (min(FlexCoV_oct_7_true$FLXMON_std)* fit8$coefficients[2,1]),
      yend = fit8$coefficients[1,1] +  (max(FlexCoV_oct_7_true$FLXMON_std)* fit8$coefficients[2,1]),
      linetype = "Fitted Values for 50 countries with\ndata on flexibility-monumentalism"), colour = "#0066FF") +
  geom_ribbon(data = FlexCoV_oct_7_true, 
              aes(ymin = lm8_dat$fit.lwr, 
                  ymax = lm8_dat$fit.upr), 
              alpha = 0.2, fill = "#0066FF") +
  geom_point(aes(shape = used4, color = used4),size = 5, alpha = .5) + 
  geom_text_repel(show.legend = FALSE, box.padding = 0.2, point.padding = 0.2, 
                  size = 6, segment.alpha = 0.5, family = "TNR") +
  scale_shape_manual(values=c(16, 17),
    labels = c("Not inclueded in Tables 2 and 4 in the\nmain article due to missing data on covariates (incl. mediators)",
               "Included in Tables 2 and 4 in main article")) + 
  scale_linetype_manual(name = NULL,  values = c(1,1)) +
  scale_color_manual(
    values=c('#FF0099','#0066FF'),
    labels = c("95% C.I. for 23 countries included in Tables 2 and 4 in the \nmain article (valid data on all covariates, incl. mediators)",
               "95% C.I. for 50 countries with\ndata on flexibility-monumentalism")) +
  scale_fill_manual(
    values=c('#FF0099','#0066FF')) +
  scale_x_continuous(limits = c(-2.5, 2.8), breaks = c(-2,-1,0,1,2,3)) +
  scale_y_continuous(limits = c(-2, 8.4), breaks = -2:8) +
  theme_bw() +
  theme(axis.text.x   = element_text(family = "TNR", size = 16),
        axis.text.y   = element_text(family = "TNR", size = 16)) +
  labs(x = "Monumentalism <<<   >>> Flexibility              ",
       y = "Log. number of deaths per million until October 31, 2020",
       shape = "", color = "") +
  theme(axis.title.x = element_text(hjust = 0.48, size = 19, family = "TNR"),
        axis.title.y = element_text(hjust = 0.5, size = 19, family = "TNR"),
        legend.position = "bottom",
        plot.caption = element_text(family = "TNR", size = 14, hjust = 0)) +
  theme(legend.text=element_text(size=16, family = "TNR", margin = margin(t = 5))) +
  guides(shape = guide_legend(nrow = 2, override.aes=list(color = c('#FF0099','#0066FF')), order = 1),
         color = guide_legend(nrow = 2, override.aes=list(shape = 15, alpha = .5, size = 5)),
         linetype = guide_legend(nrow = 2, override.aes=list(color = c('#0066FF', '#FF0099') , linetype = c(1,1)), order = 2))



ggsave(file="./graphs/appendix6.png", width=19, height=15)


############
#  Tables  #
############

#
# Tables: Figure 1 - alternative.  Flexibility (vs. Monumentalism) and COVID mortality rate as of 31 October 2020.
#

FlexCoV_oct_6_true$region  <- c("Central and South America", "Other", "Europe", "Central and South America",
                                "Other", "Central and South America", "East Asia", "Central and South America",
                                "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Other",
                                "Southeast and South Asia", "Europe", "Other", "Europe", "East Asia",
                                "Other", "East Asia", "Central and South America",
                                "Southeast and South Asia", "Europe", "Central and South America", "Europe", "Europe",
                                "Other", "East Asia", "Europe", "East Asia",
                                "Other", "Europe", "Other", "Southeast and South Asia")

FlexCoV_oct_6_true$region  <- as.factor(FlexCoV_oct_6_true$region)

plot1 <- FlexCoV_oct_6_true %>% 
  dplyr::select(ln_daily_cum_deaths_million, FLXMON_std, country_iso3, used2, used1) %>%
  drop_na() %>%
  ggplot(mapping = aes(x = FLXMON_std, y = ln_daily_cum_deaths_million, label = country_iso3)) +
  geom_point(aes(shape = FlexCoV_oct_6_true$region, color = FlexCoV_oct_6_true$region),size = 4, alpha = .8) +
  geom_text_repel(show.legend = FALSE, box.padding = 0.4, point.padding = 0.2,
                  size = 5, segment.alpha = 0.5, family = "TNR") +
  theme_bw() +
  theme(axis.text.x   = element_text(family = "TNR", size = 14),
       axis.text.y   = element_text(family = "TNR", size = 14)) +
  labs(x = "Monumentalism <<<   >>> Flexibility",
       y = "Log. number of deaths per million until October 31, 2020", 
       caption = "Source of death numbers: Ritchie et al. 2020\nRetrieval date: 20.02.2022",
       shape = "", color = "") +
  theme(axis.title.x = element_text(hjust = 0.47, size = 19, family = "TNR"),
        axis.title.y = element_text(hjust = 0.5, size = 19, family = "TNR"), 
        legend.position = "bottom",
        legend.text=element_text(size=16, family = "TNR"),
        plot.caption = element_text(family = "TNR", size = 14, hjust = 0)) +
  scale_shape_manual(values=c(15, 16, 17, 18, 9)) +
  scale_x_continuous(limits = c(-3.1, 3.1), breaks = c(-3,-2,-1,0,1,2,3))

ggsave(file="./graphs/figure1a.png", width=15, height=10)


#
# Tables: Figure 1 - alternative.  Flexibility (vs. Monumentalism) and COVID mortality rate as of 31 October 2020.
#


FlexCoV_oct_6_true %<>%
  arrange(FLXMON_std)
plot_3 <-
FlexCoV_oct_6_true%>% 
  ggplot(aes(x = FLXMON_std, y = factor(country_iso3, level = c(factor(country_iso3, level = c(country_iso3)))), color = region, shape = region)) +
  geom_point() +
  theme_bw() +
  theme(axis.title.x = element_text(hjust = 0.35, size = 10, family = "TNR"),
        axis.text.x   = element_text(family = "TNR"),
        axis.title.y = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y   = element_text(family = "TNR"), legend.position = "bottom") +
  xlab("Monumentalism <<<   >>> Flexibility") +
  geom_point(aes(shape = region, color = region),size = 2.5) +
  scale_shape_manual(values=c(15, 16, 17, 18, 9)) +
  theme(legend.position = "none") 


FlexCoV_oct_6_true %<>%
  arrange(FLXMON_std)
plot_4 <-
  FlexCoV_oct_6_true%>% 
  ggplot(aes(x = ln_daily_cum_deaths_million, y = factor(country_iso3, level = c(factor(country_iso3, level = c(country_iso3)))), color = region, shape = region)) +
  geom_point() +
  theme_bw() +
  theme(axis.title.x = element_text(hjust = 0.456, size = 10, family = "TNR"),
        axis.text.x   = element_text(family = "TNR"),
        axis.title.y = element_blank(),
        axis.text.y.left= element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y   = element_text(family = "TNR"), legend.position = "bottom") +
  xlab("Log. cum. number of deaths per million pop. as of 31 October 2020") +
  geom_point(aes(shape = region, color = region),size = 2.5) +
  scale_shape_manual(values=c(15, 16, 17, 18, 9)) +
  theme(legend.position = "none")

plot_3 + plot_4 + plot_layout(guides = "collect") & theme(legend.position = 'bottom',
                                                          legend.text=element_text(size=9, family = "TNR"),
                                                          legend.title = element_blank(),
                                                          plot.caption = element_text(family = "TNR"))

ggsave(file="./graphs/figure1b.png", width=13, height=10)











