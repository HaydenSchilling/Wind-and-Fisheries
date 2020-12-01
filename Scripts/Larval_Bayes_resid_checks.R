library(tidyverse)
library(brms)
library(tidybayes)
library(gridExtra)

fit5 <- readRDS("Coastal 14 day brms modelv3_dist_stand.rds")
fish_data <- read_csv("Fish_data_final_larval_for_katana.csv")

### Residual plots
m6.residual <- data.frame(residuals(fit5,summary = TRUE))
m6.fitted <- data.frame(fitted(fit5, summary = TRUE))
m6.predicted <- data.frame(predict(fit5,summary = TRUE))
coef(fit5)

m6.re <- cbind(m6.residual,m6.fitted,m6.predicted)
colnames(m6.re) <- c("Residual","residual.Error","residual.Q2.5","residual.Q97.5",
                     "Fitted","fitted.Error","fitted.Q2.5","fitted.Q97.5",
                     "Predicted","predicted.Error","predicted.Q2.5","predicted.Q97.5")
m6.re <- cbind(fish_data,m6.re)

# Basic version of observed vs fitted values#
ggplot(m6.re, aes(x = Fitted, y = Coastal_Normalised_Abund)) + 
  geom_point() + 
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cs', k = 5), se = FALSE, size = 1.25,colour="black") +
  xlab('Fitted age-0+ Atlantic salmon density') +
  ylab('Observed age-0+ Atlantic salmon density')+ theme_classic() +
     theme(axis.title = element_text(size=12, face = "bold"),
           axis.text = element_text(size = 10, colour = "black")) #+
#style

#ggsave("Figure_Sal_observed vs fitted.png",width = 17, height = 9)


#Density histogram of residuals
hist_p <- ggplot(m6.re, aes(x = Residual)) + 
  geom_histogram(aes(y = ..density..), bins = 15, fill = 'grey', colour = 'black') + 
  geom_line(aes(y = ..density..), stat = 'density') +
  xlab('Residual') +
  ylab('Density')+ theme_classic() +
     theme(axis.title = element_text(size=12, face = "bold"),
           axis.text = element_text(size = 10, colour = "black")) #+
#style
hist_p

#ggsave("Figure_Sal_density histogram of residuals.png",width = 17, height = 9)

#QQ plot#
QQ_p <- fish_data %>%
  add_residual_draws(fit5) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line() +
  xlab('Theoretical quantiles') +
  ylab('Sample quantiles')+ theme_classic() +
     theme(axis.title = element_text(size=12, face = "bold"),
           axis.text = element_text(size = 10, colour = "black"))# +
# #style

# qq_plot <- fish_data %>%
#   add_predicted_draws(fit5) %>%
#   summarise(
#     p_residual = mean(.prediction < Coastal_Normalised_Abund),
#     z_residual = qnorm(p_residual)
#   ) %>%
#   ggplot(aes(sample = z_residual)) +
#   geom_qq() + ylab("Sample") + xlab("Theoretical")+
#   geom_abline() + theme_classic() +
#   theme(axis.title = element_text(size=12, face = "bold"),
#         axis.text = element_text(size = 10, colour = "black"))

#ggsave("Figure_Sal_QQ_Plot.png",width = 17, height = 9)

#Residuals vs fitted
Resid_p <- ggplot(m6.re, aes(x = Fitted, y = Residual)) + 
  geom_point() + 
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = 'cs', k = 5), se = FALSE, size = 1.25,colour="black") +
  xlab('Fitted value') +
  ylab("Pearson's residuals") +
  theme(axis.title.x=element_text(vjust=1.8))+ theme_classic() +
     theme(axis.title = element_text(size=12, face = "bold"),
           axis.text = element_text(size = 10, colour = "black")) #+
#style
Resid_p
#ggsave("Figure_Sal_residuals vs fitted.png",width = 17, height = 9)

#acf#
rs <- data.frame('r' = resid(fit5),'f.' = fitted(fit5))

require(ggfortify)
acf_p <- autoplot(acf(rs$r.Estimate)) + 
  ylab("Autocorrelation function") +
  ggtitle("") + 
  theme(axis.title.x=element_text(vjust=1.8)) +
  geom_hline(yintercept = 0)+ theme_classic() +
     theme(axis.title = element_text(size=12, face = "bold"),
           axis.text = element_text(size = 10, colour = "black")) #+
#style

ggsave("Larval_Resid_Checking.png", arrangeGrob(hist_p, QQ_p, Resid_p, acf_p, ncol = 2,nrow = 2),width = 9.5, height = 7.5)
dev.off()