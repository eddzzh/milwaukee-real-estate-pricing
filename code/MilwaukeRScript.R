library(tidyverse)
library(DataExplorer)
library(GGally)
library(Hmisc)
library(ggpubr)
library(MASS)
library(corrgram)
library(ggExtra)

data <- read.csv("2002-2018-property-sales-data.csv", header=TRUE)
data$Sale_date <- as.character(data$Sale_date)
data$Sale_date <- parse_date(data$Sale_date, format="%Y-%m")
glimpse(data)

data2 <- data %>%
  filter(PropType == "Residential" & Year_Built != 0 & Fin_sqft != 0 & Lotsize != 0 & Sale_price != 0)
glimpse(data2)

data2 <- data2 %>% dplyr::select(-PropType, -Taxkey, -CondoProject, -Nr_of_rms, -Bdrms, -Fbath, -Hbath, -Address)
glimpse(data2)

plot_histogram(data2)

describe(data2)

# Create a sequence of boolean values to indicate the numeric values
data3 <- data %>%
  filter(PropType == "Residential" & Year_Built != 0 & Fin_sqft != 0 & Lotsize != 0 & Sale_price != 0)
numVar = unlist(lapply(data3, is.numeric))
ggcorr(data3[,numVar], label=TRUE, hjust=0.73, label_size=4, label_round=2 , legend.position=c(0.1, 0.8))

ggplot(data2, aes(District, Nbhd)) + geom_point() + geom_smooth(method=lm)

ggplot(data2, aes(Stories, Fin_sqft)) +geom_point() + geom_smooth(method=lm)

numVar2 = unlist(lapply(data2, is.numeric))
corrgram(data2[,numVar2], order=TRUE, lower.panel=panel.ellipse, upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax, main="Correlogram of variables in Milwaukee dataset")

data2 %>%
  dplyr::select(Style, Sale_price) %>%
  mutate(Style = fct_reorder(Style, Sale_price, .fun = "median", .desc = T)) %>% 
  ggplot(aes(Style, Sale_price, fill=Style)) +
  geom_boxplot(alpha=0.8) +
  theme(axis.text.x = element_text(hjust=1, size = 12),
        axis.text.y = element_text(size=10),
        legend.position = "none") +
  coord_flip() +
  ggtitle("Distribution of sale price by Style")

data2 %>%
  dplyr::select(Extwall, Sale_price) %>%
  mutate(Extwall = fct_reorder(Extwall, Sale_price, .fun = "median", .desc = T)) %>% 
  ggplot(aes(Extwall, Sale_price, fill=Extwall)) +
  geom_boxplot(alpha=0.8) +
  theme(axis.text.x = element_text(hjust=1, size = 12),
        axis.text.y = element_text(size=10),
        legend.position = "none") +
  coord_flip() +
  ggtitle("Distribution of sale price by Extwall")


data2 %>%
  dplyr::select(District, Sale_price) %>%
  mutate(District = fct_reorder(factor(District), Sale_price, .fun = "median", .desc = T)) %>% 
  ggplot(aes(District, Sale_price, fill=District)) +
  geom_boxplot(alpha=0.8) +
  theme(axis.text.x = element_text(hjust=1, size = 12),
        axis.text.y = element_text(size=10),
        legend.position = "none") +
  coord_flip() +
  ggtitle("Distribution of sale price by District")

data2 %>%
  dplyr::select(Units, Sale_price) %>%
  ggplot(aes(factor(Units), Sale_price, fill=factor(Units))) +
  geom_boxplot(alpha=0.8) +
  theme(axis.text.x = element_text(hjust=1, size = 12),
        axis.text.y = element_text(size=10),
        legend.position = "none") +
  coord_flip() +
  ggtitle("Distribution of sale price by Units")

data2 %>%
  dplyr::select(Stories, Sale_price) %>%
  mutate(Stories = fct_reorder(factor(Stories), Sale_price, .fun = "median", .desc = T)) %>% 
  ggplot(aes(Stories, Sale_price, fill=Stories)) +
  geom_boxplot(alpha=0.8) +
  theme(axis.text.x = element_text(hjust=1, size = 12),
        axis.text.y = element_text(size=10),
        legend.position = "none") +
  coord_flip() +
  ggtitle("Distribution of sale price by Stories")

#Supress scientific notation
options(scipen=999)

c = ggplot(data2, aes(Sale_price)) +
  geom_histogram(aes(y=..density..), bins=40) +
  scale_x_continuous(labels=function(x){x/10^6})+
  geom_density() + ggtitle("Histogram of raw Sale_price")
c1 = data2 %>%
  ggplot(aes(sample=Sale_price)) + stat_qq() + stat_qq_line() + ggtitle("QQ plot of raw Sale_price")
c2 = data2 %>%
  ggplot(aes(sample=sqrt(Sale_price))) + stat_qq() + stat_qq_line() + ggtitle("QQ plot of SQRT(Sale_price)")
c3 = data2 %>%
  ggplot(aes(sample=log(Sale_price))) + stat_qq() + stat_qq_line() + ggtitle("QQ plot of LOG(Sale_price)")
ggarrange(c, c1, c2, c3, ncol=2, nrow=2)

d = ggplot(data2, aes(Fin_sqft)) +
  geom_histogram(aes(y=..density..), bins=40) +
  scale_x_continuous(labels=function(x){x/10^6})+
  geom_density() + ggtitle("Histogram of raw Fin_sqft")
d1 = data2 %>%
  ggplot(aes(sample=Fin_sqft)) + stat_qq() + stat_qq_line() + ggtitle("QQ plot of raw Fin_sqft")
d2 = data2 %>%
  ggplot(aes(sample=sqrt(Fin_sqft))) + stat_qq() + stat_qq_line() + ggtitle("QQ plot of SQRT(Fin_sqft)")
d3 = data2 %>%
  ggplot(aes(sample=log(Fin_sqft))) + stat_qq() + stat_qq_line() + ggtitle("QQ plot of LOG(Fin_sqft)")
ggarrange(d, d1, d2, d3, ncol=2, nrow=2)


# Raw box-cox of linear model of price and size
b1 <- boxcox(lm(Sale_price~log(Fin_sqft), data = data2))
round(b1$x[which(b1$y == max(b1$y))], 2)



# box-cox of linear model of sqrt(price) and size
b2 <- boxcox(lm(sqrt(Sale_price)~log(Fin_sqft), data = data2))
round(b2$x[which(b2$y == max(b2$y))], 2)



# box-cox of linear model of sqrt(price) and sqrt(size)
b3 <- boxcox(lm(log(Sale_price)~log(Fin_sqft), data = data2))
round(b3$x[which(b3$y == max(b3$y))], 2)



b4 <- boxcox(lm(sqrt(Sale_price)~Fin_sqft, data = data2))
round(b4$x[which(b4$y == max(b4$y))], 2)

b5 <- boxcox(lm(sqrt(Sale_price)~sqrt(Fin_sqft), data = data2))
round(b5$x[which(b5$y == max(b5$y))], 2)

b6 <- boxcox(lm(log(Sale_price)~sqrt(Fin_sqft), data = data2))
round(b6$x[which(b6$y == max(b6$y))], 2)

a1 = ggplot(data2, aes(x=Fin_sqft, y=Sale_price)) + 
  geom_point(alpha=0.4, size=0.3) + geom_smooth(method=lm) + 
  ggtitle("Sale_price vs. Fin_sqft")
a2 = ggplot(data2, aes(x=Fin_sqft, y=sqrt(Sale_price))) + 
  geom_point(alpha=0.4, size=0.3) + geom_smooth(method=lm) + 
  ggtitle("SQRT(Sale_price) vs. Fin_sqft")
a3 = ggplot(data2, aes(x=Fin_sqft, y=sqrt(Sale_price))) + 
  geom_point(alpha=0.4, size=0.3) + geom_smooth(method=lm) + 
  ggtitle("SQRT(Sale_price) vs. SQRT(Fin_sqft)")
a4 = ggplot(data2, aes(x=Fin_sqft, y=sqrt(Sale_price))) + 
  geom_point(alpha=0.4, size=0.3) + geom_smooth(method=lm) + 
  ggtitle("SQRT(Sale_price) vs. LOG(Fin_sqft)")
ggarrange(a1, a2, a3, a4, nrow=2, ncol=2)


m1 = lm(sqrt(Sale_price)~Fin_sqft, data = data2)
summary(m1)
AIC(m1)
b1.1 <- boxcox(m1)
cat("\nBox-cox lamda:  ", b1.1$x[which.max(b1.1$y)])

m2 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize, data = data2)
summary(m2)
cat("AIC:            ", AIC(m2))
b7 <- boxcox(m2)
cat("\nBox-cox lamda:  ", b7$x[which.max(b7$y)])

m3 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize + Year_Built, data = data2)
summary(m3)
cat("AIC:            ", AIC(m3))
b8 <- boxcox(m3)
cat("\nBox-cox lamda:  ", b8$x[which.max(b8$y)])

m4 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize + Year_Built + factor(District), data = data2)
summary(m4)
cat("AIC:            ", AIC(m4))
b9 <- boxcox(m4)
cat("\nBox-cox lamda:  ", b9$x[which.max(b9$y)])

m5 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize + Year_Built + factor(District) + factor(Units), data = data2)
summary(m5)
cat("AIC:            ", AIC(m5))
b10 <- boxcox(m5)
cat("\nBox-cox lamda:  ", b10$x[which.max(b10$y)])

m5.1 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize + Year_Built + factor(District) + factor(Units) + factor(Extwall), 
          data = data2)
summary(m5.1)
cat("AIC:            ", AIC(m5.1))
b10.1 <- boxcox(m5.1)
cat("\nBox-cox lamda:  ", b10.1$x[which.max(b10.1$y)])

e1 = ggplot(data2, aes(x=Fin_sqft, y=sqrt(Sale_price), color=Style)) +
  geom_point(alpha=0.4, size=0.4) +
  geom_smooth(formula=y~x, method=lm, se=F) +
  ggtitle("Interaction effect - SQRT(price) vs. size by Styles") +
  xlab("Size (sq. ft.)") + ylab("SQRT(price)") +
  scale_color_manual(values = c('#00429d', '#2b57a7', '#426cb0', '#5681b9', '#6997c2', '#7daeca', '#93c4d2', '#abdad9', '#caefdf', '#e1e14d', '#eac341', '#eca639', '#ea8833', '#e36b2f', '#d74e2f', '#c63130', '#b01334', '#93003a')) +
  theme(legend.position="left")
ggMarginal(e1, type="density")

e2 = ggplot(data2, aes(x=Fin_sqft, y=sqrt(Sale_price), color=factor(Units))) +
  geom_point(alpha=0.4, size=0.4) +
  geom_smooth(formula=y~x, method=lm, se=F) +
  ggtitle("Interaction effect - SQRT(price) vs. size by Units") +
  xlab("Size (sq. ft.)") + ylab("SQRT(price)") +
  scale_color_manual(values = c('#00429d', '#4771b2', '#73a2c6', '#a5d5d8', '#ffffe0', '#ebbc3f', '#e77931', '#cb3830', '#93003a')) +
  theme(legend.position="left")
ggMarginal(e2, type="density")

e3 = ggplot(data2, aes(x=Fin_sqft, y=sqrt(Sale_price), color=factor(District))) +
  geom_point(alpha=0.4, size=0.4) +
  geom_smooth(formula=y~x, method=lm, se=F) +
  ggtitle("Interaction effect - SQRT(price) vs. size by District") +
  xlab("Size (sq. ft.)") + ylab("SQRT(price)") +
  scale_color_manual(values = c('#00429d', '#325da9', '#4e78b5', '#6694c1', '#80b1cc', '#9dced6', '#c0eade', '#ffffe0', '#e4d849', '#ecb23c', '#ea8c33', '#e1672f', '#d0412f', '#b71c32', '#93003a')) +
  theme(legend.position="left")
ggMarginal(e3, type="density")

e4 = ggplot(data2, aes(x=Fin_sqft, y=sqrt(Sale_price), color=factor(Stories))) +
  geom_point(alpha=0.4, size=0.4) +
  geom_smooth(formula=y~x, method=lm, se=F) +
  ggtitle("Interaction effect - SQRT(price) vs. size by Stories") +
  xlab("Size (sq. ft.)") + ylab("SQRT(price)") +
  scale_color_manual(values = c('#00429d', '#5681b9', '#93c4d2', '#eca639', '#d74e2f', '#93003a')) +
  theme(legend.position="left")
ggMarginal(e4, type="density")

m6 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize + Year_Built + factor(Units) + 
          Fin_sqft*factor(District), data = data2)
summary(m6)
cat("AIC:            ", AIC(m6))
b11 <- boxcox(m6)
cat("\nBox-cox lamda:  ", b11$x[which.max(b11$y)])

m7 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize + Year_Built + factor(Units) + 
          Fin_sqft*factor(District) + Fin_sqft*factor(Stories), data = data2)
summary(m7)
cat("AIC:            ", AIC(m7))
b12 <- boxcox(m7)
cat("\nBox-cox lamda:  ", b12$x[which.max(b12$y)])

m8 = lm(sqrt(Sale_price)~Fin_sqft + Lotsize + Year_Built + factor(Units) + 
          Fin_sqft*factor(District) + Fin_sqft*factor(Stories) + Fin_sqft*Style, data = data2)
summary(m8)
cat("AIC:            ", AIC(m8))
b13 <- boxcox(m8)
cat("\nBox-cox lamda:  ", b13$x[which.max(b13$y)])

par(mfrow = c(2,2))
plot(m5)