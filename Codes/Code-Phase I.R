library(magrittr) 
library(ggfortify)
library(ggplot2)
library(plyr)
library(gridExtra)
require(qqplotr)
library(moments)
library(hexbin)
library(ggmosaic)
library("plot3D")
library(plotly)
library(scatterplot3d)
library(RNHANES)
library(GGally)
library(dplyr)
library(Hmisc)
require(ggpubr)
require(Hmisc)
require(corrplot)
library(patchwork)
library(ggExtra)


theme_set(theme_minimal())

summary(StudentsPerformance)

#Question 0

missingvalues <- colSums(is.na.data.frame(StudentsPerformance))
missingvalues.proporion <- missingvalues/nrow(StudentsPerformance)
plot(missingvalues.proporion , main = "Percentage of missing values vs. variables", 
     xlab = "variables", ylab = "Missing values proportion" , type = 'l', col = 'thistle')

missingvalues.proporion <- data.frame("missing value", missingvalues/nrow(StudentsPerformance))


#QUESTION 1

#Numerical value chosen : Grade 1

StudentsPerformance$G1

#a.
breaks <- pretty(StudentsPerformance$G1, n = nclass.FD(StudentsPerformance$G1), min.n = 0)
bwidth <- breaks[2]-breaks[1]


G1_hist <- ggplot(StudentsPerformance, aes(x = G1)) +
  geom_histogram(aes(y=..density..) , binwidth = bwidth, alpha = 0.4, color="thistle1", fill="thistle1")  +
  geom_density(color = "gray87",linetype="dashed", fill = "gray87" , alpha = 0.3, size=1)  + 
  #stat_function(fun = dnorm, n = 101, args = list(mean = mu, sd = std) , color = "gray87" , size=1) +
  labs(title = "Histogram for G1", x = "Score", y="Density")


G1_hist
#----

#b.
G1.qq <- ggplot(StudentsPerformance, aes(sample = G1, color = "", alpha = 0.7)) + geom_qq() +
  geom_qq_line() + labs(title="QQ-plot for G1")

G1.qq + theme(legend.position="none") + scale_color_manual(values=c("thistle2"))
#----

#c.
print(skewness(StudentsPerformance$G1))

G1_hist + geom_vline(xintercept = mean(StudentsPerformance$G1), linetype="dashed", color = "thistle4", size = 0.5) +
  geom_vline(xintercept = median(StudentsPerformance$G1), linetype="dotdash", color = "gray29", size = 0.5)+
  annotate("text", x = mu - .2 , label = "mean", y = 0.01, size = 3.4, angle = 90 , color = 'thistle4') +
  annotate("text", x = median + 0.1 , label = "median", y = 0.06, size = 3.4, angle = 90, color = 'gray29')

#----

#d.
G1_box <- ggplot(StudentsPerformance, aes(x = G1)) + geom_boxplot(color ="thistle2", fill ="thistle2", alpha = 0.5) +
  labs(title="Boxplot for G1")

G1_box + theme(legend.position="none")
#----

#e.
mu <- mean(StudentsPerformance$G1)
median <- median(StudentsPerformance$G1)
var <- var(StudentsPerformance$G1)
std <- sd(StudentsPerformance$G1)
#----

#f.
G1_density <- ggplot(StudentsPerformance, aes(x = G1)) +
  geom_vline(xintercept = mu, linetype="dashed", color = "gray29") +
  geom_vline(xintercept = median, linetype="dashed", color = "thistle4") +
  geom_density(color = "thistle1", size = 1) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mu, sd = std) , color = "thistle2", size = 1) +
  annotate("text", x = mu - .2 , label = "mean", y = 0.01, size = 3.4, angle = 90 , color = 'gray29') +
  annotate("text", x = median + 0.1 , label = "median", y = 0.06, size = 3.4, angle = 90, color = 'thistle4')+
  labs(title="Density for G1")
  
G1_density
#----

#g.

#method1
StudentsPerformance$categorizedG1 <- ifelse(StudentsPerformance$G1 > (mu + max(StudentsPerformance$G1))/2, 'very high', ifelse(StudentsPerformance$G1 > mu, 'high', ifelse(StudentsPerformance$G1 > mu/2, 'low', 'very low')))

freq_vlow <-length(which(StudentsPerformance[17] == 'very low')) / length(StudentsPerformance$G1)
freq_low <- length(which(StudentsPerformance[17] == 'low'))/ length(StudentsPerformance$G1)
freq_high <- length(which(StudentsPerformance[17] == 'high'))/ length(StudentsPerformance$G1)
freq_vhigh <- length(which(StudentsPerformance[17] == 'very high'))/ length(StudentsPerformance$G1)


G1.categorized <- data.frame(group = c("Very Low", "Low", "High", "Very High"),
                   value = c(freq_vlow, freq_low, freq_high, freq_vhigh))



G1.pie <- ggplot(G1.categorized, aes(x="", y = value, fill = group)) +
  geom_bar(stat = "identity", alpha = 0.7) + coord_polar("y")


G1.pie  + scale_fill_manual(values = c("thistle1", "thistle2", "thistle3", "thistle4")) +
  geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5)) +
  labs(title="Pie-chart of G1", x = 'Frequency', y = 'G1')

#method2

G1.quant <- quantile(StudentsPerformance$G1)

StudentsPerformance$categorizedG1 <- ifelse(StudentsPerformance$G1 > G1.quant[[4]], 'very high', ifelse(StudentsPerformance$G1 >  G1.quant[[3]], 'high', ifelse(StudentsPerformance$G1 >  G1.quant[[2]], 'low', 'very low')))

freq_vlow <-length(which(StudentsPerformance[17] == 'very low')) / length(StudentsPerformance$G1)
freq_low <- length(which(StudentsPerformance[17] == 'low'))/ length(StudentsPerformance$G1)
freq_high <- length(which(StudentsPerformance[17] == 'high'))/ length(StudentsPerformance$G1)
freq_vhigh <- length(which(StudentsPerformance[17] == 'very high'))/ length(StudentsPerformance$G1)


G1.categorized <- data.frame(group = c("Very Low", "Low", "High", "Very High"),
                             value = c(freq_vlow, freq_low, freq_high, freq_vhigh))



G1.pie <- ggplot(G1.categorized, aes(x="", y = value, fill = group)) +
  geom_bar(stat = "identity", alpha = 0.7) + coord_polar("y")


G1.pie  + scale_fill_manual(values = c("thistle1", "thistle2", "thistle3", "thistle4")) +
  geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5)) +
  labs(title="Pie-chart of G1", x = 'Frequency', y = 'G1')


#----

#h.

boxplot.stats(StudentsPerformance$G1)

G1.quant <- quantile(StudentsPerformance$G1)
G1.iqr <- IQR(StudentsPerformance$G1)
#----
#--------------------------------------------

#QUESTION 2

#Categorical Variable chosen : sex

StudentsPerformance$sex

#a.
female.freq <- length(((StudentsPerformance %>% filter(sex == 'F'))$sex)) / length(StudentsPerformance$sex)
male.freq <- length(((StudentsPerformance %>% filter(sex == 'M'))$sex)) / length(StudentsPerformance$sex)
#----

#StudentsPerformance.se1 <- subset(StudentsPerformance, sex == "F")


#b.

#freq <-data.frame(female.freq, male.freq)
sex.barplot <- ggplot(StudentsPerformance, aes(x = " ", color = sex, fill = sex)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.5, width = 0.5) + labs(title="Stacked Barplot of sex", y = 'Frequency')

sex.barplot + scale_color_manual(values = c("thistle1", "gray67")) + xlab("sex") +
  scale_fill_manual(values = c("thistle1", "gray67")) + scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", hjust = 0.5, size = 4.5, color = 'black', vjust = 1.4, position = position_dodge(width = 0.3)) 
#----

#c.
categorizedsex.barplot <- ggplot(StudentsPerformance, aes(x = sex, color = sex, fill = sex)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.7) + labs(title="Barplot of sex", y = 'Frequency')

categorizedsex.barplot + scale_color_manual(values = c("thistle1", "gray67")) + 
  scale_fill_manual(values =c("thistle1", "gray67")) + coord_flip() + scale_x_discrete(limits=c("M", "F"))+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = round(((..count..)/sum(..count..)), 3)), 
            stat = "count", vjust = -0.25,  size = 4, color = 'black') + theme(axis.text.x=element_blank())
#----

#d.
sex.df <- data.frame(sex = c("F", "M"), frequency = c(female.freq, male.freq))
sex.violinplot <- ggplot(StudentsPerformance, aes(x = sex, y = age, color = sex, fill = sex)) + 
  geom_violin( trim=FALSE, alpha = 0.7) + labs(title="Violinplot")

sex.violinplot+ scale_color_manual(values = c("thistle1", "gray67")) + xlab("sex") +
  scale_fill_manual(values = c("thistle1", "gray67"))
#----
#--------------------------------------------

#QUESTION 3

#Numerical Variable chosen : goout and absences -> it actually depends


#b.
goout_absences.scatterplot <- ggplot(StudentsPerformance, aes(x = goout, y = absences)) + 
  geom_point(color = "thistle2", size = 2) 

goout_absences.scatterplot + labs(title="Scatterplot of Going out and Absences")
#----

#c.
goout_absences.correlation <- cor(StudentsPerformance$goout, StudentsPerformance$absences)
goout_absences.correlation
#----

#c.
ggscatter(StudentsPerformance, x = "goout", y = "absences", shape = 12, add = "reg.line", conf.int = TRUE, 
          color = "thistle2", add.params = list(color = "thistle3", fill = "gray90"), cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")) + theme_minimal() +
  labs(title="Scatterplot")

#----

#f.
goout_absences_romantic.scatterplot <- ggplot(StudentsPerformance, 
                                              aes(x = goout, y = absences, color = romantic, shape = romantic)) + 
  geom_point(size = 2) + labs(title="Scatterplot")

goout_absences_romantic.scatterplot +  scale_shape_manual(values = c(4, 16)) + 
  scale_color_manual(values=c('thistle4','thistle2')) 
#----

#g.
breaks <- pretty(StudentsPerformance$goout, n = nclass.FD(StudentsPerformance$goout), min.n = 0)
goout.hist <- ggplot(StudentsPerformance, aes(x = goout)) + geom_histogram(binwidth = breaks[2]-breaks[1],color = "thistle2", fill = "thistle2", alpha = 0.5) +theme_void()

breaks <- pretty(StudentsPerformance$absences, n = nclass.FD(StudentsPerformance$absences), min.n = 0)
absences.hist <- ggplot(StudentsPerformance, aes(x = absences)) + 
  geom_histogram(binwidth = breaks[2]-breaks[1], color = "thistle2", fill = "thistle2", alpha = 0.5) + coord_flip() + theme_void()


gar.hexbinplot.log <- ggplot(StudentsPerformance, aes(x = goout, y = log2(absences + 1))) + 
  geom_hex(bins = 10, color = "white", alpha = 0.7) + scale_fill_gradient(low =  "thistle1", high = "thistle4" ,trans="log10") 
#+ geom_smooth(col = 'grey40') 

goout.hist + plot_spacer() + gar.hexbinplot.log + absences.hist + 
  plot_layout( ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4)) 

gar.hexbinplot <- ggplot(StudentsPerformance, aes(x = goout, y = absences)) + 
  geom_hex(bins = 10, color = "white", alpha = 0.7) + scale_fill_gradient(low =  "thistle1", high = "thistle4") + 
  geom_smooth(method = "loess", col = 'grey40') 

goout.hist + plot_spacer() + gar.hexbinplot + absences.hist + 
  plot_layout( ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4)) 


#----

#h.
gar.2ddensity <- ggplot(StudentsPerformance, aes(x = goout, y = G1)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) + lims(x = c(0,6), y = c(-5, 20)) 

gar.2ddensity + scale_fill_gradient(low =  "thistle1", high = "thistle4") + 
  labs(title="2D-density plot")
#----
#--------------------------------------------

#QUESTION 4

#a.
ggpairs(dplyr::select_if(StudentsPerformance, is.numeric), title = "Correlogram")


#density, without failure
ggpairs(StudentsPerformance[, c(4, 7, 10, 12, 13, 14, 15, 16)],
        upper = list(continuous = wrap("density", colour="thistle")),
        lower = list(continuous = wrap("points", colour="grey70")))

#linear relationship
ggpairs(StudentsPerformance[, c(4, 7, 10, 11, 12, 13, 14, 15, 16)],
        upper = list(continuous = wrap("smooth", colour="thistle")),
        lower = list(continuous = wrap("points", colour="grey70")))
#barplot 
ggpairs(StudentsPerformance[, c(4, 7, 10, 11, 12, 13, 14, 15, 16)],
        upper = list(continuous = wrap("barDiag", colour="thistle", fill = "thistle", alpha = 0.5)),
        lower = list(continuous = wrap("points", colour="grey70")))
#boxplot
ggpairs(StudentsPerformance[, c(4, 7, 10, 11, 12, 13, 14, 15, 16)],
        upper = list(continuous = wrap("box_no_facet", colour="thistle", fill = "thistle3", alpha = 0.5)),
        lower = list(continuous = wrap("points", colour="grey70")))

#----
#b.

col <- colorRampPalette(c("grey80", "white", "thistle1", "thistle2"))
StudentsPerformance.corr <- rcorr(as.matrix(dplyr::select_if(StudentsPerformance, is.numeric)))
StudentsPerformance.corr.p <- StudentsPerformance.corr$P
StudentsPerformance.corr.p[is.na(StudentsPerformance.corr.p)] <- 1

M <- cor(dplyr::select_if(StudentsPerformance, is.numeric))

corrplot(M, method = "color", col = col(200), type = "upper", order = "hclust", addCoef.col = "black", 
         tl.col = "thistle4", tl.srt = 45, p.mat = StudentsPerformance.corr.p, sig.level = 0.05, diag = FALSE)
#----

#c.
cols <- c("thistle2", "grey50")

with(StudentsPerformance, scatterplot3d(age, health, failures, main="3D scatterplot",
                   pch = 16, color = cols[as.numeric(StudentsPerformance$sex)]))

legend("right", legend = levels(StudentsPerformance$sex),
       col =  c("thistle2", "grey50"), pch = 16)

#--------------------------------------------

#Question 5 

#Chosen : sex and romantic 
#a.
table <- addmargins(table(StudentsPerformance$romantic, StudentsPerformance$sex), c(1,2))
print.table(table)
#----


#b.

romantic_sex.groupedbarplot <- ggplot(StudentsPerformance, aes(x = romantic,color = sex, fill = sex)) +
  geom_bar(position = "dodge", alpha = 0.5) + labs(title="Grouped barplot", x="romantic")

romantic_sex.groupedbarplot + scale_color_manual(values = c("thistle1", "gray67")) + 
  scale_fill_manual(values = c("thistle1", "gray67")) +
  geom_text(aes(y = ..count.., label = ..count..), stat = "count", vjust = -0.25,  size = 4, color = 'black', position = position_dodge(width = 1))


romantic_sex.groupedbarplot <- ggplot(StudentsPerformance, aes(x = romantic,color = sex, fill = sex)) +
  geom_bar(alpha = 0.5, width = 0.5) + labs(title="Segmented barplot", x="romantic")

romantic_sex.groupedbarplot + scale_color_manual(values = c("thistle2", "gray68")) + 
  scale_fill_manual(values = c("thistle1", "gray67")) +
  annotate("text", x = 1 , label = "134", y = 70, size = 4, angle = 0 , color = 'gray29') +
  annotate("text", x = 1 , label = "129", y = 200, size = 4, angle = 0 , color = 'gray29') +
  annotate("text", x = 2 , label = "53", y = 25, size = 4, angle = 0 , color = 'gray29') +
  annotate("text", x = 2 , label = "79", y = 90, size = 4, angle = 0 , color = 'gray29')


romantic_sex.mosaicplot <- ggplot(StudentsPerformance) +
  geom_mosaic(aes(x = product(romantic), fill = sex), alpha = 0.5) + labs(title="Mosaicplot", y = "Frequenct") + 
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x = 0.33 , label = "49%", y = .25, size = 4, angle = 0 , color = 'gray29') +
  annotate("text", x = 0.33 , label = "51%", y = .75, size = 4, angle = 0 , color = 'gray29') +
  annotate("text", x = 0.83 , label = "62%", y = .3, size = 4, angle = 0 , color = 'gray29') +
  annotate("text", x = 0.83 , label = "38%", y = .8, size = 4, angle = 0 , color = 'gray29') 



romantic_sex.mosaicplot + scale_fill_manual(values = c("thistle1", "gray67")) 

#----
#--------------------------------------------

#Question 6
#Chosen Numerical Variable: age

breaks <- pretty(StudentsPerformance$goout, n = nclass.FD(StudentsPerformance$goout), min.n = 0)
bwidth <- breaks[2]-breaks[1]


goout_hist <- ggplot(StudentsPerformance, aes(x = goout)) +
  geom_histogram(binwidth = bwidth, alpha = 0.4, color="thistle1", fill="thistle1")  +
  labs(title = "Histogram for goout", x = "Score", y="Density")
goout_hist

#a.
CI.calculate <- function(data.sampled, alpha = 0.05){
  
  sample.len <- length(data.sampled)
  
  mu <- mean(data.sampled)
  s <- sd(data.sampled)
  SE <- s/sqrt(sample.len)
  
  if(sample.len > 30){
    print("Using Z-distribution")
    Zstar <- abs(qnorm(alpha/2))
    error.margin <- Zstar * SE}
  
  else{
    print("Using t-distribution")
    tstar <- abs(qt(alpha/2, df = sample.len - 1))
    error.margin <- tstar * SE }
  confidence.interval <- c(mu - error.margin, mu + error.margin)
  return(confidence.interval)
  
}

goout.sampled.t <- sample(StudentsPerformance$goout, 25)
confidence.interval.t <- CI.calculate(goout.sampled.t)
print(paste("Confidence Interval(using t-test) : (", round(confidence.interval.t[1], 3),",",round(confidence.interval.t[2],3),")"))


goout.sampled <- sample(StudentsPerformance$goout, 200)
confidence.interval <- CI.calculate(age.sampled) 
print(paste("Confidence Interval(using z-test) : (", round(confidence.interval[1], 3), ",", round(confidence.interval[2], 3),")"))
#----

#c.

goout.hist <- ggplot(StudentsPerformance, aes(x = goout)) +
  geom_histogram(binwidth = bwidth, alpha = 0.2, color="thistle1", fill="thistle1") +
  labs(title = "Histogram", x = "goout") +
  geom_vline(xintercept =  mean(StudentsPerformance$goout), color = "thistle3", linetype="21", size = 0.8) +
  geom_vline(xintercept =  mean(goout.sampled), color = "thistle2", linetype="dotdash", size = 0.8) +
  annotate("text", x = mean(StudentsPerformance$goout) + 0.03, label = "mean", y = 90, size = 5 , angle = 90,  color = "thistle3") +
  annotate("text", x = mean(goout.sampled) - 0.07, label = "sample mean", y = 70, size = 5 , angle = 90,  color = "thistle2") 

goout.hist


goout.hist <- ggplot(StudentsPerformance, aes(x = goout)) +
  geom_histogram(binwidth = bwidth, alpha = 0.2, color="thistle1", fill="thistle1") +
  labs(title = "Histogram", x = "goout") +
  geom_vline(xintercept =  mean(StudentsPerformance$goout), color = "thistle3", linetype="21", size = 0.8) +
  geom_vline(xintercept =  mean(goout.sampled), color = "thistle2", linetype="21", size = 0.8) +
  geom_vline(xintercept =  round(confidence.interval[1], 3), color = "grey70", linetype="dotdash", size = 1) +
  geom_vline(xintercept =  round(confidence.interval[2], 3), color = "grey70", linetype="dotdash", size = 1) +
  annotate("text", x = mean(StudentsPerformance$goout) + 0.03, label = "mean", y = 90, size = 5 , angle = 90,  color = "thistle3") +
  annotate("text", x = mean(goout.sampled) - 0.07, label = "sample mean", y = 70, size = 5 , angle = 90,  color = "thistle2") 

goout.hist



breaks <- pretty(goout.sampled, n = nclass.FD(goout.sampled), min.n = 0)
bwidth <- breaks[2]-breaks[1]

goout.df <- data.frame(goout.sampled) 
sampled.goout.hist <- ggplot(goout.df, aes(x = goout.sampled)) +
  geom_histogram(binwidth = bwidth, alpha = 0.3, color="thistle1", fill="thistle1") +
  labs(title = "Histogram for Sampled goout", x = "goout") +
  geom_vline(xintercept =  mean(goout.sampled), color = "thistle3", linetype="dotdash", size = 0.5) +
  geom_vline(xintercept =  confidence.interval[1], color = "thistle2", linetype="22", size = 1) +
  geom_vline(xintercept =  confidence.interval[2], color = "thistle2", linetype="22", size = 1) +
  annotate("text", x = mean(goout.sampled) - 0.07, label = " sample mean", y = 50, size = 4 , angle = 90,  color = "thistle3")


sampled.goout.hist


#----

#d.
Hypothesis.test <- function(data.sampled, null.value, alpha = 0.05){
  
  sample.len <- length(data.sampled)
  print(paste("Null Hypothesis: mean = ", null.value))
  print(paste("Alternative Hypothesis: mean /= ", null.value))
  
  x_bar <- mean(data.sampled)
  s <- sd(data.sampled)
  SE <- s/sqrt(sample.len)
  score <- abs((x_bar - null.value)) / SE
  
  
  if(sample.len > 30){
    print("Using Z-distribution")
    pvalue <- 2*pnorm(score, lower.tail = FALSE)}
  
  else{
    print("Using t-distribution")
    pvalue <- 2*pt(score, df = sample.len - 1, lower.tail = FALSE)}
  
  
  print(paste("p-value =", pvalue))
  
  if (pvalue < alpha) 
    print("Reject Null Hypothesis.")
  else 
    print("Fail to Reject Null Hypothesis.")
  
}
mean(goout.sampled)

Hypothesis.test(goout.sampled.t, null.value = 2.8)

Hypothesis.test(goout.sampled, null.value = 2.8)
#----

#f.and #g.
TypeIIerr <- function(data.sampled, null.value, alpha = 0.05){
  sample.len <- length(data.sampled)
  mean.actual <- mean(StudentsPerformance$goout)
  s <- sd(data.sampled)
  SE <- s/sqrt(sample.len)
  ME <- abs(qnorm((alpha/2))) * SE
  errorTypeII <- pnorm(abs(null.value + ME - mean.actual)/SE, lower.tail = F) + 
    pnorm(abs(null.value - ME - mean.actual)/SE, lower.tail = F)
  
  print(paste("TypeII error = %", 100*round(errorTypeII,3)))
  print(paste("Power = %", 100*round(1-errorTypeII,3)))

}

TypeIIerr(goout.sampled, null.value = 2.8)

power.t.test(n = 200, delta = mean(StudentsPerformance$goout) - 2.8, sd = sd(goout.sampled), type="one.sample", alternative="two.sided")




differences <- seq(from = 0,to = 1.5,by = 0.1)
power.effect <- sapply(differences, function(d){power.t.test(n = 200, delta = d, sd = sd(goout.sampled), type="one.sample")}$power)

df <- data.frame(differences, power.effect)

ggplot(data = df, aes(x = differences, y = power.effect)) + ylim(c(0, 1.2)) + 
  geom_line(linetype="dotdash", color="thistle2", size=1)+ ylab("Power") + xlab("Effect size") + 
  geom_point(color="thistle3", size = 2)


#----
#--------------------------------------------

#Question 7

#a. b)


StudentsPerformance.sampled <- sample_n(StudentsPerformance, 25)

Hypothesis.test <- function(data.sampled.var1, data.sampled.var2, null.value = 0, alpha = 0.05, paired = FALSE){
  
  sample.len <- length(data.sampled.var1)
  print(paste("Null Hypothesis: diff mean = ", null.value))
  print(paste("Alternative Hypothesis: diff mean /= ", null.value))
  
  x_bar <- mean(data.sampled.var1) - mean(data.sampled.var2)
  s1 <- sd(data.sampled.var1)
  s2 <- sd(data.sampled.var2)
  if (paired)
    SE <- sd(data.sampled.var1 - data.sampled.var2) / sqrt(sample.len)
  else 
    SE <- sqrt((s1^2/sample.len) + (s2^2/sample.len))
  score <- abs((x_bar - null.value)) / SE
  
  if(sample.len > 30){
    print("Using Z-distribution")
    pvalue <- 2*pnorm(abs(score), lower.tail = FALSE)}
  
  else{
    print("Using t-distribution")
    pvalue <- 2*pt(score, df = sample.len - 1, lower.tail = FALSE)}
  
  
  print(paste("p-value =", pvalue))
  
  if (pvalue < alpha)
    print("Reject Null Hypothesis.")
  else
    print("Fail to Reject Null Hypothesis.")
  
}


Hypothesis.test(StudentsPerformance.sampled$health, StudentsPerformance.sampled$goout, paired = TRUE)

t.test(StudentsPerformance.sampled$health, StudentsPerformance.sampled$goout, paired =  TRUE)

#----

#b.


idx.sampled <- sample(StudentsPerformance$X, 200)
health.sampled <- StudentsPerformance$health[idx.sampled[1:100]]
goout.sampled <- StudentsPerformance$goout[idx.sampled[1:100]]

Hypothesis.test(health.sampled, goout.sampled)

t.test(health.sampled, goout.sampled)

#----
#--------------------------------------------

#Question 8


absences_box <- ggplot(StudentsPerformance, aes(x = absences)) + 
  geom_boxplot(outlier.colour="thistle2", color ="gray77", fill ="gray77", alpha = 0.5, outlier.size = 2) +
  labs(title="Boxplot with outliers")

absences_box 
boxplot.stats(StudentsPerformance$absences)



#a.

quantile(StudentsPerformance$absences, c(0.025, 0.975))



bs.size <- 1000
rep.size <- 1000

absences.sample <- replicate(1, sample(StudentsPerformance$absences, size = 200, replace = FALSE))
absences.replicated <- replicate(rep.size, sample(absences.sample, size = 100, replace = FALSE))

means <- apply(X = absences.replicated, MARGIN = 2, FUN = mean, na.rm = TRUE)

means <- sort(means)
margin <- 0.025 * bs.size

print(paste("Confidence Interval: (", round(means[c(margin)], 3),",",round(means[c(bs.size - margin)],3),")"))
#----

#b.
bs.size <- 1000
rep.size <- 1000

absences.sample <- replicate(1, sample(StudentsPerformance$absences, size = 20, replace = FALSE))
absences.bootstrapped <- replicate(rep.size, sample(absences.sample, size = 1000, replace = TRUE))

means <- apply(X = absences.bootstrapped, MARGIN = 2, FUN = mean, na.rm = TRUE)

means <- sort(means)
margin <- 0.025 * bs.size

print(paste("Confidence Interval: (", round(means[c(margin)], 3),",",round(means[c(bs.size - margin)],3),")"))
#----

#c.
absences.qq <- ggplot(StudentsPerformance, aes(sample = absences, color = "", alpha = 0.7)) + geom_qq() +
  geom_qq_line() + labs(title="QQ-plot ")

absences.qq + theme(legend.position="none") + scale_color_manual(values=c("thistle2"))


m.absences.qq <- ggplot(data.frame(mean = means), aes(sample = means, color = "", alpha = 0.7)) + geom_qq() +
  geom_qq_line() + labs(title="QQ-plot ")

m.absences.qq + theme(legend.position="none") + scale_color_manual(values=c("thistle2"))

#----
#--------------------------------------------


#Question 9


StudentsPerformance$Gsum <- StudentsPerformance$G1 + StudentsPerformance$G2 + StudentsPerformance$G3


f0.Gsum <- ((StudentsPerformance %>% filter(failures == 0))$Gsum)
f1.Gsum <- ((StudentsPerformance %>% filter(failures == 1))$Gsum)
f2.Gsum <- ((StudentsPerformance %>% filter(failures == 2))$Gsum)
f3.Gsum <- ((StudentsPerformance %>% filter(failures == 3))$Gsum)

sd.df <- data.frame(groups = c("Group0", "Group1", "Group2" , "Group3" ) , 
                    sds = c(sd(f0.Gsum), sd(f1.Gsum), sd(f2.Gsum), sd(f3.Gsum)))



aov.Gsum_failures <- aov(Gsum ~ as.factor(failures), data = StudentsPerformance)
aov.Gsum_failures

summary(aov.Gsum_failures)



test1 <- lm(Gsum ~ failures, data = StudentsPerformance)
summary(test1)

TukeyHSD(aov.Gsum_failures)

plot(TukeyHSD(aov.Gsum_failures),las = 1)


sd(Gsum ~ as.factor(failures))



box <- ggplot(StudentsPerformance, aes(x = failures, y = Gsum, group = failures)) + 
  geom_boxplot(alpha = 0.5, outlier.size = 2 , color = as.factor(failures), fill = as.factor(failures)) +
  labs(title="Boxplot")


box + scale_color_manual(values=c("thistle1", "thistle2", "thistle3" , "thistle4")) +
  scale_fill_manual(values=c("thistle1", "thistle2", "thistle3" , "thistle4"))

