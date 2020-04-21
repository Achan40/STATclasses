options(warn=-1)
suppressPackageStartupMessages({
  library(faraway)
  library(ggplot2)
})

#Problem 1
#a
data(butterfat)
#create multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

p1 = ggplot(aes(x=Age, y=Butterfat), data=butterfat) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=-45)) + 
  ylab("Butterfat")

p2 = ggplot(aes(x=Breed, y=Butterfat), data=butterfat) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=-45)) + 
  ylab("Butterfat")

p3 = ggplot(butterfat, aes(x=Breed, y=Butterfat)) + 
  geom_point() + 
  stat_summary(fun="mean", geom="line", aes(group=Age, linetype=Age))+
  theme(legend.position = "top", legend.direction = "horizontal")

p4 = ggplot(butterfat, aes(x=Age, y=Butterfat)) + 
  geom_point() + 
  stat_summary(fun="mean", geom="line", aes(group=Breed, linetype=Breed))+
  theme(legend.position = "top", legend.direction = "horizontal")

multiplot(p1, p2, p3, p4, cols=2)

#b
#Fit two-way ANOVA model
mod = lm(Butterfat ~ Breed*Age, data = butterfat)
anova(mod)
#H0: no interaction between groups vs Ha: interaction between groups
#pval for Breed*Age is > alpha=.05, accept the null. No significant interaction between Breed and Age

#c
#H0: no difference between groups vs Ha: difference between groups
#pval for Age is > alpha=.05, accept the null. No significant difference between groups in Age
#pval for Breed is < alpha=.05, reject the null. There is significant difference between groups in Breed

#d
par(mfrow = c(1,2))
qqnorm(mod$res)
plot(mod$fitted, mod$res, xlab = "Fitted", ylab = "Residuals")
#There appears to be a slight "trumpet" pattern in our Fitted v Residuals, but out normal qq plot appears to be linear
#We don't need to apply a box-cox transformation in this case, our data seems to be normally distributed enough

#e
summary(mod)
#Best Breed in terms of Butterfat content: Jersey (See Age vs Butterfat interaction plot)
#Second Best Breed in terms of Butterfat content: Guemsey (See Age vs Butterfat interaction plot)
#No the best breed interms of butterfat content are not clearly superior to the second best breed, their estimated coefficients seem to be fairly close to each other.
coef(mod)
