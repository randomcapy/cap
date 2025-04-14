#pract1(Problems based on binomial distribution)
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
cat("enter x value for p(x)\n") #probability
x=scan(nmax = "")

cat("enter the size\n")
size=scan(nmax = 1)

dbinom(x,size,prob = 0.05)

probability=dbinom(x,size,prob = 0.05) %>% sum()
cat("sum of probabilities is:" ,{probability})

cat("enter x value for p(x)\n") #probability
x=scan(nmax = "")
cat("enter the size\n")
size=scan(nmax = 1)

dbinom(x,size,prob = 0.05)
probability=dbinom(x,size,prob = 0.05) %>% sum()
cat("sum of probabilities is:" ,{probability})

-------------------------------------------------------------------------------------------------------------------------------

#Pract2(Problems based on normal distribution)
#Problems based on normal distribution
install.packages("ggplot2")
install.packages("crayon")
library(ggplot2)
library(crayon)
cat(red("enter mean value mu"))
mean=scan(nmax=1)
cat(red("enter sigma value"))
sd=scan(nmax=1)
cat(red("enter value of q for p(x<=q) or p(x>=q)"))
val=scan(nmax=1)
print("Type 1 for p(x<=q)")
print("Type 2 for p(x>=q)")
Type=readline()
2
if(Type==1){
  pnorm(q=val, mean, sd,lower.tail=T)
}else if(Type==2){
  pnorm(q=val, mean, sd,lower.tail=F)
}

------------------------------------------------------------------------------------------------------------------------------

#Pract3(Property plotting of binomial distribution)
cat(red("enter the size"))
size=scan(nmax = "1") # size
cat(red("enter the Probability of success"))
p=scan(nmax = "1") # probability of success
cat(red("enter number of random sample"))
n=scan(nmax = "1") # number of random samples
set.seed(3) # set seed for reproducibility
random.binom.numbers=rbinom(n, size, p)
h=hist(random.binom.numbers,
       breaks = length(unique(random.binom.numbers)),
       plot = FALSE)
plot(h,
     freq = FALSE,
     space = NULL,
     xlim = c(0,size),
     xlab = 'Students passing the final exam', # label for x-axis
     ylab = "Probability", # label for y-axis
     main = "Binomial Probability Distribution \nfor size=25 and p=0.3", 
     # add title
     col = "#fc9d12", # add color "#fc9d12"
     xaxt = "n")# do not show ticks of the x-axis
axis(side=1, at=seq(0.5,size, by=1), labels=seq(1,size, by=1))

--------------------------------------------------------------------------------------------------------------------------------------

#Pract4(Property plotting of normal distribution)
cat("Plotting normal distribution\n")
cat("Enter the mean of the distribution")
mean = scan(nmax = "1")
cat("Enter the standard deviation of the distribution")
sd = scan(nmax = "1")
cat("Enter number of observations to be generated (for random sample)")
n = scan(nmax = "1")
generated = rnorm(n, mean, sd)
minimum = min(generated)
maximum = max(generated)
h = hist(generated,
         plot = FALSE)
plot(h,
     freq = FALSE,
     xlim = c(minimum, maximum),
     ylim = c(0, 1),
     xlab = "Observed Value",
     ylab = "Probability",
     main = "Normal Probability Distribution",
     col = "#FFD700")
line_range = seq(minimum, maximum, by = (maximum-minimum)/100)
lines(line_range, dnorm(line_range, mean(generated),
                        sd(generated)))

-----------------------------------------------------------------------------------------------------------------------------------------

#Pract5(Plotting pdf, cdf, pmf, for discrete and continuous distribution)
#Discrete Distributions:PMF(Probability Mass Function), CDF(Cumulative Distribution Function)
#Continuous Distributions:PDF(Probability Density Function) ,CDF(Cumulative Distribution Function) 
  
#PMF For Binomial Distribution
n =13
p = 0.7
dbinom(6, size = n, prob = p)
x <- 0:n
plot(x, dbinom(x, size = n, prob = p), main = "Probability mass function for Bin(13,0.7)")

#CDF For Binomial Distribution
pbinom(9, size = n, prob = p)
plot(x, pbinom(x, size = n, prob = p), type="s", main = "Cumulative distribution function for Bin(13,0.7)")

#PDF for Uniform Distribution
a <- 0
b <- 1
# plots similar to what we had before, but using a line rather than
points.
curve(dunif(x, min = a, max = b), from = -1, to = 2, xlab='y', ylab='f(y)', main='Probability density function for Unif(0,1)')

#PDF for Normal Distribution
mu <- 0
sigma <- 1 # standard deviation
curve(dnorm(x, mean = mu, sd = sigma), # notice the 3rd argument is the sd
      from = -4, to = 4,
      main = "PDF for a standard normal")

#CDF for a Normal Distribution
curve(pnorm(x, mean = mu, sd = sigma),
      from = -4, to = 4,
      main = "CDF for a standard normal",
      ylab = "F(x)")

------------------------------------------------------------------------------------------------------------------------------------------  
  
#Pract6(t test, normal test, F test)
## one sample t-test
cat(red("Enter no of Observations of Sample"))
n=scan(nmax = "1")
cat(red("Enter Observations of Sample"))
x=scan(nmax=n)
cat(red("mean of the population is"))
popu_mean=scan(nmax = "1")
t.test(x,y=NULL, mu=popu_mean)

# Two sample t-test(Dependent sample)
cat(red("Enter no of obervations of 1st data"))
n1=scan(nmax="1")
cat(red("enter observations of 1st data"))
x=scan(nmax=n1)
cat(red("Enter no of obervations of 2nd data"))
n2=scan(nmax="1")
cat(red("enter observations of 2nd data"))
y=scan(nmax=n2)
if(n1!=n2){
  print("Independent samples")
  print(t.test(x,y,mu=0,paired = FALSE))
}

# F test(Wilcoxon’s signed rank test)
A = c(16, 17, 25, 26, 32, 34, 38, 40, 42)
B = c(600, 590, 590, 630, 610, 630)
cat(red("Enter no of Observations of Sample1"))
n1=scan(nmax = "1")
cat(red("Enter Observations of Sample1"))
S1=scan(nmax=n1)
cat(red("Enter no of Observations of Sample2"))
n2=scan(nmax = "1")
cat(red("Enter Observations of Sampl21"))
S2=scan(nmax=n2)
Variance_Ratio=var.test(S1,S2)
Variance_Ratio

--------------------------------------------------------------------------------------------------------------------------------------------------------

#Pract7(Analysis of Variance)

#in excel (Anova: Single Factor)
#A->1 2 2 5 7 77 55 6
#B->77 44 5 44 7 5 5 9
#C->7 5 55 77 6 32 56 54
#Step1
#Go to File > Options > Add-ins > Manage dropdown > choose Excel Add-ins > Click Go > Check Analysis ToolPak > Click OK
#Step2
#Run ANOVA: Single Factor ->
#Data tab > Data Analysis > ANOVA: Single Factor > Click OK > Input Range:A1:C9 > Check Labels in First Row > Choose Output Range > OK
#Step3
#Go to Insert > Recommended Charts > 100% Stacked Column chart 


#(Anova: Two-Factor With Replication) ->
#Data tab > Data Analysis > ANOVA: Two-Factor With Replication > Click OK > Input Range:A1:C9 > Rows per sample:2 > Choose Output Range > OK
#Go to Insert > Recommended Charts > Clustered Column chart 


#(Anova: Two-Factor Without Replication)
#Data tab > Data Analysis > ANOVA: Two-Factor Without Replication > Click OK > Input Range:A1:C9 > Check Labels > Choose Output Range > OK
#Go to Insert > Recommended Charts > 100% Stacked Column chart 

------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
#Pract8(Non parametric  tests- I,II)
# Sign test
install.packages("BSDA")
library(BSDA)
cat("Enter the sample size")#10
n = scan(nmax = 1)
cat("Enter the values in the sample")

x = scan(nmax = n)
cat("Enter the median to test")#3
md = scan(nmax = 1)
cat("Enter type of test\ntwo.sided\ngreater\nless")
choice=readline()
cat ("Enter alpha value")
alpha = scan(nmax = 1)#0.05

test = SIGN.test(x, y = NULL, md, alternative = choice)
print(test)
if(test$p.value<alpha){
  cat("Reject Null Hypothesis; Accept Alternate Hypothesis")
}else{
  cat("Accept Null Hypothesis; Reject Alternate Hypothesis")
}

# Runs test
library(crayon)
cat(green("enter n1 value"))
n1=scan(nmax = "1")
cat(green("enter n2 value"))
n2=scan(nmax="1")
#R Bar
cat(green("enter number of runs"))
R=scan(nmax = "1")
print("Expected number of runs is:")
x=(2*n1*n2)
y=(n1+n2)
R.bar=((x/y)+1)
R.bar

#standard deviation of the runs
x=(2*n1*n2)
a=(n1-n2)
c=((n1+n2)^2)
d=(n1+n2-1)

print("standard deviation of the runs is:")
Nume=(x*(x-(a)))
Denom=(c*d)
S2=(Nume/Denom)
S2
#Test statistics for Run test
print("Calculated Z value is:")
z=((R - R.bar)/(S2))
z
if(z<1.96){
  cat(red("We Accept H0"))
  cat(red("Hence, Data is Random"))
}else if(z>1.96){
  print("We Reject H0")
  print("Hence, Data is non-Random")
}  

-----------------------------------------------------------------------------------------------------------------------------------------------------

#Pract9( Kruskal-Walis tests)
s1=sample.int(100,10)
s1
s2=sample.int(100,10)
s2
s=data.frame(s1,s2)
summary(s)

stacked=stack(s)
head(stacked)

kruskal.test(values~ind,data = stacked)

---------------------------------------------------------------------------------------------------------------------------------------------------------

#Pract10
#wilcoxon for small sample:
cat(red("enter the values of the sample"))#12
x=scan(nmax="")
cat(red("median of the population is"))#3
mu=scan(nmax = "1")
cat(red("Type 1 for Two.sided\n"))
cat(red("Type 2 for greater\n"))
cat(red("Type 3 for less\n"))
choice=readline()
wilcox.test()
2
if(choice==1){
  wilcox.test(x,y=NULL,mu, alternative = "two.sided", paired = F, exact
              = F)
}else if(choice==2){
  wilcox.test(x,y=NULL,mu, alternative = "greater", paired = F, exact =
                F)
}else if(choice==3){
  wilcox.test(x,y=NULL,mu, alternative = "less", paired = F, exact = F)
}

#Wilcoxon for Large Samples
cat("enter the values of X sample")#3
x=scan()
cat("enter the values of Y sample")#3
y=scan()
len_x = length(x)
len_y = length(y)
cat("Median of population")
mu1=scan(nmax=1)
if (len_x == len_y & len_x >= 30 & len_y >= 30){
  cat("Type 1 for Two.sided\n")
  cat("Type 2 for greater\n")
  cat("Type 3 for less\n")
}
choice = readline()
1
if(choice == 1){
  wilcox.test(x, y, mu, alternative = "two.sided", paired = T, exact = F)
}else if(choice == 2){
  wilcox.test(x, y, mu, alternative = "greater", paired = T, exact = F)
}else if(choice == 3){
  wilcox.test(x, y, mu, alternative = "less", paired = T, exact = F)
}else{
  cat("Enter Correct Values")
}

-----------------------------------------------------------------------------------------------------------------------------------------
  
#Pract11(Time series Analysis and Forecasting)
#(in excel)
#Year ->1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008
#Sales ->2354 2379 2318 2468 2386 2569 2575 2762 2844 3000 3108 3357 3075 3357 3180 3221 3420 3512 3430 3655
#calculate -> calculate 3 Years Total & 3 Years Moving Average, 5 Years Total & 5 Years Moving Average



