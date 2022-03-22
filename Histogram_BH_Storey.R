### Distribution of p-Values

library(ggplot2)

n = 10000
# mu = 0
X = rnorm(n)
# mu = 2
X1 = rnorm(n, 2)
# H0: mu = 0
# H1: mu > 0 
pval1 = pnorm(X, lower.tail = F)
pval1.1 = pnorm(X1, lower.tail = F)
p_vals <- c(pval1.1,pval1)
df <- data.frame(p_vals, mean = factor(c(rep("a", 10000),rep("b", 10000))))

ggplot(data = df, aes(x = p_vals, y = ..density..,  fill = mean)) + 
  geom_histogram(colour = 'white',binwidth=0.05,boundary=0.05) +
    xlab("p-value") + ylab("Density") + 
      ggtitle("Distribution of p-values from 10,000 tests", subtitle =  bquote("Testing" ~ H[0] ~ "against" ~ H[1])) + 
        scale_fill_discrete(name = bquote("Value of mean"), labels = c(bquote(mu==2),bquote(mu==0))) + 
          geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5)

### Benjamini-Hochberg FDR Controlling Procedure 

# For the first half of the data, mu = 0
# For the second half of the data, mu = 2
Xhalf = c(X[1:(n/2)], X1[1:(n/2)])
# H0: mu = 0
# H1: mu > 0 
pval.half = pnorm(Xhalf, lower.tail = F)
pBH = p.adjust(pval.half,method="BH")
# Identify the number of rejected null hypotheses (number of discoveries made)
rej = sum(pBH<0.05)
# Identify the number of discoveries made from the second half of the data (mu=2)
rej.true = sum(pBH[(n/2):n]<0.05) 
# % true discoveries = power
rej/(n/2) 
# FDR
(rej-rej.true)/rej
# FDR <= m0/m * q

# Plot the p values in ascending order from pval.half
plot(1:3000, head(sort(pval.half),-7000), type = 'l',xlab = 'p-value Rank', ylab = 'p-Value')
# We reject when p value < (rank/n) * desired FDR
# We reject all p values below the red line
lines(1:3000, (1:3000)*0.05/n, col='red')


### Storey's Method

# In a real test, we will never know if p-values come from a test that is true or false

# 1. Identify where the 'flatness' of the histogram begins, appears to occur when p-values greater than lambda := 0.4
# 2. Count the number of p-values to the RHS of lambda (expected to come from tests where null is true)
# 3. Divide by 1-lambda to get an estimate of # true null hypotheses (m0.est)
m0.est = sum(pval.half>0.4)/(1-0.4)
m0.est
# We estimate the FDR
FDR.est = 0.05 * m0.est / sum(pval.half<0.05)
FDR.est*100
# Control the FDR to the desired 5% 
pi0.est = m0.est / 10000
pi0.est
q.val = max(pval.half) * pi0.est
q.val
list.qvals = c(1:10000)
list.qvals[10000] = q.val
sorted.pvals = sort(pval.half, decreasing = FALSE)
for (i in (10000-1):1)
{
  opt1 = (pi0.est * 10000 * sorted.pvals[i])/i
  opt2 = list.qvals[i+1]
  list.qvals[i] = min(opt1,opt2)
}
# Reject all hypotheses with a q-value less than or equal to 0.05 to control FDR at 0.05 
# Number of significant results:
sum(list.qvals<0.05)
