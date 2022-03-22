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
