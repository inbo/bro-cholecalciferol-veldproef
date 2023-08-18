# Power analyse voor praktische resistentie proef

library(pwr)
library(readxl)

library(tidyverse)

## Berekenen succes ratio vorige proef + confidence interval
vorige_proef <- read_xlsx("input/2003-2010_praktische_resistentie.xlsx",
                          sheet = "Blad1")

vorige_proef %>% 
  filter(Bekken %in% c("Brugse Polder", "Brugse_Polders")) %>% 
  mutate(Resistent = ifelse(PCA > 10, "Ja", "Nee")) %>% 
  group_by(Resistent) %>% 
  summarise(n())



binom.test(11, 58)

## Power analyse


pwr.2p2n.test(h = ES.h(0.07, 0.19), 
              n1 = 58, 
              n2 = NULL, 
              sig.level = 0.05, 
              power = 0.8)


# range of effect sizes
# Bovenschelde seq(0.16, 0.87, 0.01)
r <- c(seq(0,0.06,0.01), seq(0.36,0.98,0.01))
nr <- length(r)

# power values
p <- 0.8
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.2p2n.test(h = ES.h(r[j], 0.18), 
                            n1 = 58, 
                            n2 = NULL, 
                            sig.level = 0.05, 
                            power = p[i])
    samsize[j,i] <- ceiling(result$n2)
  }
}

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
  Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)

pwr.p.test(h = ES.h(p1 = 1, p2 = 0.50),
           sig.level = 0.05,
           power = 0.80,
           alternative = "greater")
