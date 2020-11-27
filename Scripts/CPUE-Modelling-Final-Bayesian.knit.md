
<!-- rnb-text-begin -->

---
title: "CPUE Data Analysis"
output: html_notebook
---

Based upon conversations with Pravin from Stats, use log10 CPUE rather than standardise by estuary/species

Load data and clean up and standardise the winds

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubGlicmFyeShnZ3Bsb3QyKVxubGlicmFyeShESEFSTWEpXG5saWJyYXJ5KGJvb3RwcmVkaWN0bG1lNClcbmxpYnJhcnkobWVyVG9vbHMpXG5saWJyYXJ5KGVmZmVjdHMpXG5saWJyYXJ5KE11TUluKVxubGlicmFyeShkcGx5cilcbmxpYnJhcnkoZ2dlZmZlY3RzKVxubGlicmFyeShnbG1tVE1CKVxubGlicmFyeSh0d2VlZGllKVxubGlicmFyeShicm1zKVxuIyBVc2UgTG9nMTAoQ1BVRSkgcmF0aGVyIHRoYW4gc3RhbmRhcmRpc2VcblxuXG4jIExvYWQgRGF0YVxubXlkYXRhIDwtIHJlYWQuY3N2KFwiLi4vRGF0YS9GdWxsX0RhdGFfTW9kZWxsaW5nX0JBUlJBX2J5X2VzdHVhcnkuY3N2XCIsIGhlYWRlciA9IFQpICMgRnVsbF9EYXRhX01vZGVsbGluZ1YzX2J5X2VzdHVhcnkuY3N2XG4jIG15ZGF0YTIgPC0gIHJlYWQuY3N2KFwiRnVsbF9EYXRhX01vZGVsbGluZy5jc3ZcIiwgaGVhZGVyID0gVClcbiMgXG4jIGNvci50ZXN0KG15ZGF0YSRYNDVfZGVncmVlX3dpbmRzLCBteWRhdGEyJFgxMzVfZGVncmVlX3dpbmRzKVxuIyBjb3IudGVzdChteWRhdGEkWDEzNV9kZWdyZWVfd2luZHMsIG15ZGF0YTIkWDQ1X2RlZ3JlZV93aW5kcylcbiMgY29yLnRlc3QobXlkYXRhJFgxMzVfZGVncmVlX3dpbmRzLCBteWRhdGEyJFgxMzVfZGVncmVlX3dpbmRzKVxuIyBjb3IudGVzdChteWRhdGEkWDQ1X2RlZ3JlZV93aW5kcywgbXlkYXRhMiRYNDVfZGVncmVlX3dpbmRzKVxuIyAjIE5FIHdpbmRzIG5vdCBjb3JyZWxhdGVkIGJ5IFNFIHdpbmRzIGFyZS4uLi4uXG5cbiMgUmVtb3ZlIFRvdGFsIENQVUVcbm15ZGF0YSA8LSBzdWJzZXQobXlkYXRhLCBTcGVjaWVzICE9IFwiVG90YWxcIilcbm15ZGF0YSA8LSBzdWJzZXQobXlkYXRhLCBTcGVjaWVzICE9IFwiTHVkZXJpY2tcIilcblxuIyMjIFN0YW5kYXJkaXNlIFdpbmRzXG5teS5kZiA8LSAgbXlkYXRhICU+JSBncm91cF9ieShTcGVjaWVzLCBFc3R1YXJ5KSAlPiUgbXV0YXRlKFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkID0gYXMubnVtZXJpYyhzY2FsZShYNDVfZGVncmVlX3dpbmRzKSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQgPSBhcy5udW1lcmljKHNjYWxlKFgxMzVfZGVncmVlX3dpbmRzKSkpXG5cbmBgYCJ9 -->

```r
library(ggplot2)
library(DHARMa)
library(bootpredictlme4)
library(merTools)
library(effects)
library(MuMIn)
library(dplyr)
library(ggeffects)
library(glmmTMB)
library(tweedie)
library(brms)
# Use Log10(CPUE) rather than standardise


# Load Data
mydata <- read.csv("../Data/Full_Data_Modelling_BARRA_by_estuary.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv
# mydata2 <-  read.csv("Full_Data_Modelling.csv", header = T)
# 
# cor.test(mydata$X45_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X45_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X45_degree_winds, mydata2$X45_degree_winds)
# # NE winds not correlated by SE winds are.....

# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")
mydata <- subset(mydata, Species != "Luderick")

### Standardise Winds
my.df <-  mydata %>% group_by(Species, Estuary) %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                           X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Investigate the environmental variation over time and the stability of the CPUE over time

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuaGVhZChteS5kZilcblxuZ2dwbG90KG15LmRmLCBhZXMoeD1ZZWFyLCB5ID0gRHJvdWdodF9Nb250aHMpKSArIGdlb21fcG9pbnQoKSArIGZhY2V0X3dyYXAoflNwZWNpZXMpXG5nZ3Bsb3QobXkuZGYsIGFlcyh4PVllYXIsIHkgPSBDYXRjaC8xMDAwLCBjb2w9IEVzdHVhcnkpKSArIGdlb21fbGluZSgpICsgZmFjZXRfd3JhcCh+U3BlY2llcywgc2NhbGVzID0gXCJmcmVlX3lcIikgK1xuICB0aGVtZV9jbGFzc2ljKCkgKyB5bGFiKFwiQ2F0Y2ggKCcwMDAga2cpXCIpICtcbiAgdGhlbWUoYXhpcy50aXRsZSA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBzaXplPTEyKSxcbiAgICAgICAgYXhpcy50ZXh0LnggPSBlbGVtZW50X3RleHQoY29sb3VyID0gXCJibGFja1wiLCBzaXplID0gMTAsIGFuZ2xlPTQ1LCBoanVzdCA9IDEpLFxuICAgICAgICBheGlzLnRleHQueSA9IGVsZW1lbnRfdGV4dChjb2xvdXIgPSBcImJsYWNrXCIsIHNpemUgPSAxMCksXG4gICAgICAgIHN0cmlwLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgZmFjZSA9IFwiYm9sZFwiLCBzaXplID0gMTEsIGhqdXN0PTAuNSksXG4gICAgICAgIHN0cmlwLmJhY2tncm91bmQgPSBlbGVtZW50X2JsYW5rKCksXG4gICAgICAgIHBhbmVsLmJvcmRlciA9IGVsZW1lbnRfcmVjdChjb2xvdXIgPSBcImJsYWNrXCIsIGZpbGw9TkEsIHNpemUgPSAxKSxcbiAgICAgICAgbGVnZW5kLnRpdGxlID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxMiwgZmFjZSA9IFwiYm9sZFwiKSxcbiAgICAgICAgbGVnZW5kLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZT0xMCkpXG5cbmdnc2F2ZShcIi4uL3Bsb3RzL0NhdGNoIGJ5IHNwZWNpZXNfdGltZV9lc3R1YXJ5LnBuZ1wiLCBoZWlnaHQgPSAxNC44LCB3aWR0aD0yMSwgZHBpID02MDAsIHVuaXRzPVwiY21cIilcblxuZ2dwbG90KG15LmRmLCBhZXMoeD1ZZWFyLCB5ID0gQ1BVRSwgY29sPSBFc3R1YXJ5KSkgKyBnZW9tX2xpbmUoKSArIGZhY2V0X3dyYXAoflNwZWNpZXMsIHNjYWxlcyA9IFwiZnJlZV95XCIpICtcbiAgdGhlbWVfY2xhc3NpYygpICsgeWxhYihcIkNQVUUgKGtnIHBlciBkYXkpXCIpICtcbiAgdGhlbWUoYXhpcy50aXRsZSA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBzaXplPTEyKSxcbiAgICAgICAgYXhpcy50ZXh0LnggPSBlbGVtZW50X3RleHQoY29sb3VyID0gXCJibGFja1wiLCBzaXplID0gMTAsIGFuZ2xlPTQ1LCBoanVzdCA9IDEpLFxuICAgICAgICBheGlzLnRleHQueSA9IGVsZW1lbnRfdGV4dChjb2xvdXIgPSBcImJsYWNrXCIsIHNpemUgPSAxMCksXG4gICAgICAgIHN0cmlwLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgZmFjZSA9IFwiYm9sZFwiLCBzaXplID0gMTEsIGhqdXN0PTAuNSksXG4gICAgICAgIHN0cmlwLmJhY2tncm91bmQgPSBlbGVtZW50X2JsYW5rKCksXG4gICAgICAgIHBhbmVsLmJvcmRlciA9IGVsZW1lbnRfcmVjdChjb2xvdXIgPSBcImJsYWNrXCIsIGZpbGw9TkEsIHNpemUgPSAxKSxcbiAgICAgICAgbGVnZW5kLnRpdGxlID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxMiwgZmFjZSA9IFwiYm9sZFwiKSxcbiAgICAgICAgbGVnZW5kLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZT0xMCkpXG5cbmdnc2F2ZShcIi4uL3Bsb3RzL0NQVUUgYnkgc3BlY2llc190aW1lX2VzdHVhcnkucG5nXCIsIGhlaWdodCA9IDE0LjgsIHdpZHRoPTIxLCBkcGkgPTYwMCwgdW5pdHM9XCJjbVwiKVxuXG4jIHRoZSBuZXh0IHR3byBzdWdnZXN0IG15IHNjYWxpbmcgb2YgdGhlIHdpbmRzIHdhcyBub3QgZ29vZCBlbm91Z2guXG5nZ3Bsb3QobXkuZGYsIGFlcyh4PVllYXIsIHkgPSBYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCwgY29sPSBFc3R1YXJ5KSkgKyBnZW9tX2xpbmUoKSArIGZhY2V0X3dyYXAoflNwZWNpZXMpICtcbiAgdGhlbWVfY2xhc3NpYygpICsgeWxhYihcIlN0YW5kYXJkaXNlZCBVcHdlbGxpbmdcXG5GYXZvdXJhYmxlIFdpbmRzXCIpICtcbiAgdGhlbWUoYXhpcy50aXRsZSA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBzaXplPTEyKSxcbiAgICAgICAgYXhpcy50ZXh0LnggPSBlbGVtZW50X3RleHQoY29sb3VyID0gXCJibGFja1wiLCBzaXplID0gMTAsIGFuZ2xlPTQ1LCBoanVzdCA9IDEpLFxuICAgICAgICBheGlzLnRleHQueSA9IGVsZW1lbnRfdGV4dChjb2xvdXIgPSBcImJsYWNrXCIsIHNpemUgPSAxMCksXG4gICAgICAgIHN0cmlwLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgZmFjZSA9IFwiYm9sZFwiLCBzaXplID0gMTEsIGhqdXN0PTAuNSksXG4gICAgICAgIHN0cmlwLmJhY2tncm91bmQgPSBlbGVtZW50X2JsYW5rKCksXG4gICAgICAgIHBhbmVsLmJvcmRlciA9IGVsZW1lbnRfcmVjdChjb2xvdXIgPSBcImJsYWNrXCIsIGZpbGw9TkEsIHNpemUgPSAxKSxcbiAgICAgICAgbGVnZW5kLnRpdGxlID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxMiwgZmFjZSA9IFwiYm9sZFwiKSxcbiAgICAgICAgbGVnZW5kLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZT0xMCkpXG5cbmdnc2F2ZShcIi4uL3Bsb3RzL1Vwd2VsbGluZyBXaW5kcyBieSBzcGVjaWVzX3RpbWVfZXN0dWFyeS5wbmdcIiwgaGVpZ2h0ID0gMTQuOCwgd2lkdGg9MjEsIGRwaSA9NjAwLCB1bml0cz1cImNtXCIpXG5cbmdncGxvdChteS5kZiwgYWVzKHg9WWVhciwgeSA9IFgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCwgY29sPSBFc3R1YXJ5KSkgKyBnZW9tX2xpbmUoKSArIGZhY2V0X3dyYXAoflNwZWNpZXMpK1xuICB0aGVtZV9jbGFzc2ljKCkgKyB5bGFiKFwiU3RhbmRhcmRpc2VkIERvd253ZWxsaW5nXFxuRmF2b3VyYWJsZSBXaW5kc1wiKSArXG4gIHRoZW1lKGF4aXMudGl0bGUgPSBlbGVtZW50X3RleHQoZmFjZT1cImJvbGRcIiwgc2l6ZT0xMiksXG4gICAgICAgIGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGNvbG91ciA9IFwiYmxhY2tcIiwgc2l6ZSA9IDEwLCBhbmdsZT00NSwgaGp1c3QgPSAxKSxcbiAgICAgICAgYXhpcy50ZXh0LnkgPSBlbGVtZW50X3RleHQoY29sb3VyID0gXCJibGFja1wiLCBzaXplID0gMTApLFxuICAgICAgICBzdHJpcC50ZXh0ID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIGZhY2UgPSBcImJvbGRcIiwgc2l6ZSA9IDExLCBoanVzdD0wLjUpLFxuICAgICAgICBzdHJpcC5iYWNrZ3JvdW5kID0gZWxlbWVudF9ibGFuaygpLFxuICAgICAgICBwYW5lbC5ib3JkZXIgPSBlbGVtZW50X3JlY3QoY29sb3VyID0gXCJibGFja1wiLCBmaWxsPU5BLCBzaXplID0gMSksXG4gICAgICAgIGxlZ2VuZC50aXRsZSA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTIsIGZhY2UgPSBcImJvbGRcIiksXG4gICAgICAgIGxlZ2VuZC50ZXh0ID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemU9MTApKSMsIHNjYWxlcyA9IFwiZnJlZV95XCIpXG5nZ3NhdmUoXCIuLi9wbG90cy9Eb3dud2VsbGluZyBXaW5kcyBieSBzcGVjaWVzX3RpbWVfZXN0dWFyeS5wbmdcIiwgaGVpZ2h0ID0gMTQuOCwgd2lkdGg9MjEsIGRwaSA9NjAwLCB1bml0cz1cImNtXCIpXG5cblxuZ2dwbG90KG15LmRmLCBhZXMoeD1ZZWFyLCB5ID0gRWZmb3J0X2RheXMsIGNvbD0gRXN0dWFyeSkpICsgZ2VvbV9saW5lKCkrICMrIGZhY2V0X3dyYXAoflNwZWNpZXMpK1xuICB0aGVtZV9jbGFzc2ljKCkgKyB5bGFiKFwiRWZmb3J0IChGaXNoaW5nIERheXMpXCIpICtcbiAgc2NhbGVfeV9sb2cxMCgpK1xuICB0aGVtZShheGlzLnRpdGxlID0gZWxlbWVudF90ZXh0KGZhY2U9XCJib2xkXCIsIHNpemU9MTIpLFxuICAgICAgICBheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChjb2xvdXIgPSBcImJsYWNrXCIsIHNpemUgPSAxMCwgYW5nbGU9NDUsIGhqdXN0ID0gMSksXG4gICAgICAgIGF4aXMudGV4dC55ID0gZWxlbWVudF90ZXh0KGNvbG91ciA9IFwiYmxhY2tcIiwgc2l6ZSA9IDEwKSxcbiAgICAgICAgc3RyaXAudGV4dCA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBmYWNlID0gXCJib2xkXCIsIHNpemUgPSAxMSwgaGp1c3Q9MC41KSxcbiAgICAgICAgc3RyaXAuYmFja2dyb3VuZCA9IGVsZW1lbnRfYmxhbmsoKSxcbiAgICAgICAgcGFuZWwuYm9yZGVyID0gZWxlbWVudF9yZWN0KGNvbG91ciA9IFwiYmxhY2tcIiwgZmlsbD1OQSwgc2l6ZSA9IDEpLFxuICAgICAgICBsZWdlbmQudGl0bGUgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZSA9IDEyLCBmYWNlID0gXCJib2xkXCIpLFxuICAgICAgICBsZWdlbmQudGV4dCA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplPTEwKSkjLCBzY2FsZXMgPSBcImZyZWVfeVwiKVxuZ2dzYXZlKFwiLi4vcGxvdHMvRmlzaGluZyBFZmZvcnQgb3ZlciB0aW1lLnBuZ1wiLCBoZWlnaHQgPSAxNC44LCB3aWR0aD0yMSwgZHBpID02MDAsIHVuaXRzPVwiY21cIilcblxuYGBgIn0= -->

```r
head(my.df)

ggplot(my.df, aes(x=Year, y = Drought_Months)) + geom_point() + facet_wrap(~Species)
ggplot(my.df, aes(x=Year, y = Catch/1000, col= Estuary)) + geom_line() + facet_wrap(~Species, scales = "free_y") +
  theme_classic() + ylab("Catch ('000 kg)") +
  theme(axis.title = element_text(face="bold", size=12),
        axis.text.x = element_text(colour = "black", size = 10, angle=45, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text = element_text(colour="black", face = "bold", size = 11, hjust=0.5),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.title = element_text(colour="black", size = 12, face = "bold"),
        legend.text = element_text(colour="black", size=10))

ggsave("../plots/Catch by species_time_estuary.png", height = 14.8, width=21, dpi =600, units="cm")

ggplot(my.df, aes(x=Year, y = CPUE, col= Estuary)) + geom_line() + facet_wrap(~Species, scales = "free_y") +
  theme_classic() + ylab("CPUE (kg per day)") +
  theme(axis.title = element_text(face="bold", size=12),
        axis.text.x = element_text(colour = "black", size = 10, angle=45, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text = element_text(colour="black", face = "bold", size = 11, hjust=0.5),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.title = element_text(colour="black", size = 12, face = "bold"),
        legend.text = element_text(colour="black", size=10))

ggsave("../plots/CPUE by species_time_estuary.png", height = 14.8, width=21, dpi =600, units="cm")

# the next two suggest my scaling of the winds was not good enough.
ggplot(my.df, aes(x=Year, y = X45_degree_winds.standardised, col= Estuary)) + geom_line() + facet_wrap(~Species) +
  theme_classic() + ylab("Standardised Upwelling\nFavourable Winds") +
  theme(axis.title = element_text(face="bold", size=12),
        axis.text.x = element_text(colour = "black", size = 10, angle=45, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text = element_text(colour="black", face = "bold", size = 11, hjust=0.5),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.title = element_text(colour="black", size = 12, face = "bold"),
        legend.text = element_text(colour="black", size=10))

ggsave("../plots/Upwelling Winds by species_time_estuary.png", height = 14.8, width=21, dpi =600, units="cm")

ggplot(my.df, aes(x=Year, y = X135_degree_winds.standardised, col= Estuary)) + geom_line() + facet_wrap(~Species)+
  theme_classic() + ylab("Standardised Downwelling\nFavourable Winds") +
  theme(axis.title = element_text(face="bold", size=12),
        axis.text.x = element_text(colour = "black", size = 10, angle=45, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text = element_text(colour="black", face = "bold", size = 11, hjust=0.5),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.title = element_text(colour="black", size = 12, face = "bold"),
        legend.text = element_text(colour="black", size=10))#, scales = "free_y")
ggsave("../plots/Downwelling Winds by species_time_estuary.png", height = 14.8, width=21, dpi =600, units="cm")


ggplot(my.df, aes(x=Year, y = Effort_days, col= Estuary)) + geom_line()+ #+ facet_wrap(~Species)+
  theme_classic() + ylab("Effort (Fishing Days)") +
  scale_y_log10()+
  theme(axis.title = element_text(face="bold", size=12),
        axis.text.x = element_text(colour = "black", size = 10, angle=45, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text = element_text(colour="black", face = "bold", size = 11, hjust=0.5),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.title = element_text(colour="black", size = 12, face = "bold"),
        legend.text = element_text(colour="black", size=10))#, scales = "free_y")
ggsave("../plots/Fishing Effort over time.png", height = 14.8, width=21, dpi =600, units="cm")

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




Do full model for all species


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI203IDwtIGdsbW1UTUIoQ2F0Y2ggfiBwb2x5KFgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCwgZGVncmVlID0gMikgKyBcbiMgICAgICAgcG9seShYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCwgZGVncmVlID0gMikgK1xuIyAgICAgICBYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQ6WDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQrXG4jICAgICAgICBFc3R1YXJ5X1R5cGUgKkRyb3VnaHRfTW9udGhzICsgU3BlY2llcyArIChTcGVjaWVzfEVzdHVhcnkpLCBmYW1pbHkgPSBHYW1tYShsaW5rPVwibG9nXCIpLFxuIyAgICAgICBkYXRhID0gbXkuZGYsIG9mZnNldCA9IGxvZzEwKEVmZm9ydF9kYXlzKSlcbiNBSUMobTcpICMgLTM4LjE4XG4jcGVyZm9ybWFuY2U6OnIyKG03KSAjIDAuMDhcbiNcbiNzaW11bGF0aW9uT3V0cHV0IDwtIHNpbXVsYXRlUmVzaWR1YWxzKGZpdHRlZE1vZGVsID0gbTcsIG4gPSAyNTApXG4jcGxvdChzaW11bGF0aW9uT3V0cHV0LCBxdWFudHJlZyA9IEYpXG4jaGlzdChyZXNpZHVhbHMobTcpLCBicmVha3MgPSAzMClcbiNcbiNjYXI6OkFub3ZhKG03KVxuI3N1bW1hcnkobTcpXG4jXG4jdG90YWxfU3VtbWFyeSA8LSBicm9vbS5taXhlZDo6dGlkeShtNylcbiN0b3RhbF9TdW1tYXJ5XG4jd3JpdGUuY3N2KHRvdGFsX1N1bW1hcnksIFwiLi4vRGF0YS9BbGwgU3BlY2llcyBTdW1tYXJ5IFRhYmxlLmNzdlwiLCByb3cubmFtZXMgPSBGKVxuXG5gYGAifQ== -->

```r
#m7 <- glmmTMB(Catch ~ poly(X135_degree_winds.standardised, degree = 2) + 
#       poly(X45_degree_winds.standardised, degree = 2) +
#       X135_degree_winds.standardised:X45_degree_winds.standardised+
#        Estuary_Type *Drought_Months + Species + (Species|Estuary), family = Gamma(link="log"),
#       data = my.df, offset = log10(Effort_days))
#AIC(m7) # -38.18
#performance::r2(m7) # 0.08
#
#simulationOutput <- simulateResiduals(fittedModel = m7, n = 250)
#plot(simulationOutput, quantreg = F)
#hist(residuals(m7), breaks = 30)
#
#car::Anova(m7)
#summary(m7)
#
#total_Summary <- broom.mixed::tidy(m7)
#total_Summary
#write.csv(total_Summary, "../Data/All Species Summary Table.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Bayesian Model

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Check model

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYmF5ZXNfUjIobTdhKVxuXG5gYGAifQ== -->

```r
bayes_R2(m7a)

```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiICAgIEVzdGltYXRlICAgRXN0LkVycm9yICAgICAgUTIuNSAgICAgUTk3LjVcblIyIDAuODIxMTUyOCAwLjAwOTkwNDc2OSAwLjgwMDA2NTkgMC44MzgxNDUxXG4ifQ== -->

```
    Estimate   Est.Error      Q2.5     Q97.5
R2 0.8211528 0.009904769 0.8000659 0.8381451
```



<!-- rnb-output-end -->

<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGVyZm9ybWFuY2U6OnIyX2JheWVzKG03YSlcbmBgYCJ9 -->

```r
performance::r2_bayes(m7a)
```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiIyBCYXllc2lhbiBSMiB3aXRoIFN0YW5kYXJkIEVycm9yXG5cbiAgQ29uZGl0aW9uYWwgUjI6IDAuODIyIFswLjAxMF1cbiAgICAgTWFyZ2luYWwgUjI6IDAuNTA2IFswLjEwM11cbiJ9 -->

```
# Bayesian R2 with Standard Error

  Conditional R2: 0.822 [0.010]
     Marginal R2: 0.506 [0.103]
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

marginal effects

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI3Bsb3QoY29uZGl0aW9uYWxfZWZmZWN0cyhtN2EpKVxuXG4jcGxvdChnZ3ByZWRpY3QobTdhLCB0ZXJtcyA9IFwiWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQgW2FsbF1cIikpXG5cbk1fZGF0IDwtIGdncHJlZGljdChtN2EsIHRlcm1zID0gXCJYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCBbYWxsXVwiKVxuTV9kYXQkVGVybSA8LSBcIlVwd2VsbGluZyBcXG5GYXZvdXJhYmxlIFdpbmRzXCJcblxuTV9wbG90IDwtIGdncGxvdChNX2RhdCwgYWVzKHgsIHByZWRpY3RlZC8xMDAwKSkrXG4gIGdlb21fbGluZSgpICsgeGxhYihcIlVwd2VsbHdpbmdcXG5GYXZvdXJhYmxlIFdpbmRzXCIpK1xuICBnZW9tX3JpYmJvbihhZXMoeW1pbiA9IGNvbmYubG93LzEwMDAsIHltYXggPSBjb25mLmhpZ2gvMTAwMCksIGFscGhhID0gLjEpICtcbiAgdGhlbWVfY2xhc3NpYygpICsgdGhlbWUoYXhpcy50ZXh0ICA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTIpLCBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgYXhpcy50aXRsZSA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTQpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICBheGlzLnRpY2tzID0gZWxlbWVudF9saW5lKGNvbG91cj1cImJsYWNrXCIpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICBzdHJpcC50ZXh0ID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIGZhY2UgPSBcImJvbGRcIiwgc2l6ZSA9IDEzKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgc3RyaXAuYmFja2dyb3VuZCA9IGVsZW1lbnRfYmxhbmsoKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgc3RyaXAucGxhY2VtZW50ID0gXCJvdXRzaWRlXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICNsZWdlbmQuanVzdGlmaWNhdGlvbj1jKDEsMCksIGxlZ2VuZC5wb3NpdGlvbj1cInJpZ2h0XCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgIHBhbmVsLmJvcmRlciA9IGVsZW1lbnRfcmVjdChjb2xvdXIgPSBcImJsYWNrXCIsIGZpbGw9TkEsIHNpemUgPSAxKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgdGl0bGUgPSBlbGVtZW50X3RleHQoc2l6ZT0xMiwgZmFjZSA9IFwiYm9sZFwiKSkrXG4gIHlsYWIoXCJQcmVkaWN0ZWQgQ1BVRSAoa2cgLyBkYXkpXCIpIyArXG4gICNnZ3RpdGxlKFwiYykgTXVsbGV0XCIpXG5NX3Bsb3QgIFxuZ2dzYXZlKFwiLi4vcGxvdHMvQ1BVRSBNdWx0aXNwZWNpZXMucG5nXCIsIGRwaSA9IDYwMCwgd2lkdGggPSAxMCwgaGVpZ2h0ID0gMTIsIHVuaXRzID0gXCJjbVwiKVxuYGBgIn0= -->

```r
#plot(conditional_effects(m7a))

#plot(ggpredict(m7a, terms = "X45_degree_winds.standardised [all]"))

M_dat <- ggpredict(m7a, terms = "X45_degree_winds.standardised [all]")
M_dat$Term <- "Upwelling \nFavourable Winds"

M_plot <- ggplot(M_dat, aes(x, predicted/1000))+
  geom_line() + xlab("Upwellwing\nFavourable Winds")+
  geom_ribbon(aes(ymin = conf.low/1000, ymax = conf.high/1000), alpha = .1) +
  theme_classic() + theme(axis.text  = element_text(colour="black", size = 12), 
                          axis.title = element_text(face="bold", colour="black", size = 14),
                          axis.ticks = element_line(colour="black"),
                          strip.text = element_text(colour="black", face = "bold", size = 13),
                          strip.background = element_blank(),
                          strip.placement = "outside",
                          #legend.justification=c(1,0), legend.position="right",
                          panel.border = element_rect(colour = "black", fill=NA, size = 1),
                          title = element_text(size=12, face = "bold"))+
  ylab("Predicted CPUE (kg / day)")# +
  #ggtitle("c) Mullet")
M_plot  
ggsave("../plots/CPUE Multispecies.png", dpi = 600, width = 10, height = 12, units = "cm")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Get co-eff outputs

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubGlicmFyeSh0aWR5YmF5ZXMpXG5saWJyYXJ5KHRpZHl2ZXJzZSlcbmdldF92YXJpYWJsZXMobTdhKVxuYGBgIn0= -->

```r
library(tidybayes)
library(tidyverse)
get_variables(m7a)
```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiIFsxXSBcImJfSW50ZXJjZXB0XCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJiX3BvbHlYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRkZWdyZWVFUTIxXCIgICAgICAgICAgICAgICAgXG4gWzNdIFwiYl9wb2x5WDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkZGVncmVlRVEyMlwiICAgICAgICAgICAgICAgICBcImJfcG9seVg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkZGVncmVlRVEyMVwiICAgICAgICAgICAgICAgICBcbiBbNV0gXCJiX3BvbHlYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGRlZ3JlZUVRMjJcIiAgICAgICAgICAgICAgICAgIFwiYl9Fc3R1YXJ5X1R5cGVCYXJyaWVyUml2ZXJcIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxuIFs3XSBcImJfRXN0dWFyeV9UeXBlRHJvd25lZFJpdmVyVmFsbGV5XCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJiX0Ryb3VnaHRfTW9udGhzXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXG4gWzldIFwiYl9TcGVjaWVzRmxhdGhlYWRcIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcImJfU3BlY2llc011bGxldFwiICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcblsxMV0gXCJiX1NwZWNpZXNXaGl0aW5nXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiYl9YMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQ6WDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRcIlxuWzEzXSBcImJfRXN0dWFyeV9UeXBlQmFycmllclJpdmVyOkRyb3VnaHRfTW9udGhzXCIgICAgICAgICAgICAgICAgICAgICAgXCJiX0VzdHVhcnlfVHlwZURyb3duZWRSaXZlclZhbGxleTpEcm91Z2h0X01vbnRoc1wiICAgICAgICAgICAgICAgXG5bMTVdIFwic2RfRXN0dWFyeV9fSW50ZXJjZXB0XCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcInNkX0VzdHVhcnlfX1NwZWNpZXNGbGF0aGVhZFwiICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcblsxN10gXCJzZF9Fc3R1YXJ5X19TcGVjaWVzTXVsbGV0XCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwic2RfRXN0dWFyeV9fU3BlY2llc1doaXRpbmdcIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxuWzE5XSBcImNvcl9Fc3R1YXJ5X19JbnRlcmNlcHRfX1NwZWNpZXNGbGF0aGVhZFwiICAgICAgICAgICAgICAgICAgICAgICAgXCJjb3JfRXN0dWFyeV9fSW50ZXJjZXB0X19TcGVjaWVzTXVsbGV0XCIgICAgICAgICAgICAgICAgICAgICAgICAgXG5bMjFdIFwiY29yX0VzdHVhcnlfX1NwZWNpZXNGbGF0aGVhZF9fU3BlY2llc011bGxldFwiICAgICAgICAgICAgICAgICAgICBcImNvcl9Fc3R1YXJ5X19JbnRlcmNlcHRfX1NwZWNpZXNXaGl0aW5nXCIgICAgICAgICAgICAgICAgICAgICAgICBcblsyM10gXCJjb3JfRXN0dWFyeV9fU3BlY2llc0ZsYXRoZWFkX19TcGVjaWVzV2hpdGluZ1wiICAgICAgICAgICAgICAgICAgIFwiY29yX0VzdHVhcnlfX1NwZWNpZXNNdWxsZXRfX1NwZWNpZXNXaGl0aW5nXCIgICAgICAgICAgICAgICAgICAgIFxuWzI1XSBcInNpZ21hXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJyX0VzdHVhcnlbQ2FtZGVuLlJpdmVyLEludGVyY2VwdF1cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXG5bMjddIFwicl9Fc3R1YXJ5W0NsYXJlbmNlLlJpdmVyLEludGVyY2VwdF1cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICBcInJfRXN0dWFyeVtIYXdrZXNidXJ5LlJpdmVyLEludGVyY2VwdF1cIiAgICAgICAgICAgICAgICAgICAgICAgICBcblsyOV0gXCJyX0VzdHVhcnlbSHVudGVyLlJpdmVyLEludGVyY2VwdF1cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwicl9Fc3R1YXJ5W0xha2UuSWxsYXdhcnJhLEludGVyY2VwdF1cIiAgICAgICAgICAgICAgICAgICAgICAgICAgIFxuWzMxXSBcInJfRXN0dWFyeVtQb3J0LlN0ZXBoZW5zLEludGVyY2VwdF1cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJyX0VzdHVhcnlbVHVnZ2VyYWgsSW50ZXJjZXB0XVwiICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXG5bMzNdIFwicl9Fc3R1YXJ5W1dhbGxpcy5MYWtlLEludGVyY2VwdF1cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcInJfRXN0dWFyeVtDYW1kZW4uUml2ZXIsU3BlY2llc0ZsYXRoZWFkXVwiICAgICAgICAgICAgICAgICAgICAgICBcblszNV0gXCJyX0VzdHVhcnlbQ2xhcmVuY2UuUml2ZXIsU3BlY2llc0ZsYXRoZWFkXVwiICAgICAgICAgICAgICAgICAgICAgIFwicl9Fc3R1YXJ5W0hhd2tlc2J1cnkuUml2ZXIsU3BlY2llc0ZsYXRoZWFkXVwiICAgICAgICAgICAgICAgICAgIFxuWzM3XSBcInJfRXN0dWFyeVtIdW50ZXIuUml2ZXIsU3BlY2llc0ZsYXRoZWFkXVwiICAgICAgICAgICAgICAgICAgICAgICAgXCJyX0VzdHVhcnlbTGFrZS5JbGxhd2FycmEsU3BlY2llc0ZsYXRoZWFkXVwiICAgICAgICAgICAgICAgICAgICAgXG5bMzldIFwicl9Fc3R1YXJ5W1BvcnQuU3RlcGhlbnMsU3BlY2llc0ZsYXRoZWFkXVwiICAgICAgICAgICAgICAgICAgICAgICBcInJfRXN0dWFyeVtUdWdnZXJhaCxTcGVjaWVzRmxhdGhlYWRdXCIgICAgICAgICAgICAgICAgICAgICAgICAgICBcbls0MV0gXCJyX0VzdHVhcnlbV2FsbGlzLkxha2UsU3BlY2llc0ZsYXRoZWFkXVwiICAgICAgICAgICAgICAgICAgICAgICAgIFwicl9Fc3R1YXJ5W0NhbWRlbi5SaXZlcixTcGVjaWVzTXVsbGV0XVwiICAgICAgICAgICAgICAgICAgICAgICAgIFxuWzQzXSBcInJfRXN0dWFyeVtDbGFyZW5jZS5SaXZlcixTcGVjaWVzTXVsbGV0XVwiICAgICAgICAgICAgICAgICAgICAgICAgXCJyX0VzdHVhcnlbSGF3a2VzYnVyeS5SaXZlcixTcGVjaWVzTXVsbGV0XVwiICAgICAgICAgICAgICAgICAgICAgXG5bNDVdIFwicl9Fc3R1YXJ5W0h1bnRlci5SaXZlcixTcGVjaWVzTXVsbGV0XVwiICAgICAgICAgICAgICAgICAgICAgICAgICBcInJfRXN0dWFyeVtMYWtlLklsbGF3YXJyYSxTcGVjaWVzTXVsbGV0XVwiICAgICAgICAgICAgICAgICAgICAgICBcbls0N10gXCJyX0VzdHVhcnlbUG9ydC5TdGVwaGVucyxTcGVjaWVzTXVsbGV0XVwiICAgICAgICAgICAgICAgICAgICAgICAgIFwicl9Fc3R1YXJ5W1R1Z2dlcmFoLFNwZWNpZXNNdWxsZXRdXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxuWzQ5XSBcInJfRXN0dWFyeVtXYWxsaXMuTGFrZSxTcGVjaWVzTXVsbGV0XVwiICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJyX0VzdHVhcnlbQ2FtZGVuLlJpdmVyLFNwZWNpZXNXaGl0aW5nXVwiICAgICAgICAgICAgICAgICAgICAgICAgXG5bNTFdIFwicl9Fc3R1YXJ5W0NsYXJlbmNlLlJpdmVyLFNwZWNpZXNXaGl0aW5nXVwiICAgICAgICAgICAgICAgICAgICAgICBcInJfRXN0dWFyeVtIYXdrZXNidXJ5LlJpdmVyLFNwZWNpZXNXaGl0aW5nXVwiICAgICAgICAgICAgICAgICAgICBcbls1M10gXCJyX0VzdHVhcnlbSHVudGVyLlJpdmVyLFNwZWNpZXNXaGl0aW5nXVwiICAgICAgICAgICAgICAgICAgICAgICAgIFwicl9Fc3R1YXJ5W0xha2UuSWxsYXdhcnJhLFNwZWNpZXNXaGl0aW5nXVwiICAgICAgICAgICAgICAgICAgICAgIFxuWzU1XSBcInJfRXN0dWFyeVtQb3J0LlN0ZXBoZW5zLFNwZWNpZXNXaGl0aW5nXVwiICAgICAgICAgICAgICAgICAgICAgICAgXCJyX0VzdHVhcnlbVHVnZ2VyYWgsU3BlY2llc1doaXRpbmddXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgXG5bNTddIFwicl9Fc3R1YXJ5W1dhbGxpcy5MYWtlLFNwZWNpZXNXaGl0aW5nXVwiICAgICAgICAgICAgICAgICAgICAgICAgICBcImxwX19cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcbls1OV0gXCJhY2NlcHRfc3RhdF9fXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwic3RlcHNpemVfX1wiICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFxuWzYxXSBcInRyZWVkZXB0aF9fXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJuX2xlYXBmcm9nX19cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXG5bNjNdIFwiZGl2ZXJnZW50X19cIiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcImVuZXJneV9fXCIgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcbiJ9 -->

```
 [1] "b_Intercept"                                                    "b_polyX135_degree_winds.standardiseddegreeEQ21"                
 [3] "b_polyX135_degree_winds.standardiseddegreeEQ22"                 "b_polyX45_degree_winds.standardiseddegreeEQ21"                 
 [5] "b_polyX45_degree_winds.standardiseddegreeEQ22"                  "b_Estuary_TypeBarrierRiver"                                    
 [7] "b_Estuary_TypeDrownedRiverValley"                               "b_Drought_Months"                                              
 [9] "b_SpeciesFlathead"                                              "b_SpeciesMullet"                                               
[11] "b_SpeciesWhiting"                                               "b_X135_degree_winds.standardised:X45_degree_winds.standardised"
[13] "b_Estuary_TypeBarrierRiver:Drought_Months"                      "b_Estuary_TypeDrownedRiverValley:Drought_Months"               
[15] "sd_Estuary__Intercept"                                          "sd_Estuary__SpeciesFlathead"                                   
[17] "sd_Estuary__SpeciesMullet"                                      "sd_Estuary__SpeciesWhiting"                                    
[19] "cor_Estuary__Intercept__SpeciesFlathead"                        "cor_Estuary__Intercept__SpeciesMullet"                         
[21] "cor_Estuary__SpeciesFlathead__SpeciesMullet"                    "cor_Estuary__Intercept__SpeciesWhiting"                        
[23] "cor_Estuary__SpeciesFlathead__SpeciesWhiting"                   "cor_Estuary__SpeciesMullet__SpeciesWhiting"                    
[25] "sigma"                                                          "r_Estuary[Camden.River,Intercept]"                             
[27] "r_Estuary[Clarence.River,Intercept]"                            "r_Estuary[Hawkesbury.River,Intercept]"                         
[29] "r_Estuary[Hunter.River,Intercept]"                              "r_Estuary[Lake.Illawarra,Intercept]"                           
[31] "r_Estuary[Port.Stephens,Intercept]"                             "r_Estuary[Tuggerah,Intercept]"                                 
[33] "r_Estuary[Wallis.Lake,Intercept]"                               "r_Estuary[Camden.River,SpeciesFlathead]"                       
[35] "r_Estuary[Clarence.River,SpeciesFlathead]"                      "r_Estuary[Hawkesbury.River,SpeciesFlathead]"                   
[37] "r_Estuary[Hunter.River,SpeciesFlathead]"                        "r_Estuary[Lake.Illawarra,SpeciesFlathead]"                     
[39] "r_Estuary[Port.Stephens,SpeciesFlathead]"                       "r_Estuary[Tuggerah,SpeciesFlathead]"                           
[41] "r_Estuary[Wallis.Lake,SpeciesFlathead]"                         "r_Estuary[Camden.River,SpeciesMullet]"                         
[43] "r_Estuary[Clarence.River,SpeciesMullet]"                        "r_Estuary[Hawkesbury.River,SpeciesMullet]"                     
[45] "r_Estuary[Hunter.River,SpeciesMullet]"                          "r_Estuary[Lake.Illawarra,SpeciesMullet]"                       
[47] "r_Estuary[Port.Stephens,SpeciesMullet]"                         "r_Estuary[Tuggerah,SpeciesMullet]"                             
[49] "r_Estuary[Wallis.Lake,SpeciesMullet]"                           "r_Estuary[Camden.River,SpeciesWhiting]"                        
[51] "r_Estuary[Clarence.River,SpeciesWhiting]"                       "r_Estuary[Hawkesbury.River,SpeciesWhiting]"                    
[53] "r_Estuary[Hunter.River,SpeciesWhiting]"                         "r_Estuary[Lake.Illawarra,SpeciesWhiting]"                      
[55] "r_Estuary[Port.Stephens,SpeciesWhiting]"                        "r_Estuary[Tuggerah,SpeciesWhiting]"                            
[57] "r_Estuary[Wallis.Lake,SpeciesWhiting]"                          "lp__"                                                          
[59] "accept_stat__"                                                  "stepsize__"                                                    
[61] "treedepth__"                                                    "n_leapfrog__"                                                  
[63] "divergent__"                                                    "energy__"                                                      
```



<!-- rnb-output-end -->

<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubTdhICU+JVxuICBzcHJlYWRfZHJhd3MoYl9JbnRlcmNlcHQsYl9wb2x5WDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkZGVncmVlRVEyMSxcbiAgICAgICAgICAgICAgIGJfcG9seVgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGRlZ3JlZUVRMjIsIGJfcG9seVg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkZGVncmVlRVEyMSxcbiAgICAgICAgICAgICAgIGJfcG9seVg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkZGVncmVlRVEyMiwgYl9Fc3R1YXJ5X1R5cGVCYXJyaWVyUml2ZXIsXG4gICAgICAgICAgICAgICBiX0VzdHVhcnlfVHlwZURyb3duZWRSaXZlclZhbGxleSwgYl9Ecm91Z2h0X01vbnRocyxcbiAgICAgICAgICAgICAgIGJfU3BlY2llc0ZsYXRoZWFkLCBiX1NwZWNpZXNNdWxsZXQsIGJfU3BlY2llc1doaXRpbmcsIFxuICAgICAgICAgICAgICAgYGJfWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkOlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkYCxcbiAgICAgICAgICAgICAgIGBiX0VzdHVhcnlfVHlwZUJhcnJpZXJSaXZlcjpEcm91Z2h0X01vbnRoc2AsIGBiX0VzdHVhcnlfVHlwZURyb3duZWRSaXZlclZhbGxleTpEcm91Z2h0X01vbnRoc2ApICU+JVxuICByZW5hbWUoXCJJbnRlcmNlcHRcIiA9IGJfSW50ZXJjZXB0LCBcIlVwXCIgPSBiX3BvbHlYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGRlZ3JlZUVRMjEsXG4gICAgICAgIFwiVXAgKHF1YWRyYXRpYylcIiAgPSBiX3BvbHlYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGRlZ3JlZUVRMjIsIFwiRHJvdWdodFwiID0gYl9Ecm91Z2h0X01vbnRocyxcbiAgICAgICAgIFwiRG93blwiID0gYl9wb2x5WDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkZGVncmVlRVEyMSwgXCJEb3duIChxdWFkcmF0aWMpXCIgPSBiX3BvbHlYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRkZWdyZWVFUTIyLFxuICAgICAgICAgXCJEb3duICogVXBcIiA9IGBiX1gxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZDpYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGAsXG4gICAgICAgIFwiRXN0IFR5cGUgKEJhcnJpZXIgUml2ZXIpXCIgPSBiX0VzdHVhcnlfVHlwZUJhcnJpZXJSaXZlciwgXCJFc3QgVHlwZSAoRHJvd25lZCBSaXZlciBWYWxsZXkpXCIgPSBiX0VzdHVhcnlfVHlwZURyb3duZWRSaXZlclZhbGxleSxcbiAgICAgICAgXCJFc3QgVHlwZSAoQmFycmllciBSaXZlcikgKiBEcm91Z2h0XCIgPSBgYl9Fc3R1YXJ5X1R5cGVCYXJyaWVyUml2ZXI6RHJvdWdodF9Nb250aHNgLFxuICAgICAgICBcIkVzdCBUeXBlIChEcm93bmVkIFJpdmVyIFZhbGxleSkgKiBEcm91Z2h0XCIgPSBgYl9Fc3R1YXJ5X1R5cGVEcm93bmVkUml2ZXJWYWxsZXk6RHJvdWdodF9Nb250aHNgLFxuICAgICAgICBcIlNwZWNpZXM6IEZsYXRoZWFkXCIgPSBiX1NwZWNpZXNGbGF0aGVhZCwgXCJTcGVjaWVzOiBNdWxsZXRcIiA9IGJfU3BlY2llc011bGxldCwgXCJTcGVjaWVzOiBXaGl0aW5nXCIgPSBiX1NwZWNpZXNXaGl0aW5nKSAlPiVcbiAgdGlkeXI6OnBpdm90X2xvbmdlcihjb2xzID0gYyg0OjE3KSwgbmFtZXNfdG8gPSBcIlBhcmFtZXRlclwiKSAlPiVcbiAgI21lZGlhbl9xaSgpICU+JVxuICBnZ3Bsb3QoYWVzKHggPSB2YWx1ZSwgeSA9IFBhcmFtZXRlcikpICtcbiAgc3RhdF9oYWxmZXllKG5vcm1hbGl6ZSA9IFwiZ3JvdXBzXCIpICsgZ2VvbV92bGluZSh4aW50ZXJjZXB0ID0wICwgY29sID0gXCJyZWRcIiwgbHR5ID0gMikgK1xuICB4bGFiKFwiRXN0aW1hdGVcIikrXG4gIHRoZW1lX2NsYXNzaWMoKSArIHRoZW1lKGF4aXMudGV4dCA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgYXhpcy50aXRsZSA9IGVsZW1lbnRfdGV4dChmYWNlID0gXCJib2xkXCIpKSMgK1xuICAjc2NhbGVfeV9jb250aW51b3VzKHRyYW5zID0gZ2dhbGxpbjo6c3NxcnRfdHJhbnMsIGJyZWFrcyA9IGMoLTIwLCAtMTAsIC0xLDEsMTAsMjApKVxuZ2dzYXZlKFwiLi4vcGxvdHMvQ1BVRSBtdWxpdHNwZWNpZXMgRXN0aW1hdGUgcGxvdHMucG5nXCIsIGRwaSA9IDYwMCwgaGVpZ2h0ID0gMTIsIHdpZHRoID0gMTgsIHVuaXRzPSBcImNtXCIpXG5gYGAifQ== -->

```r
m7a %>%
  spread_draws(b_Intercept,b_polyX135_degree_winds.standardiseddegreeEQ21,
               b_polyX135_degree_winds.standardiseddegreeEQ22, b_polyX45_degree_winds.standardiseddegreeEQ21,
               b_polyX45_degree_winds.standardiseddegreeEQ22, b_Estuary_TypeBarrierRiver,
               b_Estuary_TypeDrownedRiverValley, b_Drought_Months,
               b_SpeciesFlathead, b_SpeciesMullet, b_SpeciesWhiting, 
               `b_X135_degree_winds.standardised:X45_degree_winds.standardised`,
               `b_Estuary_TypeBarrierRiver:Drought_Months`, `b_Estuary_TypeDrownedRiverValley:Drought_Months`) %>%
  rename("Intercept" = b_Intercept, "Up" = b_polyX45_degree_winds.standardiseddegreeEQ21,
        "Up (quadratic)"  = b_polyX45_degree_winds.standardiseddegreeEQ22, "Drought" = b_Drought_Months,
         "Down" = b_polyX135_degree_winds.standardiseddegreeEQ21, "Down (quadratic)" = b_polyX135_degree_winds.standardiseddegreeEQ22,
         "Down * Up" = `b_X135_degree_winds.standardised:X45_degree_winds.standardised`,
        "Est Type (Barrier River)" = b_Estuary_TypeBarrierRiver, "Est Type (Drowned River Valley)" = b_Estuary_TypeDrownedRiverValley,
        "Est Type (Barrier River) * Drought" = `b_Estuary_TypeBarrierRiver:Drought_Months`,
        "Est Type (Drowned River Valley) * Drought" = `b_Estuary_TypeDrownedRiverValley:Drought_Months`,
        "Species: Flathead" = b_SpeciesFlathead, "Species: Mullet" = b_SpeciesMullet, "Species: Whiting" = b_SpeciesWhiting) %>%
  tidyr::pivot_longer(cols = c(4:17), names_to = "Parameter") %>%
  #median_qi() %>%
  ggplot(aes(x = value, y = Parameter)) +
  stat_halfeye(normalize = "groups") + geom_vline(xintercept =0 , col = "red", lty = 2) +
  xlab("Estimate")+
  theme_classic() + theme(axis.text = element_text(colour="black"),
                          axis.title = element_text(face = "bold"))# +
  #scale_y_continuous(trans = ggallin::ssqrt_trans, breaks = c(-20, -10, -1,1,10,20))
ggsave("../plots/CPUE mulitspecies Estimate plots.png", dpi = 600, height = 12, width = 18, units= "cm")
```

<!-- rnb-source-end -->

<!-- rnb-plot-begin eyJoZWlnaHQiOjQzMi42MzI5LCJ3aWR0aCI6NzAwLCJzaXplX2JlaGF2aW9yIjowLCJjb25kaXRpb25zIjpbXX0= -->

<img src="data:image/png;base64,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" />

<!-- rnb-plot-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBXaGljaCBvbmVzIGFyZSBhY3R1YWxseSBpbXBvcnRhbnQgZWZmZWN0cy9pbnRlcmFjdGlvbnNcbm03YSAlPiVcbiAgZ2F0aGVyX2RyYXdzKGJfSW50ZXJjZXB0LGJfcG9seVgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGRlZ3JlZUVRMjEsXG4gICAgICAgICAgICAgICBiX3BvbHlYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRkZWdyZWVFUTIyLCBiX3BvbHlYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGRlZ3JlZUVRMjEsXG4gICAgICAgICAgICAgICBiX3BvbHlYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGRlZ3JlZUVRMjIsIGJfRXN0dWFyeV9UeXBlQmFycmllclJpdmVyLFxuICAgICAgICAgICAgICAgYl9Fc3R1YXJ5X1R5cGVEcm93bmVkUml2ZXJWYWxsZXksIGJfRHJvdWdodF9Nb250aHMsXG4gICAgICAgICAgICAgICBiX1NwZWNpZXNGbGF0aGVhZCwgYl9TcGVjaWVzTXVsbGV0LCBiX1NwZWNpZXNXaGl0aW5nLCBcbiAgICAgICAgICAgICAgIGBiX1gxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZDpYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZGAsXG4gICAgICAgICAgICAgICBgYl9Fc3R1YXJ5X1R5cGVCYXJyaWVyUml2ZXI6RHJvdWdodF9Nb250aHNgLCBgYl9Fc3R1YXJ5X1R5cGVEcm93bmVkUml2ZXJWYWxsZXk6RHJvdWdodF9Nb250aHNgKSAlPiVcbiAgbWVkaWFuX3FpKCkgIyU+JVxuYGBgIn0= -->

```r
# Which ones are actually important effects/interactions
m7a %>%
  gather_draws(b_Intercept,b_polyX135_degree_winds.standardiseddegreeEQ21,
               b_polyX135_degree_winds.standardiseddegreeEQ22, b_polyX45_degree_winds.standardiseddegreeEQ21,
               b_polyX45_degree_winds.standardiseddegreeEQ22, b_Estuary_TypeBarrierRiver,
               b_Estuary_TypeDrownedRiverValley, b_Drought_Months,
               b_SpeciesFlathead, b_SpeciesMullet, b_SpeciesWhiting, 
               `b_X135_degree_winds.standardised:X45_degree_winds.standardised`,
               `b_Estuary_TypeBarrierRiver:Drought_Months`, `b_Estuary_TypeDrownedRiverValley:Drought_Months`) %>%
  median_qi() #%>%
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



Model checks

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubXkuZGYyICU+JVxuICBhZGRfcmVzaWR1YWxfZHJhd3MobTdhKSAlPiVcbiAgZ2dwbG90KGFlcyh4ID0gLnJvdywgeSA9IC5yZXNpZHVhbCkpICtcbiAgc3RhdF9wb2ludGludGVydmFsKClcbmBgYCJ9 -->

```r
my.df2 %>%
  add_residual_draws(m7a) %>%
  ggplot(aes(x = .row, y = .residual)) +
  stat_pointinterval()
```

<!-- rnb-source-end -->

<!-- rnb-plot-begin eyJoZWlnaHQiOjQzMi42MzI5LCJ3aWR0aCI6NzAwLCJzaXplX2JlaGF2aW9yIjowLCJjb25kaXRpb25zIjpbXX0= -->

<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAArwAAAGwCAMAAAB8TkaXAAAAulBMVEUAAAAAADoAAGYAOpAAZmYAZrYzMzM6AAA6ADo6AGY6Ojo6OpA6kNtNTU1NTW5NTY5NbqtNjshmAABmADpmAGZmtttmtv9uTU1uTW5uTY5ubqtuq+SOTU2OTW6OTY6OyP+QOgCQOjqQZgCQkDqQkGaQ2/+rbk2rbm6rbo6ryKur5P+2ZgC2///Ijk3I///bkDrb/7bb///kq27k///r6+v/tmb/yI7/25D/5Kv//7b//8j//9v//+T///+bwch2AAAACXBIWXMAAA7DAAAOwwHHb6hkAAAdo0lEQVR4nO2dC3vbNpaG0WbTzuxGbdrZTS9J6+wlacepnWmCJK5j/v+/taJE4noAAtThISF9Z56pJYp8+QF8CUKU7KgOhWq01NoBUKi5BXlRzRbkRTVbkBfVbEFeVLMFeVHN1nx5daayL55aS8IRXBo+hw15xdkIzsWGvOJsBOdiQ15xNoJzsSGvOBvBudiQV5yN4FxsyCvORnAuNuQVZyM4FxvyirMRnIsNecXZCM7FhrzibATnYkNecTaCc7EhrzgbwbnYkFecjeBcbMgrzkZwLjbkFWcjOBcb8oqzEZyLDXnF2ZxwtSA7rq31OOQVZ0NeLjbkFWdDXi425BVnQ14uNuQVZ0NeLjbkFWdDXi425BVnQ14uNuQVZ0NeLjbkFWdDXi425BVnM8KVCu1tJDgPG/KKsyEvFxvyirMhLxcb8oqzIS8XG/KKsyEvFxvyirPZ4OpYi7DJ2lqPQ15xNuTlYkNecTYXXCnC3haCs7Ehrzgb8nKxIa84G/JysSGvOBtzXi425BVnQ14uNuQVZ+M+Lxcb8oqzIS8XG/KKsyEvFxvyirMhLxcb8oqz8WV0LjbkFWdDXi425BVnQ14u9unyolYsHLa+MPKKsTHycrEhrzgb8nKxIa84G/JysSGvOBvycrEhrzgb8nKxIa84G8G52JBXnI3gXGzIK85GcC425BVnIzgXG/KKsxGciw15xdkIzsWGvOJsBOdiQ15xNoJzsSGvOBvBudiQV5yN4FxsyCvORnAuNuQVZyM4FxvyirMRnIsNecXZCM7FhrzibATnYkNecTaCc7EhrzgbwbnYkFecjeBcbMgrzkZwLjbkFWcjOBcb8oqzEZyLDXnF2QjOxYa84mwE52JDXnE2gnOxIa84G8G52JBXnI3gXGzIK85GcC425BVnIzgXG/KKsxGciw15xdkIzsWGvOJsBOdiQ15xNoJzsSGvOBvBudiQV5yN4FxsyCvORnAuNuQVZyM4FxvyirMRnIsNecXZCM7FhrzibATnYhfK+2632317292/2D19340/IO/24BcVvFDem6v+vw+vr7p3340/IO8G4RcVvEzeh9+u+x/3v952dz/dDj8g7wbhFxW8TN79PGG3u+rufn7f3f9yPfzYL/9qX/khG4VavPLy3v143Y++H58erB1+DK9xn07l512jbATnYpfJe6ibq2jkhbxbg19U8Cp5MefdPPyigpfJ208UHn6/fXj9/Hi34TnuNmwUflHBC0fed7vdN9cd7vNuHn5RwSumDYniTlQevVE2gnOxIa84G8G52JBXnI3gXGzIK85GcC425BVnIzgXG/KKsxGciw15xdkIzsWGvOJsBOdiQ15xNoJzsSGvOBvBudiQV5yN4FxsyCvORnAuNuQVZyM4FxvyirMRnIsNecXZCM7FhrzibATnYkNecTaCc7EhrzgbwbnYkFecjeBcbMgrzkZwLjbkFWcjOBcb8oqzEZyLDXnF2QjOxYa84mwE52JDXnE2gnOxIa84G8G52JBXnI3gXGzIK85GcC425BVnIzgXG/KKsxGciw15xdkIzsWGvOJsBOdiQ15xNoJzsSGvOBvBudiQV5yN4FxsyCvORnAuNuQVZyM4FxvyirMRnIsNecXZCM7FhrzibATnYkNecTaCc7FPlxeFWrkw8oqxEZyLDXnF2QjOxYa84mwE52JDXnE2gnOxIa84G8G52JBXnI3gXGzIK85GcC425BVnIzgXG/KKsxGciw15xdkIzsWGvOJsBOdiQ15xNoJzsSGvOBvBudiQV5yN4FxsyCvORnAuNuQVZyM4FxvyirMRnIsNecXZCM7FhrzibATnYkNecTaCc7EhrzgbwbnYkFecnYSrBdkstbUeh7zibMjLxY7k/et7NdaXf0BeSTjkrd0GI680G/JysSGvOBvycrFJeT99jWnDCnDIW7sNIe/nl08+v3z21/fPMPKKwiFv7TaEvL22b550Hx79CXkF4ftr3WJsntpaj6fkffu4+4Bpgygc8lZvQ8jbvTmY+xYjrygc8lZvQ8m7n/R2b9QXr0rchbxccMhbvQ0lb1VxJyqP3igb8nKxIa84G/JysSl5x0+I8YZNED58JL8Im6uEe3yyM9Ij71//wJxXEA55ozpBXtznlYSbb0MtwGarluTFtEEODnnjRafI+wYjrxwc8saLZsk7vGHDfV5JOOa8UZ0y8hZWZSK+grylbK6CvGx1XvLiPm9U9fLi14CWZkPeMvZ0f5Aj79teW+/7vHc/7HZXXfdut9t9e9vdv9g9fQ95meGQ16958g7aOrfK7n+57u5+vO5urvpnD6+vunffQV5mOOT1i0vej72qN1cPv10fVP71trv76Rby8sIhr1+nTRueeHPb/ei7ny70s4e7n98fxuKu+2pfHYqp5r9pPsMa3neVres9+9Bv5/8K28Pr54eZw370/fh0lLevutOJsc5t5MXvsLlVcuuQljeu+xfPh0c3V3bkhbyccMhrq+gTx0J57364Gh/eXGHOuwwc8tqaJ+/+3Vr8fd7B3X6+8PD7bT+DwN0GfjjktcU48vb3d/u3avuf31x3uM+7DBzyOsU5501XTSLWgrwVbJZqQt7j1KHs02HIywaHvF7NvM/75nF/q/ftY8grCoe8Xs3+hO3zS/zFnCbhZxR8trz9x2uQt0H4OQWf9X3ezy+ffPjiVT95gLyy8NMnDufU4/O+jP7pa/W49FfYIC8jHPK6hd+k2B4b8hayIe/22JC3kD1T3rdKPSv8C6eQlxEOed2aJ++bR/863i2DvLJwyOvWzL/bcPgHKXCrTBwOed2CvNtjp+Gr/d2Gwt1urccpebu3/bQh/DUgyLs4HPJWbkPJe/w1oDJ3IS8fHPJWbkPKW1PcicqjN8qGvFxsSt7PL8v++UDIywyHvJXbEPKW/tuXkJcVzvKn9i5e3q708wnIywmHvNXbkCMv/tDeguwEnOfPS1+8vHXFnag8eqPs7clbutOt9TjkFWdDXi52Rl58wiYLX2/Oe4byYuSVhUPe6m0grzR7e/d5IS9XovLojbIhLxcb8oqztyZv+XRlaz0OecXZ5y/vnFZA3ibYqVtlK8lbcYsO8rLV+cm7ym9SQF7GROXRG2VD3lPY3jaQV5q9MXkx52VMVB69UbaIvBUkyMuXqDx6o+ytyauLP9eDvGwFeTNsyLumvNMdcF7yHgQ6D3ln3fGDvGwFeXPwaV7F3uvY/jaQV5qdk5ftQ4oqeZl/kwLyXqS8tUNvvPq68h45ly5vSX82KK9Kw9eTt/xLxJC3qM5WXpWC88pb4w+zvDO/U7+OvIuU2lognko2azjita1Ort6ziiE1K0+gOtsUwcLIK8FOjrzjcNX+yDv390jPadpQ0PozklfNPOSQF/KKsVXic7QV5a3ZL+QtqouVt+6YJ+StQdl1GSZqzb1h4050qAuTd+7Qe7q8zn5Pl/eIgbxnKW96iJsnL7F6p+uu3PzyXvx93qLOPyd57ahVyQsXbUDeOeMu5GWs5eRNWbKSvKxzXsirS3sf8ibkXe0NG+TVrry5XmhN3vz1ec6sl0NeS0mvP76S7xVzzGZ9Nw7ystWS8tIHt15ecvXOvFST6/gzWBw/hLwlVfQmojF5lcr7WW0vs7zhJrXyKjXVwmxBXrZqQF569bnyxgMm5J2RqC/Tfsg7zfOWzpB3+OAkKa+aI+/FfkjhjLtnJO/UZ8Cq/n0Wsf44fa2Rh0feE4deyEuwZiVYWl7q2NbKS68+Tl8h7zry2vafl7wTE4fhFmk1LqbMkjeCVcs7eW3J1vnJm+2FpuWNc1XLG65u3yRwyGuf2OWZXjH7ZZG3cHPIO59dWxuWN441V95cI4qvo+3K68x5z0ze7NCUkjfTAlre6rFPSt7sRPAs5U23ozV58wPvuEq8LIeMnjDJ6zxzHk7Jq8frZWKdi5B3PNkvTl7yq77JV2h5p3aRzRY8Dx6WyZvsc0pe8/Tc5J04CI3Km53YqvhRlbwF50cuWzDwOgfh8BLknS7Iq2fKWzK4Z7KFUTPyhvQLkVcFP6k1xoOcPQqZrix5JVv5zeZAPR0KyLPlrZ/zai554wPmB4S8FNw/0tFb8PrKfzxymrxxSIq8qrzTc17Im0g7XFnz8qq8vOMhV5EORR3DLq/bmIXlzY/tmXD0ktTIG3hJHzCC6Q8thtP5axblXlXe4FCNDRlfyQ4hlLzKcgZJfHkz1kT0GnmzF5HhwWnyJvJQbkzMqkkM0dGhuuLyFjRBXF77jpaUdxCOQV7DsivNktf5WwveRSN1caQ5s+UdtFhEXiKbi3NfY5bXPcLHV5uU1wy2obxUl3oqniDvZMdQ8tplRfIGF5NYXkrIoEf0hLzepl6lGxY/oTfy3FUl0wZq+kEGvGB5xy0WkPd4BHyxaHm9gxfhPEWHn3XymuB2TItWDDZ1h95E+7jkjdam5PU72j+1QnlVK/Iq83NsniudaV5GXuU3N5jqZuR1ZsPBxuMTX177cFJeZR+m5Q1eoY9PSl7KO/dBRl5FPHKeUO7WyOucl8Eb5Ly87vncRWvqyVpLXtPHoSqxvN79R+3JOzwdO8AIMi1vMLA6D82+vXCOr+7QMW4YjOvOFoG84zqF8iqiG8x+tP/AWS8cQ+2m49DhL07K69t7grxeQ9qW1/ihrLG+vOFRG7tST8trwf7mRl7nbKGkI+RV48a2c91jQyIdTCBvp52Tl+ihsZuUXlBe/+RJyevv3pdXRStF8jqhI3mVt13IjuJQJSqvOTfdEdaTzm2f129GLG2G1M4cA2+D2P9xz3l57XBQI+84gKTlVc4qtle8BVEvJZpiezGSN26uC3SMnZbX2423+85d+xR5g9a559eG5SVaUC6vmimvch0zO9XT8nrrBxe7KnntFmOveAvCXjpN3uTIOyGvu/+xJX513ulLyevuWflU4qC4gckryERtQV6q/OZm5Y36guonz/AJeckOVqXyKh1u6B7tsVcMIq5MS/SYOpQ3aq45g2xK16YCeWPsKG+iR8JcKXkJkZ0sntMTJSKvIrs4W0FznQHaPJwvr+3poduU09fBhlEuD+qZZobnVBrTKy4u7LPc7l0hrWpRzxbKqxRpb1re8QT1uyApr9fqdKP8zlHZzgn02qS8YXN1oInbdwU0ajWno/zpx1SyABvKax7TWx97xdsmKD951BJzPbeqRT079vl4atq0yp3Kq0l5yf2HVyPt5mGQ90hT5yGvQdCrFtDK5C1LFoLTT6mtj73ibxOUs5NRKru5GjxRgbyxvVbeIJw2NitK3uH6oSlslbxBqyf7yGmPuhR5p1mJYTWYc8yRt/JVNbRlUl5n9WDrSF6lqK7VGXk9qLdXp6O9KNrtpmhe5cVIyWvXnuibcA/NyqvtUSrekmAlumwe85QkKimve4zcvZTLG9kby0u3ISGv2+uBTWGTvGnMucpbc+CZ5aWXr1BDr4SL/BHQfU0F4tTLS4SIErnzFCuvSzObxfMvrwWEvDp54lCkxeW9f7F7+r5UXmXkrShX+YXkXaOGXomWpeRVtLzulgl5zX/KMtnHKn7oyevHiVpwirzKHO8gHae8D6+vunffLS2v05iaLTddo7zh2BIfcrLN0SwidWVK2JbMROwhWJobAvzTL2yI3pa897/ednc/3RbLG3RLQY0NOC95Ta9EF+5CeSOgIUR7KmYUyBv4RyLK5KXu+lAgPx2rvHc/v+/uf7neP/pqX/l1Vf+/vjr/YjlR3XH1Y7CaDbdcbqf4i81LlUBDiPZUFyoimiNgAk4wQn/81yw+4pBdEcSbqhp5Pz4d5e0rP/KqOSOvGW51MMtrumyvhO/f03Pe4+vTaDOAVZ/p3m79Mc8LGO/Sj5gYeMf3gKME+evoOA0aHqhFR94aeUs60qy7uryn3hIjOEl5VV7e9G194lGlvMFuSXnTwdx1E+7aGxglJP+gLyJvzZy36s2+HtN7s7mqIdv9sWLRI6gjb/BunDzu5TuhzpUyhLtfeuOSXBl5nRsfhS3UwVs8XnkfXj8vvttQ985jPJDNy0tVSl7/RXLLFM48zK5Zkuk0eV3DyEule4Im7z9b1oLy1tznnWhzOKeI7qq5M6YMJn5wksAs9ofnHaO8wR0yJnnz6xRyUkdLu5wCeb37FZzyejVXXj00IrgNOE/esEs45Z1LSsqrvJlR/CJJiiKZu4juVrpuzkt+qBCvNC2vexYQswaVkNc5/9wttN68vHpBeUve5UzhiCe5bSaW6iXltfuYkDfqamfHyR6rlpec8bcnb7a1Kp7/6ODMHRs7cdFS5kTwl9FrThWjvPbC7pQ6XV493mBTobz2/dFEzFJ5yVjxRIaW133kcrS2s2AVdfLG5dW+vO4xTsibU0hrQl6yx/PvFbQJ15klCZ/iCHYLamkkb4BNy3tse1ZeZ7mUvNrfj9s6T1J3dzl5I/om5TXXjby85hBbebMXtrF3vK4KV5uUV48bd/ahzu7ZizAmdYOZ62NgZ+ItGxkqkleN16g6eYkZWn7IT8aqlzc4YQN5I3vDXlxBXrIzhp7z5LWuus1xjn1OO10rb2pCoccZNiHvxCQkLy/RJ8XyWn58abUGjws72535vEzy2nNzJDlDTngRpeWlTlgigrS8ic6wopirnyuvn9ppanzA3dXcXgvOf6/DAhHtURi6/vDTyKtNXmqip2w8GzGYKwTdH23sHyPn5eillLzusuF7fI6kpo3xpMGVN2ybt2vqRPPaYHrcXjCjM95v3hnI660bbeU+Gxs7domdKw1bOVubp2532HPF69XT5LX0jcnrXbcJl9zRI3msnORU1LHVdg/OcaTldbomI68/lUj7taS8RFfYK128drSV97RUXn/+EWxuO80c2aS8Tu8OW/jDmXIX2MPlH0KdkTfsIf+5t7OgMePa4VJNyet0W7D3sNuoIl82nes2cFJeL5bdN7nT8T9uhLg2Lq+3qd3YNJBqnHIfmk0Jecc+0u5YnZbXOZG00v6xsYfLNMINQfRK1ENh28PjGc+BRmfclXx5tSdvuHe/s9yONqL7ucfb9nYDh1gmr981YR/YDEpR6wa1lrzE2tFW4RO72nDw6Nal5A2ymXMokPfYLZqS15vgZeUNQhC9EvWQ+3Q40RKv2uemiQZp5A1anpDXfzjuOy2vu9CPRcobHv5gl2EXRKur9eQlJjMF8jq9ERw9u5paSV5z7MZR3E2n/D27IaJuiTvIexrLS3SGMqeRs9DKa643QTi3M/3+M/s2neNrpju/U4JUfpT4WnG58garHbuGZvkKeYfQW4uUd+wWTRw7HckbDGQpecNdl8irnGlvqEm5vKYVdfI6p6vbjkp5CRvj/nA2J3RZUd7oQ4oieePDokh5OyZ5Kcg8eZMhgtfia3UwjRjkda+8obx+jxok8c9GBM74vRnLaxo4IW/sWjD9pgfeGHss6mswOu/uxuX1+jNcuUBezSivs5WVN3llm5A3NsB7auW1p0dggdcxK8rr5TpneVPehm3ilzdecUV5O8eAsIVDL+Xk9TrGSm+Dx1nJNoTpffHiDX15Y9ViUSl1U1ci4vcW9MryDomC832ynAE6I69dKd7YeUZJ6Gubkpfaahl53fHMnLZjP7jThBnypqNF543f2ZS8brutYuOg609mquUN7KXW9UpAXnssiiu4lhHt1Wl5wzVTBzG3eapVsbwlCUL4tLzuES+Q17aR+jdtk3km5I22y8g7RoubEw1dhfJm2zA09vzlLVgpqil59VLyBmPW8Fj7sjhrhLlq5A1tzburg15xhkcXpLwR0yTOXYns0YzvqKTDaDl56wRilre+OOWN4cGsM5aXOoLblTd8z6bPR159mrz0dX8teTW7vNE7NndZRl7/YmYenSLv5BuT8J8YcW920PKO1918Lznykk1P5mlT3iO8FlteBfImF0zDg4FIudYk5TXzYe0+DUNkD0cYOBxqZ8qrgtIny2tP0GxLlpbXJDhFXrpPG5V3+B0j53ksb3AXd3ypBXmDdzip+3VRIlrefLUu70JVwW5YXq2Crq2TVxPunp280+nzLTs7eXVGXrM4vt8aHlXq4dryasgbtGzT8tbWfHntG4hguVN1wUN5p+aY4eYWcpK8ln34RDhoZmYbKXln1wXIG849D4uJNqvs00PNkNefo+WKlte/z5uQd7IM+7zkpQ4tFzxVi8rrNyghL3X4uOWN98sib+qAlbHPSt5kX5ydvMHQS206tcYq8voDDOR1ij60TPBULSlv1KBm5XVBZt4et6+oAnnLtoG8VC3HVvHBPRN5o1lETRTIy1ei8tLX2cXlJZNkt4C81ZXqitbljZYFKxLbTuPXkfdwS8i9H13t7qy7qpCXLFl5yRtj826MlwdXirJ3JhzyJovuiWbl7eIGVR/rVG1B3lnNmdPjkJespdgpXU6T19kW8kLeC5CXnPPmC/LOqLORN6kL5D1beekp/DnJO/97S8HGtfd5qzwr/5ZzfXMgL1s1IK8iH0JeyLvwnDduUOPyRuSaGJVsZxvIK8lmkjf46spcedNfN6Xr/OSVqG2nq6r+Pi/RoLoWBojZ3UNG4SjZA4aRV4itjn+jOVpcB/G2cAfP6pG3Zs/nN/JyJ6IK8sZre19KM69AXsi7tLzx4jqIhrwa8iZq0d+kgLwnsZ1tmpBXHL59eY2x/o1jyAt5xeWtZWxc3vqCvC2wGeR1P1wIPrOrZ0NexkTl0dtkb03eitpaj0NeaTbkZWNDXmk2j7xW2FPmvJW1tR6HvNLsk+UNvocLeSGvHJtRXjU+5WLna2s9DnnF2SfCFeQ120BeaTbk5WJDXnE2o7zhjHfTwfnZkFeczfqGDfJCXkk2m7w6usu77eDsbMgrzub8kALyQl5RNuTlYkNecTbrx8OY80JeSTaPvNr7agMfO1db63HIK87m+T6v9wdFOdmZ2lqPQ15xNuTlYkNecTa/vKzsTG2txyGvOJtZXv9XITYenJcNecXZkJeLDXnF2ZCXiw15xdmQl4sNecXZkJeLDXnF2dxwyAt5xdiQl4sNecXZkJeLDXnF2QjOxYa84mwE52JDXnE2gnOxIa84G8G52JBXnI3gXGzIK85GcC425BVnIzgXG/KKsxGciw15xdkIzsWGvOJsBOdiQ15xNoJzsQvlvftht7vqune73e7b2+7+xe7pe8i7RfhFBS+T9/6X6+7ux+vu5qp/9vD6qnv3HeTdIvyigpfJ+7FX9ebq4bfrg8q/3nZ3P91C3g3CLyp44bThOPrupwv97OHu5/eHsbjrvtrX5IYo1LI1Ke/D6+eHmcN+9P34dJS3L+7Tqfy8a5SN4FzsaXlvdrv9pOH+xfPx+ZUdeSHv1uAXFbxw5L374crIfOXPeXPV7JwCwaXrlOB5eQd3+/nCw++3/QzC3m1YKtGqheDStZy8/f3d/q3a/uc3151/n3epRKsWgkvXcvLOrYvsylXrIoMvIy8KJVCQF9VsQV5UswV5Uc0W5EU1WwvIW3xDbUN1+OxlCN5Q/uEbq+0F/3j4iu2pwfnl9b842UZ97LtyCN5Q/uEbq+0F78cKm3h2cH55yz9E3kzdfPPPfeAheEP5h2+sthe8L5t4dnB+ef2v7zRSfd8NwRvLbxM3Fnw/1p4anF9e/4uTjVQv7xC8rfz9901aDH73wzfXJwfHyHuoVkfewzdWWwzOcsnAnPdQd03OeYdv/TUYvK/TJ+tL3G0o/eLkhqrvuyF4Q/mHb6y2F3yYKJwaHPd5D9Xmfd7xG6vNBQ++Y7ud+7wolFBBXlSzBXlRzRbkRTVbkBfVbEFeVLMFeVHNFuRFNVuQF9VsQV5UswV5JevT3/9XffnH55dKPe4+v3zSdR/Us65783jtXI0W5JWsT1/vPf388vHh/28f/dm9+fcn+8fP1s7VaEFeyfr09d7TD1/+cfjPp7//8fm//+/Rn/ufa+dqtCCvZH3626u9t/sRt3+0H3E//ce//vOPfgRGzSnIK1mevPu57ofHn1/+Vz/3Rc0pyCtZR3m/eHWcO3x49D/Purf/9o9Xa8dqtSCvZB3kHd+wdX99v9f4g8KsYW5BXqnqb40d5O2Ot8r29Wbv7V/fY9YwtyAvqtmCvKhmC/Kimi3Ii2q2IC+q2YK8qGYL8qKaLciLarYgL6rZgryoZuv/AY9+Gm3RmGjEAAAAAElFTkSuQmCC" />

<!-- rnb-plot-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZmlzaF9kYXRhICU+JVxuICBhZGRfcHJlZGljdGVkX2RyYXdzKGZpdDUpICU+JVxuICBzdW1tYXJpc2UoXG4gICAgcF9yZXNpZHVhbCA9IG1lYW4oLnByZWRpY3Rpb24gPCBDb2FzdGFsX05vcm1hbGlzZWRfQWJ1bmQpLFxuICAgIHpfcmVzaWR1YWwgPSBxbm9ybShwX3Jlc2lkdWFsKVxuICApICU+JVxuICBnZ3Bsb3QoYWVzKHNhbXBsZSA9IHpfcmVzaWR1YWwpKSArXG4gIGdlb21fcXEoKSArXG4gIGdlb21fYWJsaW5lKClcblxuYGBgIn0= -->

```r
fish_data %>%
  add_predicted_draws(fit5) %>%
  summarise(
    p_residual = mean(.prediction < Coastal_Normalised_Abund),
    z_residual = qnorm(p_residual)
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()

```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiRXJyb3I6IGNhbm5vdCBhbGxvY2F0ZSB2ZWN0b3Igb2Ygc2l6ZSAxMTMuNiBNYlxuIn0= -->

```
Error: cannot allocate vector of size 113.6 Mb
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubXkuZGYyICU+JVxuICBhZGRfcHJlZGljdGVkX2RyYXdzKG03YSkgJT4lXG4gIHN1bW1hcmlzZShcbiAgICBwX2xvd2VyID0gbWVhbigucHJlZGljdGlvbiA8IHlfbG93ZXIpLFxuICAgIHBfdXBwZXIgPSBtZWFuKC5wcmVkaWN0aW9uIDwgeV91cHBlciksXG4gICAgcF9yZXNpZHVhbCA9IHJ1bmlmKDEsIHBfbG93ZXIsIHBfdXBwZXIpLFxuICAgIHpfcmVzaWR1YWwgPSBxbm9ybShwX3Jlc2lkdWFsKVxuICApICU+JVxuICBnZ3Bsb3QoYWVzKHggPSAucm93LCB5ID0gel9yZXNpZHVhbCkpICtcbiAgZ2VvbV9wb2ludCgpXG5gYGAifQ== -->

```r
my.df2 %>%
  add_predicted_draws(m7a) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual)
  ) %>%
  ggplot(aes(x = .row, y = z_residual)) +
  geom_point()
```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiRXJyb3I6IFByb2JsZW0gd2l0aCBgc3VtbWFyaXNlKClgIGlucHV0IGBwX2xvd2VyYC5cbnggZXJyb3IgaW4gZXZhbHVhdGluZyB0aGUgYXJndW1lbnQgJ3gnIGluIHNlbGVjdGluZyBhIG1ldGhvZCBmb3IgZnVuY3Rpb24gJ21lYW4nOiBvYmplY3QgJ3lfbG93ZXInIG5vdCBmb3VuZFxuaSBJbnB1dCBgcF9sb3dlcmAgaXMgYG1lYW4oLnByZWRpY3Rpb24gPCB5X2xvd2VyKWAuXG5pIFRoZSBlcnJvciBvY2N1cnJlZCBpbiBncm91cCAxOiBZZWFyID0gMTk5NywgQ1BVRSA9IDAuNTAyODUxOSwgU3BlY2llcyA9IFwiV2hpdGluZ1wiLCBFc3R1YXJ5ID0gXCJIYXdrZXNidXJ5IFJpdmVyXCIsIEVzdHVhcnlfVHlwZSA9IFwiRHJvd25lZCBSaXZlciBWYWxsZXlcIiwgWDQ1X2RlZ3JlZV93aW5kcyA9IDUwNjkuOTk5LCBYMTM1X2RlZ3JlZV93aW5kcyA9IDQ1NjQ0LjIsIERyb3VnaHRfZGVjbGFyZWRfMW0gPSBcIkRyb3VnaHRcIiwgRHJvdWdodF9kZWNsYXJlZF82bSA9IFwiTm8gRHJvdWdodFwiLCBEcm91Z2h0X01vbnRocyA9IDIsIE1vZGFsX0FnZSA9IDUsIFNwYXduaW5nX1BlcmlvZCA9IFwiRGVjLUZlYlwiLCBDYXRjaCA9IDk3MSwgRWZmb3J0X2RheXMgPSAxODc0LCBYNDVfTG9uZ2VyX0xhZ18xID0gOTYyOS45MywgWDQ1X0xvbmdlcl9MYWdfMiA9IE5BLCBYNDVfU2hvcnRlcl9MYWdfMSA9IDcwNzQuMDcxLCBYNDVfU2hvcnRlcl9MYWdfMiA9IDExMTMxLjE5LCBYMTM1X0xvbmdlcl9MYWdfMSA9IDI3MjQ0LjgxLCBYMTM1X0xvbmdlcl9MYWdfMiA9IE5BLCBYMTM1X1Nob3J0ZXJfTGFnXzEgPSAzMTUzMi40NCwgWDEzNV9TaG9ydGVyX0xhZ18yID0gMjg2NjMuODgsIFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkID0gLTAuMzY2MzcyOSwgWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkID0gMS4xNDI4NTgsIC5yb3cgPSA5OC5cblJ1biBgcmxhbmc6Omxhc3RfZXJyb3IoKWAgdG8gc2VlIHdoZXJlIHRoZSBlcnJvciBcbiJ9 -->

```
Error: Problem with `summarise()` input `p_lower`.
x error in evaluating the argument 'x' in selecting a method for function 'mean': object 'y_lower' not found
i Input `p_lower` is `mean(.prediction < y_lower)`.
i The error occurred in group 1: Year = 1997, CPUE = 0.5028519, Species = "Whiting", Estuary = "Hawkesbury River", Estuary_Type = "Drowned River Valley", X45_degree_winds = 5069.999, X135_degree_winds = 45644.2, Drought_declared_1m = "Drought", Drought_declared_6m = "No Drought", Drought_Months = 2, Modal_Age = 5, Spawning_Period = "Dec-Feb", Catch = 971, Effort_days = 1874, X45_Longer_Lag_1 = 9629.93, X45_Longer_Lag_2 = NA, X45_Shorter_Lag_1 = 7074.071, X45_Shorter_Lag_2 = 11131.19, X135_Longer_Lag_1 = 27244.81, X135_Longer_Lag_2 = NA, X135_Shorter_Lag_1 = 31532.44, X135_Shorter_Lag_2 = 28663.88, X45_degree_winds.standardised = -0.3663729, X135_degree_winds.standardised = 1.142858, .row = 98.
Run `rlang::last_error()` to see where the error 
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->





Save model checking plots - These were combined manually in Inkscape later

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucG5nKFwiLi4vcGxvdHMvTW9kZWwgY2hlY2tzL011bGl0c3BlY2llcyBtb2RlbCAxLnBuZ1wiLCB3aWR0aCA9IDIxLCBoZWlnaHQgPSAxNC44LCB1bml0cyA9IFwiY21cIiwgcmVzID0gNjAwKVxucGxvdChzaW11bGF0aW9uT3V0cHV0LCBxdWFudHJlZyA9IEYpXG5kZXYub2ZmKClcblxucG5nKFwiLi4vcGxvdHMvTW9kZWwgY2hlY2tzL011bGl0c3BlY2llcyBtb2RlbCAyLnBuZ1wiLCB3aWR0aCA9IDIxLCBoZWlnaHQgPSAxNC44LCB1bml0cyA9IFwiY21cIiwgcmVzID0gNjAwKVxuaGlzdChyZXNpZHVhbHMobTcpLCBicmVha3MgPSAzMClcbmRldi5vZmYoKVxuYGBgIn0= -->

```r
png("../plots/Model checks/Mulitspecies model 1.png", width = 21, height = 14.8, units = "cm", res = 600)
plot(simulationOutput, quantreg = F)
dev.off()

png("../plots/Model checks/Mulitspecies model 2.png", width = 21, height = 14.8, units = "cm", res = 600)
hist(residuals(m7), breaks = 30)
dev.off()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->






<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChnZ2VmZmVjdChtNywgdGVybXMgPSBcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkIFthbGxdXCIpKVxucGxvdChnZ2VmZmVjdChtNywgdGVybXMgPSBcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCBbYWxsXVwiKSlcbnBsb3QoZ2dlZmZlY3QobTcsIHRlcm1zID0gXCJEcm91Z2h0X01vbnRocyBbYWxsXVwiKSlcbmBgYCJ9 -->

```r
plot(ggeffect(m7, terms = "X45_degree_winds.standardised [all]"))
plot(ggeffect(m7, terms = "X135_degree_winds.standardised [all]"))
plot(ggeffect(m7, terms = "Drought_Months [all]"))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

Heatmap making for interaction plot (Run from here) - Very slow if standard error included (was run on HPC to get SE)

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubnVtcyA8LSBzZXEoLTIsMiwgYnkgPSAwLjA1KVxuaGVhdF9kYXRhTSA8LSBkYXRhLmZyYW1lKFwiWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkXCIgPSByZXAoMCw4MSo4MSksICMgbWFrZXMgZW1wdHkgZGF0YWZyYW1lIHJlYWR5IGZvciB2YWx1ZXNcbiAgICAgICAgICAgICAgICAgICAgICAgICBcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkXCIgPSAgcmVwKDAsODEqODEpLFxuICAgICAgICAgICAgICAgICAgICAgICAgIFwiQ2F0Y2hcIiA9ICByZXAoMCw4MSo4MSkpXG5ObiA8LSAxXG4jIGxvb3AgZm9yIE5FXG5mb3IgKGkgaW4gMTpsZW5ndGgobnVtcykpe1xuICBmb3IgKGogaW4gMTpsZW5ndGgobnVtcykpe1xuICAgIHByZWRfbWFwIDwtIGRhdGEuZnJhbWUoXCJYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gbnVtc1tpXSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkXCIgPSBudW1zW2pdLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJEcm91Z2h0X01vbnRoc1wiID0gbWVhbihteS5kZiREcm91Z2h0X01vbnRocyksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIkVzdHVhcnlfVHlwZVwiID0gXCJEcm93bmVkIFJpdmVyIFZhbGxleVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJFc3R1YXJ5XCIgPSBcIkhhd2tlc2J1cnkgUml2ZXJcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiU3BlY2llc1wiID0gXCJCcmVhbVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJFZmZvcnRfZGF5c1wiID0gbWVhbihteS5kZiRFZmZvcnRfZGF5cykpXG4gICAgUHJlZFggPC0gcHJlZGljdChtNywgbmV3ZGF0YSA9IHByZWRfbWFwLCB0eXBlID0gXCJyZXNwb25zZVwiLCBzZS5maXQgPSBGKVxuICAgIGhlYXRfZGF0YU0kU291dGhlYXN0LldpbmRzW05uXSA8LSBwcmVkX21hcCRYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRbMV1cbiAgICBoZWF0X2RhdGFNJE5vcnRoZWFzdC5XaW5kc1tObl0gPC0gcHJlZF9tYXAkWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRbMV1cbiAgICBoZWF0X2RhdGFNJEFidW5kYW5jZVtObl0gPC0gUHJlZFgjJGZpdFxuICAgICNwcmludChwYXN0ZShcIlRoaXMgaXMgbGluZSBcIiwgTm4sIFwiIG91dCBvZiA2NTYxXCIpKVxuICAgIE5uIDwtIE5uICsgMVxuICB9XG59XG5cbnBIIDwtIGdncGxvdChoZWF0X2RhdGFNLCBhZXMoeCA9IFNvdXRoZWFzdC5XaW5kcyx5ID0gTm9ydGhlYXN0LldpbmRzKSkgKyBnZW9tX3RpbGUoYWVzKGZpbGwgPSBBYnVuZGFuY2UpKSArXG4gIGdlb21fY29udG91cihjb2w9XCJ3aGl0ZVwiLCBhZXMoeiA9IEFidW5kYW5jZSkpICsjLCBiaW53aWR0aCA9IDAuMDAyXG4gIHNjYWxlX3hfY29udGludW91cyhleHBhbmQgPSBjKDAsMCkpICtcbiAgc2NhbGVfeV9jb250aW51b3VzKGV4cGFuZCA9IGMoMCwwKSkgK1xuICAjc2NhbGVfZmlsbF9ncmFkaWVudChsb3cgPSBcImJsdWVcIiwgaGlnaCA9IFwicmVkXCIpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgdmlyaWRpczo6c2NhbGVfZmlsbF92aXJpZGlzKG9wdGlvbiA9IFwibWFnbWFcIiwgbmFtZT1cIlByZWRpY3RlZCBDYXRjaFwiKSArICMgb3IgZ2VvbV9yYXN0ZXIoKVxuICB4bGFiKFwiRG93bndlbGxpbmcgXFxuRmF2b3VyYWJsZSBXaW5kc1wiKSArIHlsYWIoXCJVcHdlbGxpbmcgXFxuRmF2b3VyYWJsZSBXaW5kc1wiKSArXG4gIHRoZW1lKGF4aXMudGl0bGUueCA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTUpLFxuICAgICAgICBheGlzLnRleHQueCAgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZSA9IDE0KSxcbiAgICAgICAgYXhpcy50aXRsZS55ID0gZWxlbWVudF90ZXh0KGZhY2U9XCJib2xkXCIsIGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxNSksXG4gICAgICAgIGF4aXMudGV4dC55ICA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTQpLFxuICAgICAgICBheGlzLnRpY2tzID0gZWxlbWVudF9saW5lKGNvbG91cj1cImJsYWNrXCIpLFxuICAgICAgICBsZWdlbmQudGl0bGUgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZT0xMiwgZmFjZT1cImJvbGRcIiksXG4gICAgICAgIGxlZ2VuZC50ZXh0ID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemU9MTApLFxuICAgICAgICBsZWdlbmQucG9zaXRpb24gPSBcImJvdHRvbVwiKVxuXG5wSFxuXG53cml0ZS5jc3YoaGVhdF9kYXRhTSwgXCIuLi9EYXRhL0FsbCBTcGVjaWVzIGhlYW1hcCBwcmVkaWN0aW9uIGRhdGEuY3N2XCIsIHJvdy5uYW1lcyA9IEYpXG5cbmBgYCJ9 -->

```r
nums <- seq(-2,2, by = 0.05)
heat_dataM <- data.frame("X135_degree_winds.standardised" = rep(0,81*81), # makes empty dataframe ready for values
                         "X45_degree_winds.standardised" =  rep(0,81*81),
                         "Catch" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Drought_Months" = mean(my.df$Drought_Months),
                           "Estuary_Type" = "Drowned River Valley",
                           "Estuary" = "Hawkesbury River",
                           "Species" = "Bream",
                           "Effort_days" = mean(my.df$Effort_days))
    PredX <- predict(m7, newdata = pred_map, type = "response", se.fit = F)
    heat_dataM$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_dataM$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_dataM$Abundance[Nn] <- PredX#$fit
    #print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

pH <- ggplot(heat_dataM, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) +
  geom_contour(col="white", aes(z = Abundance)) +#, binwidth = 0.002
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") +
  theme_classic() +
  viridis::scale_fill_viridis(option = "magma", name="Predicted Catch") + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 15),
        axis.text.x  = element_text(colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 15),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=10),
        legend.position = "bottom")

pH

write.csv(heat_dataM, "../Data/All Species heamap prediction data.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




Now for some species specific Models
Bream First
Prepare Data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYnJlYW0gPC0gc3Vic2V0KG15LmRmLCBTcGVjaWVzID09IFwiQnJlYW1cIilcblxuYGBgIn0= -->

```r
bream <- subset(my.df, Species == "Bream")

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Model

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubTEgPC0gZ2xtbVRNQihDYXRjaCB+IHBvbHkoWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArIFxuICAgICAgIHBvbHkoWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQsIGRlZ3JlZSA9IDIpICtcbiAgICAgICBYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQ6WDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQrXG4gICAgICAgIEVzdHVhcnlfVHlwZSAqRHJvdWdodF9Nb250aHMgKyAoMXxFc3R1YXJ5KSAsIGZhbWlseSA9IEdhbW1hKGxpbms9XCJsb2dcIiksXG4gICAgICAgZGF0YSA9IGJyZWFtLCBvZmZzZXQgPSBsb2cxMChFZmZvcnRfZGF5cykpXG5wZXJmb3JtYW5jZTo6cjIobTEpXG5BSUMobTEpICMgNDMuMTdcbmBgYCJ9 -->

```r
m1 <- glmmTMB(Catch ~ poly(X135_degree_winds.standardised, degree = 2) + 
       poly(X45_degree_winds.standardised, degree = 2) +
       X135_degree_winds.standardised:X45_degree_winds.standardised+
        Estuary_Type *Drought_Months + (1|Estuary) , family = Gamma(link="log"),
       data = bream, offset = log10(Effort_days))
performance::r2(m1)
AIC(m1) # 43.17
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


bayesian

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubTFhIDwtIGJybShDUFVFIH4gcG9seShYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQsIGRlZ3JlZSA9IDIpICsgXG4gICAgICAgcG9seShYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCwgZGVncmVlID0gMikgK1xuICAgICAgIFgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZDpYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCtcbiAgICAgICAgRXN0dWFyeV9UeXBlICpEcm91Z2h0X01vbnRocyArICgxfEVzdHVhcnkpLFxuICAgICAgIGRhdGEgPSBicmVhbSwgaXRlciA9IDEwMDAwLCBzZWVkID0gMTIzNClcbiNzYXZlUkRTKG0xLCBcIi4uL0RhdGEvQ1BVRSBCcmVhbSBicm1zIG1vZGVsLnJkc1wiKVxuI20xIDwtIHJlYWRSRFMoXCIuLi9EYXRhL0NQVUUgQnJlYW0gYnJtcyBtb2RlbC5yZHNcIilcbmBgYCJ9 -->

```r
m1a <- brm(CPUE ~ poly(X135_degree_winds.standardised, degree = 2) + 
       poly(X45_degree_winds.standardised, degree = 2) +
       X135_degree_winds.standardised:X45_degree_winds.standardised+
        Estuary_Type *Drought_Months + (1|Estuary),
       data = bream, iter = 10000, seed = 1234)
#saveRDS(m1, "../Data/CPUE Bream brms model.rds")
#m1 <- readRDS("../Data/CPUE Bream brms model.rds")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Check model

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI3Bsb3QobTcpXG5zdW1tYXJ5KG0xYSlcbnJhbmVmKG0xKVxuZml4ZWYobTEpXG5wcmlvcl9zdW1tYXJ5KG0xKVxubWNtY19wbG90KG0xKVxuYGBgIn0= -->

```r
#plot(m7)
summary(m1a)
ranef(m1)
fixef(m1)
prior_summary(m1)
mcmc_plot(m1)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

marginal effects

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChjb25kaXRpb25hbF9lZmZlY3RzKG0xYSkpXG5cbmBgYCJ9 -->

```r
plot(conditional_effects(m1a))

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



Check assumptions

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2ltdWxhdGlvbk91dHB1dCA8LSBzaW11bGF0ZVJlc2lkdWFscyhmaXR0ZWRNb2RlbCA9IG0xLCBuID0gMjUwKVxucGxvdChzaW11bGF0aW9uT3V0cHV0LCBxdWFudHJlZyA9IEYpXG5cbiMgaGlzdG9ncmFtc1xuaGlzdChzaW11bGF0aW9uT3V0cHV0JGZpdHRlZFJlc2lkdWFscylcbmBgYCJ9 -->

```r
simulationOutput <- simulateResiduals(fittedModel = m1, n = 250)
plot(simulationOutput, quantreg = F)

# histograms
hist(simulationOutput$fittedResiduals)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Model checking plots - note these were combined in Inkscape manually later

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucG5nKFwiLi4vcGxvdHMvTW9kZWwgY2hlY2tzL0JyZWFtIG1vZGVsIDEucG5nXCIsIHdpZHRoID0gMjEsIGhlaWdodCA9IDE0LjgsIHVuaXRzID0gXCJjbVwiLCByZXMgPSA2MDApXG5wbG90KHNpbXVsYXRpb25PdXRwdXQsIHF1YW50cmVnID0gRilcbmRldi5vZmYoKVxuXG5wbmcoXCIuLi9wbG90cy9Nb2RlbCBjaGVja3MvQnJlYW0gbW9kZWwgMi5wbmdcIiwgd2lkdGggPSAyMSwgaGVpZ2h0ID0gMTQuOCwgdW5pdHMgPSBcImNtXCIsIHJlcyA9IDYwMClcbmhpc3QocmVzaWR1YWxzKG0xKSlcbmRldi5vZmYoKVxuYGBgIn0= -->

```r
png("../plots/Model checks/Bream model 1.png", width = 21, height = 14.8, units = "cm", res = 600)
plot(simulationOutput, quantreg = F)
dev.off()

png("../plots/Model checks/Bream model 2.png", width = 21, height = 14.8, units = "cm", res = 600)
hist(residuals(m1))
dev.off()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



Check significance

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2FyOjpBbm92YShtMSlcblxuc3VtbWFyeShtMSlcblxuYnJlYW1fU3VtbWFyeSA8LSBicm9vbS5taXhlZDo6dGlkeShtMSlcbmJyZWFtX1N1bW1hcnlcbndyaXRlLmNzdihicmVhbV9TdW1tYXJ5LCBcIi4uL0RhdGEvQnJlYW0gU3VtbWFyeSBUYWJsZS5jc3ZcIiwgcm93Lm5hbWVzID0gRilcblxuYGBgIn0= -->

```r
car::Anova(m1)

summary(m1)

bream_Summary <- broom.mixed::tidy(m1)
bream_Summary
write.csv(bream_Summary, "../Data/Bream Summary Table.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChnZ2VmZmVjdChtMSwgdGVybXMgPSBcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkIFthbGxdXCIpKVxucGxvdChnZ2VmZmVjdChtMSwgdGVybXMgPSBcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCBbYWxsXVwiKSlcbnBsb3QoZ2dlZmZlY3QobTEsIHRlcm1zID0gYyhcIkRyb3VnaHRfTW9udGhzXCIsIFwiRXN0dWFyeV9UeXBlXCIpKSlcbmBgYCJ9 -->

```r
plot(ggeffect(m1, terms = "X45_degree_winds.standardised [all]"))
plot(ggeffect(m1, terms = "X135_degree_winds.standardised [all]"))
plot(ggeffect(m1, terms = c("Drought_Months", "Estuary_Type")))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

Heatmap making for interaction plot (Run from here) - Very slow if standard error included (was run on HPC to get SE)

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubnVtcyA8LSBzZXEoLTIsMiwgYnkgPSAwLjA1KVxuaGVhdF9kYXRhQnJlYW0gPC0gZGF0YS5mcmFtZShcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gcmVwKDAsODEqODEpLCAjIG1ha2VzIGVtcHR5IGRhdGFmcmFtZSByZWFkeSBmb3IgdmFsdWVzXG4gICAgICAgICAgICAgICAgICAgICAgICAgXCJYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gIHJlcCgwLDgxKjgxKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICBcIkNhdGNoXCIgPSAgcmVwKDAsODEqODEpKVxuTm4gPC0gMVxuIyBsb29wIGZvciBORVxuZm9yIChpIGluIDE6bGVuZ3RoKG51bXMpKXtcbiAgZm9yIChqIGluIDE6bGVuZ3RoKG51bXMpKXtcbiAgICBwcmVkX21hcCA8LSBkYXRhLmZyYW1lKFwiWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRcIiA9IG51bXNbaV0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gbnVtc1tqXSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiRHJvdWdodF9Nb250aHNcIiA9IG1lYW4obXkuZGYkRHJvdWdodF9Nb250aHMpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJFc3R1YXJ5X1R5cGVcIiA9IFwiRHJvd25lZCBSaXZlciBWYWxsZXlcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiRXN0dWFyeVwiID0gXCJIYXdrZXNidXJ5IFJpdmVyXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIkVmZm9ydF9kYXlzXCIgPSBtZWFuKG15LmRmJEVmZm9ydF9kYXlzKSlcbiAgICBQcmVkWCA8LSBwcmVkaWN0KG0xLCBuZXdkYXRhID0gcHJlZF9tYXAsIHR5cGUgPSBcInJlc3BvbnNlXCIsIHNlLmZpdCA9IEYpXG4gICAgaGVhdF9kYXRhQnJlYW0kU291dGhlYXN0LldpbmRzW05uXSA8LSBwcmVkX21hcCRYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRbMV1cbiAgICBoZWF0X2RhdGFCcmVhbSROb3J0aGVhc3QuV2luZHNbTm5dIDwtIHByZWRfbWFwJFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkWzFdXG4gICAgaGVhdF9kYXRhQnJlYW0kQWJ1bmRhbmNlW05uXSA8LSBQcmVkWCMkZml0XG4gICAgI3ByaW50KHBhc3RlKFwiVGhpcyBpcyBsaW5lIFwiLCBObiwgXCIgb3V0IG9mIDY1NjFcIikpXG4gICAgTm4gPC0gTm4gKyAxXG4gIH1cbn1cblxucEIgPC0gZ2dwbG90KGhlYXRfZGF0YUJyZWFtLCBhZXMoeCA9IFNvdXRoZWFzdC5XaW5kcyx5ID0gTm9ydGhlYXN0LldpbmRzKSkgKyBnZW9tX3RpbGUoYWVzKGZpbGwgPSBBYnVuZGFuY2UpKSArXG4gIGdlb21fY29udG91cihjb2w9XCJ3aGl0ZVwiLCBhZXMoeiA9IEFidW5kYW5jZSkpICsjLCBiaW53aWR0aCA9IDAuMDAyXG4gIHNjYWxlX3hfY29udGludW91cyhleHBhbmQgPSBjKDAsMCkpICtcbiAgc2NhbGVfeV9jb250aW51b3VzKGV4cGFuZCA9IGMoMCwwKSkgK1xuICAjc2NhbGVfZmlsbF9ncmFkaWVudChsb3cgPSBcImJsdWVcIiwgaGlnaCA9IFwicmVkXCIpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgdmlyaWRpczo6c2NhbGVfZmlsbF92aXJpZGlzKG9wdGlvbiA9IFwibWFnbWFcIiwgbmFtZT1cIlByZWRpY3RlZCBDYXRjaFwiKSArICMgb3IgZ2VvbV9yYXN0ZXIoKVxuICB4bGFiKFwiRG93bndlbGxpbmcgXFxuRmF2b3VyYWJsZSBXaW5kc1wiKSArIHlsYWIoXCJVcHdlbGxpbmcgXFxuRmF2b3VyYWJsZSBXaW5kc1wiKSArXG4gIHRoZW1lKGF4aXMudGl0bGUueCA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTUpLFxuICAgICAgICBheGlzLnRleHQueCAgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZSA9IDE0KSxcbiAgICAgICAgYXhpcy50aXRsZS55ID0gZWxlbWVudF90ZXh0KGZhY2U9XCJib2xkXCIsIGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxNSksXG4gICAgICAgIGF4aXMudGV4dC55ICA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTQpLFxuICAgICAgICBheGlzLnRpY2tzID0gZWxlbWVudF9saW5lKGNvbG91cj1cImJsYWNrXCIpLFxuICAgICAgICBsZWdlbmQudGl0bGUgPSBlbGVtZW50X2JsYW5rKCksXG4gICAgICAgIGxlZ2VuZC50ZXh0ID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemU9MTApLFxuICAgICAgICBsZWdlbmQucG9zaXRpb24gPSBcImJvdHRvbVwiKVxuXG5wQlxuXG4gd3JpdGUuY3N2KGhlYXRfZGF0YUJyZWFtLCBcIi4uL0RhdGEvQnJlYW0gaGVhbWFwIHByZWRpY3Rpb24gZGF0YS5jc3ZcIiwgcm93Lm5hbWVzID0gRilcblxuYGBgIn0= -->

```r
nums <- seq(-2,2, by = 0.05)
heat_dataBream <- data.frame("X135_degree_winds.standardised" = rep(0,81*81), # makes empty dataframe ready for values
                         "X45_degree_winds.standardised" =  rep(0,81*81),
                         "Catch" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Drought_Months" = mean(my.df$Drought_Months),
                           "Estuary_Type" = "Drowned River Valley",
                           "Estuary" = "Hawkesbury River",
                           "Effort_days" = mean(my.df$Effort_days))
    PredX <- predict(m1, newdata = pred_map, type = "response", se.fit = F)
    heat_dataBream$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_dataBream$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_dataBream$Abundance[Nn] <- PredX#$fit
    #print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

pB <- ggplot(heat_dataBream, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) +
  geom_contour(col="white", aes(z = Abundance)) +#, binwidth = 0.002
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") +
  theme_classic() +
  viridis::scale_fill_viridis(option = "magma", name="Predicted Catch") + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 15),
        axis.text.x  = element_text(colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 15),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10),
        legend.position = "bottom")

pB

 write.csv(heat_dataBream, "../Data/Bream heamap prediction data.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



##Now Mullet##
Prepare Data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubXVsbGV0IDwtIHN1YnNldChteS5kZiwgU3BlY2llcyA9PSBcIk11bGxldFwiKVxuXG5wMiA8LSBnZ3Bsb3QobXVsbGV0LCBhZXMoeCA9IFgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCx5ID0gWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQsIHNpemUgPSBsb2cxMChDUFVFKSkpICsgZ2VvbV9wb2ludChhbHBoYSA9IDAuMSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgKyBsYWJzKHRpdGxlID0gXCJDUFVFIGJ5IGxhZ2dlZCBXaW5kc1wiKVxucDJcbmBgYCJ9 -->

```r
mullet <- subset(my.df, Species == "Mullet")

p2 <- ggplot(mullet, aes(x = X135_degree_winds.standardised,y = X45_degree_winds.standardised, size = log10(CPUE))) + geom_point(alpha = 0.1) +
  theme_classic() + labs(title = "CPUE by lagged Winds")
p2
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Model

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubTIgPC0gZ2xtbVRNQihDYXRjaCB+IHBvbHkoWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArIFxuICAgICAgIHBvbHkoWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQsIGRlZ3JlZSA9IDIpICtcbiAgICAgICBYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQ6WDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQrXG4gICAgICAgIEVzdHVhcnlfVHlwZSAqRHJvdWdodF9Nb250aHMgKyAoMXxFc3R1YXJ5KSAsIGZhbWlseSA9IEdhbW1hKGxpbms9XCJsb2dcIiksXG4gICAgICAgZGF0YSA9IG11bGxldCwgb2Zmc2V0ID0gbG9nMTAoRWZmb3J0X2RheXMpKVxucGVyZm9ybWFuY2U6OnIyKG0yKSAjIDAuMTQ3XG5BSUMobTIpICMgLTIyLjcyNlxuYGBgIn0= -->

```r
m2 <- glmmTMB(Catch ~ poly(X135_degree_winds.standardised, degree = 2) + 
       poly(X45_degree_winds.standardised, degree = 2) +
       X135_degree_winds.standardised:X45_degree_winds.standardised+
        Estuary_Type *Drought_Months + (1|Estuary) , family = Gamma(link="log"),
       data = mullet, offset = log10(Effort_days))
performance::r2(m2) # 0.147
AIC(m2) # -22.726
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Check assumptions

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2ltdWxhdGlvbk91dHB1dCA8LSBzaW11bGF0ZVJlc2lkdWFscyhmaXR0ZWRNb2RlbCA9IG0yLCBuID0gMjUwKVxucGxvdChzaW11bGF0aW9uT3V0cHV0LCBxdWFudHJlZyA9RilcblxuIyBoaXN0b2dyYW1zXG5oaXN0KHNpbXVsYXRpb25PdXRwdXQkZml0dGVkUmVzaWR1YWxzKVxuYGBgIn0= -->

```r
simulationOutput <- simulateResiduals(fittedModel = m2, n = 250)
plot(simulationOutput, quantreg =F)

# histograms
hist(simulationOutput$fittedResiduals)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucG5nKFwiLi4vcGxvdHMvTW9kZWwgY2hlY2tzL011bGxldCBtb2RlbCAxLnBuZ1wiLCB3aWR0aCA9IDIxLCBoZWlnaHQgPSAxNC44LCB1bml0cyA9IFwiY21cIiwgcmVzID0gNjAwKVxucGxvdChzaW11bGF0aW9uT3V0cHV0LCBxdWFudHJlZyA9IEYpXG5kZXYub2ZmKClcblxucG5nKFwiLi4vcGxvdHMvTW9kZWwgY2hlY2tzL011bGxldCBtb2RlbCAyLnBuZ1wiLCB3aWR0aCA9IDIxLCBoZWlnaHQgPSAxNC44LCB1bml0cyA9IFwiY21cIiwgcmVzID0gNjAwKVxuaGlzdChyZXNpZHVhbHMobTIpKVxuZGV2Lm9mZigpXG5gYGAifQ== -->

```r
png("../plots/Model checks/Mullet model 1.png", width = 21, height = 14.8, units = "cm", res = 600)
plot(simulationOutput, quantreg = F)
dev.off()

png("../plots/Model checks/Mullet model 2.png", width = 21, height = 14.8, units = "cm", res = 600)
hist(residuals(m2))
dev.off()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Bayesian

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI20yIDwtIGJybShDYXRjaCB+IHBvbHkoWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArIFxuIyAgICAgICBwb2x5KFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArXG4jICAgICAgWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkOlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkK1xuIyAgICAgICAgRXN0dWFyeV9UeXBlICpEcm91Z2h0X01vbnRocyArICgxfEVzdHVhcnkpICsgb2Zmc2V0KGxvZyhFZmZvcnRfZGF5cykpLCBmYW1pbHkgPSBHYW1tYShsaW5rPVwibG9nXCIpLFxuIyAgICAgICBkYXRhID0gbXVsbGV0LCBpdGVyID0gMTAwMDAsIHNlZWQgPSAxMjM0KVxuI3NhdmVSRFMobTIsIFwiLi4vRGF0YS9DUFVFIE11bGxldCBicm1zIG1vZGVsLnJkc1wiKVxubTIgPC0gcmVhZFJEUyhcIi4uL0RhdGEvQ1BVRSBNdWxsZXQgYnJtcyBtb2RlbC5yZHNcIilcbmBgYCJ9 -->

```r
#m2 <- brm(Catch ~ poly(X135_degree_winds.standardised, degree = 2) + 
#       poly(X45_degree_winds.standardised, degree = 2) +
#      X135_degree_winds.standardised:X45_degree_winds.standardised+
#        Estuary_Type *Drought_Months + (1|Estuary) + offset(log(Effort_days)), family = Gamma(link="log"),
#       data = mullet, iter = 10000, seed = 1234)
#saveRDS(m2, "../Data/CPUE Mullet brms model.rds")
m2 <- readRDS("../Data/CPUE Mullet brms model.rds")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBydW4gdGhlc2UgaW4gY29uc29sZSBvciBpdCBjcmFzaGVzXG4jcGxvdChjb25kaXRpb25hbF9lZmZlY3RzKG0yKSlcbiNwbG90KG0yKVxuc3VtbWFyeShtMilcbmBgYCJ9 -->

```r
# run these in console or it crashes
#plot(conditional_effects(m2))
#plot(m2)
summary(m2)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




Check significance

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2FyOjpBbm92YShtMilcblxuc3VtbWFyeShtMilcbm11bGxldF9TdW1tYXJ5IDwtIGJyb29tLm1peGVkOjp0aWR5KG0yKVxubXVsbGV0X1N1bW1hcnlcbndyaXRlLmNzdihtdWxsZXRfU3VtbWFyeSwgXCIuLi9EYXRhL011bGxldCBTdW1tYXJ5IFRhYmxlLmNzdlwiLCByb3cubmFtZXMgPSBGKVxuXG5gYGAifQ== -->

```r
car::Anova(m2)

summary(m2)
mullet_Summary <- broom.mixed::tidy(m2)
mullet_Summary
write.csv(mullet_Summary, "../Data/Mullet Summary Table.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

Therefore wind is not important for mullet?


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChhbGxFZmZlY3RzKG0yKSlcbmBgYCJ9 -->

```r
plot(allEffects(m2))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChnZ2VmZmVjdChtMiwgdGVybXMgPSBcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkIFthbGxdXCIpKVxucGxvdChnZ2VmZmVjdChtMiwgdGVybXMgPSBcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCBbYWxsXVwiKSlcbnBsb3QoZ2dlZmZlY3QobTIsIHRlcm1zID0gXCJEcm91Z2h0X01vbnRocyBbYWxsXVwiKSlcbmBgYCJ9 -->

```r
plot(ggeffect(m2, terms = "X45_degree_winds.standardised [all]"))
plot(ggeffect(m2, terms = "X135_degree_winds.standardised [all]"))
plot(ggeffect(m2, terms = "Drought_Months [all]"))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubnVtcyA8LSBzZXEoLTIsMiwgYnkgPSAwLjA1KVxuaGVhdF9kYXRhTXVsbGV0IDwtIGRhdGEuZnJhbWUoXCJYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRcIiA9IHJlcCgwLDgxKjgxKSwgIyBtYWtlcyBlbXB0eSBkYXRhZnJhbWUgcmVhZHkgZm9yIHZhbHVlc1xuICAgICAgICAgICAgICAgICAgICAgICAgIFwiWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRcIiA9ICByZXAoMCw4MSo4MSksXG4gICAgICAgICAgICAgICAgICAgICAgICAgXCJDYXRjaFwiID0gIHJlcCgwLDgxKjgxKSlcbk5uIDwtIDFcbiMgbG9vcCBmb3IgTkVcbmZvciAoaSBpbiAxOmxlbmd0aChudW1zKSl7XG4gIGZvciAoaiBpbiAxOmxlbmd0aChudW1zKSl7XG4gICAgcHJlZF9tYXAgPC0gZGF0YS5mcmFtZShcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkXCIgPSBudW1zW2ldLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRcIiA9IG51bXNbal0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIkRyb3VnaHRfTW9udGhzXCIgPSBtZWFuKG15LmRmJERyb3VnaHRfTW9udGhzKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiRXN0dWFyeV9UeXBlXCIgPSBcIkRyb3duZWQgUml2ZXIgVmFsbGV5XCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIkVzdHVhcnlcIiA9IFwiSGF3a2VzYnVyeSBSaXZlclwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJFZmZvcnRfZGF5c1wiID0gbWVhbihteS5kZiRFZmZvcnRfZGF5cykpXG4gICAgUHJlZFggPC0gcHJlZGljdChtMiwgbmV3ZGF0YSA9IHByZWRfbWFwLCB0eXBlID0gXCJyZXNwb25zZVwiLCBzZS5maXQgPSBGKVxuICAgIGhlYXRfZGF0YU11bGxldCRTb3V0aGVhc3QuV2luZHNbTm5dIDwtIHByZWRfbWFwJFgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFsxXVxuICAgIGhlYXRfZGF0YU11bGxldCROb3J0aGVhc3QuV2luZHNbTm5dIDwtIHByZWRfbWFwJFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkWzFdXG4gICAgaGVhdF9kYXRhTXVsbGV0JEFidW5kYW5jZVtObl0gPC0gUHJlZFgjJGZpdFxuICAgICNwcmludChwYXN0ZShcIlRoaXMgaXMgbGluZSBcIiwgTm4sIFwiIG91dCBvZiA2NTYxXCIpKVxuICAgIE5uIDwtIE5uICsgMVxuICB9XG59XG5cbnBNIDwtIGdncGxvdChoZWF0X2RhdGFNdWxsZXQsIGFlcyh4ID0gU291dGhlYXN0LldpbmRzLHkgPSBOb3J0aGVhc3QuV2luZHMpKSArIGdlb21fdGlsZShhZXMoZmlsbCA9IEFidW5kYW5jZSkpICtcbiAgZ2VvbV9jb250b3VyKGNvbD1cIndoaXRlXCIsIGFlcyh6ID0gQWJ1bmRhbmNlKSkgKyMsIGJpbndpZHRoID0gMC4wMDJcbiAgc2NhbGVfeF9jb250aW51b3VzKGV4cGFuZCA9IGMoMCwwKSkgK1xuICBzY2FsZV95X2NvbnRpbnVvdXMoZXhwYW5kID0gYygwLDApKSArXG4gICNzY2FsZV9maWxsX2dyYWRpZW50KGxvdyA9IFwiYmx1ZVwiLCBoaWdoID0gXCJyZWRcIikgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICB2aXJpZGlzOjpzY2FsZV9maWxsX3ZpcmlkaXMob3B0aW9uID0gXCJtYWdtYVwiLCBuYW1lPVwiUHJlZGljdGVkIENhdGNoXCIpICsgIyBvciBnZW9tX3Jhc3RlcigpXG4gIHhsYWIoXCJEb3dud2VsbGluZyBcXG5GYXZvdXJhYmxlIFdpbmRzXCIpICsgeWxhYihcIlVwd2VsbGluZyBcXG5GYXZvdXJhYmxlIFdpbmRzXCIpICtcbiAgdGhlbWUoYXhpcy50aXRsZS54ID0gZWxlbWVudF90ZXh0KGZhY2U9XCJib2xkXCIsIGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxNSksXG4gICAgICAgIGF4aXMudGV4dC54ICA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTQpLFxuICAgICAgICBheGlzLnRpdGxlLnkgPSBlbGVtZW50X3RleHQoZmFjZT1cImJvbGRcIiwgY29sb3VyPVwiYmxhY2tcIiwgc2l6ZSA9IDE1KSxcbiAgICAgICAgYXhpcy50ZXh0LnkgID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxNCksXG4gICAgICAgIGF4aXMudGlja3MgPSBlbGVtZW50X2xpbmUoY29sb3VyPVwiYmxhY2tcIiksXG4gICAgICAgIGxlZ2VuZC50aXRsZSA9IGVsZW1lbnRfYmxhbmsoKSxcbiAgICAgICAgbGVnZW5kLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZT0xMCksXG4gICAgICAgIGxlZ2VuZC5wb3NpdGlvbiA9IFwiYm90dG9tXCIpXG5cbnBNXG5cbndyaXRlLmNzdihoZWF0X2RhdGFNdWxsZXQsIFwiLi4vRGF0YS9NdWxsZXQgaGVhbWFwIHByZWRpY3Rpb24gZGF0YS5jc3ZcIiwgcm93Lm5hbWVzID0gRilcblxuYGBgIn0= -->

```r
nums <- seq(-2,2, by = 0.05)
heat_dataMullet <- data.frame("X135_degree_winds.standardised" = rep(0,81*81), # makes empty dataframe ready for values
                         "X45_degree_winds.standardised" =  rep(0,81*81),
                         "Catch" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Drought_Months" = mean(my.df$Drought_Months),
                           "Estuary_Type" = "Drowned River Valley",
                           "Estuary" = "Hawkesbury River",
                           "Effort_days" = mean(my.df$Effort_days))
    PredX <- predict(m2, newdata = pred_map, type = "response", se.fit = F)
    heat_dataMullet$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_dataMullet$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_dataMullet$Abundance[Nn] <- PredX#$fit
    #print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

pM <- ggplot(heat_dataMullet, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) +
  geom_contour(col="white", aes(z = Abundance)) +#, binwidth = 0.002
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") +
  theme_classic() +
  viridis::scale_fill_viridis(option = "magma", name="Predicted Catch") + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 15),
        axis.text.x  = element_text(colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 15),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10),
        legend.position = "bottom")

pM

write.csv(heat_dataMullet, "../Data/Mullet heamap prediction data.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



##Now Whiting##
Prepare Data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxud2hpdGluZyA8LSBzdWJzZXQobXkuZGYsIFNwZWNpZXMgPT0gXCJXaGl0aW5nXCIpXG5cbiNtMSA8LSBsbWVyKENQVUUuc3RhbmRhcmRpc2VkIH4gWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkICogWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQgKyBcbiMgICAgICAgICAgIEVzdHVhcnlfVHlwZSArIERyb3VnaHRfTW9udGhzICsgKDF8RXN0dWFyeSksIGRhdGEgPSBicmVhbSlcbiNBSUMobTEpICMyMTUuNDJcblxucDMgPC0gZ2dwbG90KHdoaXRpbmcsIGFlcyh4ID0gWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLHkgPSBYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCwgc2l6ZSA9IGxvZzEwKENQVUUpKSkgKyBnZW9tX3BvaW50KGFscGhhID0gMC4xKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArIGxhYnModGl0bGUgPSBcIkNQVUUgYnkgbGFnZ2VkIFdpbmRzXCIpXG5wM1xuYGBgIn0= -->

```r
whiting <- subset(my.df, Species == "Whiting")

#m1 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
#           Estuary_Type + Drought_Months + (1|Estuary), data = bream)
#AIC(m1) #215.42

p3 <- ggplot(whiting, aes(x = X135_degree_winds.standardised,y = X45_degree_winds.standardised, size = log10(CPUE))) + geom_point(alpha = 0.1) +
  theme_classic() + labs(title = "CPUE by lagged Winds")
p3
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Model

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubTMgPC0gZ2xtbVRNQihDYXRjaCB+IHBvbHkoWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArIFxuICAgICAgIHBvbHkoWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQsIGRlZ3JlZSA9IDIpICtcbiAgICAgICBYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQ6WDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQrXG4gICAgICAgIEVzdHVhcnlfVHlwZSAqRHJvdWdodF9Nb250aHMgKyAoMXxFc3R1YXJ5KSAsIGZhbWlseSA9IEdhbW1hKGxpbms9XCJsb2dcIiksXG4gICAgICAgZGF0YSA9IHdoaXRpbmcsIG9mZnNldCA9IGxvZzEwKEVmZm9ydF9kYXlzKSlcbnBlcmZvcm1hbmNlOjpyMihtMykgIyAwLjEyN1xuQUlDKG0zKSAjIDY0LjI2XG5gYGAifQ== -->

```r
m3 <- glmmTMB(Catch ~ poly(X135_degree_winds.standardised, degree = 2) + 
       poly(X45_degree_winds.standardised, degree = 2) +
       X135_degree_winds.standardised:X45_degree_winds.standardised+
        Estuary_Type *Drought_Months + (1|Estuary) , family = Gamma(link="log"),
       data = whiting, offset = log10(Effort_days))
performance::r2(m3) # 0.127
AIC(m3) # 64.26
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Check assumptions

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2ltdWxhdGlvbk91dHB1dCA8LSBzaW11bGF0ZVJlc2lkdWFscyhmaXR0ZWRNb2RlbCA9IG0zLCBuID0gMjUwKVxucGxvdChzaW11bGF0aW9uT3V0cHV0LCBxdWFudHJlZyA9IEYpXG5cbiMgaGlzdG9ncmFtc1xuaGlzdChzaW11bGF0aW9uT3V0cHV0JGZpdHRlZFJlc2lkdWFscylcbmBgYCJ9 -->

```r
simulationOutput <- simulateResiduals(fittedModel = m3, n = 250)
plot(simulationOutput, quantreg = F)

# histograms
hist(simulationOutput$fittedResiduals)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucG5nKFwiLi4vcGxvdHMvTW9kZWwgY2hlY2tzL1doaXRpbmcgbW9kZWwgMS5wbmdcIiwgd2lkdGggPSAyMSwgaGVpZ2h0ID0gMTQuOCwgdW5pdHMgPSBcImNtXCIsIHJlcyA9IDYwMClcbnBsb3Qoc2ltdWxhdGlvbk91dHB1dCwgcXVhbnRyZWcgPSBGKVxuZGV2Lm9mZigpXG5cbnBuZyhcIi4uL3Bsb3RzL01vZGVsIGNoZWNrcy9XaGl0aW5nIG1vZGVsIDIucG5nXCIsIHdpZHRoID0gMjEsIGhlaWdodCA9IDE0LjgsIHVuaXRzID0gXCJjbVwiLCByZXMgPSA2MDApXG5oaXN0KHJlc2lkdWFscyhtMykpXG5kZXYub2ZmKClcbmBgYCJ9 -->

```r
png("../plots/Model checks/Whiting model 1.png", width = 21, height = 14.8, units = "cm", res = 600)
plot(simulationOutput, quantreg = F)
dev.off()

png("../plots/Model checks/Whiting model 2.png", width = 21, height = 14.8, units = "cm", res = 600)
hist(residuals(m3))
dev.off()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Bayesian

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI20zIDwtIGJybShDYXRjaCB+IHBvbHkoWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArIFxuIyAgICAgICBwb2x5KFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArXG4jICAgICAgWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkOlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkK1xuIyAgICAgICAgRXN0dWFyeV9UeXBlICpEcm91Z2h0X01vbnRocyArICgxfEVzdHVhcnkpICsgb2Zmc2V0KGxvZyhFZmZvcnRfZGF5cykpLCBmYW1pbHkgPSBHYW1tYShsaW5rPVwibG9nXCIpLFxuIyAgICAgICBkYXRhID0gd2hpdGluZywgaXRlciA9IDEwMDAwLCBzZWVkID0gMTIzNClcbiNzYXZlUkRTKG0zLCBcIi4uL0RhdGEvQ1BVRSBXaGl0aW5nIGJybXMgbW9kZWwucmRzXCIpXG5tMyA8LSByZWFkUkRTKFwiLi4vRGF0YS9DUFVFIFdoaXRpbmcgYnJtcyBtb2RlbC5yZHNcIilcbmBgYCJ9 -->

```r
#m3 <- brm(Catch ~ poly(X135_degree_winds.standardised, degree = 2) + 
#       poly(X45_degree_winds.standardised, degree = 2) +
#      X135_degree_winds.standardised:X45_degree_winds.standardised+
#        Estuary_Type *Drought_Months + (1|Estuary) + offset(log(Effort_days)), family = Gamma(link="log"),
#       data = whiting, iter = 10000, seed = 1234)
#saveRDS(m3, "../Data/CPUE Whiting brms model.rds")
m3 <- readRDS("../Data/CPUE Whiting brms model.rds")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBydW4gdGhlc2UgaW4gY29uc29sZSBvciBpdCBjcmFzaGVzXG4jcGxvdChjb25kaXRpb25hbF9lZmZlY3RzKG0zKSlcbiNwbG90KG0zKVxuc3VtbWFyeShtMylcbmBgYCJ9 -->

```r
# run these in console or it crashes
#plot(conditional_effects(m3))
#plot(m3)
summary(m3)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




Check significance

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2FyOjpBbm92YShtMylcbnN1bW1hcnkobTMpXG53aGl0aW5nX1N1bW1hcnkgPC0gYnJvb20ubWl4ZWQ6OnRpZHkobTMpXG53aGl0aW5nX1N1bW1hcnlcbndyaXRlLmNzdih3aGl0aW5nX1N1bW1hcnksIFwiLi4vRGF0YS9XaGl0aW5nIFN1bW1hcnkgVGFibGUuY3N2XCIsIHJvdy5uYW1lcyA9IEYpXG5cbmBgYCJ9 -->

```r
car::Anova(m3)
summary(m3)
whiting_Summary <- broom.mixed::tidy(m3)
whiting_Summary
write.csv(whiting_Summary, "../Data/Whiting Summary Table.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

Therefore wind is not important for whiting?


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChnZ2VmZmVjdChtMywgdGVybXMgPSBcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkIFthbGxdXCIpKVxucGxvdChnZ2VmZmVjdChtMywgdGVybXMgPSBcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCBbYWxsXVwiKSlcbiNwbG90KGdnZWZmZWN0KG0zLCB0ZXJtcyA9IFwiRXN0dWFyeV9UeXBlOkRyb3VnaHRfTW9udGhzXCIpKVxuYGBgIn0= -->

```r
plot(ggeffect(m3, terms = "X45_degree_winds.standardised [all]"))
plot(ggeffect(m3, terms = "X135_degree_winds.standardised [all]"))
#plot(ggeffect(m3, terms = "Estuary_Type:Drought_Months"))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubnVtcyA8LSBzZXEoLTIsMiwgYnkgPSAwLjA1KVxuaGVhdF9kYXRhV2hpdGluZyA8LSBkYXRhLmZyYW1lKFwiWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkXCIgPSByZXAoMCw4MSo4MSksICMgbWFrZXMgZW1wdHkgZGF0YWZyYW1lIHJlYWR5IGZvciB2YWx1ZXNcbiAgICAgICAgICAgICAgICAgICAgICAgICBcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkXCIgPSAgcmVwKDAsODEqODEpLFxuICAgICAgICAgICAgICAgICAgICAgICAgIFwiQ2F0Y2hcIiA9ICByZXAoMCw4MSo4MSkpXG5ObiA8LSAxXG4jIGxvb3AgZm9yIE5FXG5mb3IgKGkgaW4gMTpsZW5ndGgobnVtcykpe1xuICBmb3IgKGogaW4gMTpsZW5ndGgobnVtcykpe1xuICAgIHByZWRfbWFwIDwtIGRhdGEuZnJhbWUoXCJYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gbnVtc1tpXSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkXCIgPSBudW1zW2pdLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJEcm91Z2h0X01vbnRoc1wiID0gbWVhbihteS5kZiREcm91Z2h0X01vbnRocyksXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIkVzdHVhcnlfVHlwZVwiID0gXCJEcm93bmVkIFJpdmVyIFZhbGxleVwiLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJFc3R1YXJ5XCIgPSBcIkhhd2tlc2J1cnkgUml2ZXJcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiRWZmb3J0X2RheXNcIiA9IG1lYW4obXkuZGYkRWZmb3J0X2RheXMpKVxuICAgIFByZWRYIDwtIHByZWRpY3QobTMsIG5ld2RhdGEgPSBwcmVkX21hcCwgdHlwZSA9IFwicmVzcG9uc2VcIiwgc2UuZml0ID0gRilcbiAgICBoZWF0X2RhdGFXaGl0aW5nJFNvdXRoZWFzdC5XaW5kc1tObl0gPC0gcHJlZF9tYXAkWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkWzFdXG4gICAgaGVhdF9kYXRhV2hpdGluZyROb3J0aGVhc3QuV2luZHNbTm5dIDwtIHByZWRfbWFwJFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkWzFdXG4gICAgaGVhdF9kYXRhV2hpdGluZyRBYnVuZGFuY2VbTm5dIDwtIFByZWRYIyRmaXRcbiAgICAjcHJpbnQocGFzdGUoXCJUaGlzIGlzIGxpbmUgXCIsIE5uLCBcIiBvdXQgb2YgNjU2MVwiKSlcbiAgICBObiA8LSBObiArIDFcbiAgfVxufVxuXG5wVyA8LSBnZ3Bsb3QoaGVhdF9kYXRhV2hpdGluZywgYWVzKHggPSBTb3V0aGVhc3QuV2luZHMseSA9IE5vcnRoZWFzdC5XaW5kcykpICsgZ2VvbV90aWxlKGFlcyhmaWxsID0gQWJ1bmRhbmNlKSkgK1xuICBnZW9tX2NvbnRvdXIoY29sPVwid2hpdGVcIiwgYWVzKHogPSBBYnVuZGFuY2UpKSArIywgYmlud2lkdGggPSAwLjAwMlxuICBzY2FsZV94X2NvbnRpbnVvdXMoZXhwYW5kID0gYygwLDApKSArXG4gIHNjYWxlX3lfY29udGludW91cyhleHBhbmQgPSBjKDAsMCkpICtcbiAgI3NjYWxlX2ZpbGxfZ3JhZGllbnQobG93ID0gXCJibHVlXCIsIGhpZ2ggPSBcInJlZFwiKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIHZpcmlkaXM6OnNjYWxlX2ZpbGxfdmlyaWRpcyhvcHRpb24gPSBcIm1hZ21hXCIsIG5hbWU9XCJQcmVkaWN0ZWQgQ2F0Y2hcIikgKyAjIG9yIGdlb21fcmFzdGVyKClcbiAgeGxhYihcIkRvd253ZWxsaW5nIFxcbkZhdm91cmFibGUgV2luZHNcIikgKyB5bGFiKFwiVXB3ZWxsaW5nIFxcbkZhdm91cmFibGUgV2luZHNcIikgK1xuICB0aGVtZShheGlzLnRpdGxlLnggPSBlbGVtZW50X3RleHQoZmFjZT1cImJvbGRcIiwgY29sb3VyPVwiYmxhY2tcIiwgc2l6ZSA9IDE1KSxcbiAgICAgICAgYXhpcy50ZXh0LnggID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxNCksXG4gICAgICAgIGF4aXMudGl0bGUueSA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTUpLFxuICAgICAgICBheGlzLnRleHQueSAgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZSA9IDE0KSxcbiAgICAgICAgYXhpcy50aWNrcyA9IGVsZW1lbnRfbGluZShjb2xvdXI9XCJibGFja1wiKSxcbiAgICAgICAgbGVnZW5kLnRpdGxlID0gZWxlbWVudF9ibGFuaygpLFxuICAgICAgICBsZWdlbmQudGV4dCA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplPTEwKSxcbiAgICAgICAgbGVnZW5kLnBvc2l0aW9uID0gXCJib3R0b21cIilcblxucFdcblxud3JpdGUuY3N2KGhlYXRfZGF0YVdoaXRpbmcsIFwiLi4vRGF0YS9XaGl0aW5nIGhlYW1hcCBwcmVkaWN0aW9uIGRhdGEuY3N2XCIsIHJvdy5uYW1lcyA9IEYpXG5cbmBgYCJ9 -->

```r
nums <- seq(-2,2, by = 0.05)
heat_dataWhiting <- data.frame("X135_degree_winds.standardised" = rep(0,81*81), # makes empty dataframe ready for values
                         "X45_degree_winds.standardised" =  rep(0,81*81),
                         "Catch" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Drought_Months" = mean(my.df$Drought_Months),
                           "Estuary_Type" = "Drowned River Valley",
                           "Estuary" = "Hawkesbury River",
                           "Effort_days" = mean(my.df$Effort_days))
    PredX <- predict(m3, newdata = pred_map, type = "response", se.fit = F)
    heat_dataWhiting$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_dataWhiting$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_dataWhiting$Abundance[Nn] <- PredX#$fit
    #print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

pW <- ggplot(heat_dataWhiting, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) +
  geom_contour(col="white", aes(z = Abundance)) +#, binwidth = 0.002
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") +
  theme_classic() +
  viridis::scale_fill_viridis(option = "magma", name="Predicted Catch") + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 15),
        axis.text.x  = element_text(colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 15),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10),
        legend.position = "bottom")

pW

write.csv(heat_dataWhiting, "../Data/Whiting heamap prediction data.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- ##Now Luderick## --- REOMVED AS SPAWNING PERIOD IS UNCERTAIN -->
<!-- Prepare Data -->
<!-- ```{r} -->
<!-- luderick <- subset(my.df, Species == "Luderick") -->

<!-- #m1 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised +  -->
<!-- #           Estuary_Type + Drought_Months + (1|Estuary), data = bream) -->
<!-- #AIC(m1) #215.42 -->

<!-- p4 <- ggplot(luderick, aes(x = X135_degree_winds.standardised,y = X45_degree_winds.standardised, size = log10(CPUE))) + geom_point(alpha = 0.1) + -->
<!--   theme_classic() + labs(title = "CPUE by lagged Winds") -->
<!-- p4 -->
<!-- ``` -->

<!-- Model -->
<!-- ```{r} -->
<!-- m4 <- glmmTMB(Catch ~ poly(X135_degree_winds.standardised, degree = 2) +  -->
<!--        poly(X45_degree_winds.standardised, degree = 2) + -->
<!--        X135_degree_winds.standardised:X45_degree_winds.standardised+ -->
<!--         Estuary_Type *Drought_Months + (1|Estuary) , Gamma(link="log"), -->
<!--        data = luderick, offset = log10(Effort_days)) -->
<!-- performance::r2(m4) # 0.127 -->
<!-- AIC(m4) # 64.26 -->
<!-- ``` -->

<!-- Check assumptions -->
<!-- ```{r} -->
<!-- simulationOutput <- simulateResiduals(fittedModel = m4, n = 250) -->
<!-- plot(simulationOutput, quantreg = F) -->

<!-- # histograms -->
<!-- hist(simulationOutput$fittedResiduals) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- png("../plots/Model checks/Luderick model 1.png", width = 21, height = 14.8, units = "cm", res = 600) -->
<!-- plot(simulationOutput, quantreg = F) -->
<!-- dev.off() -->

<!-- png("../plots/Model checks/Luderick model 2.png", width = 21, height = 14.8, units = "cm", res = 600) -->
<!-- hist(residuals(m4)) -->
<!-- dev.off() -->
<!-- ``` -->


<!-- Check significance -->
<!-- ```{r} -->
<!-- car::Anova(m4) -->
<!-- summary(m4) -->
<!-- luderick_Summary <- broom.mixed::tidy(m4) -->
<!-- luderick_Summary -->
<!-- write.csv(luderick_Summary, "../Data/Luderick Summary Table.csv", row.names = F) -->
<!-- ``` -->
<!-- Therefore wind is not important for luderick? -->

<!-- ```{r} -->
<!-- plot(ggeffect(m4, terms = "X45_degree_winds.standardised [all]")) -->
<!-- plot(ggeffect(m4, terms = "X135_degree_winds.standardised [all]")) -->
<!-- plot(ggeffect(m4, terms = "Drought_Months [all]")) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- nums <- seq(-2,2, by = 0.05) -->
<!-- heat_dataLuderick <- data.frame("X135_degree_winds.standardised" = rep(0,81*81), # makes empty dataframe ready for values -->
<!--                          "X45_degree_winds.standardised" =  rep(0,81*81), -->
<!--                          "Catch" =  rep(0,81*81)) -->
<!-- Nn <- 1 -->
<!-- # loop for NE -->
<!-- for (i in 1:length(nums)){ -->
<!--   for (j in 1:length(nums)){ -->
<!--     pred_map <- data.frame("X45_degree_winds.standardised" = nums[i], -->
<!--                            "X135_degree_winds.standardised" = nums[j], -->
<!--                            "Drought_Months" = mean(my.df$Drought_Months), -->
<!--                            "Estuary_Type" = "Drowned River Valley", -->
<!--                            "Estuary" = "Hawkesbury River", -->
<!--                            "Effort_days" = mean(my.df$Effort_days)) -->
<!--     PredX <- predict(m4, newdata = pred_map, type = "response", se.fit = F) -->
<!--     heat_dataLuderick$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1] -->
<!--     heat_dataLuderick$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1] -->
<!--     heat_dataLuderick$Abundance[Nn] <- PredX#$fit -->
<!--     #print(paste("This is line ", Nn, " out of 6561")) -->
<!--     Nn <- Nn + 1 -->
<!--   } -->
<!-- } -->

<!-- pL <- ggplot(heat_dataLuderick, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) + -->
<!--   geom_contour(col="white", aes(z = Abundance)) +#, binwidth = 0.002 -->
<!--   scale_x_continuous(expand = c(0,0)) + -->
<!--   scale_y_continuous(expand = c(0,0)) + -->
<!--   #scale_fill_gradient(low = "blue", high = "red") + -->
<!--   theme_classic() + -->
<!--   viridis::scale_fill_viridis(option = "magma", name="Predicted Catch") + # or geom_raster() -->
<!--   xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") + -->
<!--   theme(axis.title.x = element_text(face="bold", colour="black", size = 15), -->
<!--         axis.text.x  = element_text(colour="black", size = 14), -->
<!--         axis.title.y = element_text(face="bold", colour="black", size = 15), -->
<!--         axis.text.y  = element_text(colour="black", size = 14), -->
<!--         axis.ticks = element_line(colour="black"), -->
<!--         legend.title = element_blank(), -->
<!--         legend.text = element_text(colour="black", size=10), -->
<!--         legend.position = "bottom") -->

<!-- pL -->

<!-- write.csv(heat_dataLuderick, "../Data/Luderick heamap prediction data.csv", row.names = F) -->

<!-- ``` -->


##Now Flathead##
Prepare Data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZmxhdGhlYWQgPC0gc3Vic2V0KG15LmRmLCBTcGVjaWVzID09IFwiRmxhdGhlYWRcIilcbmBgYCJ9 -->

```r
flathead <- subset(my.df, Species == "Flathead")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Model

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubTUgPC0gZ2xtbVRNQihDYXRjaCB+IHBvbHkoWDEzNV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkLCBkZWdyZWUgPSAyKSArIFxuICAgICAgIHBvbHkoWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQsIGRlZ3JlZSA9IDIpICtcbiAgICAgICBYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQ6WDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQrXG4gICAgICAgIEVzdHVhcnlfVHlwZSAqRHJvdWdodF9Nb250aHMgKyAoMXxFc3R1YXJ5KSAsIEdhbW1hKGxpbms9XCJsb2dcIiksXG4gICAgICAgZGF0YSA9IGZsYXRoZWFkLCBvZmZzZXQgPSBsb2cxMChFZmZvcnRfZGF5cykpXG5wZXJmb3JtYW5jZTo6cjIobTUpICMgMC4xMjdcbkFJQyhtNSkgIyA2NC4yNlxuYGBgIn0= -->

```r
m5 <- glmmTMB(Catch ~ poly(X135_degree_winds.standardised, degree = 2) + 
       poly(X45_degree_winds.standardised, degree = 2) +
       X135_degree_winds.standardised:X45_degree_winds.standardised+
        Estuary_Type *Drought_Months + (1|Estuary) , Gamma(link="log"),
       data = flathead, offset = log10(Effort_days))
performance::r2(m5) # 0.127
AIC(m5) # 64.26
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Check assumptions

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2ltdWxhdGlvbk91dHB1dCA8LSBzaW11bGF0ZVJlc2lkdWFscyhmaXR0ZWRNb2RlbCA9IG01LCBuID0gMjUwKVxucGxvdChzaW11bGF0aW9uT3V0cHV0LCBxdWFudHJlZyA9IEYpXG5cbiMgaGlzdG9ncmFtc1xuaGlzdChzaW11bGF0aW9uT3V0cHV0JGZpdHRlZFJlc2lkdWFscylcbmBgYCJ9 -->

```r
simulationOutput <- simulateResiduals(fittedModel = m5, n = 250)
plot(simulationOutput, quantreg = F)

# histograms
hist(simulationOutput$fittedResiduals)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucG5nKFwiLi4vcGxvdHMvTW9kZWwgY2hlY2tzL0ZsYXRoZWFkIG1vZGVsIDEucG5nXCIsIHdpZHRoID0gMjEsIGhlaWdodCA9IDE0LjgsIHVuaXRzID0gXCJjbVwiLCByZXMgPSA2MDApXG5wbG90KHNpbXVsYXRpb25PdXRwdXQsIHF1YW50cmVnID0gRilcbmRldi5vZmYoKVxuXG5wbmcoXCIuLi9wbG90cy9Nb2RlbCBjaGVja3MvRmxhdGhlYWQgbW9kZWwgMi5wbmdcIiwgd2lkdGggPSAyMSwgaGVpZ2h0ID0gMTQuOCwgdW5pdHMgPSBcImNtXCIsIHJlcyA9IDYwMClcbmhpc3QocmVzaWR1YWxzKG01KSlcbmRldi5vZmYoKVxuYGBgIn0= -->

```r
png("../plots/Model checks/Flathead model 1.png", width = 21, height = 14.8, units = "cm", res = 600)
plot(simulationOutput, quantreg = F)
dev.off()

png("../plots/Model checks/Flathead model 2.png", width = 21, height = 14.8, units = "cm", res = 600)
hist(residuals(m5))
dev.off()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Bayesian

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubTUgPC0gYnJtKENQVUUgfiBwb2x5KFgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCwgZGVncmVlID0gMikgKyBcbiAgICAgIHBvbHkoWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWQsIGRlZ3JlZSA9IDIpICtcbiAgICAgIFgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZDpYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCtcbiAgICAgICAgRXN0dWFyeV9UeXBlICpEcm91Z2h0X01vbnRocyArICgxfEVzdHVhcnkpLFxuICAgICAgIGRhdGEgPSBmbGF0aGVhZCwgaXRlciA9IDEwMDAwLCBzZWVkID0gMTIzNClcbnNhdmVSRFMobTUsIFwiLi4vRGF0YS9DUFVFIEZsYXRoZWFkIGJybXMgbW9kZWwucmRzXCIpXG5tNSA8LSByZWFkUkRTKFwiLi4vRGF0YS9DUFVFIEZsYXRoZWFkIGJybXMgbW9kZWwucmRzXCIpXG5gYGAifQ== -->

```r
m5 <- brm(CPUE ~ poly(X135_degree_winds.standardised, degree = 2) + 
      poly(X45_degree_winds.standardised, degree = 2) +
      X135_degree_winds.standardised:X45_degree_winds.standardised+
        Estuary_Type *Drought_Months + (1|Estuary),
       data = flathead, iter = 10000, seed = 1234)
saveRDS(m5, "../Data/CPUE Flathead brms model.rds")
m5 <- readRDS("../Data/CPUE Flathead brms model.rds")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBydW4gdGhlc2UgaW4gY29uc29sZSBvciBpdCBjcmFzaGVzXG4jcGxvdChjb25kaXRpb25hbF9lZmZlY3RzKG01KSlcbiNwbG90KG01KVxuc3VtbWFyeShtNSlcbmBgYCJ9 -->

```r
# run these in console or it crashes
#plot(conditional_effects(m5))
#plot(m5)
summary(m5)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




Check significance

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuY2FyOjpBbm92YShtNSlcbnN1bW1hcnkobTUpXG5mbGF0aGVhZF9TdW1tYXJ5IDwtIGJyb29tLm1peGVkOjp0aWR5KG01KVxuZmxhdGhlYWRfU3VtbWFyeVxud3JpdGUuY3N2KGZsYXRoZWFkX1N1bW1hcnksIFwiLi4vRGF0YS9GbGF0aGVhZCBTdW1tYXJ5IFRhYmxlLmNzdlwiLCByb3cubmFtZXMgPSBGKVxuYGBgIn0= -->

```r
car::Anova(m5)
summary(m5)
flathead_Summary <- broom.mixed::tidy(m5)
flathead_Summary
write.csv(flathead_Summary, "../Data/Flathead Summary Table.csv", row.names = F)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdChnZ2VmZmVjdChtNSwgdGVybXMgPSBcIlg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkIFthbGxdXCIpKVxucGxvdChnZ2VmZmVjdChtNSwgdGVybXMgPSBcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZCBbYWxsXVwiKSlcbnBsb3QoZ2dlZmZlY3QobTUsIHRlcm1zID0gXCJEcm91Z2h0X01vbnRocyBbYWxsXVwiKSlcbmBgYCJ9 -->

```r
plot(ggeffect(m5, terms = "X45_degree_winds.standardised [all]"))
plot(ggeffect(m5, terms = "X135_degree_winds.standardised [all]"))
plot(ggeffect(m5, terms = "Drought_Months [all]"))
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubnVtcyA8LSBzZXEoLTIsMiwgYnkgPSAwLjA1KVxuaGVhdF9kYXRhRmxhdGhlYWQgPC0gZGF0YS5mcmFtZShcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gcmVwKDAsODEqODEpLCAjIG1ha2VzIGVtcHR5IGRhdGFmcmFtZSByZWFkeSBmb3IgdmFsdWVzXG4gICAgICAgICAgICAgICAgICAgICAgICAgXCJYNDVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gIHJlcCgwLDgxKjgxKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICBcIkNhdGNoXCIgPSAgcmVwKDAsODEqODEpKVxuTm4gPC0gMVxuIyBsb29wIGZvciBORVxuZm9yIChpIGluIDE6bGVuZ3RoKG51bXMpKXtcbiAgZm9yIChqIGluIDE6bGVuZ3RoKG51bXMpKXtcbiAgICBwcmVkX21hcCA8LSBkYXRhLmZyYW1lKFwiWDQ1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRcIiA9IG51bXNbaV0sXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIlgxMzVfZGVncmVlX3dpbmRzLnN0YW5kYXJkaXNlZFwiID0gbnVtc1tqXSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiRHJvdWdodF9Nb250aHNcIiA9IG1lYW4obXkuZGYkRHJvdWdodF9Nb250aHMpLFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJFc3R1YXJ5X1R5cGVcIiA9IFwiRHJvd25lZCBSaXZlciBWYWxsZXlcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiRXN0dWFyeVwiID0gXCJIYXdrZXNidXJ5IFJpdmVyXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBcIkVmZm9ydF9kYXlzXCIgPSBtZWFuKG15LmRmJEVmZm9ydF9kYXlzKSlcbiAgICBQcmVkWCA8LSBwcmVkaWN0KG01LCBuZXdkYXRhID0gcHJlZF9tYXAsIHR5cGUgPSBcInJlc3BvbnNlXCIsIHNlLmZpdCA9IEYpXG4gICAgaGVhdF9kYXRhRmxhdGhlYWQkU291dGhlYXN0LldpbmRzW05uXSA8LSBwcmVkX21hcCRYMTM1X2RlZ3JlZV93aW5kcy5zdGFuZGFyZGlzZWRbMV1cbiAgICBoZWF0X2RhdGFGbGF0aGVhZCROb3J0aGVhc3QuV2luZHNbTm5dIDwtIHByZWRfbWFwJFg0NV9kZWdyZWVfd2luZHMuc3RhbmRhcmRpc2VkWzFdXG4gICAgaGVhdF9kYXRhRmxhdGhlYWQkQWJ1bmRhbmNlW05uXSA8LSBQcmVkWCMkZml0XG4gICAgI3ByaW50KHBhc3RlKFwiVGhpcyBpcyBsaW5lIFwiLCBObiwgXCIgb3V0IG9mIDY1NjFcIikpXG4gICAgTm4gPC0gTm4gKyAxXG4gIH1cbn1cblxucEYgPC0gZ2dwbG90KGhlYXRfZGF0YUZsYXRoZWFkLCBhZXMoeCA9IFNvdXRoZWFzdC5XaW5kcyx5ID0gTm9ydGhlYXN0LldpbmRzKSkgKyBnZW9tX3RpbGUoYWVzKGZpbGwgPSBBYnVuZGFuY2UpKSArXG4gIGdlb21fY29udG91cihjb2w9XCJ3aGl0ZVwiLCBhZXMoeiA9IEFidW5kYW5jZSkpICsjLCBiaW53aWR0aCA9IDAuMDAyXG4gIHNjYWxlX3hfY29udGludW91cyhleHBhbmQgPSBjKDAsMCkpICtcbiAgc2NhbGVfeV9jb250aW51b3VzKGV4cGFuZCA9IGMoMCwwKSkgK1xuICAjc2NhbGVfZmlsbF9ncmFkaWVudChsb3cgPSBcImJsdWVcIiwgaGlnaCA9IFwicmVkXCIpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgdmlyaWRpczo6c2NhbGVfZmlsbF92aXJpZGlzKG9wdGlvbiA9IFwibWFnbWFcIiwgbmFtZT1cIlByZWRpY3RlZCBDYXRjaFwiKSArICMgb3IgZ2VvbV9yYXN0ZXIoKVxuICB4bGFiKFwiRG93bndlbGxpbmcgXFxuRmF2b3VyYWJsZSBXaW5kc1wiKSArIHlsYWIoXCJVcHdlbGxpbmcgXFxuRmF2b3VyYWJsZSBXaW5kc1wiKSArXG4gIHRoZW1lKGF4aXMudGl0bGUueCA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiLCBjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTUpLFxuICAgICAgICBheGlzLnRleHQueCAgPSBlbGVtZW50X3RleHQoY29sb3VyPVwiYmxhY2tcIiwgc2l6ZSA9IDE0KSxcbiAgICAgICAgYXhpcy50aXRsZS55ID0gZWxlbWVudF90ZXh0KGZhY2U9XCJib2xkXCIsIGNvbG91cj1cImJsYWNrXCIsIHNpemUgPSAxNSksXG4gICAgICAgIGF4aXMudGV4dC55ICA9IGVsZW1lbnRfdGV4dChjb2xvdXI9XCJibGFja1wiLCBzaXplID0gMTQpLFxuICAgICAgICBheGlzLnRpY2tzID0gZWxlbWVudF9saW5lKGNvbG91cj1cImJsYWNrXCIpLFxuICAgICAgICBsZWdlbmQudGl0bGUgPSBlbGVtZW50X2JsYW5rKCksXG4gICAgICAgIGxlZ2VuZC50ZXh0ID0gZWxlbWVudF90ZXh0KGNvbG91cj1cImJsYWNrXCIsIHNpemU9MTApLFxuICAgICAgICBsZWdlbmQucG9zaXRpb24gPSBcImJvdHRvbVwiKVxuXG5wRlxuXG53cml0ZS5jc3YoaGVhdF9kYXRhRmxhdGhlYWQsIFwiLi4vRGF0YS9GbGF0aGVhZCBoZWFtYXAgcHJlZGljdGlvbiBkYXRhLmNzdlwiLCByb3cubmFtZXMgPSBGKVxuXG5gYGAifQ== -->

```r
nums <- seq(-2,2, by = 0.05)
heat_dataFlathead <- data.frame("X135_degree_winds.standardised" = rep(0,81*81), # makes empty dataframe ready for values
                         "X45_degree_winds.standardised" =  rep(0,81*81),
                         "Catch" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Drought_Months" = mean(my.df$Drought_Months),
                           "Estuary_Type" = "Drowned River Valley",
                           "Estuary" = "Hawkesbury River",
                           "Effort_days" = mean(my.df$Effort_days))
    PredX <- predict(m5, newdata = pred_map, type = "response", se.fit = F)
    heat_dataFlathead$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_dataFlathead$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_dataFlathead$Abundance[Nn] <- PredX#$fit
    #print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

pF <- ggplot(heat_dataFlathead, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) +
  geom_contour(col="white", aes(z = Abundance)) +#, binwidth = 0.002
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") +
  theme_classic() +
  viridis::scale_fill_viridis(option = "magma", name="Predicted Catch") + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 15),
        axis.text.x  = element_text(colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 15),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=10),
        legend.position = "bottom")

pF

write.csv(heat_dataFlathead, "../Data/Flathead heamap prediction data.csv", row.names = F)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



Drought plots

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucDEgPC0gcGxvdChnZ2VmZmVjdChtMSwgdGVybXMgPSBjKFwiRHJvdWdodF9Nb250aHNcIiwgXCJFc3R1YXJ5X1R5cGVcIikpKSArIGdndGl0bGUoXCJhKSBCcmVhbVwiKSsgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gXCJib3R0b21cIikgXG5wMiA8LSBwbG90KGdnZWZmZWN0KG0yLCB0ZXJtcyA9IGMoXCJEcm91Z2h0X01vbnRoc1wiLCBcIkVzdHVhcnlfVHlwZVwiKSkpICsgZ2d0aXRsZShcImMpIE11bGxldFwiKSsgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gXCJib3R0b21cIilcbnAzIDwtIHBsb3QoZ2dlZmZlY3QobTMsIHRlcm1zID0gYyhcIkRyb3VnaHRfTW9udGhzXCIsIFwiRXN0dWFyeV9UeXBlXCIpKSkgKyBnZ3RpdGxlKFwiZCkgV2hpdGluZ1wiKSsgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gXCJib3R0b21cIilcbiNwNCA8LSBwbG90KGdnZWZmZWN0KG00LCB0ZXJtcyA9IGMoXCJEcm91Z2h0X01vbnRoc1wiLCBcIkVzdHVhcnlfVHlwZVwiKSkpICsgZ2d0aXRsZShcImMpIEx1ZGVyaWNrXCIpKyB0aGVtZShsZWdlbmQucG9zaXRpb24gPSBcImJvdHRvbVwiKVxucDUgPC0gcGxvdChnZ2VmZmVjdChtNSwgdGVybXMgPSBjKFwiRHJvdWdodF9Nb250aHNcIiwgXCJFc3R1YXJ5X1R5cGVcIikpKSArIGdndGl0bGUoXCJiKSBGbGF0aGVhZFwiKSsgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gXCJib3R0b21cIilcblxubGlicmFyeShwYXRjaHdvcmspXG5wMSArIHA1ICsgcDIgK3AzICtwbG90X2xheW91dChucm93ID0gMiwgZ3VpZGVzID0gXCJjb2xsZWN0XCIpKyBwbG90X2xheW91dChndWlkZXMgPSBcImNvbGxlY3RcIikgJiBcbiAgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gJ2JvdHRvbScpKyB0aGVtZShheGlzLnRleHQgPSBlbGVtZW50X3RleHQoY29sb3VyID0gXCJibGFja1wiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBheGlzLnRpdGxlID0gZWxlbWVudF90ZXh0KGZhY2UgPSBcImJvbGRcIiwgY29sb3VyID0gXCJibGFja1wiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0aXRsZSA9IGVsZW1lbnRfdGV4dChmYWNlPVwiYm9sZFwiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBsZWdlbmQudGl0bGUgPSBlbGVtZW50X3RleHQoY29sb3VyID0gXCJibGFja1wiKSxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBsZWdlbmQudGV4dCA9IGVsZW1lbnRfdGV4dChjb2xvdXIgPSBcImJsYWNrXCIpKSAmXG4gIHhsYWIoXCJNb250aHMgb2YgRHJvdWdodFwiKSAmIHNjYWxlX3hfY29udGludW91cyhicmVha3M9YygwLDMsNiw5LDEyKSlcblxuZ2dzYXZlKFwiLi4vcGxvdHMvU3BlY2llcyBTcGVjaWZpYyBEcm91Z2h0IFJlc3BvbnNlcy5wbmdcIiwgZHBpID0gNjAwLCB3aWR0aCA9IDIxLCBoZWlnaHQgPSAxNC44LCB1bml0cz1cImNtXCIpXG5gYGAifQ== -->

```r
p1 <- plot(ggeffect(m1, terms = c("Drought_Months", "Estuary_Type"))) + ggtitle("a) Bream")+ theme(legend.position = "bottom") 
p2 <- plot(ggeffect(m2, terms = c("Drought_Months", "Estuary_Type"))) + ggtitle("c) Mullet")+ theme(legend.position = "bottom")
p3 <- plot(ggeffect(m3, terms = c("Drought_Months", "Estuary_Type"))) + ggtitle("d) Whiting")+ theme(legend.position = "bottom")
#p4 <- plot(ggeffect(m4, terms = c("Drought_Months", "Estuary_Type"))) + ggtitle("c) Luderick")+ theme(legend.position = "bottom")
p5 <- plot(ggeffect(m5, terms = c("Drought_Months", "Estuary_Type"))) + ggtitle("b) Flathead")+ theme(legend.position = "bottom")

library(patchwork)
p1 + p5 + p2 +p3 +plot_layout(nrow = 2, guides = "collect")+ plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')+ theme(axis.text = element_text(colour = "black"),
                                           axis.title = element_text(face = "bold", colour = "black"),
                                           title = element_text(face="bold"),
                                           legend.title = element_text(colour = "black"),
                                           legend.text = element_text(colour = "black")) &
  xlab("Months of Drought") & scale_x_continuous(breaks=c(0,3,6,9,12))

ggsave("../plots/Species Specific Drought Responses.png", dpi = 600, width = 21, height = 14.8, units="cm")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->

