#######
# Titulo: Replicación 1 - Metodología Política Avanzada
# Author: Raúl Abraham Castro Corona, 132148
#         María Ballesteros Suárez, 148939
#         Pierre Cilveti Saldívar, 133129
# Entrega: 12-10-2018
#######

library (lfe)
library (ggplot2)
library (readstata13)
library (stargazer)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(cowplot)
library(dplyr)


# Importación de datos
setwd ("/Volumes/RAC BANXICO/Semestre Otoño 2018/Metodología Política Avanzada/Replicacion 01/Archivos de Trabajo")

genold108 <- read.dta13 ("genold108.dta")
basic <- read.dta13 ("basic.dta")
View(genold108)
View(basic)

##############################################################################################
##############################################################################################
# I. Replicación
## (1) Congresos.
table(basic$congress) # Vemos cuántas observaciones existen por cada Congreso
basic$religion <- factor(basic$rgroup, levels = c(1,0,2,3,4), labels = c("Protestant", "None", "Catholic","Other Christian", "Other religion")) # Configuramos religión como factor
basic$partyname <- ifelse(basic$party == 1, "Democrats", ifelse(basic$party == 2, "Republicans", 0)) # Asignamos nombres a los partidos

# Creamos una base para cada Congreso
congress105 <- subset(basic, congress==105)
congress106 <- subset(basic, congress==106)
congress107 <- subset(basic, congress==107)
congress108 <- subset(basic, congress==108)

##############################################################################################
## (2) Estadísticas descriptivas

### (a) Relación entre puntajes 2D scatterplot
#### i) Visualización
grafico1 <- ggplot (basic, aes (x=aauw, y=nowtot)) + geom_point (size=1)  + geom_count(aes(color = ..n.., size = ..n..)) + geom_smooth (method="lm", se=F) + labs (x="AAUW", y="NOWTOT") + ggtitle("Scatterplot")

grafico2 <- ggplot (basic, aes (x=aauw, y=rtl)) + geom_point (size=1)  + geom_count(aes(color = ..n.., size = ..n..)) + geom_smooth (method="lm", se=F) + labs (x="AAUW", y="NRLC") + ggtitle("Scatterplot")

grafico3 <- ggplot (basic, aes (x=nowtot, y=rtl)) + geom_point (size=1)  + geom_count(aes(color = ..n.., size = ..n..)) + geom_smooth (method="lm", se=F) + labs (x="nowtot", y="NRLC") + ggtitle("Scatterplot")

grafico1
grafico2
grafico3

# Tanto AAUW como NOWTOT capturan casi lo mismo; NRLC captura lo contrario

#### ii) Correlación entre variables
cor(basic$aauw,basic$nowtot, use = "complete.obs")
cor(basic$aauw,basic$rtl, use = "complete.obs")
cor(basic$nowtot,basic$rtl, use = "complete.obs")

### (b) Replicar Figura 1

      # Separamos las bases de datos por número de hijos y creamos la variable meannow
      basic2 <- subset(congress105, congress105$totchi == 2)
      basic2<- basic2 %>% group_by(ngirls) %>% mutate(mean(nowtot, na.rm =T))
      mean_now <- ncol(basic2)
      names(basic2)[mean_now] <- "mean_now"
      names(basic2)[mean_now]
      rm(mean_now)
      
      basic3<- subset(congress105, congress105$totchi == 3)
      basic3<- basic3 %>% group_by(ngirls) %>% mutate(mean(nowtot, na.rm =T))
      mean_now <- ncol(basic3)
      names(basic3)[mean_now] <-"mean_now"
      names(basic3)[mean_now]
      rm(mean_now)
      
      # Separamos datos por partido
      democrats2 <- subset(basic2, party == 1)
      republicans2 <- subset(basic2, party == 2)
      
      # Creamos el primer Plot (para puntajes NOW)
      figure <- ggplot(basic2) +
        geom_bar( aes( x = partyname, y = nowtot, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean Now Score") +
        ggtitle( "Representatives with Two Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand= c(0,0))
      print(figure)
      
      figure2 <- ggplot(basic3) +
        geom_bar( aes( x = partyname, y = nowtot, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters", "3 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean Now Score") +
        ggtitle( "Representatives with Three Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0,0)) 
      print(figure2)
      
      ### (c) Replicar Figura 1 pero con puntaje AAUW
      
      figure3 <- ggplot(basic2) +
        geom_bar( aes( x = partyname, y = aauw, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean AAUW Score") +
        ggtitle( "Representatives with Two Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand= c(0,0))
      print(figure3)
      
      figure4 <- ggplot(basic3) +
        geom_bar( aes( x = partyname, y = aauw, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters", "3 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean AAUW Score") +
        ggtitle( "Representatives with Three Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0,0)) 
      print(figure4)

      rm(basic2,basic3)
### (c) Replicar Figura 1 pero con puntaje AAUW y otro año a elección
# Nota: Lo haremos con Congreso 108
      
      # Separamos las bases de datos por número de hijos y creamos la variable meannow
      basic2 <- subset(congress108, congress108$totchi == 2)
      basic2<- basic2 %>% group_by(ngirls) %>% mutate(mean(nowtot, na.rm =T))
      mean_now <- ncol(basic2)
      names(basic2)[mean_now] <- "mean_now"
      names(basic2)[mean_now]
      rm(mean_now)
      
      basic3<- subset(congress108, congress108$totchi == 3)
      basic3<- basic3 %>% group_by(ngirls) %>% mutate(mean(nowtot, na.rm =T))
      mean_now <- ncol(basic3)
      names(basic3)[mean_now] <-"mean_now"
      names(basic3)[mean_now]
      rm(mean_now)
      
      # Separamos datos por partido
      democrats2 <- subset(basic2, party == 1)
      republicans2 <- subset(basic2, party == 2)
      
      # Creamos el primer Plot (para puntajes NOW)
      figure5 <- ggplot(basic2) +
        geom_bar( aes( x = partyname, y = nowtot, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean Now Score") +
        ggtitle( "Representatives with Two Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand= c(0,0))
      print(figure5)
      
      figure6 <- ggplot(basic3) +
        geom_bar( aes( x = partyname, y = nowtot, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters", "3 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean Now Score") +
        ggtitle( "Representatives with Three Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0,0)) 
      print(figure6)
      
      ### (c) Replicar Figura 1 pero con puntaje AAUW
      
      figure7 <- ggplot(basic2) +
        geom_bar( aes( x = partyname, y = aauw, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean AAUW Score") +
        ggtitle( "Representatives with Two Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand= c(0,0))
      print(figure7)
      
      figure8 <- ggplot(basic3) +
        geom_bar( aes( x = partyname, y = aauw, fill = factor(ngirls)), color = "black", position= "dodge", stat = "summary", width = 0.5, fun.y="mean") +
        theme_hc(base_size = 20)  +
        scale_fill_grey(name="", labels=c("0 Daughters", "1 Daughter", "2 Daughters", "3 Daughters"), start = .95, end = 0.2) + 
        labs( x = " ", y = "Mean AAUW Score") +
        ggtitle( "Representatives with Three Children") +
        theme(legend.position = c(0.7, 0.8), 
              legend.box.background = element_rect(linetype="solid", colour ="black"), 
              legend.key = element_rect(size = 5),
              legend.key.size = unit(2, 'lines'), 
              plot.title = element_text(size = 18, 
                                        hjust = 0)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,100), expand = c(0,0)) 
      print(figure8)

      ### (e) Crédito extra 
      
##############################################################################################
## (3) Tabla 2

### (a)
reg1 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress105)

reg2 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress105)

reg3 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress106)

reg4 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress107)

reg5 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress108)

stargazer (reg1, reg2, reg3, reg4, reg5
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , covariate.labels = c("Number of female children","Female","White","Republican", "Age", "Age squared", "Service length", "Service length squared", "No religion", "Catholic", "Other Christian", "Other religion", "Democratic vote share",  "constant")
           , title="Table 2 - Impact of Female Children on Legislator Voting on Women's Issues", align=TRUE, type="latex")

### (b) Errores robustos y agrupados por estado
basic$id <-  1:nrow(basic)
congress105$id <- 1:nrow(congress105)
congress106$id <- 1:nrow(congress106)
congress107$id <- 1:nrow(congress107)
congress108$id <- 1:nrow(congress108)

# Errores agrupados por Estado
reg6 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | statenam, data = congress105)

reg7 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | statenam, data = congress105)

reg8 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | statenam, data = congress106)

reg9 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | statenam, data = congress107)

reg10 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | statenam, data = congress108)

stargazer (reg6, reg7, reg8, reg9, reg10
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , covariate.labels = c("Number of female children","Female","White","Republican", "Age", "Age squared", "Service length", "Service length squared", "No religion", "Catholic", "Other Christian", "Other religion", "Democratic vote share",  "constant")
           , title="Grouped Errors. Table 2 - Impact of Female Children on Legislator Voting on Women's Issues", align=TRUE, type="latex")

# Errores Robustos
reg11 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | id, data = congress105)

reg12 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | id, data = congress105)

reg13 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | id, data = congress106)

reg14 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | id, data = congress107)

reg15 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | id, data = congress108)

stargazer (reg11, reg12, reg13, reg14, reg15
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , covariate.labels = c("Number of female children","Female","White","Republican", "Age", "Age squared", "Service length", "Service length squared", "No religion", "Catholic", "Other Christian", "Other religion", "Democratic vote share",  "constant")
           , title="Robust Errors. Table 2 - Impact of Female Children on Legislator Voting on Women's Issues", align=TRUE, type="latex")

### (c) Mismos modelos, menos controles
reg16 <- felm(nowtot ~ ngirls + demvote + as.factor(totchi) + as.factor(region) | 0 | 0 | statenam, data = congress105)

reg17 <- felm(aauw ~ ngirls + demvote + as.factor(totchi) + as.factor(region) | 0 | 0 | statenam, data = congress105)

reg18 <- felm(aauw ~ ngirls + demvote + as.factor(totchi) + as.factor(region) | 0 | 0 | statenam, data = congress106)

reg19 <- felm(aauw ~ ngirls + demvote + as.factor(totchi) + as.factor(region) | 0 | 0 | statenam, data = congress107)

reg20 <- felm(aauw ~ ngirls + demvote + as.factor(totchi) + as.factor(region) | 0 | 0 | statenam, data = congress108)

stargazer (reg16, reg17, reg18, reg19, reg20
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , covariate.labels = c("Number of female children","Democratic vote share","1 child","2 children","3 children","4 children","5 children","6 children","7 children","8 children","9 children","10 children","Region 2","Region 3","Region 4","Region 5","Region 6","Region 7","Region 8","Region 9", "Region 1")
           , title="Table 2 - Impact of Female Children on Legislator Voting on Women's Issues. Grouped SE.", align=TRUE, type="latex")

#### Todos los congresos juntos

reg21 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = basic)

reg22 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | id, data = basic)

reg23 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | name, data = basic)

stargazer (reg21,reg22,reg23
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , title="Table 2 - Impact of Female Children on Legislator Voting on Women's Issues. All Congresses.", align=TRUE, type="latex")

##############################################################################################
## (4) Tabla 3

reg24 <- felm(nowtot ~ ngirls + female + I(ngirls*female) + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress105)

reg25 <- felm(nowtot ~ ngirls + repub + I(ngirls*repub) + white + female + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress105)

##############################################################################################
## (5) Crédito extra: Tabla A1

##############################################################################################
##############################################################################################
# II. Afirmaciones
## (1) Efectos fijos por hijo.
### (a) ¿Quién tiene más hijos en promedio?
reg25 <- felm(as.factor(totchi) ~ repub + nowtot + religion | 0 | 0 | 0, data = congress105)
reg26 <- felm(as.factor(totchi) ~ repub + aauw + religion | 0 | 0 | 0, data = congress105)
reg27 <- felm(as.factor(totchi) ~ repub + aauw + religion | 0 | 0 | 0, data = congress106)
reg28 <- felm(as.factor(totchi) ~ repub + aauw + religion | 0 | 0 | 0, data = congress107)
reg29 <- felm(as.factor(totchi) ~ repub + aauw + religion | 0 | 0 | 0, data = congress108)
reg30 <- felm(as.factor(totchi) ~ repub + rtl + religion | 0 | 0 | 0, data = congress105)
reg31 <- felm(as.factor(totchi) ~ repub + rtl + religion | 0 | 0 | 0, data = congress106)
reg32 <- felm(as.factor(totchi) ~ repub + rtl + religion | 0 | 0 | 0, data = congress107)
reg33 <- felm(as.factor(totchi) ~ repub + rtl + religion | 0 | 0 | 0, data = congress108)

stargazer (reg25, reg26, reg27, reg28, reg29, reg30, reg31, reg32, reg33
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , title="¿Quién tiene más hijes en promedio?", align=TRUE, type="latex")

### (b) Tabla 2 pero sin efectos fijos por hijo
reg34 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(region) | 0 | 0, data = congress105)

reg35 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(region) | 0 | 0, data = congress105)

reg36 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(region) | 0 | 0, data = congress106)

reg37 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(region) | 0 | 0, data = congress107)

reg38 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(region) | 0 | 0, data = congress108)

stargazer (reg34, reg35, reg36, reg37, reg38
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , covariate.labels = c("Number of female children","Female","White","Republican", "Age", "Age squared", "Service length", "Service length squared", "No religion", "Catholic", "Other Christian", "Other religion", "Democratic vote share",  "constant")
           , title="Impact of Female Children on Legislator Voting on Women's Issues - NO totchi (number of children) Fixed Effects" , align=TRUE, type="latex")


##############################################################################################
## (2) Legisladores sin hijos

reg39 <- felm(nowtot ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = subset(congress105, congress105$totchi>0))

reg40 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = subset(congress105, congress105$totchi>0))

reg41 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = subset(congress106, congress106$totchi>0))

reg42 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = subset(congress107, congress107$totchi>0))

reg43 <- felm(aauw ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = subset(congress108, congress108$totchi>0))

stargazer (reg39, reg40, reg41, reg42, reg43
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , covariate.labels = c("Number of female children","Female","White","Republican", "Age", "Age squared", "Service length", "Service length squared", "No religion", "Catholic", "Other Christian", "Other religion", "Democratic vote share",  "constant")
           , title="Impact of Female Children on Legislator Voting on Women's Issues, excluding members of Congress without children.", align=TRUE, type="latex")

##############################################################################################
## (3) Proporción de mujeres por partido político.
reg44 <- lm(ngirls/totchi ~ repub, data = congress105)
reg45 <- lm(ngirls/totchi ~ repub, data = basic)
summary(reg44)
summary(reg45)

stargazer (reg44,reg45
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , title="Proporción de mujeres por partido político", align=TRUE, type="latex")
##############################################################################################
## (4) Puntajes de la NRLC

reg46 <- felm(I(100-rtl) ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress105)

reg47 <- felm(I(100-rtl) ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress106)

reg48 <- felm(I(100-rtl) ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress107)

reg49 <- felm(I(100-rtl) ~ ngirls + female + white + repub + age + I(age^2) + srvlng + I(srvlng^2) + religion + demvote | as.factor(totchi) + as.factor(region) | 0 | 0, data = congress108)

stargazer (reg46, reg47, reg48, reg49
           , digits=3
           , omit.stat="f"
           , star.cutoffs=NA, omit.table.layout="n"
           , no.space=TRUE
           , covariate.labels = c("Number of female children","Female","White","Republican", "Age", "Age squared", "Service length", "Service length squared", "No religion", "Catholic", "Other Christian", "Other religion", "Democratic vote share",  "constant")
           , title="Impact of Female Children on Legislator Voting on Women's Issues (using RTL)", align=TRUE, type="latex")
