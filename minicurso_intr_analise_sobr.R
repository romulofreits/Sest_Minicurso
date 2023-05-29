# Introdução à Análise de Sobrevivência

library(survival)                                                      
base = lung
head(base)
?lung

# Indicando que são dados de sobrevivência
Surv(base$time, base$status)

ekm0 = survfit(formula = Surv(time, status == 2) ~ 1, 
               data = base, conf.type = 'log-log'); ekm0
summary(ekm0)
plot(ekm0, lty=c(2,1), xlab='Tempo (dias)', ylab = 'S(t) estimada',
     mark.time = T)
####################################################################
install.packages('survminer')
library(survminer)
ggsurvplot(ekm0, conf.int = TRUE, pval = TRUE, risk.table = TRUE,
           surv.median.line = 'hv')

ggsurvplot(ekm0, conf.int = TRUE, pval = TRUE, risk.table = TRUE,
           surv.median.line = 'hv', fun = 'cumhaz')

plot(ekm0, conf.int = F)

summary(survfit(Surv(time, status)~1, data = base), times = 180)

# Colocar que nosso estimador dependa de alguma variável, nesse caso, o gênero
ekm = survfit(formula = Surv(time, status == 2) ~ sex, 
               data = base, conf.type = 'log-log'); ekm

summary(ekm)
plot(ekm, lty=c(2,1), xlab='Tempo (dias)', ylab = 'S(t) estimada',
     mark.time = T)

#ggsurvplot(ekm, conf.int = TRUE, pval = TRUE, risk.table = TRUE,
           #legen.labs = c('Masculino', 'Feminino')
           #legend.title = 'Sexo',
          #palette = c(''),
           #risk.table.height = T
           #surv.median.line = 'hv')


survdiff(Surv(time, status)~sex, data = base, rho = 0)

# usando outra variável
ekm1 = survfit(formula = Surv(time, status == 2) ~ ph.ecog, 
               data = base, conf.type = 'log-log'); ekm1
summary(ekm1)

ggsurvplot(ekm1, conf.int = TRUE, pval = TRUE, risk.table = TRUE,
           surv.median.line = 'hv')

survdiff(Surv(time, status)~ph.ecog, data = base, rho = 1)
survdiff(Surv(time, status)~ph.ecog, data = base, rho = 0)

nova_base = lung[-28,]
ekm1 = survfit(formula = Surv(time, status == 2) ~ ph.ecog, 
               data = base[-28,], conf.type = 'log-log'); ekm1
ggsurvplot(ekm1, conf.int = TRUE, pval = TRUE, risk.table = TRUE,
           surv.median.line = 'hv')

