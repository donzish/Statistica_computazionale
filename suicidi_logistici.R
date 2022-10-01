setwd('F:/statistica computazionale/nostro lavoro')
library(readr)
suicidi <- read_csv("suicidi.csv")

#Teniamo solo le osservazioni dall'anno 2000 in avanti
suicidi <- suicidi[order(suicidi$year),]
suicidi <- suicidi[-c(1:11652),]

#Il tasso di suicidi in Italia nel 2019 è 6 persone su 100k abitanti. 
#Prendiamo quindi questo valore come soglia per stabilire quali osservazioni che hanno un tasso maggiore di quello
#italiano attuale o minore.
#Il valore soglia è quindi pari a 6.00.
summary(suicidi$`suicides/100k pop`)
#Vediamo che il valore scelto si trova leggermente al di sopra della mediana della variabile target. 
#Ci aspettiamo quindi che poco più della metà delle osservazioni, avrà un tasso minore.
suicidi$more6=ifelse(suicidi$`suicides/100k pop`>6,1,0) #aggiunngiamo la variabile dummy al dataset
table(suicidi$more6) #vediamo quante osservazioni sono sopra la soglia e quaanti sotto
prop.table(table(suicidi$more6)) #qua vediamo la percentuale; il 48% delle osservazioni è al di sopra della soglia

#Facciamo la regressione logistica, dato che il nostro target è una variabile dummy:
colnames(suicidi)
fitmore6 = glm(more6 ~ year + sex + age , data=suicidi,family = binomial)
#Costruiamo un modello tenendo solo alcune variabili del dataset, quelle considerate più interessanti per studiare
#le caratteristiche del singolo individuo.
summary(fitmore6)
#L'intercetta e la variabile year risultano poco significative, a differenza delle altre. La variabile più
#significativa risulta essere il sesso.
R2 <- 1-(15403/22392)
#L'R2 è pari a 0.312, vuol dire che il modello spiega il 31% della log-verosimiglianza del modello NULL (cioè il
#modello con solo l'intercetta).
#Vediamo i coefficieti:
library(coefplot)
coefplot(fitmore6, intercept=FALSE)
#Tutti i coefficienti dei parametri risultano essere positivi, tranne per age5-14years che abbassa notevolmente i
#valori del target. Il coefficiente del parametro sexmale è invece quello più elevato e influisce positivamente
#sul risultato della varibile.
#Ora calcoliamo gli odds ratio.
exp(fitmore6$coefficients) #esponenziamo i coeff
exp(confint(fitmore6))    #esponenziamo pure gli intervalli di confidenza

a=exp(cbind(OR=coef(fitmore6), confint(fitmore6)))
a
round(a, digits = 2)
#Tutti gli odds ratio ricadono negli intervalli di confidenza trovati. Proviamo in un altro modo.
#Le variabili di riferimento sono female per la variabile sex e age15-24 per la variabile age. Da qui capiamo quindi
#che i maschi hanno un'attitudine quasi 8 volte superiore a suicidarsi rispetto alle donne; oppure che gli anziani hanno
#una tendenza al suicidio due volte maggiore rispetto a quella dei giovani.

library(forestmodel)
print(forest_model(fitmore6),text_size = 5)
#Questa è una tabella riassuntiva dei calcoli fatti prima.

#Prima di fare questo, dobbiamo però verificare la convergenza del modello.
drop1(fitmore6, test="LRT") 
#La variabile year è la meno significativa. Infatti è caratterizzata da un valore LRT estremamente basso rispetto a 
#quello delle altre due variabili, molto più significative. Decidiamo quindi di eliminare la variabile year dal 
#modello.
fitmore6less = glm(more6 ~ sex + age , data=suicidi,family = binomial)
summary(fitmore6less)
R2less <- 1-(15406/22392)
#L'R2 è rimasto praticamente invariato. Decidiamo quindi di tenere quest'ultimo modello per procedere nelle analisi.
#I coefficienti del nuovo modello:
library(forestmodel)
print(forest_model(fitmore6less),text_size = 5)
#L'unico coefficiente ad essere cambiato nettamente è l'intercetta, da 16 a 0.3.
#Creiamo il modello nullo e lo confrontiamo col nostro modello:
null = glm(more6 ~ 1, data=suicidi,family = binomial)
summary(null) #vediamo che in questo caso l'intercetta è negativa ed estremamente bassa
summary(fitmore6less)
#Già prima abbiamo calcolato il valore dell'R2 quadro, pari a 0.31. Quindi conviene tenere il modello di riferimento
#per l'analisi, dato che la capacità esplicativa migliora del 31%.

#Riportiamo le statistiche del nostro modello
predicted_p <- predict(fitmore6less, suicidi, type="response") 
suicidi$predicted_p <- predict(fitmore6less, suicidi, type="response")
suicidi$predicted_y <- ifelse(suicidi$predicted_p > 0.5,1,0)
round(table(observed=suicidi$more6,predicted=suicidi$predicted_y)/nrow(suicidi),2)
#Il nostro modello ha un'accuratezza del 77%. Il modello assegna un valore veritiero nel 77% dei casi (somma dei 
#valori sulla diagonale, 0.44+0.33=0.77).
#Il 15% delle volte attribuiamo un tasso di suicidi inferiore alla soglia, quando in realtà non lo è.
#Nel restante 8% assegnamo un valore superiore al 6/1000 quando in realtà non lo è.
