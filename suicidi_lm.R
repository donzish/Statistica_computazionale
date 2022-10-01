setwd('F:/statistica computazionale/nostro lavoro')
library(readr)
suicidi <- read_csv("suicidi.csv")
View(suicidi)

#Il dataset originale di partenza è composto da 12 variabili e 27820 osservazioni.
#Vengono calcolate le statistiche descrittive delle variabili:

library(psych)
describe(suicidi)
summary(suicidi) #qui controlliamo gli ordini di grandezza delle nostre variabili e le loro medie
colnames(suicidi)
str(suicidi) #qui vediamo il tipo di variabile che andremo a usare nelle nostre analisi 

#Mettiamo l'output della str  e della sumary del file del datset.

####Valori mancanti####
sapply(suicidi, function(x)(sum(is.na(x))))
#Con questa funzione contiamo quanti valori mancanti ha ogni variabile. In questo caso l'unica variabile ad
#avere valori mancanti è 'HDI for year' (mancano 19456 dati).
library(VIM)
missingness<- aggr(suicidi, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(suicidi), cex.axis=.5,gap=2)
#Sono presenti valori mancanti solo per la variabile HDI for year, cioè l'indice di sviluppo umano. 
#Possiamo vedere che il 70% dei casi presenta valori mancanti per questa variabile, che quindi converrebbe 
#eliminare dal dataset.
#Selezioniamo dunque le unità in cui abbiamo dei valori per la variabile in esame, eliminando tutte le osservazioni
#con almeno un valore mancante.

completo <- suicidi[complete.cases(suicidi[c("HDI for year")]),]
missingness<- aggr(completo, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(completo), cex.axis=.7,gap=2)
#In questo caso il nostro datset avrebbe 8364 osservazioni. Vuol dire che ne perderemmo più di 2/3. Conviene 
#piuttosto eliminare la variabile HDI for year dal dataset.

suicidi1 <- suicidi[,-9] 
#Questo è il dataset senza le variabile HDI for year, quindi senza valori mancanti.
suicidi1 <- suicidi1[,-8] 
#Eliminiamo anche la variabile country_year perchè formata dall'unione di variabili già presenti nel dataset, che 
#quindi in futuro avrebbe causato problemi di collinearità.
#Queste sono quindi le variabili del dataset, dal quale andremo a stimare il modello:
str(suicidi1)
#Vediamo le statistiche descrittive dei dati:
summary(suicidi1)
#Non ci sono anomalie, ma la variabile gdp_for_year è definita su una scala diversa rispetto alle altre variabili.
#Decidiamo quindi di riscriverla con un diverso ordine di grandezza, lo stesso delle altre variabili:
suicidi1$gdp_year <- suicidi1$`gdp_for_year ($)`/10000000
suicidi1$`gdp_for_year ($)`= NULL #sostituiamo la variabile originale con la nuova 'gdp_year'

#Siccome il dataset è particolarmente ampio, decidiamo eseguire l'analisi per le osservazioni dall'anno 2000 
#in avanti. Modifichiamo dunque il dataset.
#Riordiniamo il datset in ordine crescente in base all'anno e teniamo solo le osservazioni dall'anno 2000 in avanti.

suicidi2 <- suicidi1[order(suicidi1$year),]
suicidi2 <- suicidi2[-c(1:11652),]
#Questo diventa dunque il nostro dataset di partenza, costituito da 16168 osservazioni e 10 variabili.

#Facciamo ora il modello full, lo starting model:
starting_model <- lm(suicidi2$`suicides/100k pop` ~ ., data=suicidi2)
summary(starting_model)
#Il modello ha un R2 pari a 0.5588 e un R2 aggiustato di 0.5556.
#Vediamo però l'eccessiva numerosità delle variabili presenti nel modello; questo è dovuto principalmente alla
#variabile country, che è costituita da 101 livelli. 
#Conviene quindi ridurre i livelli di questa variabile facendo optimal grouping.
#Vediamo i livelli delle variabili country, age e generation:
data.frame(table(suicidi1$country))
#Qui abbiamo 101 livelli (le nazioni del dataset). Sono troppi
data.frame(table(suicidi1$age))
#Qui vediamo che abbiamo solo 6 livelli, quindi la cosa non ci crea problemi
data.frame(table(suicidi1$generation))
#Anche qui abbiamo solo 6 livelli.
#L'unica variabile da ricodificare è country. 

library(dplyr)
library(factorMerger)
reduce_levels <- mergeFactors(response = suicidi2$`suicides/100k pop`, factor = suicidi2$country)
plot(reduce_levels , panel = "GIC",title = "", panelGrid = FALSE )
#Questo è il grafico dell'optimal grouping della variabile country.
#Le nazioni sono state accorpate in 16 gruppi.
og=cutTree(reduce_levels)
length(og)
table(og) 
#questo è il raggruppamento anova. Così sappiamo la numerosità di ciascun gruppo.
og2=as.numeric(og)
table(og2) 
#trasformiamo la variabile di prima in numerica. Così vediamo in modo chiaro la numerosità di ogni gruppo creato.
suicidi2$optimal_group=as.factor(og2)
head(suicidi2)
#così abbiamo aggiunto la nuova variabile og2 (come factor di 16 livelli) al datset.
plot(suicidi2$optimal_group, suicidi2$`suicides/100k pop`)
#Facendo i boxplot della variabile og2, vediamo che l'optimal groupig ha avuto successo.
#Il raggruppamento ha funzionato: il gruppo 1 è quello col tasso di suicidi più basso (senza considerare gli outlier)
#mentre il gruppo 16 è quello dei paesi caratterizzati dai tassi più elevati, ed è privo di outlier.

#Ora confrontiamo i modelli con country (e senza optimal_group) e con optimal_group (e senza country).
Formula <- paste(colnames(suicidi2), collapse="+")
Formula
full <- lm(suicidi2$`suicides/100k pop` ~ country+year+sex+age+suicides_no+population+suicidi2$`gdp_per_capita ($)`+generation+gdp_year, data=suicidi2)
summary(full)
#Qua l'R2 è di 0.5588.
full_opt <- lm(suicidi2$`suicides/100k pop` ~ year+sex+age+suicides_no+population+suicidi2$`gdp_per_capita ($)`+generation+gdp_year+optimal_group, data=suicidi2)
summary(full_opt)
#Qua l'R2 è di 0.554.
#L'R2 peggiora, ma di pochissimo. Tutte le modalità dell'optimal grouping vediamo che però sono molto significative,
#quindi decidiamo di sostituire la variabile country con la variabille optimal_group.

#Vediamo come si presentano graficamente le diagnostiche del modello:
par(mfrow=c(2,2))
plot(full_opt)
par(mfrow=c(1,1))
#Commentiamo i grafici:
#Dal primo grafico (fitted vs residuals) si apprende che il modello non è lineare e presenta eteroschedasticità.
#Dal secondo (quantili teorici vs residui standardizzati) vediamo che i residui seguono una distribuzione normale
#fino al primo quantile circa.
#Nel terzo grafico (fitted vs residui standardizzati) viene confermata la presenza di eteroschedasticità.
#Nell'ultimo grafico (leverage vs residui) si evidenzia la presenza di punti di leva, punti influenti e outliers.

#Vediamo anche il boxplot e l'istogramma della variabile target:
boxplot(suicidi2$`suicides/100k pop`)
#Dal boxplot vediamo un'elevata presenza di outliers.
hist(suicidi2$`suicides/100k pop`)
hist(suicidi2$`suicides/100k pop`, breaks=50)
#La variabile target presenta una notevole asimmetria positiva. Trasformandola potremmo migliorare la situazione.

#Come prima cosa analizziamo la multicollinearità delle variabili usate nel modello.

#####  MULTICOLLINEARITA'  ######

#Studiamo la correlazione tra le variabili quantitative.
require(dplyr)
require(corrgram)
corrgram(suicidi2 %>% dplyr::select_if(is.numeric), use = "complete.obs", lower.panel = panel.cor, cex=1, cex.labels = 1)
library(corrplot)
corrplot(cor(suicidi2 %>% dplyr::select_if(is.numeric), use = "complete.obs"),
         method='color',  tl.cex=.6, tl.col='black')
#Vediamo quantitativamente e graficamente la collinearità tra le variabili quantitative del dataset. 
#Le due variabili più correlate sono population e gdp_year, con un valore pari a 0.73. 
#Sono anche abbastanza correlate le variabili suicides_no e population, con un coefficiente di correlazione di 0.64.
#Tutte le altre variabili sono poco correlate, con coefficienti minori di 0.50.
#Alcune variabili (come year e suicides_no) sono completamente incorrelate.
#Calcoliamo i valori di TOL e VIF per capire quali covariate converebbe tenere e quali eliminare.
cov=attr(terms(full_opt), "term.labels") 
cov
b_covar <- suicidi2[,1:9] 

nums <- sapply(b_covar, is.numeric)
b_numeric <- b_covar[,nums] 


y = as.numeric(suicidi2$`suicides/100k pop`)
X<-b_numeric
X=as.matrix(X)

library(mctest)
imcdiag(X,y)
#Non ci sono variabili da eliminare (cioè non ci sono valori troppo elevati di collinearità), in quanto i valori di
#VIF e TOL non superano le soglie critiche.
#Ora dobbiamo controllare la connessione tra le variabili qualitative.
#Rinominiamo la variabile gdp_per_capita ($), perchè il nome crea difficoltà:
colnames(suicidi2)[8] <- "gdp_per_capita"
suicidi2$optimal_group <- as.character(suicidi2$optimal_group)
full_opt <- lm(suicidi2$`suicides/100k pop` ~ year+sex+age+suicides_no+population+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
cov=attr(terms(full_opt), "term.labels") 
cov
library(dplyr)
b_fac <- suicidi2[,cov]%>% dplyr::select_if(is.character) #è una serie di colonne
library(plyr)
combos <- combn(ncol(b_fac),2) #vediamo le coppie possibili
b.table <- table(b_fac$generation,b_fac$optimal_group)
b.table
numerosita=sum(b.table)
numerosita
# no missing values

chi=chisq.test(b.table)
chi$statistic

# chi normalized
chi$statistic/ (n*min(nrow(b.table)-1,ncol(b.table)-1)) #calcoliamo il Chi normalizzato
#Le variabili sex e age sono indipendenti, hanno Chi quadro pari a zero.
#Le variabili sex e generation sono indipendenti, hanno Chi quadro pari a zero.
#La variabile sex è indipendente anche dalla variabile optimal_group.
#Le variabili age e generation sono connesse, hanno un Chi quadro normalizzato pari a 0.45. Dato che comunque
#sono il valore è inferiore alla soglia critica, cioè 0.80, decidiamo di tenere entrambe le variabili nel datset.
#Le variabili age e optimal_group hanno un Chi quadro normalizzato estremamente basso, sono praticamente 
#indipendenti.
#Lo stesso tipodi commento vale anche per le variabili age e optimal_grop.
#In definitiva possiamo quindi tenere tutte le varibili sia quantitative che qualitative.

####LINEARITA'####
#Ora per migliorare il modello decidiamo di risolvere come primo il problema della linearità.
#Usiamo box-cox per vedere se modificando la variabile target il modello potrebbe migliorare.
summary(suicidi2$`suicides/100k pop`)
#Come già visto la variabile target ha un andamento asimmetrico, infatti 3/4 dei valori sono caratterizzati da
#valori molto più bassi rispetto a quelli dell'ultimo quantile, il cui massimo è pari a 204.92.
suicidi2$`suicides/100k pop`=suicidi2$`suicides/100k pop`+0.01
#Incrementiamo la variabile target di 0.01 per far sì che il suo valore minimo non fosse 0, così da poter 
#applicare tranquillamente box-cox.
full_opt2 <- lm(suicidi2$`suicides/100k pop` ~ year+sex+age+suicides_no+population+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
#Questo è il nuovo modello, con il target incrementato di 0.01.
library(MASS)
boxcoxreg1<-boxcox(full_opt2)
title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda
#Il valore di lambda viene pari a 0.22, quindi lo possiamo approssimare a 0. Siccome lambda=0, allora ci conviene
#modificare il target e usare il suo logaritmo. Così il modello ha un R2 pari a 0.5694.
#Incrementando la variabile di 0.01, invece che di 0.1, troviamo un lambda pari a 0.26, più vicino a 0.5.
#In questo caso ci conviene modificare la variabile target facendo la sua radice. Così facendo otteniamo un modello
#con un R2 ancora più alto, pari a 0.6816.
mod_sqrt = lm(sqrt(suicidi2$`suicides/100k pop`) ~ year+sex+age+suicides_no+population+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
summary(mod_sqrt)
#Il modello con la radice quadrata è nettamente migliore di quello logaritmico (ha un R2 migliore del 12%).
#L'R2 del modello iniziale (full_opt) era pari a 0.554, quindi abbiamo avuto un netto miglioramento.
#Vediamo graficamente come cambia la situazione:
par(mfrow=c(2,2))
plot(mod_sqrt)
par(mfrow=c(1,1))
#Tutti e 4 i grafici migliorano, anche se di poco. I dati si distribuiscno in modo più omogeneo e casuale.
#I residui vengono ridotti notevolmente, andando a restringere il range di vaariazione.
#Rinominiamo la variabile target e la aggiungiamo al dataset:
suicidi2$tasso <- sqrt(suicidi2$`suicides/100k pop`)
mod_sqrt = lm(tasso ~ year+sex+age+suicides_no+population+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
#Ora vediamo se il modello può essere migliorato ulteriormente lavorando sulle x.
library(mgcv)
gam1 = gam(tasso ~ s(year)+sex+age+s(suicides_no)+s(population)+s(gdp_per_capita)+generation+s(gdp_year)+optimal_group, data=suicidi2)
summary(gam1)
#Vediamo che modificando le covariate l'R2 aggiustato migliorerebbe (0.71).
#Studiamo quindi come riscrivere le covariate, che tra l'altro risultano essere tutte molto significative.
#Per primaa cosa confrontiamo i due modelli, quello con le covariate trasformate e non:
anova(mod_sqrt, gam1, test="LRT")
#Vediamo che la differenza tra i modelli è molto significativa. Ci conviene quindi procedere col modello gam1
#trasformando le x.
#Facciamo i grafici di gam1 per capire come trasformare al meglio le covariate.
par(mfrow=c(3,2)) 
plot(gam1)
par(mfrow=c(1,1)) 

#Le variabili year, gdp_per_capita, gdp_year hanno un andamento lineare costante, quindi non necessitano di alcuna
#modifica. Population ha un andamento lineare decrescente costante, quindi potrebbe essere approssimata con una 
#proporzionalità inversa. La variabile 
#riguardante il numero di suicidi è quella con un andamento più incerto. La maggior parte delle osservazioni però 
#(fino al terzo quartile) sono concentrate nella parte iniziale, che potrebbe essere approssimata con una parabola
#con la curva rivolta verso il basso.
#Proviamo a costruire vari modelli e vedere quale approssima al meglio i dati.

nuovo1 = lm(tasso~ year+sex+age+I(suicides_no^3)+population+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
summary(nuovo1) #così l'R2 si abbassa a 0.673
nuovo2 = lm(tasso~ year+sex+age+I(-suicides_no^2)+population+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
summary(nuovo2) #così l'R2 si abbassa a 0.674
nuovo3 = lm(tasso~ year+sex+age+I(sqrt(suicides_no))+population+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
summary(nuovo3) #così l'R2 migliora notevolmente e arriva a 0.720
nuovo4 = lm(tasso~ year+sex+age+suicides_no+I(1/population)+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
summary(nuovo4) #così l'R2 migliora e arriva a 0.6947
nuovo5 = lm(tasso~ year+sex+age+I(sqrt(suicides_no))+I(1/population)+gdp_per_capita+generation+gdp_year+optimal_group, data=suicidi2)
summary(nuovo5) #così l'R2 aggiustato migliora a 0.717
#Quindi è meglio trasformare solo la variabile numero di suicidi (facendone la radice quadrata) e 
#tenere la variabile population lineare.
#Vediamo graficamente come cambia la situazione:
par(mfrow=c(3,2)) 
plot(nuovo3)
par(mfrow=c(1,1)) 

par(mfrow=c(2,2))
plot(mod_sqrt)
par(mfrow=c(1,1))
#I grafici migliorano. In particolare quello dei residui vs leverage, in cui i valori dei punti di leva 
#diminuiscono notevolmente. Nel primo grafico invece notiamo un netto miglioramento della linearità, dato da una 
#migliore casualità nella distribuzione dei residui.
#Decidiamo quindi di tenere il modello nuovo3 per le prossime analisi.
#Confrontiamo pure le summary:
summary(mod_sqrt)
summary(nuovo3)
#L'R2 aggiustato migliora di circa il 3%. Non tutte le variabili sono significatiive, in entrambi i casi.
#Procediamo quindi a selezionare solo le variabili fondamentali per la costruzione del modello.

####MODEL SELECTION####
#Per migliorare il modello facciamo la model selection:
selectedMod1 <- step(nuovo3, direction="both")
drop1(selectedMod1, test = 'F')
#queste sono le variabili che otterremmo usando l'AIC: dovremmo togliere gdp_per_capita.
#Vediamo comunque che la variabile generation ha un livello di significatività più basso rispetto a quello delle 
#altre variabili. La variabile più significativa, caratterizzata da un F-value più elevato, è sex.
#Proviamo ora a fare la model selection usando SBC, più severo rispetto all'AIC.
selectedMod2 <- step(nuovo3, direction="both", k = log(nrow(suicidi2)))
drop1(selectedMod2, test = 'F')
#Queste sono le variabili che otterremmo usando l'SBC.In questo caso oltre alla variabile gdp_per_capita, dovremmo
#eliminare pure la variabile generation (che infatti risultava essere quella con il livello di significatività
#più basso). Tutte le altre covariate risultano essere significative.
#Dato che la variabile generation ha comunque un F-value molto più basso rispetto a quello di tutte le altre 
#variabili significative, decidiamo di eliminarla dal modello tenendo buono come selettore del modello il 
#metodo SBC.
#Riscriviamo il modello:
mod_sel = lm(tasso~ year+sex+age+I(sqrt(suicides_no))+population+gdp_year+optimal_group, data=suicidi2)
summary(mod_sel)
#Vediamo che l'R2 è rimasto praticamente invariato. Quindi possiamo considerare questo modello da ora in avanti.
#Ricontrolliamo i grafici (e la collinearità):
par(mfrow=c(2,2))
plot(mod_sel)
par(mfrow=c(1,1))
#Sono molto simili a prima.

####CASI INFLUUENTI####
#Ora vediamo come trattare i casi influenti.
library(car)
influencePlot(mod_sel,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Vediamo graficamente una consistente presenza di punti influenti, punti di leva e outliers.
#Innanzitutto cerchiamo di capire quali sono i punti influenti e di eliminarli.
who=suicidi2[c(49,1837,5869,6973,12133,15985),c(1,2,3,4,8,11)]
who
#Questi sono i 6 valori più influenti. Sono quasi tutte persone anziane (tranne uno) e maschi (tranne uno).
#I soggetti non sono concentrati in una particolare area geografica.
#Calcoliamo la distanza di Cook:
n=length(mod_sel$residuals)

res=data.frame(mod_sel$residuals)
data_used=suicidi2[rownames(res),]

cooksd <- cooks.distance(mod_sel)
cd=data.frame(cooksd)

cutoff <- 4/(length(mod_sel$residuals)-length(mod_sel$coefficients)-1) #pari a 0.0002478

influ=data.frame(data_used[cooksd > 0.0002478, ])  
nrow(influ)
#Abbiamo 1063 osservazioni che superano la soglia critica della distanza di Cook (poco più del 5%), quindi 
#accettiamo questo cutoff, perchè non troppo severo considerando la mole di dati.
#Selezioniamo solo le osservazioni non ifluenti togliendo quelle influenti dal datset.
NOinflu=data.frame(data_used[cooksd < 0.0002478, ]) 
#Questo è il nuovo daset, senza le osservazioni influenti, costituito da 15105 osservazioni e 12 variabili.
non_influ = lm(tasso~ year+sex+age+I(sqrt(suicides_no))+population+gdp_year+optimal_group, data=NOinflu)
summary(non_influ)
#L'R2 aumenta e arriva a 0.7866.
#Ricontrolliamo il grafico:
library(car)
influencePlot(non_influ,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Vediamo che il grafico migliora notevolmente senza i punti influenti. Infatti il range dei residui studentizzati
#si restringe e diventa circa (-3;4).
par(mfrow=c(2,2))
plot(non_influ)
par(mfrow=c(1,1))
#Dai grafici possiamo constatare un netto miglioramento.

####PUNTI DI LEVA####
#Per migliorare la situazione ci conviene eliminare alcuni punti di leva.
#Andiamo a identificare i punti di leva per ogni variabile.
lev <- hatvalues(non_influ)
lev <- data.frame(lev)
 #punti_leva <- data.frame(lev)
 #punti_leva <- data.frame(lev[ lev > 3*(25/15110)])
 #rownames(punti_leva)
#individuiamo i punti di leva che superano la soglia critica (cioè maggiori di 3*p/n). Sono 378.

NOinflu$lev <- lev$lev
NOinflu2 <- data.frame (NOinflu[lev < (2*(26/15105)),]) 
#Questo è il nuovo dataset di riferimento, senza i punti di leva (che sono 597).
#Volendo si potrebbe condurre un'analisi a parte per le osservazioni eliminate.
non_influ2 <- lm(tasso~ year+sex+age+I(sqrt(suicides_no))+population+gdp_year+optimal_group, data=NOinflu2)
summary(non_influ2)
#L'R2 diminuisce, ma di pochissimo. Conviene quindi eliminare le osservazioni con valori di x elevati per avere un
#dataset più omogeneo su cui lavorare. Vediamo come cambiano i grafici.
influencePlot(non_influ2,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Il grafico migliora molto.
par(mfrow=c(2,2))
plot(non_influ2)
par(mfrow=c(1,1))
#I miglioramenti sono evidenti anche nel grafico dei leverage vs residuals.
#Così abbiamo reso il modello più omogeneo. Ora possiamo controllare l'eteroschedasticità.

####ETEROSCHEDASTICITA'####
#Facciamo prima il test di Breusch-Pagan.
library(lmtest)
bptest(non_influ2)
#Il p-value è pari a 2.2e-16. Viene dunque rifiutata l'ipotesi nulla di omoschedasticità.
#Per conferma facciamo pure  il test di White, più severo:
library(car)
ncvTest(non_influ2)
#Il p-value del test di White è pari a 0.00296, che conferma la presenza di eteroschedasticità.
#Proviamo a correggerla con gli standard error di White.
library(lmSupport)
modelCorrectSE(non_influ2)
#Così confrontiamo gli standard error corretti con quelli del modello, omoschedastico.
#Anche se di poco, si osservano cambiamenti dei valori degli standard error.
#Nell'eventualità in cui dovremmo fare previsioni, sarà utile usare questi standard error come riferimento.
#(Dobbiamo usare gli standar error di White per fare inferenza in modo corretto).

####COEFFICIENTI E VARIABILI DEL MODELLO####
#Siccome vi è eteroschedasticità, dalla summary teniamo buone solo le stime dei coefficienti. 
#Facciamo quindi il coefficient plot, per studiare i coefficienti.
install.packages("coefplot")
library(coefplot)
coefplot(non_influ2, intercept=FALSE)
coefplot(non_influ2, decreasing=TRUE, sort='magnitude', intercept=FALSE) #così abbiamo i coefficienti ordinati 
#L'unica variabile caratterizzata da un coefficiente negativo è age5-14years (ha senso, in quanto è difficile 
#che un ragazzo in quella fascia d'età si suicidi). Le variabili year, gdp_year e population hanno coefficienti
#prossimmi allo 0, quindi influiscono di poco sul tasso dei suicidi. Le variabili caratterizzate da coefficienti 
#più elevati, oltre a quelle dell'optimal gruoping, sono sexmale e age75+years. Si può dedurre quindi che gli 
#anziani di sesso maschile soono i soggetti che influiscono di più sul tasso di suicidi, aumentandolo.
install.packages("forestmodel")
library(forestmodel)
forest_model(non_influ2)
#Le stesse considerazioni fatte prima sono riscontrabili nel forest model.
#In questo grafico non dobbiamo considerare la colonna dei p-value, in quanto non utilizziamo
#la correzione di White per gli standard error.

#Verifichiamo che tutte le variabili considerate nel modello finale siano significative:
drop1(non_influ2, .~., test='F')
#Tutte le variabili sono significative, la principale è sex, caratterizzata da un F value maggiore delle altre.

####CONFRONTO DEI MODELLI####
#Vediamo come il nostro modello è cambiato dopo tutte le modifiche.
#Confrontiamo il modello iniziale con quello finale.
summary(full)
summary(non_influ2)
#E' evidente il netto miglioramento dell'R2, che passa da 0.554 a 0.7807. Abbiamo quindi un miglioramento del 23% 
#circa. Diminuisce notevolmente il numero dei parametri, che risultano essere tutti altamente significativi nel
#modello finale. Cambia anche il range dei residui, che diminuisce drasticamente (da (-45.257;180.832) a (-2.618;
#3.4552)) assumendo una distribuzione più simmetrica. 

#Vediamo ora com'è cambiata graficamente la situazione rispetto all'inizio.
#Grafico di previsione col modello iniziale:
plot(suicidi2$`suicides/100k pop` , full$fitted.values)
abline(suicidi2$`suicides/100k pop`)
#Grafico di previsione col modello finaale:
plot(NOinflu2$tasso , non_influ2$fitted.values)

#Vediamo se le assunzioni per un modello lineare robusto sono rispettate.
library(gvlma)
gvlma(non_influ2)
#Nonostante le ipotesi alla base non siano verificate, constatiamo un netto miglioramento del modello.

###########MODELLO LOGISTICO##############
