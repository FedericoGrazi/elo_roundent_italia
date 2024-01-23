# Progetto ELO per Roundnet Italia

Il sistema di [punteggi ELO](https://it.m.wikipedia.org/wiki/Elo_(scacchi)) è uno dei più noti sistemi di ranking ed è il sistema ufficiale di ranking per gli scacchi.
L'idea di implementare l'ELO generando il RIR (Roundnet Italia Ranking) è nata dalla necessità di avere un sistema di ranking nello sport, e l'implementazione dell'ELO aggiustato con criteri simili al [RGX](https://playerzone.roundnetgermany.de/ranking/rg-index/) (Roundet Germany Index) ha generato un sistema funzionale per il ranking di Roundnet Italia.
Il progetto è open-source ed è stato compilato interamente su R con l'aiuto di Tommaso Marzozzi.

Il risultato del ranking si può vedere a questo [link](https://docs.google.com/spreadsheets/d/13FbtZEBHiLP55SYc5CCueYtzcvP2WgjJUsmjRe2vlCk/edit?usp=sharing) in cui è presente il ranking diviso in femminile e maschile. 
Per ora la divisione è solamente apparente, in quanto i tornei open, femminili e misti influiscono sullo stesso ranking.

>[!NOTE]  
>Il sistema non ha necessariamente raggiunto la sua forma finale. Ci potrebbero essere variazioni nei parametri della formula per adattare ulteriormente il sistema alla scena sportiva italiana.

## Cartella `scripts` 
La cartella contiene tutti i file necessari per riprodurre il sistema RIR. 
Il file `function_elo` contiene tutte le funzioni per utilizzare il sistema e fare le simulazioni
Istruzione per usare le funzioni
### `elo_tornei`
Parametri da inserire:  
1. `data`: file excel importato in R. Il formato di `.xslx` per far funzionare la funzione è questo. Prossimamente implementerò una funzione che trasformi i file di fwango direttamente in questo formato su R

    <img src="https://github.com/FedericoGrazi/elo_roundent_italia/blob/main/datasets/esempio dataset input.jpg?raw=true" alt="plot" width="450"/>
2. `elo_ratings`: possibilmente un output della funzione `elo_tornei`. Se lasciato vuoto inizializzerà tutti i giocatori con il valore di `elo_base`

### `simul_torneo`
Parametri da inserire
1. `data`: file che deve essere formattato in questa maniera

<img src="https://github.com/FedericoGrazi/elo_roundent_italia/blob/main/simulazioni/input simmul.jpg?raw=true" alt="plot" width="350"/>

2. `elo_ratings`: un ranking restituito dalla funzione `elo_tornei`.
   
## Cartella`dataset`
Sono stati caricati in `.xlsx` i file formattati nella stessa maniera dei tornei presi in considerazione per il ranking.
La legenda per i file è:
  
  - `ir_pt`: ItalRoundnet Prima Tappa Bologna,
  - `hot_mil`: ItalRoundnet Terza Tappa Hot Milano,
  - `Nationals`: ItalRoundent Nationals Forlì,
  - `ranking_winterspecial`: RCB Winter Special Open e Mixed,
  - `femminile`: RCB Winter Special Femminile,

## Cartella `simulazioni`
Utilizzando l'ELO è possibile ricavare una probabilità di vittoria. Con questa probabilità si può poi simulare partite e intere tornei sulla base di una distribuzione di Bernuolli con parametro la probabilità di vittoria. 
La formula per calcolare la probabilità di vittoria della squadra A contro la squadra B è
$$\frac{1}{1+10^{\frac{ELO_B - ELO_A}{400}}} $$

Un esempio di simulazione è calcolare la probabilità di vittoria ai Nationals, e fare di nuovo i complimenti ad Andrea Borsotti per la vittoria! (cit)
<img src="https://github.com/FedericoGrazi/elo_roundent_italia/blob/main/simulazioni/esempio_simulazione_nationals.jpg?raw=true" alt="plot" width="650"/>

Per richieste di simulazione scrivetemi in pvt.

>[!WARNING]  
>Per ora è possibile fare simulazioni solamente con 16 squadre e tutti i giocatori devono essere presenti nel ranking


## Cartella RCB
Per tutti i tesserati che si lamentano dei manini all'interno dei vari ranking, ho inserito anche i file necessari per ottenere i punteggi dei vari eventi interni nella stagione 2024.

### `punti_evento`
Sulla base della probabilità di vittoria del sistema ELO, assegna in maniera meritocratica i punti disponibili per il torneino. 
Facendo riferimento quanto un giocatore ha giocato bene nel torneino gli si assegna una *potenza* che viene utilizzata come punteggio per calcolare i punti vinti.
Il concetto è il medesimo del *Strength of Schedule* utilizzato negli sports analytics: a posteriori valutiamo quanto sia forte un giocatore sulla base di quanto ha performato bene, poi andiamo a ritroso a confrontare le partite che ha giocato.
