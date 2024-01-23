# Progetto ELO per Roundnet Italia

Il sistema di [punteggi ELO](https://it.m.wikipedia.org/wiki/Elo_(scacchi)) è uno dei più noti sistemi di ranking ed è il sistema ufficiale di ranking per gli scacchi.
L'idea di implementare l'ELO generando il RIR (Roundnet Italia Ranking) è nata dalla necessità di avere un sistema di ranking nello sport, e l'implementazione dell'ELO aggiustato con criteri simili al [RGX](https://playerzone.roundnetgermany.de/ranking/rg-index/) (Roundet Germany Index) ha generato un sistema funzionale per il ranking di Roundnet Italia.
Il progetto è open-source ed è stato compilato interamente su R con l'aiuto di Tommaso Marzozzi.

Il risultato del ranking si può vedere a questo [link](https://docs.google.com/spreadsheets/d/13FbtZEBHiLP55SYc5CCueYtzcvP2WgjJUsmjRe2vlCk/edit?usp=sharing) in cui è presente il ranking diviso in femminile e maschile. 
Per ora la divisione è solamente apparente, in quanto i tornei open, femminili e misti influiscono sullo stesso ranking.

**Note** Il sistema non ha necessariamente raggiunto la sua forma finale. Ci potrebbero essere variazioni nei parametri della formula per adattare ulteriormente il sistema alla scena sportiva italiana.

## Cartella `scripts` 
La cartella contiene tutti i file necessari per riprodurre il sistema RIR. Il file `function_elo` contiene tutte le funzioni per utilizzare il sistema e fare le simulazioni


## Cartella`dataset`
Sono stati caricati in `.xlsx` i file formattati nella stessa maniera dei tornei presi in considerazione per il ranking.
La legenda per i file è:
  
  - `ir_pt`: ItalRoundnet Prima Tappa Bologna,
  - `hot_mil`: ItalRoundnet Terza Tappa Hot Milano,
  - `Nationals`: ItalRoundent Nationals Forlì,
  - `rank...`: RCB Winter Special Open e Mixed,
  - `femminile`: RCB Winter Special Femminile,

## Cartella `simulazioni`
Utilizzando l'ELO è possibile ricavare una probabilità di vittoria. Con questa probabilità si può poi simulare partite e intere tornei sulla base di una distribuzione di Bernuolli con parametro la probabilità di vittoria.
Per richieste di simulazione scrivetemi in pvt.


## Cartella RCB
Per tutti i tesserati che si lamentano dei manini all'interno dei vari ranking, ho inserito anche i file necessari per ottenere i punteggi dei vari eventi interni nella stagione 2024
