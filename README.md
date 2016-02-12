# Jerry

Tento program slouzi jako klientska aplikace k hJOP serveru k rizeni hnacich
vozidel.

Funkce:
 - rizeni jizdniho stupne, smeru
 - moznost nouzoveho zastaveni, moznost plynuleho zastaveni
 - multitrakce
 - rozliseni mezi totalnim rucnim rizenim (vhodne napr. pro posun) a
   polorucnim rizenim (vhodne napr. pro rizeni funkci HV v trati)
 - kontrola odpovedi serveru, resp. centraly, na prikaz
 - moznost prevzit hnaci vozidla na Rocomouse a zpet do regulatoru
 - zadost o lokomotivu(y) do oblasti rizeni
 - zmena serveru, portu, uzivatelskeho jmena a hesla
 - prevzeti hnaciho vozidla za pomoci autorizacniho tokenu
 - kompatibilita s vnejsimi programy akceptovanim argumentu

Viz src/Jerry.dpr

(c) Jan Horacek 2015-2016
Licensed under Apache License v2.0.