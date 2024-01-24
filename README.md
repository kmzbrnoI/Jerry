# Jerry

Tento program slouží jako klientská aplikace k hJOP serveru k řízeni hnacích
vozidel.

## Funkce
 - řízení jízdního stupně, směru
 - možnost nouzového zastaveni, možnost plynulého zastavení
 - multitrakce
 - rozlišení mezi totálním ručním řízením (vhodné např. pro posun) a
   poloručním řízením (vhodné např. pro řízení funkcí HV v trati)
 - kontrola odpovědi serveru na přikazy
 - možnost převzít hnací vozidla na Rocomouse a zpět do regulátoru
 - žádost o lokomotivu(y) do oblastí řízení
 - změna serveru, portu, uživatelského jména a hesla
 - převzetí hnacího vozidla za pomocí autorizačního tokenu
 - kompatibilita s vnějšími programy akceptováním argumentů

## Spouštění programu

Jerry přebírá tyto argumenty:

 * '-u' username (přihlašovací jméno)
 * '-p' password (hash přihlašovacího hesla)
 * '-s' server (ip/dns) (specifikace umístění serveru)
 * '-pt' port (specifikace portu serveru)
 * '-a' (automatické připojení k serveru)
 * addr:token (adresa a token hnacího vozidla v převzetí)

např.
```
jerry.exe -a -u root -p heslo 1521:8afff1s86fs4sf86hy16j 2341:f4w64fe5f4wefew4fryh4
```

Pokud některý z argumentů uživatelské jméno, heslo, server nebo port není
vyplněn, je použit údaj z nastavení, popřípade je uživatel vyzván k zadání
loginu. Pokud není program volán s žádnými hnacími vozidly, pouze se připojí
k serveru a autorizuje.

Vytvořil Jan Horáček
Licensed under Apache License v2.0.
