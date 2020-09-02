---
date: "2019-12-16"
draft: false
image: ''
showonlyimage: false
title: my_thoughts
weight: 0
summary: mina små tankar...
---

Listar lite små tankar för fortsättningen av detta hobby-projekt...


* Sparar mappen `/content/portfolio/` som mall för hur man kan använda
detta tema. Då allt är klart kan den raderas.


* Implementera allt från `Touren_new2/touren_qnd.Rmd` (inklusive
   det jag lekt med under `"quick tests"`)

* För att sortera ordningen på t.ex. tourer/spelare använd `front matter`-
  variabeln `weight` (kan t.ex. extraheras från tourid) - för att sortera
  omvänt borde man kunna ta `weight = 100 - tourid-nr`...

*


* Resultat
    + Lägg till en tabell med poäng för varje runda


* Ta tillbaka rankingen (från förr)
    + plotta likt vanlig score-plott


* Tankar kring stats att lägga till:

    + sannolikhet att vinna totalt, givet ledning efter:
        + runda 1
        + runda 1 med > 5 slag
        + runda 2
        + runda 2 med > 5 slag

    + sannolikhet att bli loser, givet loser efter:
        + runda 1
        + runda 1 med > 5 slag
        + runda 2
        + runda 2 med > 5 slag

    * Marginaler
        + Tappade ledningar
            + tabellera
                + tour
                + ledare
                + vinnare (inklusive position; eftersom det inte behöver vara 2an)
                + differans
                + markera ifall ledaren höll eller tappade...
            + efter 1 runda
            + efter 2 rundor
            + **tanke:** från denna tabell ska det bli tydligt vilka som är de
              största tappade ledningen t.ex... (kan säkert se många andra
              intressanta fynd också)

        + Kan man göra samma fast för "loser"?


* Tankar kring spelar-fakta att lägga till:

    + antal ledar-/mellan-/loser-boll inför sista dagen
    + antal ledare inför sista dagen + antal förvaltade
    + antal loser inför sista dagen + antal vändningar
    + plotta över alla spelade rundor
      "antal poäng per runda" (lolipop-plot; färgat per runda) /
      "antal slag på slopen" (line)
    + procent {slag_typ} (ranking: t.ex. spelare slår 46% "par" - ranking = 2a)
        +  brutto och netto
    + head2head mot varje annan spelare - illustrera hur h2h har utvecklats
      över alla tourer...
        + jag har plus-stats på Karlsson - mest sannolikt pga
          jag vann mkt för länge sedan... trenden borde vara att han vinner det
          mesta nu för tiden...)


* Småfix
    + Plotta score-kort, istället för att skriva ut tabeller... ?  
      Det borde bli snyggare.

--------------------------------------------------------------------------------
<br><br>
#page-summary stular

>Ifall man använder `<!--more-->` för att bestämma var `summary` ska "avslutas",
då inkldueras bilder i summary... det blir konstigt för spelare.


>Ifall man __inte använder__ `<!--more-->`, då kan man istället använda
`summary` i Hugo front matter (YAML).

Sidor där jag desperat letade medan detta strulade som värst:

* https://github.com/levitanong/hyde/commit/a2bf8a1a7c1bdab1f2bcbb32d895b6c73aa47e9e#diff-1578c58a761c0846adb0e788c860a12b
* https://github.com/gohugoio/hugo/issues/1634
* https://github.com/halogenica/beautifulhugo/pull/179
* https://github.com/spf13/hyde/pull/61
* https://gohugo.io/content-management/summaries/#manual-summary-splitting


--------------------------------------------------------------------------------
<br><br>
#tabset
Hade på flera ställen gärna använt `.tabset`, för att t.ex. lista resultat
netto och brutto i olika tabbar. T.ex. avseende:

* topscores.Rmd (för att växla mellan figur och tabell)
* placing.Rmd (för att växla mellan brutto och netto)

`.tabset` verkar dock inte möjligt just nu:
https://github.com/rstudio/blogdown/issues/69

Så kör `details`/`summary` i html istället...
