---  
draft: false  
image: /img/players/wibom230x230.jpg  
showonlyimage: true  
title: Wibom  
summary: '**Status:** aktiv <br> **Antal Tourer:** 25'  
---

![Wibom](/img/players/wibom230x230.jpg)

------------------------------------------------------------------------

**Status:** aktiv  
**Antal tourer:** 25

------------------------------------------------------------------------

### Placeringar genom tiderna

![placeringshistorik](/playerstats/Wibom.placing.net.png) <br><br>
<details> <summary>Nyfiken hur det sett ut ifall Touren spelats
brutto?</summary> <p>

![placeringshistorik](/playerstats/Wibom.placing.gross.png) </p>
</details>

------------------------------------------------------------------------

### Samtliga scores per runda

    ## Warning: `expand_scale()` is deprecated; use `expansion()` instead.

![totscores](/playerstats/Wibom.totscores.png)

Samma plot finns för varje enskild spelare (med upplösning per Tour) på
`stats/playerscores`. Tänker att denna kan bytas ut mot en mer
informativ plot här på spelar-sidan… Här kan man ha
total-score/poäng/erhållna slag i en plot med 3 paneler (upplösning per
runda / markera vilka rundor som “hör ihop”)…

Ifall denna plot tas bort både härifrån och från `stats/playerscores`,
kan även funktionen raderas från `helpers.Rmd`.

------------------------------------------------------------------------

### Innbördes möten

Innbördes möten räknas per runda, då två spelare deltagit vid samma Tour
och således sepelat samma bana på samma dag.

![h2h.net](/playerstats/Wibom.h2h.net.png) <br><br> <details>
<summary>Nyfiken hur det sett ut ifall Touren spelats brutto?</summary>
<p>

![h2h.gross](/playerstats/Wibom.h2h.gross.png) </p> </details>

------------------------------------------------------------------------

Todo…
=====

lägg till:

-   aktuell ranking (bästa ranking; senaste tid för bästa ranking)
    -   lägg till även i `summary`
    -   sortera spelarna i ranking-ordning (front-matter `weight`)
-   figur: ranking (highligh player)
-   <s>figur: placerings-historia (highlight player)</s>
-   <s>figur: alla round-scorer (brutto och netto)</s>
-   figur: antal slag till godo/runda över tid (figur)
-   figur: vår-vs-höst + en mening med p-värde
