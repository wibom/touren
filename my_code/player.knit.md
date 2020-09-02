---
always_allow_html: true
output:
  md_document:
    pandoc_args: [
      "--from", "markdown-markdown_in_html_blocks+raw_html+line_blocks",
    ]
---





\-\-\-  
draft: false  
image: /img/players/karlsson230x230.jpg  
showonlyimage: true  
title: Karlsson  
summary:   \'**Status:** aktiv <br>     **Antal Tourer:** 24\'  
\-\-\-

![Karlsson](/img/players/karlsson230x230.jpg)

--------------------------------------------------------------------------------


__Status:__ aktiv  
__Antal tourer:__ 24  


--------------------------------------------------------------------------------

### Placeringar genom tiderna
![placeringshistorik](/playerstats/Karlsson.placing.net.png)
<br><br>
<details>
<summary>Nyfiken hur det sett ut ifall Touren spelats brutto?</summary>
<p>

![placeringshistorik](/playerstats/Karlsson.placing.gross.png)
</p>
</details>

--------------------------------------------------------------------------------

### Samtliga scores per runda

```
## Warning: `expand_scale()` is deprecated; use `expansion()` instead.
```

![totscores](/playerstats/Karlsson.totscores.png)

Samma plot finns för varje enskild spelare (med upplösning per Tour) på 
`stats/playerscores`. Tänker att denna kan bytas ut mot en mer informativ 
plot här på spelar-sidan... Här kan man ha total-score/poäng/erhållna slag i 
en plot med 3 paneler (upplösning per runda / markera vilka rundor som "hör 
ihop")...

Ifall denna plot tas bort både härifrån och från `stats/playerscores`, kan även 
funktionen raderas från `helpers.Rmd`.


--------------------------------------------------------------------------------

### Innbördes möten
Innbördes möten räknas per runda, då två spelare deltagit vid samma Tour och 
således sepelat samma bana på samma dag.





