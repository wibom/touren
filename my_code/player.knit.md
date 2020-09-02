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

Samma plot finns f�r varje enskild spelare (med uppl�sning per Tour) p� 
`stats/playerscores`. T�nker att denna kan bytas ut mot en mer informativ 
plot h�r p� spelar-sidan... H�r kan man ha total-score/po�ng/erh�llna slag i 
en plot med 3 paneler (uppl�sning per runda / markera vilka rundor som "h�r 
ihop")...

Ifall denna plot tas bort b�de h�rifr�n och fr�n `stats/playerscores`, kan �ven 
funktionen raderas fr�n `helpers.Rmd`.


--------------------------------------------------------------------------------

### Innb�rdes m�ten
Innb�rdes m�ten r�knas per runda, d� tv� spelare deltagit vid samma Tour och 
s�ledes sepelat samma bana p� samma dag.





