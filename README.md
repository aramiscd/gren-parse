# ulme-parse

Eine Bibliothek für [kombinatorisches
Parsen](https://en.wikipedia.org/wiki/Parser_combinator)

Der Code geht zurück auf meinen ersten erfolgreichen Versuch,
nicht-triviale Parser zu schreiben.  Der Ansatz wird hier erklärt:

https://vimeo.com/171704565 (sofern das Video noch online ist...)

Die Frage, wie man Eingaben systematisch in eine Datenstruktur
parst, hatte mich schon eine Weile umgetrieben, bis dahin ohne
befriedigende Antwort.  Parserkombinatoren haben für mich auf
Anhieb und intuitiv Sinn ergeben und mir auf einen Schlag ganz
neue Möglichkeiten beim Programmieren eröffnet.


## Tests ausführen

```
$ cd tests
$ gren make Main.gren
$ node app
```
