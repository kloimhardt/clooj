---
This "clooj for students" is a hacked forge of [clooj](https://github.com/clj-commons/clooj) that depends only on the  `RSyntaxTextArea` library
---

### Rationale

In order to provide a Clojure setup for students, Prof. Lee Spector reached out in [clojurian slack](https://clojurians.slack.com/archives/C0BQDEJ8M/p1768618664898069)

Quotes:

> Just watched the live-reload [noj in a jar videos]( https://www.youtube.com/watch?v=tDz1x2d65C0) and that project is a thing of beauty! For that to be our primary platform, however, we'd have to pair it with the right editor (where right for me means
> - trivial installation/setup,
> - includes bracket matching and
> - auto-reindenting, and
> - doesn't force parinfer or paredit or otherwise get in the way of normal text editing
>
> a student who is just working on code in a single Clojure file (yes, you can write interesting evolutionary algorithms in a single short file!), can add a bit of code to get a basic plot.

clooj-for-students fulfills these requirements for writing "a bit of code in a short file". Importantly, its only dependency is the actively developed Java library [com.fifesoft/rsyntaxtextarea](https://github.com/bobbylight/RSyntaxTextArea/releases)

Try out clooj-for-students: `lein repl` and then call `(-main)`
