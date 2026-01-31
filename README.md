---
This code editor, "clooj-for-students", is a fork of [clooj](https://github.com/clj-commons/clooj) that only depends on one Java library named  `RSyntaxTextArea`.
---

### Try out clooj-for-students

1) To check if Java is installed, type `java` on your command shell (you can decide to [install Java](https://adoptium.net/en-GB))
2) [Download noj-<version>-uber.jar](https://github.com/scicloj/noj/releases) for <version> `2-beta19.2` or higher.
3) Download `clooj_student.clj` by pressing the "Download raw file" button [on this page](https://github.com/kloimhardt/clooj/blob/master/clooj_student.clj)
4) After having the two downloaded files in one and the same directory, on the command line type:

```
java -cp noj-2-beta19.2-uber.jar clojure.main -e "(do (load-file \"clooj_student.clj\") (clooj.core/-main))"
```

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

### How to build clooj_student.clj

This step is for contributers to the clooj-for-students project only:
```
java -cp noj-2-beta19.2-uber.jar clojure.main -e "(load-file \"build_clooj_student.clj\")"
```
