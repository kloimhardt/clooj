---
This code editor, "clooj-for-students", is a fork of [clooj](https://github.com/clj-commons/clooj) that only depends on one Java library named  `RSyntaxTextArea`.
---

### Try out clooj-for-students

1) Download the file `clooj_student.clj` by pressing the "Download raw file" button on the right hand side of [this page](https://github.com/kloimhardt/clooj/blob/master/clooj_student.clj)![(the down arrow next to "Raw")](download_button.png)
2) Download the file 'rsyntaxtextarea-3.6.1.jar' from [maven-central](https://repo1.maven.org/maven2/com/fifesoft/rsyntaxtextarea/3.6.1/)
3) On the command line, in the directory of the downloaded file, type:

```
java -cp noj-2-beta19.2-uber.jar:rsyntaxtextarea-3.6.1.jar clojure.main -e "(do (load-file \"clooj_student.clj\") (clooj.core/-main))"
```

### Troubleshooting

If the above does not yet work, try the following:

a) To check if Java is installed, type `java` on your command shell (you may decide to [install Java](https://adoptium.net/en-GB))

b) Do you have your own `noj-version-uber.jar`? If so, in the command line of step 2), change the version `2-beta19.2` to your version. Or download the `.jar` file  from the  [github-repo](https://github.com/scicloj/noj/releases) and copy it into the same directory as `clooj_student.clj`.

c) Try step 3) again

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
