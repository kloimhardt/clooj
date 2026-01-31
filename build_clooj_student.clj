(def d "src/clooj/")
(def name-target "clooj_student.clj")
(def names-source
  ["protocols.clj"
   "utils.clj"
   "collaj.clj"

   "settings.clj"
   "highlighting.clj"
   "navigate.clj"
   "brackets.clj"
   "indent.clj"
   "project.clj"
   "repl/main.clj"
   "help.clj"
   "search.clj"

   "core.clj"])

(spit name-target "")

(run! #(spit name-target (str % "\n\n") :append true)
      (map slurp
           (map #(str d %) names-source)))
