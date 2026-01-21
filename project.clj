(defproject clooj "0.5"
  :description "clooj, a small IDE for clojure"
  :url "https://github.com/clj-commons/clooj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main clooj.core
  :jvm-opts ["--add-exports" "java.desktop/com.apple.eawt=ALL-UNNAMED"]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 #_[clj-inspector "0.0.16"]
                 #_[slamhound "1.5.5"]
                 #_[com.cemerick/pomegranate "1.1.0"]
                 [com.fifesoft/rsyntaxtextarea "3.4.0"]
                 #_[nrepl/nrepl "1.1.1"]]) ;;klm
