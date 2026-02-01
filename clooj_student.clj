; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.protocols)

;; Repl protocol

(defprotocol Repl
  (evaluate [this code] "Evaluate code (a string).")
  (close [this] "Stop the repl instance."))


; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.utils
  (:require [clojure.string :as string])
  (:import (java.awt FileDialog Point Window)
           (java.awt.event ActionListener MouseAdapter)
           (java.util.prefs Preferences)
           (java.security MessageDigest)
           (java.io ByteArrayInputStream ByteArrayOutputStream
                    File FilenameFilter BufferedReader
                    InputStreamReader
                    ObjectInputStream ObjectOutputStream
                    OutputStream PrintStream)
           (javax.swing AbstractAction JButton JFileChooser JMenu JMenuItem BorderFactory
                        JOptionPane JSplitPane KeyStroke SpringLayout SwingUtilities)
           (javax.swing.event CaretListener DocumentListener UndoableEditListener)
           (javax.swing.undo UndoManager)))

;; general

(defmacro do-when [f & args]
  (let [args_ args]
    `(when (and ~@args_)
       (~f ~@args_))))

(defmacro when-lets [bindings & body]
  (assert (vector? bindings))
  (let [n (count bindings)]
    (assert (zero? (mod n 2)))
    (assert (<= 2 n))
  (if (= 2 n)
    `(when-let ~bindings ~@body)
    (let [[a b] (map vec (split-at 2 bindings))]
      `(when-let ~a (when-lets ~b ~@body))))))

(defn count-while [pred coll]
  (count (take-while pred coll)))

(defn remove-nth [s n]
  (lazy-cat (take n s) (drop (inc n) s)))

(defmacro awt-event [& body]
  `(SwingUtilities/invokeLater
     (fn [] (try ~@body
                 (catch Throwable t#
                        (.printStackTrace t#))))))

(defmacro gen-map [& args]
  (let [kw (map keyword args)]
    (zipmap kw args)))

(defn class-for-name
  "Returns true if a class represented by class-name
   can be found by the class loader."
  [class-name]
  (try (Class/forName class-name)
       (catch Throwable _ nil)))

;; preferences

;; define a UUID for clooj preferences
(def clooj-prefs (.. Preferences userRoot
                     (node "clooj") (node "c6833c87-9631-44af-af83-f417028ea7aa")))

(defn partition-str [n s]
  (let [l (.length s)]
    (for [i (range 0 l n)]
      (.substring s i (Math/min l (+ (int i) (int n)))))))

(def pref-max-bytes (* 3/4 Preferences/MAX_VALUE_LENGTH))

(defn write-value-to-prefs
  "Writes a pure clojure data structure to Preferences object."
  [prefs key value]
  (let [chunks (partition-str pref-max-bytes (with-out-str (pr value)))
        node (. prefs node key)]
    (.clear node)
    (doseq [i (range (count chunks))]
      (. node put (str i) (nth chunks i)))))

(defn read-value-from-prefs
  "Reads a pure clojure data structure from Preferences object."
  [prefs key]
  (when-not (.endsWith key "/")
    (let [node (.node prefs key)
          s (apply str
                   (for [i (range (count (. node keys)))]
                     (.get node (str i) nil)))]
      (when (and s (pos? (.length s))) (read-string s)))))

(defn write-obj-to-prefs
  "Writes a java object to a Preferences object."
  [prefs key obj]
  (let [bos (ByteArrayOutputStream.)
        os (ObjectOutputStream. bos)
        node (.node prefs key)]
    (.writeObject os obj)
    (. node putByteArray "0" (.toByteArray bos))))

(defn read-obj-from-prefs
  "Reads a java object from a Preferences object."
  [prefs key]
  (let [node (.node prefs key)
        bis (ByteArrayInputStream. (. node getByteArray "0" nil))
        os (ObjectInputStream. bis)]
    (.readObject os)))

;; identify OS

(defn get-os []
  (string/lower-case (System/getProperty "os.name")))

(def is-win
  (memoize #(not (neg? (.indexOf (get-os) "win")))))

(def is-mac
  (memoize #(not (neg? (.indexOf (get-os) "mac")))))

(def is-unix
  (memoize #(not (and (neg? (.indexOf (get-os) "nix"))
                      (neg? (.indexOf (get-os) "nux"))))))

;; swing layout

(defn put-constraint [comp1 edge1 comp2 edge2 dist]
  (let [edges {:n SpringLayout/NORTH
               :w SpringLayout/WEST
               :s SpringLayout/SOUTH
               :e SpringLayout/EAST}]
    (.. comp1 getParent getLayout
        (putConstraint (edges edge1) comp1
                       dist (edges edge2) comp2))))

(defn put-constraints [comp & args]
  (let [args (partition 3 args)
        edges [:n :w :s :e]]
    (dorun (map #(apply put-constraint comp %1 %2) edges args))))

(defn constrain-to-parent
  "Distance from edges of parent comp args"
  [comp & args]
  (apply put-constraints comp
         (flatten (map #(cons (.getParent comp) %) (partition 2 args)))))

;; text components

(defn get-line-text [text-pane line]
  (let [start (.getLineStartOffset text-pane line)
        length (- (.getLineEndOffset text-pane line) start)]
    (.. text-pane getDocument (getText start length))))

(defn append-text
  ([text-pane text scroll-to-end?]
    (append-text text-pane text))
  ([text-pane text]
    (.append text-pane text)))

(defn get-coords [text-comp offset]
  (let [row (.getLineOfOffset text-comp offset)
        col (- offset (.getLineStartOffset text-comp row))]
    {:row row :col col}))

(defn get-caret-coords [text-comp]
  (get-coords text-comp (.getCaretPosition text-comp)))

(defn add-text-change-listener
  "Executes f whenever text is changed in text component."
  [text-comp f]
  (.addDocumentListener
    (.getDocument text-comp)
    (reify DocumentListener
      (insertUpdate [this evt] (f text-comp))
      (removeUpdate [this evt] (f text-comp))
      (changedUpdate [this evt]))))

(defn remove-text-change-listeners [text-comp]
  (let [d (.getDocument text-comp)]
    (doseq [l (.getDocumentListeners d)]
      (.removeDocumentListener d l))))

(defn get-text-str [text-comp]
  (let [doc (.getDocument text-comp)]
    (.getText doc 0 (.getLength doc))))

(defn add-caret-listener [text-comp f]
  (.addCaretListener text-comp
                     (reify CaretListener (caretUpdate [this evt]
                                                       (f text-comp)))))

(defn set-selection [text-comp start end]
  (doto text-comp (.setSelectionStart start) (.setSelectionEnd end)))

(defn scroll-to-pos [text-area offset]
  (let [r (.modelToView text-area offset)
        v (.getParent text-area)
        l (.. v getViewSize height)
        h (.. v getViewRect height)]
    (when r
      (.setViewPosition v
                        (Point. 0 (min (- l h) (max 0 (- (.y r) (/ h 2)))))))))

(defn scroll-to-line [text-comp line]
    (let [text (.getText text-comp)
          pos (inc (.length (string/join "\n" (take (dec line) (string/split text #"\n")))))]
      (.setCaretPosition text-comp pos)
      (scroll-to-pos text-comp pos)))

(defn scroll-to-caret [text-comp]
  (scroll-to-pos text-comp (.getCaretPosition text-comp)))

(defn focus-in-text-component [text-comp]
  (.requestFocusInWindow text-comp)
  (scroll-to-caret text-comp))

(defn get-selected-lines [text-comp]
  (let [row1 (.getLineOfOffset text-comp (.getSelectionStart text-comp))
        row2 (inc (.getLineOfOffset text-comp (.getSelectionEnd text-comp)))]
    (doall (range row1 row2))))

(defn get-selected-line-starts [text-comp]
  (map #(.getLineStartOffset text-comp %)
       (reverse (get-selected-lines text-comp))))

(defn insert-in-selected-row-headers [text-comp txt]
  (awt-event
    (let [starts (get-selected-line-starts text-comp)
          document (.getDocument text-comp)]
      (dorun (map #(.insertString document % txt nil) starts)))))

(defn remove-from-selected-row-headers [text-comp txt]
  (awt-event
    (let [len (count txt)
          document (.getDocument text-comp)]
      (doseq [start (get-selected-line-starts text-comp)]
        (when (= (.getText (.getDocument text-comp) start len) txt)
          (.remove document start len))))))

(defn comment-out [text-comp]
  (insert-in-selected-row-headers text-comp ";"))

(defn uncomment-out [text-comp]
  (remove-from-selected-row-headers text-comp ";"))

(defn toggle-comment [text-comp]
  (if (= (.getText (.getDocument text-comp)
                   (first (get-selected-line-starts text-comp)) 1)
         ";")
    (uncomment-out text-comp)
    (comment-out text-comp)))

(defn indent [text-comp]
  (when (.isFocusOwner text-comp)
    (insert-in-selected-row-headers text-comp " ")))

(defn unindent [text-comp]
  (when (.isFocusOwner text-comp)
    (remove-from-selected-row-headers text-comp " ")))

;; other gui

(defn make-split-pane [comp1 comp2 horizontal divider-size resize-weight]
  (doto (JSplitPane. (if horizontal JSplitPane/HORIZONTAL_SPLIT
                                    JSplitPane/VERTICAL_SPLIT)
                     true comp1 comp2)
        (.setResizeWeight resize-weight)
        (.setOneTouchExpandable false)
        (.setBorder (BorderFactory/createEmptyBorder))
        (.setDividerSize divider-size)))

;; keys

(defn get-keystroke [key-shortcut]
  (KeyStroke/getKeyStroke
    (-> key-shortcut
      (.replace "cmd1" (if (is-mac) "meta" "ctrl"))
      (.replace "cmd2" (if (is-mac) "ctrl" "alt")))))

;; actions

(defn attach-child-action-key
  "Maps an input-key on a swing component to an action,
  such that action-fn is executed when pred function is
  true, but the parent (default) action when pred returns
  false."
  [component input-key pred action-fn]
  (let [im (.getInputMap component)
        am (.getActionMap component)
        input-event (get-keystroke input-key)
        parent-action (when-let [tag (.get im input-event)]
                        (.get am tag))
        child-action
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (if (pred)
                (action-fn)
                (when parent-action
                  (.actionPerformed parent-action e)))))
        uuid (str (random-uuid))]
    (.put im input-event uuid)
    (.put am uuid child-action)))


(defn attach-child-action-keys [comp & items]
  (run! #(apply attach-child-action-key comp %) items))

(defn attach-action-key
  "Maps an input-key on a swing component to an action-fn."
  [component input-key action-fn]
  (attach-child-action-key component input-key
                           (constantly true) action-fn))

(defn attach-action-keys
  "Maps input keys to action-fns."
  [comp & items]
  (run! #(apply attach-action-key comp %) items))

;; buttons

(defn create-button [text fn]
  (doto (JButton. text)
    (.addActionListener
      (reify ActionListener
        (actionPerformed [_ _] (fn))))))

;; menus

(defn add-menu-item
  ([menu item-name key-mnemonic key-accelerator response-fn]
    (let [menu-item (JMenuItem. item-name)]
      (when key-accelerator
        (.setAccelerator menu-item (get-keystroke key-accelerator)))
      (when (and (not (is-mac)) key-mnemonic)
        (.setMnemonic menu-item (.getKeyCode (get-keystroke key-mnemonic))))
      (.addActionListener menu-item
                          (reify ActionListener
                            (actionPerformed [this action-event]
                                             (response-fn))))
      (.add menu menu-item)))
  ([menu item]
    (condp = item
      :sep (.addSeparator menu))))

(defn add-menu
  "Each item-tuple is a vector containing a
  menu item's text, mnemonic key, accelerator key, and the function
  it executes."
  [menu-bar title key-mnemonic & item-tuples]
  (let [menu (JMenu. title)]
    (when (and (not (is-mac)) key-mnemonic)
      (.setMnemonic menu (.getKeyCode (get-keystroke key-mnemonic))))
    (run! #(apply add-menu-item menu %) item-tuples)
    (.add menu-bar menu)
    menu))

;; mouse

(defn on-click [comp num-clicks fun]
  (.addMouseListener comp
    (proxy [MouseAdapter] []
      (mouseClicked [event]
        (when (== num-clicks (.getClickCount event))
          (.consume event)
          (fun))))))

;; undoability

(defn make-undoable [text-area]
  (let [undoMgr (UndoManager.)]
    (.setLimit undoMgr 1000)
    (.. text-area getDocument (addUndoableEditListener
        (reify UndoableEditListener
          (undoableEditHappened [this evt] (.addEdit undoMgr (.getEdit evt))))))
    (attach-action-keys text-area
      ["cmd1 Z" #(when (.canUndo undoMgr) (.undo undoMgr))]
      ["cmd1 shift Z" #(when (.canRedo undoMgr) (.redo undoMgr))])))


;; file handling

(defn choose-file [parent title suffix load]
  (let [dialog
    (doto (FileDialog. parent title
            (if load FileDialog/LOAD FileDialog/SAVE))
      (.setFilenameFilter
        (reify FilenameFilter
          (accept [this _ name] (. name endsWith suffix))))
      (.setVisible true))
    d (.getDirectory dialog)
    n (.getFile dialog)]
    (when (and d n)
      (File. d n))))

;doesn't work with Java 7 -- see version below
;(defn choose-directory [parent title]
;  (if (is-mac)
;    (let [dirs-on #(System/setProperty
;                     "apple.awt.fileDialogForDirectories" (str %))]
;      (dirs-on true)
;        (let [f (choose-file parent title "" true)]
;          (dirs-on false)
;          (.getParentFile f)))
;    (let [fc (JFileChooser.)
;          last-open-dir (read-value-from-prefs clooj-prefs "last-open-dir")]
;      (doto fc (.setFileSelectionMode JFileChooser/DIRECTORIES_ONLY)
;               (.setDialogTitle title)
;               (.setCurrentDirectory (if last-open-dir (File. last-open-dir) nil)))
;       (if (= JFileChooser/APPROVE_OPTION (.showOpenDialog fc parent))
;         (.getSelectedFile fc)))))

(defn choose-directory [parent title]
  (let [fc (JFileChooser.)
        last-open-dir (read-value-from-prefs clooj-prefs "last-open-dir")]
    (doto fc (.setFileSelectionMode JFileChooser/DIRECTORIES_ONLY)
      (.setDialogTitle title)
      (.setCurrentDirectory (if last-open-dir (File. last-open-dir) nil)))
    (when (= JFileChooser/APPROVE_OPTION (.showOpenDialog fc parent))
      (.getSelectedFile fc))))


(defn get-directories [path]
  (filter #(and (.isDirectory %)
                (not (.startsWith (.getName %) ".")))
          (.listFiles path)))

(defn file-exists? [file]
  (and file (.. file exists)))

;; tree seq on widgets (awt or swing)

(defn widget-seq [^java.awt.Component comp]
  (tree-seq #(instance? java.awt.Container %)
            #(seq (.getComponents %))
            comp))

;; saving and restoring window shape in preferences

(defn get-shape [components]
  (for [comp components]
    (condp instance? comp
      Window
        [:window {:x (.getX comp) :y (.getY comp)
                  :w (.getWidth comp) :h (.getHeight comp)}]
      JSplitPane
        [:split-pane {:location (.getDividerLocation comp)}]
      nil)))

(defn watch-shape [components fun]
  (doseq [comp components]
    (condp instance? comp
      Window
        (.addComponentListener comp
          (proxy [java.awt.event.ComponentAdapter] []
            (componentMoved [_] (fun))
            (componentResized [_] (fun))))
      JSplitPane
        (.addPropertyChangeListener comp JSplitPane/DIVIDER_LOCATION_PROPERTY
          (proxy [java.beans.PropertyChangeListener] []
            (propertyChange [_] (fun))))
      nil)))

(defn set-shape [components shape-data]
  (loop [comps components shapes shape-data]
    (let [comp (first comps)
          shape (first shapes)]
      (try
        (when shape
          (condp = (first shape)
            :window
            (let [{:keys [x y w h]} (second shape)]
              (.setBounds comp x y w h))
            :split-pane
            (.setDividerLocation comp (:location (second shape)))
            nil))
        (catch Exception e nil)))
    (when (next comps)
      (recur (next comps) (next shapes)))))

(defn save-shape [prefs name components]
  (write-value-to-prefs prefs name (get-shape components)))

(defn restore-shape [prefs name components]
  (try
    (set-shape components (read-value-from-prefs prefs name))
    (catch Exception e)))

(defn confirmed? [question title]
  (= JOptionPane/YES_OPTION
     (JOptionPane/showConfirmDialog
       nil question title  JOptionPane/YES_NO_OPTION)))

(defn ask-value [question title]
  (JOptionPane/showInputDialog nil question title JOptionPane/QUESTION_MESSAGE))

(defn persist-window-shape [prefs name ^java.awt.Window window]
  (let [components (widget-seq window)
        shape-persister (agent nil)]
    (restore-shape prefs name components)
    (watch-shape components
                 #(send-off shape-persister
                            (fn [old-shape]
                              (let [shape (get-shape components)]
                                (when (not= old-shape shape)
                                  (write-value-to-prefs prefs name shape))
                                shape))))))

(defn sha1-str [obj]
   (let [bytes (.getBytes (with-out-str (pr obj)))]
     (String. (.digest (MessageDigest/getInstance "MD") bytes))))

;; streams, writers and readers

(defn printstream-to-writer [writer]
  (->
    (proxy [OutputStream] []
      (write
        ([^bytes bs offset length]
          (.write writer
                  (.toCharArray (String. ^bytes bs "utf-8"))
                  offset length))
        ([b]
          (.write writer b)))
      (flush [] (.flush writer))
      (close [] (.close writer)))
    (PrintStream. true)))

(defn process-reader
  "Create a buffered reader from the output of a process."
  [process]
  (-> process
      .getInputStream
      InputStreamReader.
      BufferedReader.))

(defn copy-input-stream-to-writer
  "Continuously copies all content from a java InputStream
   to a java Writer. Blocks until InputStream closes."
  [input-stream writer]
  (let [reader (InputStreamReader. input-stream)]
    (loop []
      (let [c (.read reader)]
        (when (not= c -1)
          (.write writer c)
          (recur))))))

;; .clj file in current jar

(defn local-clj-source
  "Reads a clj source file inside a jar from the current classpath."
  [clj-file]
  (try
    (-> (Thread/currentThread)
        .getContextClassLoader
        (.getResource clj-file)
        .toString
        java.net.URL.
        slurp)
    (catch Exception _ nil)))


; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.collaj
  (:require [clojure.edn :as edn])
  (:import (java.net URLEncoder)))

(defn url-encode
  "URL-encode a string."
  [s]
  (URLEncoder/encode s "UTF-8"))

(defn raw-data
  "Get a clojure data collection of raw search
   results from collaj.net"
  [terms]
  (edn/read-string (slurp (str "http://collaj.net/?format=raw&q="
                               (url-encode terms)))))



; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.settings
  (:import
    (javax.swing JFrame JTabbedPane JLabel
                 JPanel JComboBox Box
                 JTextField JTextArea
                 BoxLayout SpringLayout
                 JButton JCheckBox)
    (java.awt Font GraphicsEnvironment Dimension)
    (java.awt.image BufferedImage)
    (javax.swing.event DocumentListener)
    (java.awt.event ActionListener ItemListener ItemEvent))
  (:require
    [clooj.utils :as utils]))

(def settings (atom nil))

(defn combo-box [items default-item change-fun]
  (doto (JComboBox. (into-array items))
    (.setSelectedItem default-item)
    (.addActionListener
      (reify ActionListener
        (actionPerformed [_ e]
          (change-fun (.. e getSource getSelectedItem)))))))

(defn text-field [default-value change-fun]
  (let [tf (JTextField. (str default-value))]
    (.addDocumentListener
      (.getDocument tf)
      (reify DocumentListener
        (insertUpdate [_ e]
                      (change-fun (.getText tf)))
        (removeUpdate [_ e]
                      (change-fun (.getText tf)))
        (changedUpdate [_ e])))
    tf))

(defn check-box [text checked? change-fun]
  (doto (JCheckBox. text checked?)
    (.addItemListener
      (reify ItemListener
        (itemStateChanged [_ e]
          (change-fun
            (=
              (.getStateChange e)
              ItemEvent/SELECTED)))))))

(defn font-panel []
  (let [graphics-object (delay (.createGraphics
                                 (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB)))
        monospaced? (fn [font]
                      (let [g @graphics-object
                            m (.getFontMetrics g font)]
                        (apply == (map #(.charWidth m %) [\m \n \. \M \-]))))
        get-all-font-names (fn []
                             (.. GraphicsEnvironment
                                 getLocalGraphicsEnvironment
                                 getAvailableFontFamilyNames))
        get-all-fonts-12 (fn []
                           (map #(Font. % Font/PLAIN 12) (get-all-font-names)))
        get-monospaced-font-names (fn []
                                    (map #(.getName %) (filter monospaced? (get-all-fonts-12))))
        get-necessary-fonts (fn []
                              (if (:show-only-monospaced-fonts @settings)
                                (get-monospaced-font-names)
                                (get-all-font-names)))
        example-text-area (doto (JTextArea.
                                  "abcdefghijklmnopqrstuvwxyz 0123456789 (){}[]\nABCDEFGHIJKLMNOPQRSTUVWXYZ +-*/= .,;:!? #&$%@|^")
                            (.setFont (Font. (:font-name @settings) Font/PLAIN (:font-size @settings))))
        example-pane (doto (JPanel. (SpringLayout.))
                       (.add example-text-area))
        font-box (combo-box
                   (get-necessary-fonts)
                   (:font-name @settings)
                   #(do
                      (swap! settings assoc :font-name %)
                      (.setFont
                        example-text-area
                        (Font. % Font/PLAIN (:font-size @settings)))))
        size-box (combo-box
                   (range 5 49)
                   (:font-size @settings)
                   #(do
                      (swap! settings assoc :font-size %)
                      (.setFont
                        example-text-area
                        (Font. (:font-name @settings) Font/PLAIN %))))
        monospaced-check-box (check-box
                               "Show only monospaced fonts"
                               (:show-only-monospaced-fonts @settings)
                               #(do
                                  (swap! settings
                                         assoc :show-only-monospaced-fonts %)
                                  (doto font-box
                                    (.setModel
                                      (.getModel
                                        (JComboBox.
                                          (into-array
                                            (get-necessary-fonts)))))
                                    (.setSelectedItem (:font-name @settings)))))
        controls-pane (JPanel.)
        font-pane (JPanel.)]

    (utils/constrain-to-parent example-text-area :n 20 :w 15 :s -15 :e -15)

    (doto controls-pane
      (.setLayout (BoxLayout. controls-pane BoxLayout/X_AXIS))
      (.add (Box/createRigidArea (Dimension. 20 0)))
      (.add (JLabel. "Font:"))
      (.add (Box/createRigidArea (Dimension. 25 0)))
      (.add font-box)
      (.add (Box/createRigidArea (Dimension. 25 0)))
      (.add (JLabel. "Size:"))
      (.add (Box/createRigidArea (Dimension. 25 0)))
      (.add size-box)
      (.add (Box/createHorizontalGlue)))

    (doto font-pane
      (.setLayout (BoxLayout. font-pane BoxLayout/Y_AXIS))
      (.add controls-pane)
      (.add monospaced-check-box)
      (.add example-pane))))

(defn editor-options-panel []
  (let [options-pane (JPanel.)]
    (doto options-pane
      (.setLayout (BoxLayout. options-pane BoxLayout/Y_AXIS))
      (.add (check-box
              "Wrap lines in source editor"
              (:line-wrap-doc @settings)
              #(swap! settings assoc :line-wrap-doc %)))
      #_(.add (check-box
              "Wrap lines in repl output"
              (:line-wrap-repl-out @settings)
              #(swap! settings assoc :line-wrap-repl-out %)))
      #_(.add (check-box
              "Wrap lines in repl input"
              (:line-wrap-repl-in @settings)
              #(swap! settings assoc :line-wrap-repl-in %))))))

(defmacro tabs [& elements]
  `(doto (JTabbedPane.)
     ~@(map #(list '.addTab (first %) (second %)) elements)))

(defn make-settings-window [app apply-fn]
  (let [bounds (.getBounds (:frame app))
        x (+ (.x bounds) (/ (.width bounds) 2))
        y (+ (.y bounds) (/ (.height bounds) 2))
        settings-frame (JFrame. "Settings")
        button-pane (JPanel.)]

    (doto button-pane
      (.setLayout (BoxLayout. button-pane BoxLayout/X_AXIS))
      (.add (utils/create-button "OK" #(do
                                        (apply-fn app @settings)
                                        (.dispose settings-frame))))
      (.add (utils/create-button "Apply" #(apply-fn app @settings)))
      (.add (utils/create-button "Cancel" #(.dispose settings-frame))))

    (doto settings-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setLayout (BoxLayout. (.getContentPane settings-frame) BoxLayout/Y_AXIS))
      (.setBounds (- x 250) (- y 250) 500 500)

      (.add (tabs
              ["Font" (font-panel)]
              ["Editor options" (editor-options-panel)]))
      (.add (Box/createRigidArea (Dimension. 0 25)))
      (.add button-pane))))


(defn show-settings-window [app apply-fn]
  (reset! settings @(:settings app))
  (.show (make-settings-window app apply-fn)))


; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.highlighting
  (:import (javax.swing.text DefaultHighlighter
                             DefaultHighlighter$DefaultHighlightPainter)
           (java.awt Color)
           (javax.swing.event CaretListener))
  (:require [clooj.utils :as utils]))

(defn highlight
  ([text-comp start stop color]
    (when (and (<= 0 start) (<= stop (.. text-comp getDocument getLength)))
      (.. text-comp getHighlighter
          (addHighlight start stop
                        (DefaultHighlighter$DefaultHighlightPainter. color)))))
  ([text-comp pos color] (highlight text-comp pos (inc pos) color)))

(defn remove-highlight
  ([text-comp highlight-object]
    (when highlight-object
      (.removeHighlight (.getHighlighter text-comp)
                        highlight-object))))

(defn remove-highlights [text-comp highlights]
  (dorun (map #(remove-highlight text-comp %) highlights)))

(def highlights (atom {}))

(defn highlight-brackets [text-comp good-enclosures bad-brackets]
  (utils/awt-event
    (remove-highlights text-comp (get @highlights text-comp))
    (swap! highlights assoc text-comp
           (doall (concat
                    (map #(highlight text-comp % Color/LIGHT_GRAY) good-enclosures)
                    (map #(highlight text-comp % Color/PINK) bad-brackets))))))



; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.navigate
  (:import (org.fife.ui.rsyntaxtextarea RSyntaxTextArea))
  (:require [clooj.utils :as utils]))

(defn get-caret-line-number [comp]
  (.getLineOfOffset comp (.getCaretPosition comp)))

(defn move-to-doc-start [comp]
  (.setCaretPosition comp 0))

(defn move-to-doc-end [comp]
  (.setCaretPosition comp
    (.. comp getDocument getLength)))

(defn move-to-line-start [comp]
  (.setCaretPosition comp
    (.getLineStartOffset comp
      (get-caret-line-number comp))))

(defn move-to-line-end [comp]
  (.setCaretPosition comp
    (let [p (.getLineEndOffset comp
              (get-caret-line-number comp))]
      (if (= p (.. comp getDocument getLength))
        p
        (dec p)))))

(defn attach-navigation-keys [comp]
  (utils/attach-action-keys comp
    ["cmd1 LEFT" #(move-to-line-start comp)]
    ["cmd1 RIGHT" #(move-to-line-end comp)]
    ["cmd1 UP" #(move-to-doc-start comp)]
    ["cmd1 DOWN" #(move-to-doc-end comp)]))

; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.brackets
  (:import (javax.swing.text JTextComponent))
  (:require [clojure.string :as string]
            [clooj.utils :as utils]))

(defn mismatched-brackets [a b]
  (and (or (nil? a) (some #{a} [\( \[ \{]))
       (some #{b} [\) \] \}])
       (not (some #{[a b]} [[\( \)] [\[ \]] [\{ \}]]))))

(defn process-bracket-stack
  "Receiving a bracket stack s, deal with the next character c
   and datum dat."
  [s c dat]
  (let [l (ffirst s)        ;last char
        p (next s)          ;pop stack
        j (conj s [c dat])] ;conj [char dat] to stack
    (condp = l
      \\ p
      \" (condp = c, \" p, \\ j, s)
      \; (if (= c \newline) p s)
      (condp = c
        \" j \\ j \; j ;"
        \( j \[ j \{ j
        \) p \] p \} p
        s))))

(defn find-enclosing-brackets [text pos]
  (let [process #(process-bracket-stack %1 %2 nil)
        reckon-dist (fn [stacks]
                      (let [scores (map count stacks)]
                        (utils/count-while #(<= (first scores) %) scores)))
        before (.substring text 0 (Math/min (.length text) pos))
        stacks-before (reverse (reductions process nil before))
        left (- pos (reckon-dist stacks-before))
        after (.substring text (Math/min (.length text) pos))
        stacks-after (reductions process (first stacks-before) after)
        right (+ -1 pos (reckon-dist stacks-after))]
    [left right]))

(defn find-bad-brackets [text]
  (loop [t text pos 0 stack nil errs nil]
    (let [c (first t)        ;this char
          new-stack (process-bracket-stack stack c pos)
          e (when (mismatched-brackets (ffirst stack) c)
              (list (first stack) [c pos]))
          new-errs (if e (concat errs e) errs)]
        (if (next t)
          (recur (next t) (inc pos) new-stack new-errs)
          (filter identity
                  (map second (concat new-stack errs)))))))

(defn blank-line-matcher [s]
  (re-matcher #"[\n\r]\s*?[\n\r]" s))

(defn find-left-gap [text pos]
  (let [p (min (.length text) (inc pos))
        before-reverse (string/reverse (.substring text 0 p))
        matcher (blank-line-matcher before-reverse)]
    (if (.find matcher)
      (- p (.start matcher))
      0)))

(defn find-right-gap [text pos]
  (let [p (max 0 (dec pos))
        after (.substring text p)
        matcher (blank-line-matcher after) ]
    (if (.find matcher)
      (+ p (.start matcher))
      (.length text))))

(defn find-line-group [text-comp]
  (let [text (utils/get-text-str text-comp)
        pos (.getCaretPosition text-comp)]
    [(find-left-gap text pos) (find-right-gap text pos)]))


; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.indent
  (:require [clooj.utils :as utils]
            [clooj.brackets :as brackets]
            [clojure.string :as string])
  (:import  (javax.swing.text DocumentFilter)))

;(defn t [] (@clooj.core/current-app :doc-text-area))

(def special-tokens
  ["def" "defn" "defmacro" "let" "for" "loop" "doseq" "if" "when"
   "binding" "case" "definline" "defmacro" "condp" "when-let" "if-let" "fn"
   "proxy" "reify" "when-first" "defmethod" "defmulti" "defn-" "defprotocol"
   "defrecord" "defstruct" "deftype" "dotimes" "doto" "extend" "extend-protocol"
   "extend-type" "if-not" "letfn" "ns" "update-proxy" "with-in-str"
   "with-local-vars" "with-out-str"
   "when-let" "when-not" "while" "with-bindings" "with-bindings*"])

(defn first-token [txt]
  (second (re-find #"\((.+?)\s" txt)))

(defn second-token-pos [txt]
  (when-let [x (re-find #".+?\s" (string/trimr (first (.split #"\r?\n" txt))))]
    (.length x)))

(defn left-paren-indent-size [txt]
  (let [token1 (first-token txt)]
    (or
      (when (and token1
                 (not (or (some #{token1} special-tokens)
                          (.startsWith (string/triml token1) "["))))
        (second-token-pos txt))
      2)))

(defn compute-indent-size [text-comp offset]
  (let [bracket-pos (first (brackets/find-enclosing-brackets
                             (utils/get-text-str text-comp) offset))]
    (when (<= 0 bracket-pos)
      (let [bracket (.. text-comp getText (charAt bracket-pos))
            col (:col (utils/get-coords text-comp bracket-pos))]
        (if (= bracket \;)
          (compute-indent-size text-comp bracket-pos)
          (+ col
             (condp = bracket
               \( (left-paren-indent-size (.. text-comp getDocument
                                              (getText bracket-pos
                                                       (- offset bracket-pos))))
               \\ 0  \[ 1  \{ 1  \" 1
               1)))))))

(defn fix-indent [text-comp line]
  (let [start (.getLineStartOffset text-comp line)
        end (.getLineEndOffset text-comp line)
        document (.getDocument text-comp)
        line-text (.getText document start (- end start))
        old-indent-size (count (re-find #"\A\ +" line-text))]
    (when-let [new-indent-size (compute-indent-size text-comp start)]
      (let [delta (- new-indent-size old-indent-size)]
        (if (pos? delta)
          (.insertString document start (apply str (repeat delta " ")) nil)
          (.remove document start (- delta)))))))

(defn fix-indent-selected-lines [text-comp]
  (utils/awt-event
    (dorun (map #(fix-indent text-comp %)
                (utils/get-selected-lines text-comp)))))

(defn auto-indent-str [text-comp offset]
  (let [indent-size (or (compute-indent-size text-comp offset) 0)]
    (apply str "\n" (repeat indent-size " "))))

(defn setup-autoindent [text-comp]
  (utils/attach-action-keys text-comp
    ["cmd1 BACK_SLASH" #(fix-indent-selected-lines text-comp)] ; "cmd1 \"
    ["cmd1 CLOSE_BRACKET" #(utils/indent text-comp)]   ; "cmd1 ]"
    ["cmd1 OPEN_BRACKET" #(utils/unindent text-comp)]) ; "cmd1 ["
  (.. text-comp getDocument
    (setDocumentFilter
      (proxy [DocumentFilter] []
        (replace [fb offset len text attrs]
          (.replace
            fb offset len
            (condp = text
              "\n" (auto-indent-str text-comp offset)
              text)
            attrs))
        (remove [fb offset len]
          (.remove fb offset len))
        (insertString [fb offset string attr]
          (.insertString fb offset string attr))))))


; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.project
  (:import (java.io File)
           (java.awt GridLayout)
           (javax.swing JButton JOptionPane JWindow)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel))
  (:require [clooj.utils :as utils]
            [clojure.java.io :as io]))

;; projects tree

(declare restart-doc)

(def project-set (atom (sorted-set)))

(defn save-project-set []
  (utils/write-value-to-prefs utils/clooj-prefs "project-set" @project-set))

(defn load-project-set []
  (reset! project-set (into (sorted-set)
                            (utils/read-value-from-prefs utils/clooj-prefs "project-set"))))

(defn tree-path-to-file [^TreePath tree-path]
  (when tree-path
    (try (.. tree-path getLastPathComponent getUserObject getAbsolutePath)
         (catch Exception e nil))))

;; loading and saving expanded paths

(defn get-row-path [tree row]
  (tree-path-to-file (. tree getPathForRow row)))

(defn get-expanded-paths [tree]
  (for [i (range (.getRowCount tree)) :when (.isExpanded tree i)]
    (get-row-path tree i)))

(defn save-expanded-paths [tree]
  (utils/write-value-to-prefs utils/clooj-prefs "expanded-paths" (get-expanded-paths tree)))

(defn expand-paths [tree paths]
  (doseq [i (range) :while (< i (.getRowCount tree))]
    (when-let [x (some #{(tree-path-to-file (. tree getPathForRow i))} paths)]
      (.expandPath tree (. tree getPathForRow i)))))

(defn load-expanded-paths [tree]
  (let [paths (utils/read-value-from-prefs utils/clooj-prefs "expanded-paths")]
    (when paths
      (expand-paths tree paths))))

;; loading and saving tree selection

(defn save-tree-selection [tree path]
  (utils/write-value-to-prefs
    utils/clooj-prefs "tree-selection"
    (tree-path-to-file path)))

(defn path-components
  "Generates a sequence of the components in a file path."
  [the-file]
  (->>
    (-> the-file
        io/file
        .getAbsolutePath
        (.split File/separator))
    (remove empty?)
    (remove #(= % "."))))

(defn file-ancestor?
  "In the file tree, returns true if descendant-file
   is a direct descendant of ancestor-file.
   Also returns true if the files are the same."
  [ancestor-file descendant-file]
  (let [ancestor (path-components ancestor-file)
        descendant (path-components descendant-file)]
    (and (every? true? (map = ancestor descendant))
         (<= (count ancestor) (count descendant)))))

(defn node-children [node]
  (when-not (.isLeaf node)
    (for [i (range (.getChildCount node))]
      (.getChildAt node i))))

(defn path-to-node
  "Find the tree node corresponding to a particular file path."
  [tree path]
  (let [root-node (.. tree getModel getRoot)]
    (loop [node root-node]
      (when (and node (not (.isLeaf node)))
        (when-let [children (node-children node)]
                   (let [closer-node (first
                            (filter #(file-ancestor?
                                       (.getUserObject %) path)
                                    children))]
          (when closer-node
            (if (= (io/file path)
                   (.getUserObject closer-node))
              closer-node
              (recur closer-node)))))))))

(defn row-for-path [tree path]
  (first
    (for [i (range 1 (.getRowCount tree))
          :when (= path
                   (-> tree (.getPathForRow i)
                            .getPath last .getUserObject .getAbsolutePath))]
      i)))

(defn set-tree-selection [tree path]
  (utils/awt-event
    (when-let [node (path-to-node tree path)]
      (let [node-path (.getPath node)
            paths (map #(.. % getUserObject getAbsolutePath) (rest node-path))]
        (expand-paths tree paths)
        (when-let [row (row-for-path tree path)]
          (.setSelectionRow tree row))))))

(defn load-tree-selection [tree]
  (let [path (utils/read-value-from-prefs utils/clooj-prefs "tree-selection")]
     (if (nil? path)
       false
       (do
         (set-tree-selection tree path)
         true))))

;;;;;;;;;;;;;;;;;;;

(defn get-code-files [dir suffix]
  (let [dir (io/file dir)]
    (sort (filter #(.endsWith (.getName %) suffix)
                  (file-seq dir)))))

(defn get-temp-file [^File orig]
  (when orig
    (io/file (str (.getAbsolutePath orig) "~"))))

(defn get-projects
  "Load projects from preferences, and return
   a sorted vector."
  []
  (->> (utils/read-value-from-prefs utils/clooj-prefs "project-set")
      set
      (sort-by #(.toLowerCase (.getName (io/file %))))
      vec))

(defn visible-children
  "Get a vector of a directory's children, if there are any.
   Omits hidden and temporary files."
  [file]
  (->> (.listFiles file)
       (remove #(.startsWith (.getName %) "."))
       (remove #(.endsWith (.getName %) "~"))
       vec))

(defn file-name-text
  "Show a file's name, with *stars* if it is the temp file."
  [file]
  (if (.exists (get-temp-file file))
    (str "*" (.getName file) "*")
    (str (.getName file) "    ")))

(defn file-node
  "Tree node representing a file (possibly a directory)."
  [^File file]
  (let [children (delay (visible-children file))]
    (proxy [DefaultMutableTreeNode] [file]
      (getChildAt [i] (file-node (@children i)))
      (getChildCount [] (count @children))
      (toString [] (file-name-text file))
      (isLeaf [] (not (.isDirectory file))))))

(defn root-node
  "The root tree node, given a vector of project locations."
  [projects]
  (proxy [DefaultMutableTreeNode] []
    (getChildAt [i] (file-node (io/file (nth projects i))))
    (getChildCount [] (count projects))
    (toString [] "root")))

(defn file-tree-model [projects]
    (DefaultTreeModel. (root-node projects) false))

(defn update-project-tree [tree]
  (let [model (file-tree-model (vec @project-set))]
    (utils/awt-event
      ;(time (do
      (.setModel tree model)
      (save-project-set)
      (load-expanded-paths tree)
      (load-tree-selection tree)
      (save-expanded-paths tree))))
    ;))

(defn get-selected-file-path [app]
  (when-let [tree-path (-> app :docs-tree .getSelectionPaths first)]
    (-> tree-path .getLastPathComponent .getUserObject .getAbsolutePath)))

(defn get-selected-namespace [tree]
  (-> tree .getSelectionPaths first
      .getLastPathComponent .getUserObject .toString
      (.replace ".clj" "") (.replace "/" ".")))

(defn get-selected-projects [app]
  (let [tree (app :docs-tree)
        selections (.getSelectionPaths tree)]
    (for [selection selections]
      (-> selection .getPath second .getUserObject))))

(defn add-project [app project-path]
  (swap! project-set conj project-path))

(defn rename-project [app]
  (when-let [dir (utils/choose-file (app :frame) "Move/rename project directory" "" false)]
    (let [old-project (first (get-selected-projects app))]
      (if (.renameTo (io/file old-project) dir)
        (do
          (swap! project-set
                 #(-> % (disj old-project) (conj (.getAbsolutePath dir))))
          (update-project-tree (:docs-tree app)))
        (JOptionPane/showMessageDialog nil "Unable to move project.")))))

(defn remove-selected-project [app]
  (apply swap! project-set disj (map #(.getAbsolutePath %)
                                     (get-selected-projects app)))
  (update-project-tree (app :docs-tree)))



; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.repl.main
  (:import (java.io
             BufferedReader BufferedWriter
             InputStreamReader
             File PipedReader PipedWriter PrintWriter Writer
                    StringReader PushbackReader)
           (clojure.lang LineNumberingPushbackReader)
           (java.awt Rectangle)
           (java.net URL URLClassLoader URLDecoder)
           (java.util.concurrent LinkedBlockingQueue))
  (:require #_[clj-inspector.jars :as jars]
            [clojure.string :as string]
            #_[nrepl.core :as nrepl]
            [clojure.java.io :as io]
            [clooj.brackets :as brackets]
            #_[clooj.help :as help]
            [clooj.project :as project]
            #_[clooj.repl.external :as external]
            #_[clooj.repl.lein :as lein]
            [clooj.protocols :as protocols]
            [clooj.utils :as utils]))

#_{:clj-kondo/ignore [:use]}
(use 'clojure.java.javadoc)

(def repl-history {:items (atom nil) :pos (atom 0)})

;; utils

(defn tokens
  "Finds all the tokens in a given string."
  [text]
  (re-seq #"[\w\d/\-\.\?\+\!\*\$\>\<]+" text))

(defn namespaces-from-code
  "Take tokens from text and extract namespace symbols."
  [text]
  (->> text tokens (filter #(.contains % "/"))
       (map #(.split % "/"))
       (map first)
       (map #(when-not (empty? %) (symbol %)))
       (remove nil?)))

(defn is-eof-ex? [throwable]
  (and (instance? clojure.lang.LispReader$ReaderException throwable)
       (or
         (.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
         (.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn get-project-path [app]
  (when-let [repl (:repl app)]
    (-> repl deref :project-path)))

(defn initialize-repl [repl]
  ;;klm
  #_(.evaluate repl
    (str
      "(do"
      (utils/local-clj-source "clooj/cemerick/pomegranate.clj")
      (utils/local-clj-source "clooj/repl/remote.clj")
      "(clooj.repl.remote/repl)"
      ")"
      )))

(defn replace-first [coll x]
  (cons x (next coll)))

(defn update-repl-history [app]
  (swap! (:items repl-history) replace-first
         (utils/get-text-str (app :repl-in-text-area))))

(defn read-string-at [source-text start-line]
  `(let [sr# (java.io.StringReader. (str (apply str (repeat ~start-line "\n"))
                                         ~source-text))
         rdr# (clojure.lang.LineNumberingPushbackReader. sr#)]
     (take-while #(not= % :EOF_REACHED)
                 (repeatedly #(try (read rdr#)
                                   (catch Exception e# :EOF_REACHED))))))

#_(defn cmd-attach-file-and-line [cmd file line classpaths] ;;klm
  (let [read-string-code (read-string-at cmd line)
        short-file (last (.split file "/"))
        namespaces (namespaces-from-code cmd)]
    ;(println namespaces)
    (pr-str
      `(do
         (dorun (map #(try (clooj.cemerick.pomegranate/add-classpath %)
                           (catch Exception e# (println e#))) '~classpaths))
         (dorun (map #(try (require %) (catch Exception _#)) '~namespaces))
         (binding [*source-path* ~short-file
                   *file* ~file]
           (last (map eval ~read-string-code)))))))

(defn print-to-repl
  [app cmd-str silent?]
  nil
  #_(when-let [repl @(app :repl)] ;;klm
    (.evaluate repl
               (if silent?
                      (str "(clooj.repl.remote/silent" cmd-str ")")
                      cmd-str))))

(defn send-to-repl
  ([app cmd silent?] (send-to-repl app cmd "NO_SOURCE_PATH" 0 silent?))
  ([app cmd file line silent?]
    (let [cmd-ln (str cmd \newline)]
      (when-not silent?
        (utils/append-text (app :repl-out-text-area) cmd-ln))
      (print-to-repl app cmd silent?)
      (when-not silent?
        (when (not= cmd (second @(:items repl-history)))
          (swap! (:items repl-history)
                 replace-first cmd)
          (swap! (:items repl-history) conj ""))
        (reset! (:pos repl-history) 0)))))

(defn relative-file [app]
  (let [prefix (str (get-project-path app) File/separator
                    "src"  File/separator)]
    (utils/when-lets [f @(app :file)
                      path (.getAbsolutePath f)]
      (subs path (count prefix)))))

(defn selected-region [ta]
  (if-let [text (.getSelectedText ta)]
    {:text text
     :start (.getSelectionStart ta)
     :end   (.getSelectionEnd ta)}
    (let [[a b] (brackets/find-line-group ta)]
      (when (and a b (< a b))
        {:text (.. ta getDocument (getText a (- b a)))
         :start a
         :end b}))))

(defn send-selected-to-repl [app]
  (let [ta (app :doc-text-area)
        region (selected-region ta)
        txt (:text region)]
    (if-not txt
      (.setText (app :arglist-label) "Malformed expression")
      (let [line (.getLineOfOffset ta (:start region))]
        (send-to-repl app txt (relative-file app) line false)))))

(defn send-doc-to-repl [app]
  (let [text (->> app :doc-text-area .getText)]
    (utils/append-text (app :repl-out-text-area) "Evaluating file...")
    (send-to-repl app text (relative-file app) 0 true)))

(defn make-repl-writer [ta-out]
  (->
    (proxy [Writer] []
      (write
        ([char-array offset length]
          ;(println "char array:" (apply str char-array) (count char-array))
          (utils/append-text ta-out (apply str char-array)))
        ([t]
          ;(println "t:" t)
          (if (= Integer (type t))
            (utils/append-text ta-out (str (char t)))
            (utils/append-text ta-out (apply str t)))))
      (flush [])
      (close [] nil))
    (PrintWriter. true)))

(defn update-repl-in [app]
  (when (pos? (count @(:items repl-history)))
    (.setText (:repl-in-text-area app)
              (nth @(:items repl-history) @(:pos repl-history)))))

(defn show-previous-repl-entry [app]
  (when (zero? @(:pos repl-history))
    (update-repl-history app))
  (swap! (:pos repl-history)
         #(min (dec (count @(:items repl-history))) (inc %)))
  (update-repl-in app))

(defn show-next-repl-entry [app]
  (when (pos? @(:pos repl-history))
    (swap! (:pos repl-history)
           #(Math/max 0 (dec %)))
    (update-repl-in app)))

(defn get-file-ns [app]
  (try
    (when-let [sexpr (read-string (.getText (app :doc-text-area)))]
      (when (= 'ns (first sexpr))
        (str (second sexpr))))
    (catch Exception e)))

(defn start-repl [app project-path]
  nil ;; klm
  #_(let [project-path (if (utils/file-exists? project-path) project-path nil)]
    (utils/append-text (app :repl-out-text-area)
                       (str "\n=== Starting new REPL at " project-path " ===\n"))
    (let [classpath-items ;(lein/lein-classpath-items project-path)
          (external/repl-classpath-items project-path)
          repl ;(lein/lein-repl project-path (app :repl-out-writer))
          (external/repl project-path classpath-items
                         (app :repl-out-writer))
          ]
      (initialize-repl repl)
      (help/update-var-maps! project-path classpath-items)
      (reset! (:repl app) repl))))

(defn stop-repl [app]
  (utils/append-text (app :repl-out-text-area)
                     "\n=== Shutting down REPL ===")
  (when-let [repl @(:repl app)]
    (.close repl)))

(defn apply-namespace-to-repl [app]
  (when-not @(:repl app)
    (start-repl app (first (project/get-selected-projects app))))
  (when-let [current-ns (get-file-ns app)]
    (send-to-repl app (str "(ns " current-ns ")") true)))

(defn restart-repl [app project-path]
  (stop-repl app)
  (start-repl app project-path)
  (apply-namespace-to-repl app))

(defn add-repl-input-handler [app]
  (let [ta-in (app :repl-in-text-area)
        get-caret-pos #(.getCaretPosition ta-in)
        ready #(let [caret-pos (get-caret-pos)
                     txt (.getText ta-in)
                     trim-txt (string/trimr txt)]
                 (and
                   (pos? (.length trim-txt))
                   (<= (.length trim-txt)
                       caret-pos)
                   (= -1 (first (brackets/find-enclosing-brackets
                                  txt
                                  caret-pos)))))
        submit #(when-let [txt (.getText ta-in)]
                  (send-to-repl app txt false)
                  (.setText ta-in ""))
        at-top #(zero? (.getLineOfOffset ta-in (get-caret-pos)))
        at-bottom #(= (.getLineOfOffset ta-in (get-caret-pos))
                      (.getLineOfOffset ta-in (.. ta-in getText length)))
        prev-hist #(show-previous-repl-entry app)
        next-hist #(show-next-repl-entry app)]
    (utils/attach-child-action-keys ta-in ["UP" at-top prev-hist]
                              ["DOWN" at-bottom next-hist]
                              ["ENTER" ready submit])
    (utils/attach-action-keys ta-in ["cmd1 UP" prev-hist]
                        ["cmd1 DOWN" next-hist]
                        ["cmd1 ENTER" submit])))

(defn print-stack-trace [app]
  (send-to-repl app "(when *e (.printStackTrace *e))" true))





; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector)
           (java.lang.reflect Modifier)
           (java.awt Color Point)
           (java.util Vector)
           (javax.swing DefaultListCellRenderer ListSelectionModel)
           (javax.swing.event ListSelectionListener)
           (java.io File))
  (:require [clojure.repl]
            [clojure.string :as string]
            [clooj.collaj :as collaj]
            [clooj.utils :as utils]
            [clooj.brackets :as brackets]
            ;; [cemerick.pomegranate.aether :as aether]
            #_[clj-inspector.jars :as jars]
            #_[clj-inspector.vars :as vars]))

(def var-maps-agent (agent nil))

; from http://clojure.org/special_forms
(def special-forms
  {"def" "(def symbol init?)"
   "if"  "(if test then else?)"
   "do"  "(do exprs*)"
   "let" "(let [bindings* ] exprs*)"
   "quote" "(quote form)"
   "var" "(var symbol)"
   "fn"  "(fn name? [params* ] exprs*)"
   "loop" "(loop [bindings* ] exprs*)"
   "recur" "(recur exprs*)"
   "throw" "(throw expr)"
   "try"   "(try expr* catch-clause* finally-clause?)"
   "catch" "(catch classname name expr*)"
   "monitor-enter" "Avoid!"
   "monitor-exit"  "Avoid!"})

(defn present-item [item]
  (str (:name item) " [" (:ns item) "]"))

(defn make-var-super-map [var-maps]
  (into {}
        (for [var-map var-maps]
          [[(:ns var-map) (:name var-map)] var-map])))

#_(defn classpath-to-jars [project-path classpath]
  (apply concat
    (for [item classpath]
      (cond (.endsWith item "*") (jars/jar-files (apply str (butlast item)))
            (.endsWith item ".jar") (list (File. item))
            :else (jars/jar-files item)))))

#_(defn get-sources-from-jars [project-path classpath]
   (->> (classpath-to-jars project-path classpath)
       (mapcat jars/clj-sources-from-jar)
       merge
       vals))

(defn get-sources-from-clj-files [classpath]
  (map slurp
       (apply concat
              (for [item classpath]
                (let [item-file (File. item)]
                  (when (.isDirectory item-file)
                    (filter #(.endsWith (.getName %) ".clj")
                            (file-seq item-file))))))))

(defn get-var-maps [project-path classpath]
  (make-var-super-map
    []
      #_(mapcat #(vars/analyze-clojure-source "clj" %) ;;klm
              (concat
                (get-sources-from-jars project-path classpath)
                (get-sources-from-clj-files classpath)))))

(defn update-var-maps! [project-path classpath]
  (send-off var-maps-agent #(merge % (get-var-maps project-path classpath))))

(defn find-form-string [text pos]
  (let [[left right] (brackets/find-enclosing-brackets text pos)]
    (when (> (.length text) left)
      (.substring text (inc left)))))

(def non-token-chars [\; \~ \@ \( \) \[ \] \{ \} \  \. \newline \/ \" \'])

(defn local-token-location [text pos]
  (let [n (.length text)
        pos (-> pos (Math/max 0) (Math/min n))]
    [(loop [p (dec pos)]
       (if (or (neg? p)
               (some #{(.charAt text p)} non-token-chars))
         (inc p)
         (recur (dec p))))
     (loop [p pos]
       (if (or (>= p n)
               (some #{(.charAt text p)} non-token-chars))
         p
         (recur (inc p))))]))

(defn head-token [form-string]
  (when form-string
    (second
      (re-find #"(.*?)[\s|\)|$]"
               (str (.trim form-string) " ")))))

(defn current-ns-form [app]
  (-> app :doc-text-area .getText read-string))

(defn ns-available-names [app]
  {}
  #_(vars/parse-ns-form (current-ns-form app))) ;;klm

(defn arglist-from-var-map [m]
  (or
    (when-let [args (:arglists m)]
      (str (-> m :ns) "/" (:name m) ": " args))
    ""))

(defn token-from-caret-pos [text pos]
  (head-token (find-form-string text pos)))

(defn var-from-token [app current-ns token]
  (when token
    (if (.contains token "/")
      (vec (.split token "/"))
      (or ((ns-available-names app) token)
          [current-ns token]))))

(defn arglist-from-token [app ns token]
  (or (special-forms token)
      (when-let [repl (:repl app)]
        (-> @var-maps-agent (get (var-from-token app ns token))
            arglist-from-var-map))))

(defn arglist-from-caret-pos [app ns text pos]
  (let [token (token-from-caret-pos text pos)]
    (arglist-from-token app ns token)))

;; tab help


(defonce help-state (atom {:visible false :token nil :pos nil}))

(defn var-map [v]
  (when-let [m (meta v)]
    (let [ns (:ns m)]
      (-> m
          (select-keys [:doc :ns :name :arglists])
          (assoc :source (binding [*ns* ns]
                           (clojure.repl/source-fn (symbol (str ns "/" name)))))))))

(defn var-help [var-map]
  (let [{:keys [doc ns name arglists source]} var-map]
    (str name
         (if ns (str " [" ns "]") "") "\n"
         arglists
         "\n\n"
         (if doc
           (str "Documentation:\n" doc)
           "No documentation found.")
         "\n\n"
         (if source
           (str "Source:\n"
                (if doc
                  (.replace source doc "...docs...")
                  source))
           "No source found."))))

(defn create-param-list
  ([method-or-constructor static]
    (str " (["
         (let [type-names (map #(.getSimpleName %)
                               (.getParameterTypes method-or-constructor))
               param-names (if static type-names (cons "this" type-names))]
           (apply str (interpose " " param-names)))
         "])"))
  ([method-or-constructor]
    (create-param-list method-or-constructor true)))

(defn constructor-help [constructor]
  (str (.. constructor getDeclaringClass getSimpleName) "."
       (create-param-list constructor)))

(defn method-help [method]
  (let [stat (Modifier/isStatic (.getModifiers method))]
    (str
      (if stat
        (str (.. method getDeclaringClass getSimpleName)
             "/" (.getName method))
        (str "." (.getName method)))
     (create-param-list method stat)
      " --> " (.getName (.getReturnType method)))))

(defn field-help [field]
  (let [c (.. field getDeclaringClass getSimpleName)]
  (str
    (if (Modifier/isStatic (.getModifiers field))
      (str (.. field getDeclaringClass getSimpleName)
           "/" (.getName field)
           (when (Modifier/isFinal (.getModifiers field))
             (str " --> " (.. field (get nil) toString))))
      (str "." (.getName field) " --> " (.getName (.getType field)))))))

(defn class-help [c]
  (apply str
         (concat
           [(present-item c) "\n  java class"]
           ["\n\nCONSTRUCTORS\n"]
           (interpose "\n"
                      (sort
                        (for [constructor (.getConstructors c)]
                          (constructor-help constructor))))
           ["\n\nMETHODS\n"]
           (interpose "\n"
                      (sort
                        (for [method (.getMethods c)]
                          (method-help method))))
           ["\n\nFIELDS\n"]
           (interpose "\n"
                      (sort
                        (for [field (.getFields c)]
                          (field-help field)))))))

(defn item-help [item]
  (cond (map? item) (var-help item)
        (class? item) (class-help item)))

(defn set-first-component [split-pane comp]
  (let [loc (.getDividerLocation split-pane)]
    (.setTopComponent split-pane comp)
    (.setDividerLocation split-pane loc)))

(defn clock-num [i n]
  (if (zero? n)
    0
    (cond (< i 0) (dec n)
          (>= i n) 0
          :else i)))

(defn list-size [list]
  (-> list .getModel .getSize))

(defn match-items [pattern items]
  (->> items
    (filter #(re-find pattern (:name %)))
    (sort-by #(.toLowerCase (:name %)))))

(defn hits [token]
  (let [token-pat1 (re-pattern (str "(?i)\\A\\Q" token "\\E"))
        token-pat2 (re-pattern (str "(?i)\\A.\\Q" token "\\E"))
        items (vals @var-maps-agent)
        best (match-items token-pat1 items)
        others (match-items token-pat2 items)
        ;collaj-items (or (try (collaj/raw-data token) (catch Throwable _)))
        ]
    (concat best others #_collaj-items)))

(defn show-completion-list [{:keys [completion-list
                                    repl-split-pane
                                    help-text-scroll-pane
                                    doc-split-pane
                                    completion-panel
                                    repl-label]:as app}]
    (when (pos? (list-size completion-list))
      (set-first-component repl-split-pane help-text-scroll-pane)
      (set-first-component doc-split-pane completion-panel)
      (.setText repl-label "Documentation")
      (.ensureIndexIsVisible completion-list
                             (.getSelectedIndex completion-list))))

(defn advance-help-list [app token index-change-fn]
  (let [help-list (app :completion-list)]
    (if (not= token (@help-state :token))
      (do
        (swap! help-state assoc :token token)
        (.setListData help-list (Vector. (hits token)))
        (.setSelectedIndex help-list 0))
      (let [n (list-size help-list)]
        (when (pos? n)
          (.setSelectedIndex help-list
                             (clock-num
                               (index-change-fn
                                    (.getSelectedIndex help-list))
                               n))))))
  (show-completion-list app))

(defn get-list-item [app]
  (-> app :completion-list .getSelectedValue))

(defn get-list-artifact [app]
  (when-let [artifact (:artifact (get-list-item app))]
    (binding [*read-eval* false]
      (read-string artifact))))

(defn get-list-token [app]
  (let [val (get-list-item app)]
    (str (:ns val) "/" (:name val))))

(defn show-help-text [app choice]
  (let [help-text (or (when choice (item-help choice)) "")]
    (.setText (app :help-text-area) help-text))
  (-> app :help-text-scroll-pane .getViewport
      (.setViewPosition (Point. (int 0) (int 0)))))

(defn show-tab-help [app text-comp index-change-fn]
  (utils/awt-event
    (let [text (utils/get-text-str text-comp)
          pos (.getCaretPosition text-comp)
          [start stop] (local-token-location text pos)]
      (when-let [token (.substring text start stop)]
        (swap! help-state assoc :pos start :visible true)
        (advance-help-list app token index-change-fn)))))

(defn hide-tab-help [app]
  (utils/awt-event
    (when (@help-state :visible)
      (set-first-component (app :repl-split-pane)
                           (app :repl-out-scroll-pane))
      (set-first-component (app :doc-split-pane)
                           (app :docs-tree-panel))
      (.setText (app :repl-label) "Clojure REPL output"))
    (swap! help-state assoc :visible false :pos nil)))

(defn help-handle-caret-move [app text-comp]
  (utils/awt-event
    (when (@help-state :visible)
      (let [[start _] (local-token-location (utils/get-text-str text-comp)
                                            (.getCaretPosition text-comp))]
        (if-not (= start (@help-state :pos))
          (hide-tab-help app)
          (show-tab-help app text-comp identity))))))

(defn update-ns-form [app]
  (current-ns-form app))

(defn add-classpath-to-repl
  [app files]
  (.addAll (app :classpath-queue) files))

(defn load-dependencies [app artifact]
  (utils/awt-event (utils/append-text (app :repl-out-text-area)
               (str "\nLoading " artifact " ... (not implemented) "))) ;; klm
  #_(let [deps (cemerick.pomegranate.aether/resolve-dependencies  ;;klm
               :coordinates [artifact]
               :repositories
                 (merge aether/maven-central
                        {"clojars" "http://clojars.org/repo"}))]
    (add-classpath-to-repl app (aether/dependency-files deps)))
  (utils/append-text (app :repl-out-text-area)
                     (str "not done in this fork of clooj."))) ;;klm

(defn update-token [app text-comp new-token]
  (utils/awt-event
    (let [[start stop] (local-token-location
                         (utils/get-text-str text-comp)
                         (.getCaretPosition text-comp))
          len (- stop start)]
      (when (and (seq new-token) (-> app :completion-list .getModel .getSize pos?))
        (.. text-comp getDocument
            (replace start len new-token nil))))))

(defn setup-tab-help [text-comp app]
  (utils/attach-action-keys text-comp
    ["TAB" #(show-tab-help app text-comp inc)]
    ["shift TAB" #(show-tab-help app text-comp dec)]
    ["ESCAPE" #(hide-tab-help app)])
  (utils/attach-child-action-keys text-comp
    ["ENTER" #(@help-state :visible)
             #(do (hide-tab-help app)
                  (.start (Thread. (fn [] (load-dependencies app (get-list-artifact app)))))
                  (update-token app text-comp (get-list-token app)))]))

(defn find-focused-text-pane [app]
  (let [t1 (app :doc-text-area)
        t2 (app :repl-in-text-area)]
    (cond (.hasFocus t1) t1
          (.hasFocus t2) t2)))

(defn setup-completion-list [l app]
  (doto l
    (.setBackground (Color. 0xFF 0xFF 0xE8))
    (.setFocusable false)
    (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
    (.setCellRenderer
      (proxy [DefaultListCellRenderer] []
        (getListCellRendererComponent [list item index isSelected cellHasFocus]
          (doto (proxy-super getListCellRendererComponent list item index isSelected cellHasFocus)
            (.setText (present-item item))))))
    (.addListSelectionListener
      (reify ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (.ensureIndexIsVisible l (.getSelectedIndex l))
            (show-help-text app (.getSelectedValue l))))))
    (utils/on-click 2 #(when-let [text-pane (find-focused-text-pane app)]
                        (update-token app text-pane (get-list-token app))))))


; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.search
  (:import (java.awt Color)
           (java.util.regex Pattern Matcher))
  (:require [clooj.highlighting :as highlighting]
            [clooj.utils :as utils]))

(defn configure-search [match-case use-regex]
  (bit-or Pattern/CANON_EQ
          Pattern/UNICODE_CASE
          (if match-case 0 Pattern/CASE_INSENSITIVE)
          (if use-regex 0 Pattern/LITERAL)))

(defn find-all-in-string
  [s t match-case use-regex]
  (try
    (when (pos? (.length t))
      (let [p (Pattern/compile t (configure-search match-case use-regex))
            m (re-matcher p s)]
        (loop [positions []]
          (if (.find m)
            (recur (conj positions [(.start m) (.end m)] ) )
            positions))))
    (catch Exception _ [])))

(defn highlight-found [text-comp posns]
    (doall
      (map #(highlighting/highlight text-comp (first %) (second %) Color/YELLOW)
        posns)))

(defn next-item [cur-pos posns]
  (or (first (drop-while #(> cur-pos (first %)) posns)) (first posns)))

(defn prev-item [cur-pos posns]
  (or (first (drop-while #(< cur-pos (first %)) (reverse posns))) (last posns)))

(def search-highlights (atom nil))

(def current-pos (atom 0))

(defn update-find-highlight [sta app back]
  (let [dta (:doc-text-area app)
        match-case (.isSelected (:search-match-case-checkbox app))
        use-regex (.isSelected (:search-regex-checkbox app))
        posns (find-all-in-string (utils/get-text-str dta)
                                  (utils/get-text-str sta)
                                  match-case
                                  use-regex)]
    (highlighting/remove-highlights dta @search-highlights)
    (if (pos? (count posns))
      (let [selected-pos
             (if back (prev-item (dec @current-pos) posns)
                      (next-item @current-pos posns))
            posns (remove #(= selected-pos %) posns)
            pos-start (first selected-pos)
            pos-end (second selected-pos)]
        (.setBackground sta Color/WHITE)
        (doto dta
          (.setSelectionStart pos-end)
          (.setSelectionEnd pos-end))
        (reset! current-pos pos-start)
        (reset! search-highlights
                (conj (highlight-found dta posns)
                      (highlighting/highlight dta pos-start
                                              pos-end (.getSelectionColor dta))))
        (utils/scroll-to-pos dta pos-start))
      (.setBackground sta  Color/PINK))))

(defn start-find [app]
  (let [sta (:search-text-area app)
        case-checkbox (:search-match-case-checkbox app)
        regex-checkbox (:search-regex-checkbox app)
        close-button (:search-close-button app)
        arg (:arglist-label app)
        dta (:doc-text-area app)
        sel-text (.getSelectedText dta)]
    (.setVisible arg false)
    (doto sta
      (.setVisible true)
      (.requestFocus)
      (.selectAll))
    (.setVisible case-checkbox true)
    (.setVisible regex-checkbox true)
    (.setVisible close-button true)
    (when (seq sel-text)
      (.setText sta sel-text))))

(defn stop-find [app]
  (let [sta (app :search-text-area)
        dta (app :doc-text-area)
        case-checkbox (:search-match-case-checkbox app)
        regex-checkbox (:search-regex-checkbox app)
        close-button (:search-close-button app)
        arg (app :arglist-label)]
    (.setVisible arg true)
    (.setVisible sta false)
    (.setVisible case-checkbox false)
    (.setVisible regex-checkbox false)
    (.setVisible close-button false)
    (highlighting/remove-highlights dta @search-highlights)
    (reset! search-highlights nil)
    (reset! current-pos 0)))

(defn escape-find [app]
  (stop-find app)
  (.requestFocus (:doc-text-area app)))

(defn highlight-step [app back]
  (let [doc-text-area (:doc-text-area app)
        search-text-area (:search-text-area app)]
    (start-find app)
    (when-not back
      (swap! current-pos inc))
    (update-find-highlight search-text-area app back)))



; Copyright (c) 2011-2013, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.core
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout
                        JTextPane JCheckBox JButton
                        ListSelectionModel
                        UIManager)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Rectangle Window)
           (java.awt.event AWTEventListener FocusAdapter
                           MouseAdapter WindowAdapter
                           ActionListener KeyAdapter)
           (java.awt AWTEvent Color Font GridLayout Toolkit)
           (java.net URL)
           (java.util.concurrent LinkedBlockingQueue)
           (java.util Map)
           (java.io File FileReader StringReader
                    BufferedWriter OutputStreamWriter FileOutputStream)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants
                                        TokenMakerFactory)
           (org.fife.ui.rtextarea RTextScrollPane))
  (:require [clojure.set]
            [clooj.repl.main :as repl]
            #_[clooj.repl.output :as repl-output]
            [clooj.utils :as utils]
            [clooj.help :as help]
            [clooj.navigate :as navigate]
            [clooj.project :as project]
            [clooj.indent :as indent]
            [clooj.brackets :as brackets]
            [clooj.highlighting :as highlighting]
            [clooj.search :as search]
            [clooj.settings :as settings])
  (:gen-class
   :methods [^{:static true} [show [] void]]))

(def gap 5)

(def embedded (atom false))

(def changing-file (atom false))

(defprotocol DynamicWordHighlighter
  (addWordToHighlight [this word token-type]))

(extend-type RSyntaxTextArea
  DynamicWordHighlighter
  (addWordToHighlight [word token-type]))

(defn make-rsyntax-text-area []
  (let [tmf (TokenMakerFactory/getDefaultInstance)
        token-maker (.getTokenMaker tmf "text/clojure")
        token-map (.getWordsToHighlight token-maker)
        rsta (proxy [RSyntaxTextArea] []
               (addWordToHighlight [word token-type]
                                   (do
                                     (.put token-map word token-type)
                                     token-type)))]
      (.. rsta getDocument (setTokenMakerFactory tmf))
    rsta))

(defn make-text-area [wrap]
  (doto (RSyntaxTextArea.)
    (.setAnimateBracketMatching false)
    (.setBracketMatchingEnabled false)
    (.setAutoIndentEnabled false)
    (.setAntiAliasingEnabled true)
    (.setLineWrap wrap)
    ))

(def get-clooj-version
  (memoize
    (fn []
      (try
        (-> (Thread/currentThread) .getContextClassLoader
            (.getResource "clooj/core.class") .toString
            (.replace "clooj/core.class" "project.clj")
            URL. slurp read-string (nth 2))
        (catch Exception _ nil)))))

;; settings

(def default-settings
  (merge
    (zipmap [:font-name :font-size]
            (cond (utils/is-mac) ["Monaco" 11]
                  (utils/is-win) ["Courier New" 12]
                  :else    ["Monospaced" 12]))
  {:line-wrap-doc false
   :line-wrap-repl-out false
   :line-wrap-repl-in false
   :show-only-monospaced-fonts true
   }))

(defn load-settings []
  (atom
    (merge default-settings
           (utils/read-value-from-prefs utils/clooj-prefs "settings"))))

(defn save-settings [settings]
  (utils/write-value-to-prefs
    utils/clooj-prefs
    "settings"
    settings))

(defn apply-settings [app settings]
  (letfn [(set-line-wrapping [text-area mode]
            (.setLineWrap text-area mode))
          (set-font
            [app font-name size]
            (let [f (Font. font-name Font/PLAIN size)]
              (utils/awt-event
                (dorun (map #(.setFont (app %) f)
                            [:doc-text-area :repl-in-text-area
                             :repl-out-text-area :arglist-label
                             :search-text-area :help-text-area
                             :completion-list])))))]

    (set-line-wrapping
      (:doc-text-area app)
      (:line-wrap-doc settings))
    (set-line-wrapping
      (:repl-in-text-area app)
      (:line-wrap-repl-in settings))
    (set-line-wrapping
      (:repl-out-text-area app)
      (:line-wrap-repl-out settings))

    (set-font app
              (:font-name settings)
              (:font-size settings)))
  (reset! (:settings app) settings)
  (save-settings settings))

;; font

(defn resize-font [app fun]
  (apply-settings app (update-in @(:settings app)
                                 [:font-size]
                                 fun)))

(defn grow-font [app] (resize-font app inc))

(defn shrink-font [app] (resize-font app dec))


;; caret finding

(def highlight-agent (agent nil))

(def arglist-agent (agent nil))

(def caret-position (atom nil))

(defn save-caret-position [app]
  (utils/when-lets [text-area (app :doc-text-area)
                    pos (get @caret-position text-area)
                    file @(:file app)]
    (when-not (.isDirectory file)
      (let [key-str (str "caret_" (.hashCode (.getAbsolutePath file)))]
        (utils/write-value-to-prefs utils/clooj-prefs key-str pos)))))

(defn load-caret-position [app]
  (utils/when-lets [text-area (app :doc-text-area)
              file @(:file app)]
    (when-not (.isDirectory file)
      (utils/when-lets [key-str (str "caret_" (.hashCode (.getAbsolutePath file)))
                  pos (utils/read-value-from-prefs utils/clooj-prefs key-str)]
        (let [length (.. text-area getDocument getLength)
              pos2 (Math/min pos length)]
          (.setCaretPosition text-area pos2)
          (utils/scroll-to-caret text-area))))))

(defn update-caret-position [text-comp]
  (swap! caret-position assoc text-comp (.getCaretPosition text-comp)))

(defn display-caret-position [doc-text-area app]
  (let [{:keys [row col]} (utils/get-caret-coords doc-text-area)]
    (.setText (:pos-label app) (str " " (inc row) "|" (inc col)))))

(defn handle-caret-move [app text-comp ns]
  (update-caret-position text-comp)
  (help/help-handle-caret-move app text-comp)
  (let [text (utils/get-text-str text-comp)]
    (send-off highlight-agent
              (fn [old-pos]
                (try
                  (let [pos (@caret-position text-comp)]
                    (when-not (= pos old-pos)
                      (let [enclosing-brackets (brackets/find-enclosing-brackets text pos)
                            bad-brackets (brackets/find-bad-brackets text)
                            good-enclosures (clojure.set/difference
                                              (set enclosing-brackets) (set bad-brackets))]
                        (utils/awt-event
                          (highlighting/highlight-brackets text-comp good-enclosures bad-brackets)))))
                  (catch Throwable t (utils/awt-event (.printStackTrace t))))))
    (when ns
      (send-off arglist-agent
                (fn [old-pos]
                  (try
                    (let [pos (@caret-position text-comp)]
                      (when-not (= pos old-pos)
                        (let [arglist-text
                              (help/arglist-from-caret-pos app ns text pos)]
                          (utils/awt-event (.setText (:arglist-label app) arglist-text)))))
                    (catch Throwable t (utils/awt-event (.printStackTrace t)))))))))

;; highlighting

(defn activate-caret-highlighter [app]
  (when-let [text-comp (app :doc-text-area)]
    (let [f #(handle-caret-move app % (repl/get-file-ns app))]
      (utils/add-caret-listener text-comp f)
      (utils/add-text-change-listener text-comp f)))
  (when-let [text-comp (app :repl-in-text-area)]
    (let [f #(handle-caret-move app % (repl/get-file-ns app))]
      (utils/add-caret-listener text-comp f)
      (utils/add-text-change-listener text-comp f))))

;; double-click paren to select form

(defn double-click-selector [text-comp]
  (.addMouseListener text-comp
    (proxy [MouseAdapter] []
      (mouseClicked [e]
        (when (== 2 (.getClickCount e))
          (utils/when-lets [pos (.viewToModel text-comp (.getPoint e))
                            c (.. text-comp getDocument (getText pos 1) (charAt 0))
                            pos (cond (#{\( \[ \{ \"} c) (inc pos)
                                      (#{\) \] \} \"} c) pos)
                            [a b] (brackets/find-enclosing-brackets (utils/get-text-str text-comp) pos)]
            (utils/set-selection text-comp a (inc b))))))))

;; temp files

(defn dump-temp-doc [app orig-f txt]
  (try
    (when orig-f
      (let [orig (.getAbsolutePath orig-f)
            f (.getAbsolutePath (project/get-temp-file orig-f))]
        (spit f txt)
        (utils/awt-event (.repaint (app :docs-tree)))
        ))
       (catch Exception e nil)))

(def temp-file-manager (agent 0))

(defn update-temp [app]
  (let [text-comp (app :doc-text-area)
        txt (utils/get-text-str text-comp)
        f @(app :file)]
    (send-off temp-file-manager
              (fn [old-pos]
                (try
                  (when-let [pos (get @caret-position text-comp)]
                    (when-not (= old-pos pos)
                      (dump-temp-doc app f txt))
                    pos)
                     (catch Throwable t (utils/awt-event (.printStackTrace t))))))))

(defn setup-temp-writer [app]
  (let [text-comp (:doc-text-area app)]
    (utils/add-text-change-listener text-comp
      #(when-not @changing-file
         (update-caret-position %)
         (update-temp app)))))

(declare restart-doc)

(defn file-suffix [^File f]
  (utils/when-lets [name (.getName f)
             last-dot (.lastIndexOf name ".")
             suffix (.substring name (inc last-dot))]
    suffix))

(defn text-file? [f]
  (not (some #{(file-suffix f)}
             ["jar" "class" "dll" "jpg" "png" "bmp"])))

(defn setup-tree [app]
  (let [tree (:docs-tree app)
        save #(project/save-expanded-paths tree)]
    (doto tree
      (.setRootVisible false)
      (.setShowsRootHandles true)
      (.. getSelectionModel (setSelectionMode TreeSelectionModel/SINGLE_TREE_SELECTION))
      (.addTreeExpansionListener
        (reify TreeExpansionListener
          (treeCollapsed [this e] (save))
          (treeExpanded [this e] (save))))
      (.addTreeSelectionListener
        (reify TreeSelectionListener
          (valueChanged [this e]
            (utils/awt-event
              (project/save-tree-selection tree (.getNewLeadSelectionPath e))
              (let [f (.. e getPath getLastPathComponent
                            getUserObject)]
                (when (and
                        (not= f @(app :file))
                        (text-file? f))
                  (restart-doc app f))))))))))

;; build gui

(defn make-scroll-pane [text-area]
  (RTextScrollPane. text-area))

(defn setup-search-elements [app]
  (.setVisible (:search-match-case-checkbox app) false)
  (.setVisible (:search-regex-checkbox app) false)
  (doto (:search-close-button app)
    (.setVisible false)
    (.setBorder nil)
    (.addActionListener
      (reify ActionListener
        (actionPerformed [_ _] (search/stop-find app)))))
  (let [sta (doto (app :search-text-area)
      (.setVisible false)
      (.setBorder (BorderFactory/createLineBorder Color/DARK_GRAY)))]
    (utils/add-text-change-listener sta #(search/update-find-highlight % app false))
    (utils/attach-action-keys sta ["ENTER" #(search/highlight-step app false)]
                            ["shift ENTER" #(search/highlight-step app true)]
                            ["ESCAPE" #(search/escape-find app)])))

(defn create-arglist-label []
  (doto (JLabel.)
    (.setVisible true)
    ))

(defn exit-if-closed [^java.awt.Window f app]
  (when-not @embedded
    (.addWindowListener f
      (proxy [WindowAdapter] []
        (windowClosing [_]
          (save-caret-position app)
          (System/exit 0))))))

(def no-project-txt
    "\n Welcome to clooj, a lightweight IDE for clojure\n
     vital hint for Clojure coding:
     double-click a paren within your code, then cut&paste
     double-click a paren selects the form (as they call it)

     now you can either
     a. create a new file
            (select File > New...), or
     b. open an existing file
            (click on it in the tree at left).")

(def no-file-txt
    "To edit source code you need to either: <br>
     &nbsp;1. create a new file
     (select menu <b>File > New...</b>)<br>
     &nbsp;2. edit an existing file by selecting one at left.</html>")

(defn move-caret-to-line
  "Move caret to choosen line"
  [textarea]
  (let [current-line (fn []
                       (inc (.getLineOfOffset textarea (.getCaretPosition textarea))))
        line-str (utils/ask-value "Line number:" "Go to Line")
        line-num  (Integer.
                    (if (or (nil? line-str) (nil? (re-find #"\d+" line-str)))
                      (current-line)
                      (re-find #"\d+" line-str)))]
  (utils/scroll-to-line textarea line-num)
  (.requestFocus textarea)))

(defn open-project [app]
  (when-let [dir (utils/choose-directory (app :f) "Choose a project directory")]
    (let [project-dir (if (= (.getName dir) "src") (.getParentFile dir) dir)]
      (utils/write-value-to-prefs utils/clooj-prefs "last-open-dir" (.getAbsolutePath (.getParentFile project-dir)))
      (project/add-project app (.getAbsolutePath project-dir))
      (project/update-project-tree (:docs-tree app))
      (when-let [clj-file (or (-> (File. project-dir "src")
                                 .getAbsolutePath
                                 (project/get-code-files ".clj")
                                 first)
                              project-dir)]
        (utils/awt-event (project/set-tree-selection (app :docs-tree) (.getAbsolutePath clj-file)))))))

(defn attach-global-action-keys [comp app]
  (utils/attach-action-keys comp
    ["cmd1 EQUALS" #(grow-font app)]
    ["cmd1 shift EQUALS" #(grow-font app)]
    ["cmd1 PLUS" #(grow-font app)]
    ["cmd2 MINUS" #(.toBack (:frame app))]
    ["cmd2 PLUS" #(.toFront (:frame app))]
    ["cmd2 EQUALS" #(.toFront (:frame app))]
    ["cmd1 shift O" #(open-project app)]
    ["cmd1 K"#(.setText (app :repl-out-text-area) "")]))

(defn on-window-activation [win fun]
  (.addWindowListener win
    (proxy [WindowAdapter] []
      (windowActivated [_]
        (fun)))))

(defn new-doc-text-area [app]
  (doto (make-text-area false)
    navigate/attach-navigation-keys
    double-click-selector
    (utils/add-caret-listener #(display-caret-position % app))
    (help/setup-tab-help app)
    indent/setup-autoindent
    ))

(defn create-app []
  (let [doc-text-panel (JPanel.)
        doc-label (JLabel. "Source Editor")
        repl-out-text-area (make-text-area false)
        repl-out-scroll-pane nil #_(repl-output/tailing-scroll-pane repl-out-text-area)
        repl-out-writer (repl/make-repl-writer repl-out-text-area)
        repl-in-text-area (make-text-area false)
        help-text-area (make-text-area true)
        help-text-scroll-pane (JScrollPane. help-text-area)
        completion-panel (JPanel.)
        completion-label (JLabel. "Name search")
        completion-list (JList.)
        completion-scroll-pane (JScrollPane. completion-list)
        search-text-area (JTextField.)
        search-match-case-checkbox (JCheckBox. "Match case")
        search-regex-checkbox (JCheckBox. "Regex")
        search-close-button (JButton. "X")
        arglist-label (create-arglist-label)
        pos-label (JLabel.)
        frame (JFrame.)
        cp (.getContentPane frame)
        layout (SpringLayout.)
        docs-tree (JTree.)
        docs-tree-scroll-pane (JScrollPane. docs-tree)
        docs-tree-panel (JPanel.)
        docs-tree-label (JLabel. "Projects")
        doc-split-pane (utils/make-split-pane
                         docs-tree-panel
                         doc-text-panel true gap 0.25)
        repl-split-pane (utils/make-split-pane
                          repl-out-scroll-pane
                          (make-scroll-pane repl-in-text-area) false gap 0.75)
        repl-panel (JPanel.)
        repl-label (JLabel. "Clojure REPL output")
        repl-input-label (JLabel. "Clojure REPL input \u2191")
        split-pane doc-split-pane #_(utils/make-split-pane doc-split-pane repl-panel true gap 0.5)
        app (merge {:file (atom nil)
                    :repl (atom nil)
                    :var-maps (atom nil)
                    :classpath-queue (LinkedBlockingQueue.)
                    :changed false}
                   (utils/gen-map
                     doc-label
                     repl-out-text-area
                     repl-in-text-area
                     repl-label
                     frame
                     help-text-area
                     help-text-scroll-pane
                     repl-out-scroll-pane
                     docs-tree
                     docs-tree-scroll-pane
                     docs-tree-panel
                     docs-tree-label
                     search-text-area
                     search-match-case-checkbox
                     search-regex-checkbox
                     search-close-button
                     pos-label
                     repl-out-writer
                     doc-split-pane
                     repl-split-pane
                     split-pane
                     arglist-label
                     completion-list
                     completion-scroll-pane
                     completion-panel
                     ))
        doc-text-area (new-doc-text-area app)
        doc-scroll-pane (make-scroll-pane doc-text-area)
        app (assoc app :doc-text-area doc-text-area)
        app (assoc app :settings (load-settings))]
    (doto frame
      (.setBounds 25 50 950 700)
      (.setLayout layout)
      (.add split-pane)
      (.setTitle (str "clooj " (get-clooj-version))))
    (doto doc-text-panel
      (.setLayout (SpringLayout.))
      (.add doc-scroll-pane)
      (.add doc-label)
      (.add pos-label)
      (.add search-text-area)
      (.add arglist-label)
      (.add search-match-case-checkbox)
      (.add search-regex-checkbox)
      (.add search-close-button))
    (doto docs-tree-panel
      (.setLayout (SpringLayout.))
      (.add docs-tree-label)
      (.add docs-tree-scroll-pane))
    (doto repl-panel
      (.setLayout (SpringLayout.))
      (.add repl-label)
      (.add repl-input-label)
      (.add repl-split-pane))
    (doto completion-panel
      (.setLayout (SpringLayout.))
      (.add completion-label)
      (.add completion-scroll-pane))
    #_(utils/constrain-to-parent completion-label :n 0 :w 0 :n 15 :e 0)
    #_(utils/constrain-to-parent completion-scroll-pane :n 16 :w 0 :s 0 :e 0)
    #_(utils/constrain-to-parent repl-label :n 0 :w 0 :n 15 :e 0)
    #_(utils/constrain-to-parent repl-input-label :s -15 :w 0 :s 0 :e 0)
    #_(utils/constrain-to-parent repl-split-pane :n 16 :w 0 :s -16 :e 0)
    (utils/constrain-to-parent docs-tree-label :n 0 :w 0 :n 15 :e 0)
    (utils/constrain-to-parent docs-tree-scroll-pane :n 16 :w 0 :s 0 :e 0)
    #_(help/setup-completion-list completion-list app)
    (doto pos-label
      (.setFont (Font. "Courier" Font/PLAIN 13)))
    (doto repl-in-text-area
      double-click-selector
      navigate/attach-navigation-keys)
    (.setSyntaxEditingStyle repl-in-text-area
                            SyntaxConstants/SYNTAX_STYLE_CLOJURE)
    (.setSyntaxEditingStyle repl-out-text-area
                            SyntaxConstants/SYNTAX_STYLE_CLOJURE)
    (.setModel docs-tree (DefaultTreeModel. nil))
    (utils/constrain-to-parent split-pane :n gap :w gap :s (- gap) :e (- gap))
    (utils/constrain-to-parent doc-label :n 0 :w 0 :n 15 :e 0)
    (utils/constrain-to-parent doc-scroll-pane :n 16 :w 0 :s -16 :e 0)
    (utils/constrain-to-parent pos-label :s -14 :w 0 :s 0 :w 100)
    (utils/constrain-to-parent search-text-area :s -15 :w 100 :s 0 :w 350)
    (utils/constrain-to-parent search-match-case-checkbox :s -15 :w 355 :s 0 :w 470)
    (utils/constrain-to-parent search-regex-checkbox :s -15 :w 475 :s 0 :w 550)
    (utils/constrain-to-parent search-close-button :s -15 :w 65 :s 0 :w 95)
    (utils/constrain-to-parent arglist-label :s -14 :w 80 :s -1 :e -10)
    (.layoutContainer layout frame)
    (exit-if-closed frame app)
    (setup-search-elements app)
    (activate-caret-highlighter app)
    (setup-temp-writer app)
    (utils/attach-action-keys doc-text-area
      ["cmd1 ENTER" #(repl/send-selected-to-repl app)])
    (doto repl-out-text-area (.setEditable false))
    (doto help-text-area (.setEditable false)
                         (.setBackground (Color. 0xFF 0xFF 0xE8)))
    (indent/setup-autoindent repl-in-text-area)

    (dorun (map #(attach-global-action-keys % app)
                [docs-tree doc-text-area repl-in-text-area repl-out-text-area (.getContentPane frame)]))
    app))

;; clooj docs

(defn restart-doc [app ^File file]
  (let [f @(:file app)
        txt (utils/get-text-str (:doc-text-area app))]
    (send-off temp-file-manager
              (let [temp-file (project/get-temp-file f)]
                (fn [_] (when (and f temp-file (.exists temp-file))
                          (dump-temp-doc app f txt))
                  0))))
  (await temp-file-manager)
  (let [frame (app :frame)
        text-area (app :doc-text-area)
        temp-file (project/get-temp-file file)
        file-to-open (if (and temp-file (.exists temp-file)) temp-file file)
        doc-label (app :doc-label)]
    ;(utils/remove-text-change-listeners text-area)
    (reset! changing-file true)
    (save-caret-position app)
    (.. text-area getHighlighter removeAllHighlights)
    (if (and file-to-open (.exists file-to-open) (.isFile file-to-open))
      (do (let [txt (slurp file-to-open)
                rdr (StringReader. txt)]
            (.read text-area rdr nil))
          (.discardAllEdits text-area)
          (.setText doc-label (str "Source Editor \u2014 " (.getPath file)))
          (.setEditable text-area true)
          (.setSyntaxEditingStyle text-area
            (let [file-name (.getName file-to-open)]
              (if (or (.endsWith file-name ".clj")
                      (.endsWith file-name ".clj~"))
                SyntaxConstants/SYNTAX_STYLE_CLOJURE
                SyntaxConstants/SYNTAX_STYLE_NONE))))
      (do (.setText text-area no-project-txt)
          (.setText doc-label (str "Source Editor (No file selected)"))
          (.setEditable text-area false)))

    (indent/setup-autoindent text-area)
    (reset! (app :file) file)
    (load-caret-position app)
    (update-caret-position text-area)
    (repl/apply-namespace-to-repl app)
    (reset! changing-file false)))

(defn save-file [app]
  (try
    (let [f @(app :file)
          ft (File. (str (.getAbsolutePath f) "~"))]
      (with-open [writer (BufferedWriter.
                           (OutputStreamWriter.
                             (FileOutputStream. f)
                             "UTF-8"))]
        (.write (app :doc-text-area) writer))
      (send-off temp-file-manager (fn [_] 0))
      (.delete ft)
      (.repaint (app :docs-tree))
      )
    (catch Exception e (JOptionPane/showMessageDialog
                         nil "Unable to save file."
                         "Oops" JOptionPane/ERROR_MESSAGE))))

(def project-clj-text (.trim
"
(defproject PROJECTNAME \"1.0.0-SNAPSHOT\"
  :description \"FIXME: write description\"
  :dependencies [[org.clojure/clojure \"1.5.1\"]])
"))

(defn specify-source [project-dir title default-namespace]
  (when-let [namespace (JOptionPane/showInputDialog
                         nil
                         (str "Enter a name ending with .clj \n
Directory of the new file is the top directory \n" project-dir)
                         title
                         JOptionPane/QUESTION_MESSAGE
                         nil
                         nil
                         default-namespace)]
    (let [tokens    (map munge (.split namespace "\\."))
          dirs      (cons "src" (butlast tokens))
          dirstring (apply str (interpose File/separator dirs))
          name      (last tokens)
          the-dir   (File. project-dir dirstring)
          len       (count namespace)]
      #_(.mkdirs the-dir) ;;klm
      #_[(File. the-dir (str name ".clj")) namespace] ;; klm
      (if (and (> len 4) (= (subs namespace (- len 4)) ".clj"))
        [(File. project-dir namespace)
         (subs namespace 0 (- len 4))]
        (do
          (JOptionPane/showMessageDialog
            nil "Nothing done. Filename needs to end with .clj")
          [nil nil])))))

(defn create-file [app project-dir default-namespace]
  (let [[file namespace] (specify-source project-dir
                                         "Create a source file"
                                         default-namespace)]
    (when file
      (let [tree (:docs-tree app)]
        (spit file (str "(ns " namespace ")\n"))
        (project/update-project-tree (:docs-tree app))
        (project/set-tree-selection tree (.getAbsolutePath file))))))

(defn new-project-clj [app project-dir]
  (let [project-name (.getName project-dir)
        file-text (.replace project-clj-text "PROJECTNAME" project-name)]
    (spit (File. project-dir "project.clj") file-text)))

(defn new-project [app]
  (try
    (when-let [dir (utils/choose-file (app :frame) "Create a project directory" "" false)]
      (utils/awt-event
        (let [path (.getAbsolutePath dir)]
          (.mkdirs (File. dir "src"))
          (new-project-clj app dir)
          (project/add-project app path)
          (project/update-project-tree (:docs-tree app))
          (project/set-tree-selection (app :docs-tree) path)
          (create-file app dir (str (.getName dir) ".core")))))
      (catch Exception e (do (JOptionPane/showMessageDialog nil
                               "Unable to create project."
                               "Oops" JOptionPane/ERROR_MESSAGE)
                           (.printStackTrace e)))))

(defn rename-file [app]
  (when-let [old-file @(app :file)]
    (let [tree (app :docs-tree)
          [file namespace] (specify-source
                             (first (project/get-selected-projects app))
                             "Rename a source file"
                             (repl/get-file-ns app))]
      (when file
        (.renameTo @(app :file) file)
        (project/update-project-tree (:docs-tree app))
        (utils/awt-event (project/set-tree-selection tree (.getAbsolutePath file)))))))

(defn delete-file [app]
  (let [path (project/get-selected-file-path app)]
    (when (utils/confirmed? "Are you sure you want to delete this file?\nDeleting cannot be undone." path)
      (loop [f (File. path)]
        (when (and (empty? (.listFiles f))
                   (let [p (-> f .getParentFile .getAbsolutePath)]
                     (or (.contains p (str File/separator "src" File/separator))
                         (.endsWith p (str File/separator "src")))))
          (.delete f)
          (recur (.getParentFile f))))
      (project/update-project-tree (app :docs-tree)))))

(defn remove-project [app]
  (when (utils/confirmed? "Remove the project from list? (No files will be deleted.)"
                    "Remove project")
    (project/remove-selected-project app)))

(defn revert-file [app]
  (when-let [f @(:file app)]
    (let [temp-file (project/get-temp-file f)]
      (when (.exists temp-file)
        (let [path (.getAbsolutePath f)]
          (when (utils/confirmed? "Revert the file? This cannot be undone." path)
            (.delete temp-file)
            (project/update-project-tree (:docs-tree app))
            (restart-doc app f)))))))

(defn- dir-rank [dir]
  (get {"src" 0 "test" 1 "lib" 2} (.getName dir) 100))

(defn- find-file [project-path relative-file-path]
  (let [classpath-dirs (sort-by dir-rank < (utils/get-directories (File. project-path)))
        file-candidates (map
                          #(File. (str (.getAbsolutePath %) File/separatorChar relative-file-path))
                          classpath-dirs)]
    (first (filter #(and (.exists %) (.isFile %)) file-candidates))))

(defn goto-definition [ns app]
  (let [text-comp (:doc-text-area app)
        pos (.getCaretPosition text-comp)
        text (.getText text-comp)
        src-file (:file (meta (do (help/token-from-caret-pos text pos) nil)))
        line (:line (meta (do (find-ns (symbol ns))
                              (help/token-from-caret-pos text pos) nil)))
        project-path (first (project/get-selected-projects app))
        file (find-file project-path src-file)]
    (when (and file line)
      (when (not= file @(:file app))
        (restart-doc app file)
        (project/set-tree-selection (:docs-tree app) (.getAbsolutePath file)))
      (utils/scroll-to-line text-comp line))))

(defn make-menus [app]
  (when (utils/is-mac)
    (System/setProperty "apple.laf.useScreenMenuBar" "true"))
  (let [menu-bar (JMenuBar.)]
    (. (app :frame) setJMenuBar menu-bar)
    (let [file-menu
          (utils/add-menu
            menu-bar
            "File" "F"
            ["Open Directory..." "O" "cmd1 shift O" #(open-project app)]
            ["New..." "N" "cmd1 N"
             #(cond
                (empty? @project/project-set)
                (JOptionPane/showMessageDialog
                  nil "Open a directory first \n (select the File > Open... menu)")

                (not (first (project/get-selected-projects app)))
                (JOptionPane/showMessageDialog
                  nil "Click on your directory in the \"Projects\" pane
(so that it's blue)
and get a vital Clojure coding hint!")

                :else
                (create-file app (first (project/get-selected-projects app)) "")
                )]
            ["Save" "S" "cmd1 S" #(save-file app)]
            #_["Move/Rename" "M" nil #(rename-file app)]
            ["Revert" "R" nil #(revert-file app)]
            #_["Delete" nil nil #(delete-file app)])]
      (when-not (utils/is-mac)
        (utils/add-menu-item file-menu "Exit" "X" nil #(System/exit 0))))
    (utils/add-menu menu-bar "Project" "P"
      #_["New..." "N" "cmd1 shift N" #(new-project app)] ;;klm
      #_["Open Directory..." "O" "cmd1 shift O" #(open-project app)]
      #_["Move/Rename" "M" nil #(project/rename-project app)] ;;klm
      ["Remove from pane" nil nil #(remove-project app)])
    (utils/add-menu menu-bar "Source" "U"
      ["Comment" "C" "cmd1 SEMICOLON" #(utils/toggle-comment (:doc-text-area app))]
      ["Fix indentation" "F" "cmd1 BACK_SLASH" #(indent/fix-indent-selected-lines (:doc-text-area app))]
      ["Indent lines" "I" "cmd1 CLOSE_BRACKET" #(utils/indent (:doc-text-area app))]
      ["Unindent lines" "D" "cmd1 OPEN_BRACKET" #(utils/unindent (:doc-text-area app))]
      #_["Name search/docs" "S" "TAB" #(help/show-tab-help app (help/find-focused-text-pane app) inc)]
      ["Go to line..." "G" "cmd1 L" #(move-caret-to-line (:doc-text-area app))]
      ["Editor hints ..." "H" nil
       #(JOptionPane/showMessageDialog
          nil "Select expression: double-click paren \n
Undo: Cmd+z or Ctrl + z\nRedo: Cmd+Shift+Z\nEnd of File: Cmd+CursorDown\n
You actually can get a block commented with double ;;
-- select a  block already commented by singe ; and
-- make sure that the curser is in the empty line below

https://github.com/bobbylight/RSyntaxTextArea/wiki/Keyboard-Shortcut-List")]
      ;["Go to definition" "G" "cmd1 D" #(goto-definition (repl/get-file-ns app) app)]
      )
    #_(utils/add-menu menu-bar "REPL" "R"
      ["Evaluate here" "E" "cmd1 ENTER" #(repl/send-selected-to-repl app)]
      ["Evaluate entire file" "F" "cmd1 E" #(repl/send-doc-to-repl app)]
      ["Apply file ns" "A" "cmd1 shift A" #(repl/apply-namespace-to-repl app)]
      ["Clear output" "C" "cmd1 K" #(.setText (app :repl-out-text-area) "")]
      ["Restart" "R" "cmd1 R" #(repl/restart-repl app
                            (first (project/get-selected-projects app)))]
      ["Print stack trace for last error" "T" "cmd1 T" #(repl/print-stack-trace app)])
    (utils/add-menu menu-bar "Search" "S"
      ["Find" "F" "cmd1 F" #(search/start-find app)]
      ["Find next" "N" "cmd1 G" #(search/highlight-step app false)]
      ["Find prev" "P" "cmd1 shift G" #(search/highlight-step app true)])
    (utils/add-menu menu-bar "Window" "W"
      #_["Go to REPL input" "R" "cmd1 3" #(.requestFocusInWindow (:repl-in-text-area app))]
      ["Go to Editor" "E" "cmd1 2" #(.requestFocusInWindow (:doc-text-area app))]
      ["Go to Project Tree" "P" "cmd1 1" #(.requestFocusInWindow (:docs-tree app))]
      ["Increase font size" nil "cmd1 PLUS" #(grow-font app)]
      ["Decrease font size" nil "cmd1 MINUS" #(shrink-font app)]
      ["Settings" nil nil #(settings/show-settings-window
                             app apply-settings)])))


(defn add-visibility-shortcut [app]
  (let [shortcuts [(map utils/get-keystroke ["cmd2 EQUALS" "cmd2 PLUS"])]]
    (.. Toolkit getDefaultToolkit
      (addAWTEventListener
        (proxy [AWTEventListener] []
          (eventDispatched [e]
            (when (some #{(KeyStroke/getKeyStrokeForEvent e)}
                     shortcuts)
              (.toFront (:frame app)))))
        AWTEvent/KEY_EVENT_MASK))))

;; startup

(defonce current-app (atom nil))

(defn startup []
  (Thread/setDefaultUncaughtExceptionHandler
    (proxy [Thread$UncaughtExceptionHandler] []
      (uncaughtException [thread exception]
                       (println thread) (.printStackTrace exception))))
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [app (create-app)]
    (reset! current-app app)
    (make-menus app)
    (add-visibility-shortcut app)
    #_(repl/add-repl-input-handler app) ;; klm
    #_(help/setup-tab-help (app :repl-in-text-area) app)
    (doall (map #(project/add-project app %) (project/load-project-set)))
    (let [frame (app :frame)]
      (utils/persist-window-shape utils/clooj-prefs "main-window" frame)
      (.setVisible frame true)
      (on-window-activation frame #(project/update-project-tree (app :docs-tree))))
    (setup-temp-writer app)
    (setup-tree app)
    (let [tree (app :docs-tree)]
      (project/load-expanded-paths tree)
      (when (false? (project/load-tree-selection tree))
        #_(repl/start-repl app nil) nil)) ;;klm
    (apply-settings app @(:settings app))))

(defn -show []
  (reset! embedded true)
  (if (not @current-app)
    (startup)
    (.setVisible (:frame @current-app) true)))

(defn -main [& args]
  (reset! embedded false)
  (startup))

;; testing

(defn get-text []
  (utils/get-text-str (@current-app :doc-text-area)))

; not working yet:
;(defn restart
;   "Restart the application"
;   []
;  (.setVisible (@current-app :frame) false)
;  (startup))


(ns fegloj.main
  (:require [clooj.core :as cc]
            [clooj.project :as project]
            #_[scicloj.clay.v2.api :as clay]
            [clojure.main :as cm]
            [clojure.java.io :as io])
  (:gen-class))

(defn default-notebook []
  ;; create default notebook if it doesn't exist
  (let [notebooks-dir (io/file "notebooks")
        default-notebook (io/file notebooks-dir "my_notebook.clj")]
    (.mkdirs notebooks-dir)
    (when-not (.exists default-notebook)
      (io/make-parents default-notebook)
      (spit default-notebook "(ns my-notebook)

;; # My Notebook

;; A clay notebook for exploring Clojure

(+ 1 2 3)"))))

(defn open-notebook []
    ;; automatically open current directory as a project
  (let [app @cc/current-app
        project-dir (io/file ".")
        abs-path (.getAbsolutePath project-dir)
        default-notebook (io/file "notebooks" "my_notebook.clj")]
    (project/add-project app abs-path)
    (project/update-project-tree (:docs-tree app))
    (when-let [clj-file (or (-> (io/file project-dir "src")
                                .getAbsolutePath
                                (project/get-code-files ".clj")
                                first)
                            project-dir)]
      (project/set-tree-selection (:docs-tree app) (.getAbsolutePath clj-file)))
    ;; open the default notebook in the editor
    #_(when (.exists default-notebook)
      (cc/restart-doc app default-notebook))))

(defn -main []
  #_(default-notebook)
  #_(clay/make! {:live-reload      true
                 :source-path      "my_notebook.clj"
                 :base-source-path "notebooks"
                 :base-target-path "temp"})
  (cc/-show)
  (open-notebook)
  (println "REPL started")
  (cm/repl))


