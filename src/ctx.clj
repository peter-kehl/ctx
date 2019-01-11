(ns ctx)

;(def tagged-scope-stamp (list `tagged-scope-stamp-token))
;(defn tagged-scope-name? [tagged] (and (symbol? (first tagged)) (= (rest tagged tagged-scope-stamp))))
#_(defn tag-scope-name [name] {:pre [(symbol? name)] :post [(tagged-scope-name? %)]}
  ;TODO start new collector
  (cons name tagged-scope-stamp))

; TODO can it be private, even though it's injected in the user's scope?
(defmacro env "Local symbols, quoted, in a seq, if any. Otherwise nil." []
  (println &env) (if &env `(list '~@(keys &env)) '(-> nil)))

(def ^:dynamic ^:private collected-symbols false) ;Map: user's symbol->`++ or `--. Assoc fails for false. Don't initialise to nil, because that proceeds silently, hiding an error.
(def ^:private ++ `++) (def ^:private -- `--)
(defn ^:private collect-symbol [action-sym user-sym]
  {:pre[(symbol? action-sym) (symbol? user-sym)]}
  (when-not (collected-symbols user-sym) ;We collect from the bottom up. If already collected, that deeper occurrence hides any upper symbols with the same name.
              (set! collected-symbols (assoc collected-symbols user-sym action-sym))))
(def reader-++ (partial collect-symbol ++))
(def reader--- (partial collect-symbol --))

(def ^:private reader-bottom-token ::reader-bottom-token)
(def ^:private reader-start-token ::reader-start-token)
; This will be run by the parser before macro expansion of its enclosing (scope ...).
; It resets the symbol collector. Not private, so that it works from data_readers.clj.
; If the Clojure language scope contains several bound symbols with the same name (any deeper one shadowing the outer one), then this captures only the leaf binding. That's even if you apply #ctx/++ on any of the higher symbols. #ctx/++ only registers the symbol name, but not its level. (It could register the symbol binding's unique identifier (hashcode), but at the bottom level it can't access values of the higher symbols anyway.)
; Alternative naming #ctx/end (ctx/scope), which would involve 0 arity of (scope), could
; be confusing: It would be an end of the recorded lexical scope if the innermost bound symbols come from (let[...]). However, if the innermost bound symbols come from (letfn[...]), then this #ctx/end (ctx/scope) wouldn't be at the end of the recorded lexical scope, because then the available lexical scope includes any further functions defined in that (letfn[...]).
(defn reader-bottom "Capture a scope bottom. Don't call directly. See level()." [level-call]
  {:pre [(list? level-call) (= (first level-call) `level)
    (false? collected-symbols)]}
  (set! collected-symbols {})
  (list `level reader-bottom-token))

(defmacro level "Use within (scope ...) to indicate the bottom of the scope. Prefix it with #ctx/bottom. Hence call it as: #ctx/bottom (ctx/level). You must have exactly one #ctx/bottom (ctx/level) per one #ctx/start (ctx/scope...). Don't pass any token parameter, it gets added behind the scenes."
  [reader-token]
  {:pre[(= reader-token reader-bottom-token)]}
  )

(defn reader-start "Capture a scope start. Don't call directly. See scope()." [scope-call]
  {:pre [(list? scope-call) (= (first scope-call) `scope)
    collected-symbols]}
)
;; Obsolete
;; (ctx/scope #ctx/scope MyTopScope (...))
;; (#ctx/scope #scope/namespace/MyTopScope (...)) ;<< default tagged reader
;; (#scope/namespace/MyTopScope (...))
; #ctx
; TODO Run at the top level only?
; TODO inherit & multiple inheritance. "Child" scopes contain all symbols from all their parents. Any later parent shadows the same symbols from earlier parents.
(defmacro scope "Declare (an outer boundary of) a new scope. Prefix it with #ctx/start. Hence call it as: #ctx/start (ctx/scope ...). (ctx/scope...) on its own doesn't return the captured scope. Common practice is to have (let[...]) or (letfn[...]) inside (ctx/scope), and return #ctx/bottom (ctx/level). (Don't pass any token parameter, it gets added behind the scenes."
[reader-token scope-name & form]
  {:pre [;(tagged-scope-name? scope-name "Prefix the scope name with #ctx/scope.")
         (= reader-token reader-start-token)
         (symbol? scope-name) collected-symbols]}
  (list `env-and-pass '`scope-at-top nil)
  "TODO"
  (set! collected-symbols false))

(set! *data-readers* (assoc *data-readers*
      'ctx/bottom reader-bottom
      'ctx/start reader-start
      'ctx/++ reader-++
      'ctx/-- reader---))
