(ns ctx)

;(def tagged-scope-stamp (list `tagged-scope-stamp-token))
;(defn tagged-scope-name? [tagged] (and (symbol? (first tagged)) (= (rest tagged tagged-scope-stamp))))
#_(defn tag-scope-name [name] {:pre [(symbol? name)] :post [(tagged-scope-name? %)]}
  ;TODO start new collector
  (cons name tagged-scope-stamp))

; TODO private? or run in user's scope?
;(defmacro env [] )

; TODO can it be private, even though it's injected in the user's scope?
#_(defmacro env-and-pass "Capture local symbols (function parameters, from (let [...]) and similar." [variable any-value]
  (set! variable &env) `(-> ~any-value))

(defmacro env "Local symbols, quoted, in a seq, if any. Otherwise nil." []
  (println &env) (if &env `(list '~@(keys &env)) '(-> nil)))

;(def ^:dynamic scope-at-bottom false) ;different to empty scope, because then &env is nil
;(def ^:dynamic scope-at-top false)

; This will be run by the parser before macro expansion of its enclosing (scope ...).
; generate a macro call and capture &env. Not private, so that it works from data_readers.clj.
; If the scope contains several bound symbols with the same name (every deeper one shadowing the outer one), then this captures only the leaf binding. That's even if you apply #ctx/<< on any of the higher symbols. #ctx/<< only registers the symbol name, but not its level.
;
;
  (defn scope-bottom "Capture a scope bottom. Don't call directly. See level()." [level-call]
  ("TODO" list `env-and-pass '`scope-at-bottom level-call))

  (defmacro level " Use within (scope ...) to indicate the bottom of the scope. Prefix it with #ctx/bottom. Hence call it as: #ctx/bottom (ctx/level)." []
  )

;; Obsolete
;; (ctx/scope #ctx/scope MyTopScope (...))
;; (#ctx/scope #scope/namespace/MyTopScope (...)) ;<< default tagged reader
;; (#scope/namespace/MyTopScope (...))
; TODO Run at the top level only?
; TODO inherit & multiple inheritance
(defmacro scope "Declare a new scope." [scope-name & form]
  {:pre [;(tagged-scope-name? scope-name "Prefix the scope name with #ctx/scope.")
         (symbol? scope-name) (not (false? scope-at-bottom))]}
  (list `env-and-pass '`scope-at-top nil)
  "TODO"
  (set! scope-at-bottom false) (set! scope-at-top false))

(set! *data-readers* (assoc *data-readers*
      'ctx/bottom scope-bottom))
