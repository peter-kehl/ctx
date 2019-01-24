(ns subcontext.tags-and-macros)

(defn reader-tag-from-macro [ent] (with-meta {:tag-from-macro-processed true} ent))
(set! *data-readers* {'test/from-macro reader-tag-from-macro})

; Following tag #test/from-macro gets parsed before the macro is run
;(defmacro macro-to-tag [ent] `(str #test/from-macro ~ent))
;(meta (macro-to-tag {}))

; https://clojuredocs.org/clojure.core/*read-eval* also accepts :unknown.
; It controls the eval reader (#=) and record/type literal syntax in read-string:
; From https://clojuredocs.org/clojure.core/read-string
; - skip comments: (read-string "; foo\n5")
; - caller's namespace - similar to *ns* ? : (read-string "::whatever-namespace-you-are-in")
; ---- generate from a macro
; - convert binary number: (read-string (str "2r" "1011"))

;(defn -main [])
