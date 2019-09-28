#lang br
(require jsonic/parser jsonic/tokenizer brag/support)

(parse-to-datum
 (apply-tokenizer-maker make-tokenizer
                        "// line comment \n"))
