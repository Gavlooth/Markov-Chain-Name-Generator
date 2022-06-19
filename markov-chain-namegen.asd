(defsystem "markov-chain-namegen"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria :uiop :iterate)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "markov chain name generator in common lisp")

