((haskell-mode .
  ((haskell-process-type . ghci)
   (haskell-process-path-ghci . "ghci-ng")
   (haskell-process-wrapper-function
    . (lambda (args)
        (append
         (list "fpbuild"
               "--docker-run-args=[\"--interactive=true\",\"--tty=false\"]"
               "exec"
               "--")
         args))))))
