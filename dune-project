(lang dune 2.8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  This file is part of fontforge-of-ocaml library                           ;;
;;                                                                            ;;
;;  Copyright (C) 2017-2022, Patrick BAUDIN                                   ;;
;;                           (https://github.com/pbaudin/fontforge-of-ocaml)  ;;
;;                                                                            ;;
;;  you can redistribute it and/or modify it under the terms of the GNU       ;;
;;  Lesser General Public License as published by the Free Software           ;;
;;  Foundation, version 2.1.                                                  ;;
;;                                                                            ;;
;;  It is distributed in the hope that it will be useful, but WITHOUT ANY     ;;
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or         ;;
;;  FITNESS FOR A PARTICULAR PURPOSE.                                         ;;
;;                                                                            ;;
;;  See the GNU Lesser General Public License version 2.1                     ;;
;;  for more details (enclosed in the file licenses/LGPLv2.1)                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(generate_opam_files true)

(name fontforge-of-ocaml)

(package
  (name fontforge-of-ocaml)
  (depends
    (ocaml (>= 4.09.0))
    (base (>= v0.12.2))
    (fmt (>= 0.8.9 ))
    (pyml (>= 20200518 ))
    (ppx_python (>= v0.12.0))
  )
  (depopts
     conf-python-3 ; for users of the library
     headache      ; for developpers only
  )
  (conflicts
    (headache (< 1.05))
  )
  (tags ("font" "fontforge" "binding")
  )
)
