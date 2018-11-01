open Tea
open Html


let license _model =
	[ p [] 
    [ text "The OCaml system is copyright &#XA9; 1996&#X2013;2013
      Institut National de Recherche en Informatique et en
      Automatique (INRIA).
      INRIA holds all ownership rights to the OCaml system." ]
  ; p [] 
    [ text "The OCaml system is open source and can be freely
      redistributed. See the file <span class=\"c003\">LICENSE</span> in the distribution for
      licensing information." ]
  ; p [] 
    [ text "The present documentation is copyright &#XA9; 2013
      Institut National de Recherche en Informatique et en
      Automatique (INRIA). The OCaml documentation and user&#X2019;s
      manual may be reproduced and distributed in whole or
      in part, subject to the following conditions:" ]
  ; ul
      []
      [ li
          []
          [ text " The copyright notice above and this permission notice must be
            preserved complete on all complete or partial copies." ]
      ; li
          []
          [ text "Any translation or derivative work of the OCaml
            documentation and user&#X2019;s manual must be approved by the authors in
            writing before distribution." ]
      ; li
          []
          [ text "If you distribute the OCaml
            documentation and user&#X2019;s manual in part, instructions for obtaining
            the complete version of this manual must be included, and a
            means for obtaining a complete version provided." ]
      ; li
          []
          [ text "Small portions may be reproduced as illustrations for reviews or
            quotes in other works without this permission notice if proper
            citation is given." ]
      ]
    ]