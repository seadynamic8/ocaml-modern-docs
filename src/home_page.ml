open Tea
open Html

let home_page _model =
  [ div
      [ id "home" ]
      [ main [ class' "content" ] 
          [ h1 [ class' "title" ] [ text "OCaml Standard Library - Modernized - Unofficial" ]
          ; p
              []
              [ text "This is an unoffical version of the OCaml Standard Library docs
                with a modernized theme.  It should (hopefully) be the same content as the
                original just looks nicer and easier to find things.  If you are looking
                for official website or docs, look under the logo on the sidebar." ]
          ; p
              []
              [ text "I couldn't stand the way the official docs looked, so I decided to
                create my own styled version of it, maybe it can help others too." ]
          ; p
              []
              [ text "It is open source.  The code I used to create this was put here at
                Github: "
              ; a [ href "https://github.com/seadynamic8/ocaml-modern-docs" ]
                  [ text "https://github.com/seadynamic8/ocaml-modern-docs" ] 
              ]
          ; ul
              []
              [ li
                  []
                  [ text "It's an example of using the following libraries: lambdasoup,
                  atdgen (from OCaml opam libraries) and bs-json, bucklescript-tea (from
                  the Bucklescript npm libraries)"]
              ; li
                  []
                  [ text "I learned a lot from doing this and while a bit frustrating
                  because the original docs don't parse easily with not much structure
                  and things in the wrong places, it was fun.  Also, the divergence of
                  the two families (opam vs npm) wasn't fun either, but I got around it." ]
              ; li
                  []
                  [ text "If you want to use this, its a work in progress.  But let me
                  know if you have any major problems or suggestions.  Though I'm not
                  going to be devoting much time to it probably." ]
              ; li
                  []
                  [ text "It was inspired by Elixir docs."]
              ; li
                  []
                  [ text "I still have some more planned features." ]
              ; li
                  []
                  [ text "Since I'm still learning OCaml, bear with me on the code, it
                    definitely needs some refactoring and could be done better." ]
              ]
          ; p [] [ text "<-------- Go checkout the docs!" ]
          ]
      ; footer 
          [ class' "footer" ]
          [ a [ href "#license" ] [ text "Original License" ] 
          ; p [] [ text "Star Cache, LLC" ]
          ]
      ]
  ]