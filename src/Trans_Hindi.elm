module Trans_Hindi exposing (transl)

import Dict
import Set exposing (Set)

selectFull: Set String
selectFull = Set.fromList [" ","a","â","i","î","u","û","e","ê","o","ô"]

vowel: Set String
vowel = Set.fromList ["a","â","i","î","u","û","e","ê","o","ô","r'"]

-- vowel rendering:
-- if preceding char is in selectFull, use full vowel
-- else use matra vowel

latFull : Dict.Dict String String
latFull = Dict.fromList[("a" , "अ")
                       ,("â" , "आ")
                       ,("i" , "इ")
                       ,("î" , "ई")
                       ,("u" , "उ")
                       ,("û" , "ऊ")
                       ,("e" , "ए")
                       ,("ê" , "ऐ")
                       ,("o" , "ओ")
                       ,("ô" , "औ")
                       ,("r'" , "ऋ")
                       ]

latMatra : Dict.Dict String String
latMatra = Dict.fromList[("a" , "")
                        ,("â" , "\u{093E}")
                        ,("i" , "\u{093F}")
                        ,("î" , "\u{0940}")
                        ,("u" , "\u{0941}")
                        ,("û" , "\u{0942}")
                        ,("e" , "\u{0947}")
                        ,("ê" , "\u{0948}")
                        ,("o" , "\u{094B}")
                        ,("ô" , "\u{094C}")
                        ,("r'" , "\u{0943}")
                        ]

latHindi : Dict.Dict String String
latHindi = Dict.fromList [("k" , "\u{0915}") ,("k'" , "\u{0916}")
                         ,("g" , "\u{0917}"), ("g'" , "\u{0918}"), ("G" , "\u{0919}")
                         ,("q" , "\u{0958}"), ("x" , "\u{0959}"), ("g\"" , "\u{095A}")
                         ,("c" , "\u{091A}"), ("c'" , "\u{091B}")
                         ,("j" , "\u{091C}"), ("j'" , "\u{091D}"), ("n'" , "\u{091E}")
                         ,("z" , "\u{095B}")
                         ,("T" , "\u{091F}"), ("T'", "\u{0920}")
                         ,("D" , "\u{0921}"), ("D'" , "\u{0922}"), ("N" , "\u{0923}")
                         ,("R" , "\u{095C}"), ("R'", "\u{095D}")
                         ,("t" , "\u{0924}"), ("t'" , "\u{0925}")
                         ,("d" , "\u{0926}"), ("d'" , "\u{0927}"), ("n" , "\u{0928}")
                         ,("p" , "\u{092A}"), ("p'" , "\u{092B}")
                         ,("b" , "\u{092C}"), ("b'" , "\u{092D}"), ("m" , "\u{092E}")
                         ,("f" , "\u{095E}")
                         ,("y" , "\u{092F}"), ("r" , "\u{0930}"), ("l" , "\u{0932}"),("v","\u{0935}")
                         ,("s'" ,"\u{0936}"), ("s\"","\u{0937}"), ("s" , "\u{0938}"),("h","\u{0939}")
                         ,("(" , "\u{0901}"),("('" , "\u{0902}") -- candrabindu, anusvara
                         ,("-" , "\u{094D}")  -- virama
                         ,("0","\u{0966}"),("1","\u{0967}"),("2","\u{0968}"),("3","\u{0969}"),("4","\u{096A}")
                         ,("5","\u{096B}"),("6","\u{096C}"),("7","\u{096D}"),("8","\u{096E}"),("9","\u{096F}")
                         , ("." , "\u{0964}") -- purna viram = danda
                         , (".'" , "\u{0970}")
                         , ("_" , "") -- filter _
                         ]

latHindi_ : Dict.Dict String String -- characters to be preceded by _ (underscore)
latHindi_= Dict.fromList  [("g" , "\u{095A}") -- use rather g" , see above

                          ]

diacritics : List String
diacritics = ["'", "\""]

subst : String -> (Dict.Dict String String) -> String -- substitute one char (or char + diacritics) on the basis of dictionary
subst car dict =
  Maybe.withDefault car (Dict.get car dict) -- if car is in dict, substitute, else keep car

subst_ : (String,String) -> String -- select dictionary on the basis of previous char : _ or not _, and substitute char
subst_ dble =
  let
     (carac, sub) = dble
  in
    if sub == "_" then subst carac latHindi_ else
      if Set.member carac vowel then
        if Set.member sub selectFull then subst carac latFull
        else subst carac latMatra
    else subst carac latHindi

szip : List String -> List (String,String) -- zip s with a right shift of itself
szip s =
    List.map2 Tuple.pair s (" " :: s)

foldp : List String -> List String -> List String -- concatenate letters with their diacritics, if any
foldp acc list =
  case list of
    [] ->
      acc
    x::xs ->
      case xs of
        [] ->
          x::acc
        y::ys ->
          if List.member y diacritics then -- 1 diacritic
            case ys of
              [] ->
                (x++y)::acc
              _ ->
                foldp ((x++y)::acc) ys
          else
            foldp (x::acc) xs

trich : String -> String -- sort diacritics, if more than 1 present
trich s =
  if String.length s < 3 then s -- 0 or 1 diacritic, no need to sort
  else
    let
      h = String.slice 0 1 s -- head character
      b = String.slice 1 5 s -- b contains the diacritics, which will be sorted according to Unicode value
    in
      h ++ (b |> String.toList |> List.map String.fromChar |> List.sort |> List.foldr (++) "")

transl : String -> String
transl chaine =
    chaine
    |> String.toList
    |> List.map String.fromChar
    |> foldp []
    |> List.reverse
    |> List.map trich
    |> szip
    |> List.map subst_
    |> List.foldr (++) ""
