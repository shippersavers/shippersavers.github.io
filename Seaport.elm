module Seaport where

import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)

--  View

view : String -> Result String (List String) -> Html
view message result=
  let header =
        h1 [ ] [ text "Seaports" ]
      field =
        div [ ]
          [ input [
             placeholder "Code of seaport",
             on "input" targetValue (Signal.message query.address) ]
             [ ],
            hr [ ] [ ]                  
          ]
      message =
        case result of
          Err msg ->
            [ div [  ] [ text msg ] ]

          Ok seaports ->
            List.map (\seaport -> div [ ] [ text seaport ]) seaports
            -- [ div [  ] [ text "OK" ] ]
          
  in
    div [ ] (header :: field :: message)
    


-- WIRING

-- type alias Mailbox a =
--   { address : Address a,
--     signal : Signal a
--   }
-- In our case
-- Mailbox string
-- Address string
-- Signal  string


-- main : Signal Html
main =
  Signal.map2 view query.signal results.signal

-- inbox : Signal.Mailbox String
-- inbox =
--   Signal.mailbox "Waiting"

-- messages : Signal String
-- messages =
--   inbox.signal 
  
-- type Result error value
--     = Ok value
--     | Err error

query : Signal.Mailbox String
query =
  Signal.mailbox ""


results : Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "Waiting")


port requests : Signal (Task x ())
port requests =
  -- Signal.map lookupSeaport query.signal
  --   |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)
  
  Signal.map
          (\task -> Task.toResult task `andThen` Signal.send results.address)
          (Signal.map lookupSeaport query.signal)



-- port requests : Signal (Task String (String))
-- port requests =
--   Signal.map lookupSeaport query.signal
-- f : Json.Decoder (List String) -> String -> Task.Task Http.Error (List String)
-- f = Http.get
    
lookupSeaport : String -> Task String (List String)
lookupSeaport inquiry =
  let
    toUrl  =
      if String.length inquiry >= 1
        then succeed ("http://seaports.herokuapp.com/seaports.json?q=" ++ inquiry)
        else fail "Please input some character"
  in
    toUrl `andThen` (mapError (always "Not found :(") << Http.get places)


places : Json.Decoder (List String)
places =
  let place =
        Json.object2 (++)
          ("code" := Json.string)
          ("country" := Json.string)
  in
      "seaports" := Json.list place

