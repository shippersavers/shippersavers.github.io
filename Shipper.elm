module Shipper where

import String exposing (toUpper, repeat, trimRight)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)

import StartApp.Simple as StartApp

-- MODEL

-- The full state of our app.

type alias Model =
  { seaports : List Seaport
  }
  
                 
type alias Seaport =
  { code: String,
    city: String,
    country: String
  }

initialModel : Model
initialModel =
  { seaports =
    [ Seaport "ALSAR" "Sarande" "Albania",
      Seaport "RUVVO" "Vladivostok" "Russia",
      Seaport "SEHAD" "Halmstad" "Sweden"
    ]
  }

-- UPDATE

-- A description of the kinds of actions that can be performed on the model of
-- our application.

type Action
  = NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

-- VIEW

view : Address Action -> Model -> Html
view address model =
  div
    [ id "container" ]
    [ pageHeader
    , seaportList
    , pageFooter
    ]

pageHeader : Html
pageHeader =
  header [ class "" ]
    [ h1 [ ] [ text "Shipper Savers"] ]

seaportItem code city country =
  tr [ ]
    [ td [ class "code" ] [ text code],
      td [ class "city" ] [ text city],
      td [ class "country" ] [ text country]        
    ]
     

-- seaportList : Html
seaportList =
  div [ id "main",
        class "container"
      ]
    [ h2 [ class "content-subhead"] [ text "Seaports" ],
      table
      [ class "pure-table"
      ]
      [ thead [ ]
        [ tr [ ]
          [ th [ ] [ text "Code" ],
            th [ ] [ text "City" ],
            th [ ] [ text "Country" ]
          ]
        ],
        tbody [ ]
        [ seaportItem "ALSAR" "Sarande" "Albania",
          seaportItem "RUVVO" "Vladivostok" "Russia",
          seaportItem "SEHAD" "Halmstad" "Sweden"
        ]
      ]
    ]

pageFooter : Html
pageFooter =
  footer [ ]
    [
     a [ href "https://shippersavers.com" ] [ text "Shipper Savers" ]
    ]
  

-- WIRE THE APP TOGETHER!

main: Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update
    }
