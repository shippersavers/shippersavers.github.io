module Shipper where

import String exposing (toUpper, repeat, trimRight)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)

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


newSeaport : String -> String -> String -> Seaport
newSeaport code city country =
  { code = code ,
    city = city,
    country = country
  }


initialModel : Model
initialModel =
  { seaports =
    [ newSeaport "ALSAR" "Sarande" "Albania",
      newSeaport "RUVVO" "Vladivostok" "Russia",
      newSeaport "BLA" "BLA" "BLA",
      newSeaport "SEHAD" "Halmstad" "Sweden"
    ]
  }

-- UPDATE

-- A description of the kinds of actions that can be performed on the model of
-- our application.

type Action
  = NoOp
  | Sort

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Sort ->
      { model | seaports <- List.sortBy .code model.seaports }


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div
    [ class "container" ]
    [ pageHeader
    , seaportList model.seaports
    , button
      [ class "sort", onClick address Sort ]
      [ text "Sort" ] 
    , pageFooter
    ]

pageHeader : Html
pageHeader =
  header [ class "" ]
    [ h1 [ ] [ text "Shipper Savers"] ]

seaportItem seaport =
  tr [ ]
    [ td [ class "code" ] [ text seaport.code],
      td [ class "city" ] [ text seaport.city],
      td [ class "country" ] [ text seaport.country]        
    ]
     

-- seaportList : Html
seaportList seaports =
  div [ id "main",
        class ""
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
        tbody [ ] (List.map seaportItem seaports)
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
