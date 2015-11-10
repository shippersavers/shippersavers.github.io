module Seaport where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task
import Char
import Task exposing (..)

-- MODEL

type alias Model =
    { seaportCode : String
    , seaport : Maybe Seaport
    , ports : List Seaport
    , hideList : Bool
    }

type alias Seaport =
  { code: String,
    name: String,
    country: String
  }

init : (Model, Effects Action)
init =
  ( Model "" Nothing [] True
  , Effects.none
  )


-- UPDATE

type Action
    = RequestMore String
    | NewList (Maybe (List Seaport))
    | PortUpdate String
    | Pickup Seaport


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestMore query->
      (model, getListPort query)

    NewList maybeSeaport ->
      ( Model model.seaportCode model.seaport (Maybe.withDefault model.ports maybeSeaport) False
      , Effects.none
      )

    PortUpdate code ->
      ( Model code model.seaport model.ports False
      , getListPort code
      )

    Pickup seaport ->
      ( Model seaport.code (Just seaport) model.ports True
      , Effects.none
      )


-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ id "main" ]
    [ h3 [headerStyle] [text "From:"]
    , input
      [ class "autocomplete"
      , on "input" targetValue (Signal.message address << PortUpdate)
      , value model.seaportCode
      ] [ ]
    , div
      [ class "autocomplete" ]
      [ p [ hidden (not model.hideList) ] [ text (seaportStr model.seaport) ]      
      , ul
        [ hidden model.hideList
        , class "select"
        ]
        (seaportList address model.ports)
      ]
    ]

seaportStr : Maybe Seaport -> String
seaportStr seaport =
  case seaport of
    Just p -> String.concat [p.code, ", ", p.name, ", ", p.country, ", "]
    Nothing -> ""

seaportList : Signal.Address Action -> List Seaport -> List Html
seaportList address seaports =
  List.map (\seaport -> li [ onClick address (Pickup seaport) ] [ text (seaportStr (Just seaport))] ) seaports

headerStyle : Attribute
headerStyle =
  style
    [ "width" => "500px"
    , "text-align" => "left"
    ]


-- EFFECTS

getListPort : String -> Effects Action
getListPort query = 
  Http.get decodePorts (portUrl query)
    |> Task.toMaybe 
    |> Task.map NewList
    |> Effects.task
    

portUrl : String -> String
portUrl query=
  Http.url ("http://seaports.herokuapp.com/seaports.json?q=" ++ query) [ ]
         

decodePorts : Json.Decoder (List Seaport)
decodePorts =
  let place =
        Json.object3 Seaport
          ("code" := Json.string)
          ("name" := Json.string)
          ("country" := Json.string)          
  in
      "seaports" := Json.list place


lookupSeaport : String -> Task String (List Seaport)
lookupSeaport inquiry =
  let
    toUrl =
      if String.length inquiry >= 1
        then succeed ("http://seaports.herokuapp.com/seaports.json?q=" ++ inquiry)
        else fail "Please input some character"
  in
    toUrl `andThen` (mapError (always "Not found :(") << Http.get places)


places : Json.Decoder (List Seaport)
places =
  let place =
        Json.object3 Seaport
          ("code" := Json.string)
          ("name" := Json.string)
          ("country" := Json.string)          
  in
      "seaports" := Json.list place
