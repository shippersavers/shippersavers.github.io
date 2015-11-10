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
    { topic : String
    , ports : List Seaport
    }

type alias Seaport =
  { code: String,
    name: String,
    country: String
  }


init : (Model, Effects Action)
init =
  ( Model "ru" []
  , Effects.none
  )


-- UPDATE

type Action
    = RequestMore String
    | NewList (Maybe (List Seaport))
    | PortUpdate String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestMore query->
      (model, getListPort query)

    NewList maybeSeaport ->
      ( Model model.topic (Maybe.withDefault model.ports maybeSeaport)
      , Effects.none
      )

    PortUpdate code ->
      ( Model code model.ports
      , getListPort code
      )

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ id "main" ]
    [ h2 [headerStyle] [text ("From: " ++ model.topic)]
    , input
      [ class "autocomplete"
      , on "input" targetValue (Signal.message address << PortUpdate)
      , value model.topic
      ] [ ]
    , div [ class "autocomplete" ] [ ul [ class "select" ] (seaportList model.ports) ]
    ]

seaportStr : Seaport -> String
seaportStr seaport =
  String.concat [seaport.code,    ", ",
                 seaport.name,    ", ",
                 seaport.country, ", "
                ]

seaportList : List Seaport -> List Html
seaportList seaports =
  List.map
    (\seaport -> li
      [ class "" ]
      [ text (seaportStr seaport)]
    ) seaports

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
