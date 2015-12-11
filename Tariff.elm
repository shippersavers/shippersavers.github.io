module Tariff (Model, Tariff, init, Action, update, view) where

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

import StartApp

-- MODEL

type alias Model =
  { pol     : String,
    pod     : String,          
    tariffs : List Tariff
  }

type alias Tariff =
  { company  :  String,
    pol      :  String,
    pod      :  String,
    container:  String,
    status   :  String,
    owners   :  String,
    freight  :  String,
    baf      :  String
  }

init : (Model, Effects Action)
init =
  ( Model "RUVVO" "HKHKG" []
  , Effects.none
  )

-- UPDATE
type Action
    = RequestMore String String
    | NewList (Maybe (List Tariff))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestMore pol pod ->
      (model, getListTariff model.pol model.pod)
      
    NewList maybeTariff ->
      ( Model model.pol model.pod (Maybe.withDefault model.tariffs maybeTariff) 
      , Effects.none
      )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  
  div [ class "inline" ]
    [ button
      [ classList
        [ ("pure-button", True)
        , ("pure-button-primary", True)
        ]
      , onClick address (RequestMore model.pol model.pod)
      ]
      [ text "Search"]
    , ul
      [ ]
      (tariffList model.tariffs)
    ]

tariffStr : Tariff -> String
tariffStr t =
  String.concat [ "Company: ",   t.company
                , " POL: ",       t.pol
                , " POD: ",       t.pod
                , " Container: ", t.container
                , " Status: ",    t.status
                , " Owners: ",    t.owners
                , " Freight: ",   t.freight
                , " BAF: ",       t.baf
                ]

tariffList : List Tariff -> List Html
tariffList tariffs =
  List.map (\t ->
              li [ ] [ text (tariffStr t) ]
           ) tariffs


-- EFFECTS

getListTariff : String -> String -> Effects Action
getListTariff pol pod = 
  Http.get decodePorts (portUrl pol pod)
    |> Task.toMaybe 
    |> Task.map NewList
    |> Effects.task
    

portUrl : String -> String -> String
portUrl pol pod =
  Http.url ("http://seaports.herokuapp.com/api/v1/tariffs/pols/"
            ++ pol
            ++ "/pods/"
            ++ pod
           ) [ ]
         

decodePorts : Json.Decoder (List Tariff)
decodePorts =
  let tariff =
        Json.object8 Tariff
          ("company"   := Json.string)
          ("pol"       := Json.string)
          ("pod"       := Json.string)
          ("container" := Json.string)
          ("status"    := Json.string)
          ("owners"    := Json.string)
          ("freight"   := Json.string)
          ("baf"       := Json.string)
  in
      "tariffs" := Json.list tariff

  -- let tariff =
  --       Json.object2 Tariff
  --         ("pol" := Json.string)
  --         ("pod" := Json.string)
  -- in
  --     "tariffs" := Json.list tariff
