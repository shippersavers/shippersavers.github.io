module Tariff (Model, Tariff, init, Action, update, view, tariffList) where

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
  { pol     : String,
    pod     : String,          
    tariffs : List Tariff,
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
  ( Model "" "" []
  , Effects.none
  )


priceTariff : Tariff -> String
priceTariff t =
  let freight =
        (String.toFloat t.freight)
      baf =
        (String.toFloat t.baf)
      result = Result.map2 (+) freight baf
  in Result.toMaybe result |> Maybe.withDefault 0.0 |> toString 


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
        , ("button-xlarge ", True)
        ]
      , onClick address (RequestMore model.pol model.pod)
      ]
      [ text "Search"]
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
              div
              [ class "callout" ]
              [ div
                [ class "pure-g" ]
                [ div [ class "pure-u-1-5" ]
                  [ div [ class "logo" ]
                    [ h3 [ ] [ text t.company ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "pol" ] 
                    [ h3 [ ] [ text t.pol ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "container" ]
                    [ h3 [ ] [ text t.container ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "pod" ]
                    [ h3 [ ] [ text t.pod ] ]
                  ]
                , div [ class "pure-u-1-5" ]
                  [ div [ class "price" ]
                    [ h3 [ ] [ text ("$ " ++ (priceTariff t)) ] ]
                  ]
                ]
              , div
                [ class "pure-g" ]
                [ div [ class "pure-u-1-5" ] []
                , div [ class "pure-u-3-5" ]
                  [ div [ class "pure-g arrow" ]
                    [ div [ class "pure-u-2-24" ] []
                    , div [ class "pure-u-20-24" ]
                      [ div [class "line"] []
                      ]
                    , div [ class "pure-u-2-24" ]
                      [ div [class "point"] [] ]
                    ]
                  ]
                , div [ class "pure-u-1-5" ] []
                ]
              , div
                [ class "pure-g" ]
                [ div [ class "pure-u-1-24" ] []
                , div [ class "pure-u-22-24" ]
                  [ hr [] []
                  , p [ ] [ text "Details" ]
                  ] 
                , div [ class "pure-u-1-24" ] []
                ]
              , div
                [ class "pure-g details" ]
                [ div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "status" ] [ text "Status: " ]
                    , span [ ] [ text t.status ]
                    ]
                  ]
                , div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "owners" ] [ text "Owners: " ]
                    , span [ ] [ text t.owners ]
                    ]
                  ]
                , div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "freight" ] [ text "Freight: " ]
                    , span [ ] [ text t.freight ]
                    ]
                  ]
                , div [ class "pure-u-6-24" ]
                  [ p [ ]
                    [ span [ class "baf" ] [ text "BAF: " ]
                    , span [ ] [ text t.baf ]
                    ]
                  ]
                ]
              ]
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
