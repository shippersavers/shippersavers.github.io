module Tariff (Model, Tariff, Filter, emptyFilter, init, Action, update, view, view', tariffList) where

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
import Set exposing (..)

-- MODEL

type alias Model =
  { pol     : String
  , pod     : String
  , tariffs : List Tariff
  , filter  : Filter
  , setFilter : Filter
  , filterTariffs : List Tariff
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

type alias Filter =
  { owners : String }

emptyFilter = Filter ""

init : (Model, Effects Action)
init =
  -- ( Model "" "" []
  -- , Effects.none
  -- )
  ( Model "RUVVO" "HKHKG"  []  emptyFilter emptyFilter []
  , getListTariff "RUVVO" "HKHKG"
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
    | Filtrate
    | SetFilter String


update : Action -> Model -> (Model, Effects Action)
update a m =
  case a of
    RequestMore pol pod ->
      ( m
      , getListTariff m.pol m.pod)
      
    NewList maybeTariff ->
      ( Model m.pol m.pod (Maybe.withDefault m.tariffs maybeTariff) m.filter m.setFilter m.filterTariffs
      , Effects.none
      )

    Filtrate ->
      ( Model m.pol m.pod m.tariffs m.filter m.setFilter m.filterTariffs
      , Effects.none
      )

    SetFilter string ->
      let
        setFilter = Filter string
      in
        ( Model m.pol m.pod m.tariffs m.filter setFilter m.filterTariffs
        , Effects.none
        )

-- VIEW

radioFilterOwners : Signal.Address Action -> Filter -> String -> Html
radioFilterOwners address filter style =
  div
  []
  [ input
    [ type' "radio"
    , checked (filter.owners == style )
    , on "change" targetChecked (\_ -> Signal.message address (SetFilter style))
-- (Signal.message address << PortUpdate)
    ]
    []
  , text style ]


view' : Signal.Address Action -> Model -> Html
view' address model =
    div
    [ class "filters"]
    [ h2 [] [ text "Filters"]
    , h1 [] [ text model.setFilter.owners ]
    , div [ class "owners pure-form"]
      [ h3 [] [ text "owners"]
      , radioFilterOwners address model.setFilter "SOC"
      , radioFilterOwners address model.setFilter "COC"
      ]
    , div [ class "containers"]
      [ h3
        [ class "" ]
        [ text "Containers"]
      ]          
    , h3
      [ class "" ]
      [ text "Status"]
    , h3
      [ class "" ]
      [ text "Freight"]
    , h3
      [ class "" ]
      [ text "BAF"]
    , h3
      [ class "" ]
      [ text "Companies"]
    ]


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
