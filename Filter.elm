module Filter (Model, init, Action, update, view) where

import Tariff exposing (Tariff)
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
  { owners : Set String
  , tariffs: List Tariff
  } 

init : Model
init = Model Set.empty []

-- UPDATE

type Action
  = Filtrate

update : Action -> Model -> Model
update action model =
  case action of
    Filtrate ->
      model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "filters"]
    [ h2
      [ class "" ]
      [ text "Filters"]
    , div [ class "containers"]
      [ h3
        [ class "" ]
        [ text "Containers"]
      ]          
    , div [ class "owners pure-form"]
      [ h3
        [ class "" ]
        [ text "owners"]
      , label [ class "pure-checkbox" ]
        [ input
          [ for "option-one"
          , type' "radio" ]
          []
        , text "SOC"
        ]
      , label [ class "pure-checkbox" ]
        [ input
          [ for "option-one"
          , type' "radio" ]
          []
        , text "COC"
        ]
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




-- filtrateTariffs : List Tariff -> List Tariff
-- filtrateTariffs tariffs 
--   = List.filter isSOC tariffs

-- isSOC : Tariff -> Bool
-- isSOC tariff = tariff.owners == "SOC"



