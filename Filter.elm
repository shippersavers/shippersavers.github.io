module Filter (Model, Filter, init, Action, update, view, emptyFilter) where

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

type alias Filter =
  { owners : List Holder
  }

type Holder
  = SOC
  | COC

emptyFilter : Filter
emptyFilter = { owners = [] }

              init : (Model, Effects Action)


filtrateTariffs : List Tariff -> List Tariff
filtrateTariffs tariffs 
  = List.filter isSOC tariffs

isSOC : Tariff -> Bool
isSOC tariff = tariff.owners == "SOC"



