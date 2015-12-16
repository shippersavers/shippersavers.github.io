module SeaportPair where

import Seaport
import Tariff
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
--  Model

type alias Model =
  { from   : Seaport.Model
  , to     : Seaport.Model
  , tariff : Tariff.Model
  }

init : String -> String -> (Model, Effects Action)
init fromPort toPort =
  let
    (from, fromFx)
      = Seaport.init fromPort
    (to, toFx) =
      Seaport.init toPort
    (tariff, tariffFx) =
      Tariff.init
  in
    ( Model from to tariff
    , Effects.batch
        [ Effects.map From fromFx
        , Effects.map To toFx
        , Effects.map Tariff tariffFx
        ]
    )

-- UPDATE

type Action
  = From Seaport.Action
  | To Seaport.Action
  | Tariff Tariff.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    From act ->
      let
        (from, fx) = Seaport.update act model.from
      in
        (Model from model.to model.tariff
        , Effects.map From fx
        )

    To act ->
      let
        (to, fx) = Seaport.update act model.to
      in
        (Model model.from to model.tariff
        , Effects.map To fx
        )

    Tariff act ->
      let
        (tariff, fx) =
          Tariff.update act (Tariff.Model model.from.seaportCode model.to.seaportCode model.tariff.tariffs model.tariff.filter model.tariff.setFilter model.tariff.filterTariffs) 
      in
        (Model model.from model.to tariff 
        , Effects.map Tariff fx
        )

-- VIEW

filtrateTariff : Tariff.Model -> Tariff.Model
filtrateTariff tarrif = { tarrif | tariffs <- [] }


view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ id "layout" ]
  [ a [ href "#menu"
      , id "menuLink"
      , class "menu-link"
      ]
    [ span [] [] ]
  , div [ id "menu"]
    [ div [ class "pure-menu" ]
      [ a [ href "/"
        , class "pure-menu-heading" ]
        [ text "Shipper Savers"]
      , ul [ class "pure-menu-list"]
        [ li [ class "pure-menu-item"]
          [ a
            [ href "#"
            , class "pure-menu-link" ]
            [ text "About"]
          ]
        ]
      , Tariff.view' (Signal.forwardTo address Tariff) model.tariff
      ]
    ]
  , Html.main' [ ]
    [ header [ ]
      [ div [ class "hero-titles" ]
        [ h1 [] [ text "Shipper Savers" ]
        , h3 [] [ text "We compare sea freight from shipping lines and help save money"]
        ]
      , div [ class "hero-form" ]
        [ div [ class "pure-form" ]
          [ fieldset []
            [ Seaport.view (Signal.forwardTo address From) model.from
            , Seaport.view (Signal.forwardTo address To) model.to
            , Tariff.view (Signal.forwardTo address Tariff) model.tariff
            ]
          ]
        ]
      ]
      , section [ class "content" ]
        (Tariff.tariffList model.tariff.tariffs)
    ]
  ]
