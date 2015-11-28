module SeaportPair where

import Seaport
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
--  Model

type alias Model =
  { from : Seaport.Model
  , to : Seaport.Model }

init : String -> String -> (Model, Effects Action)
init fromPort toPort =
  let
    (from, fromFx)   = Seaport.init fromPort
    (to, toFx) = Seaport.init toPort
  in
    ( Model from to
    , Effects.batch
        [ Effects.map From fromFx
        , Effects.map To toFx
        ]
    )

-- UPDATE

type Action
  = From Seaport.Action
  | To Seaport.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    From act ->
      let
        (from, fx) = Seaport.update act model.from
      in
        (Model from model.to
        , Effects.map From fx
        )

    To act ->
      let
        (to, fx) = Seaport.update act model.to
      in
        (Model model.from to
        , Effects.map To fx
        )

-- VIEW
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
      [ a [ class "pure-menu-heading" ]
        [ text "Shipper Savers"]
      , ul [ class "pure-menu-list"]
        [ li [ class "pure-menu-item"]
          [ a
            [ href "#"
            , class "pure-menu-link" ]
            [ text "Home"]
          ]
        ]
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
            , button [
               classList
               [ ("pure-button", True)
               , ("pure-button-primary", True)
               ]
              ]
              [ text "Search"]
            ]
          ]
        ]
      ]
    ]
  ]
