module Main exposing (..)

import String exposing (padLeft)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)
import Luchador


type Msg
    = Tick Time
    | Minutos Int
    | StartStop
    | Reset
    | Amarillo Luchador.Msg
    | Verde Luchador.Msg


type alias Model =
    { amarillo : Luchador.Model
    , verde : Luchador.Model
    , tiempoRestante : Int
    , correReloj : Bool
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


initModel : ( Model, Cmd Msg )
initModel =
    ( { amarillo = Luchador.reset
      , verde = Luchador.reset
      , tiempoRestante = 5 * 60
      , correReloj = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartStop ->
            ( { model | correReloj = not model.correReloj }, Cmd.none )

        Tick newTime ->
            if model.tiempoRestante > 0 && model.correReloj then
                ( { model | tiempoRestante = model.tiempoRestante - 1 }, Cmd.none )
            else
                ( model, Cmd.none )

        Minutos t ->
            if not model.correReloj then
                ( { model | tiempoRestante = Basics.max 0 (model.tiempoRestante + (t * 60)) }, Cmd.none )
            else
                ( model, Cmd.none )

        Reset ->
            initModel

        Amarillo msg ->
            ( { model | amarillo = Luchador.update msg model.amarillo }, Cmd.none )

        Verde msg ->
            ( { model | verde = Luchador.update msg model.verde }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ id "top" ]
            [ div [ style [ ( "flex-grow", "1" ) ], onClick (Minutos -1) ] []
            , div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
                [ div [ class "reset", onDoubleClick Reset ] [ text "R" ]
                , div [ class "tiempo", style [ ( "flex-grow", "1" ) ], onClick StartStop ] [ text (tiempo model.tiempoRestante) ]
                ]
            , div [ style [ ( "flex-grow", "1" ) ], onClick (Minutos 1) ] []
            ]
        , div [ id "amarillo" ]
            [ App.map
                Amarillo
                (Luchador.view model.amarillo)
            ]
        , div [ id "verde" ]
            [ App.map
                Verde
                (Luchador.view model.verde)
            ]
        ]


tiempo : Int -> String
tiempo t =
    toString (t // 60) ++ ":" ++ padLeft 2 '0' (toString (rem t 60))


main : Program Never
main =
    App.program
        { init = initModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
