module Main exposing (..)

import String exposing (padLeft)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, second)


type Msg
    = Tick Time
    | Minutos Int
    | StartStop
    | Reset
    | Puntos Int Luchador
    | Ventajas Int Luchador
    | Puniciones Int Luchador


type Color
    = Verde
    | Amarillo


type alias Luchador =
    { color : Color
    , puntos : Int
    , puniciones : Int
    , ventajas : Int
    }


type alias Model =
    { verde : Luchador
    , amarillo : Luchador
    , tiempoRestante : Int
    , correReloj : Bool
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


initModel : ( Model, Cmd Msg )
initModel =
    ( { verde = Luchador Verde 0 0 0
      , amarillo = Luchador Amarillo 0 0 0
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

        Puntos p l ->
            let
                nuevo =
                    { l | puntos = Basics.max 0 (l.puntos + p) }
            in
                ( actualizarLuchador model nuevo, Cmd.none )

        Ventajas v l ->
            let
                nuevo =
                    { l | ventajas = Basics.max 0 (l.ventajas + v) }
            in
                ( actualizarLuchador model nuevo, Cmd.none )

        Puniciones p l ->
            let
                nuevo =
                    { l | puniciones = Basics.max 0 (l.puniciones + p) }
            in
                ( actualizarLuchador model nuevo, Cmd.none )


actualizarLuchador : Model -> Luchador -> Model
actualizarLuchador model luchador =
    if luchador.color == Amarillo then
        { model | amarillo = luchador }
    else
        { model | verde = luchador }


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
            [ div [ style [ ( "flex-grow", "3" ), ( "display", "flex" ) ] ]
                [ div [ style [ ( "flex-grow", "1" ) ], onClick (Puntos -1 model.amarillo) ] []
                , div [ class "puntos" ] [ text (toString model.amarillo.puntos) ]
                , div [ style [ ( "flex-grow", "1" ) ], onClick (Puntos 1 model.amarillo) ] []
                ]
            , div [ style [ ( "flex-grow", "2" ), ( "display", "flex" ) ] ]
                [ div [ style [ ( "flex-grow", "1" ) ], onClick (Ventajas -1 model.amarillo) ] []
                , div [ class "ventajas" ] [ text (toString model.amarillo.ventajas) ]
                , div [ style [ ( "flex-grow", "1" ) ], onClick (Ventajas 1 model.amarillo) ] []
                ]
            , div [ style [ ( "flex-grow", "1" ), ( "display", "flex" ) ] ]
                [ div [ style [ ( "flex-grow", "1" ) ], onClick (Puniciones -1 model.amarillo) ] []
                , div [ class "puniciones" ] [ text (toString model.amarillo.puniciones) ]
                , div [ style [ ( "flex-grow", "1" ) ], onClick (Puniciones 1 model.amarillo) ] []
                ]
            ]
        , div [ id "verde" ]
            [ div [ style [ ( "flex-grow", "3" ), ( "display", "flex" ) ] ]
                [ div [ style [ ( "flex-grow", "1" ) ], onClick (Puntos -1 model.verde) ] []
                , div [ class "puntos" ] [ text (toString model.verde.puntos) ]
                , div [ style [ ( "flex-grow", "1" ) ], onClick (Puntos 1 model.verde) ] []
                ]
            , div [ style [ ( "flex-grow", "2" ), ( "display", "flex" ) ] ]
                [ div [ style [ ( "flex-grow", "1" ) ], onClick (Ventajas -1 model.verde) ] []
                , div [ class "ventajas" ] [ text (toString model.verde.ventajas) ]
                , div [ style [ ( "flex-grow", "1" ) ], onClick (Ventajas 1 model.verde) ] []
                ]
            , div [ style [ ( "flex-grow", "1" ), ( "display", "flex" ) ] ]
                [ div [ style [ ( "flex-grow", "1" ) ], onClick (Puniciones -1 model.verde) ] []
                , div [ class "puniciones" ] [ text (toString model.verde.puniciones) ]
                , div [ style [ ( "flex-grow", "1" ) ], onClick (Puniciones 1 model.verde) ] []
                ]
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
