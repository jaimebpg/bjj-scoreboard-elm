module Luchador exposing (Model, Msg, reset, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type Msg
    = Puntos Int
    | Ventajas Int
    | Puniciones Int


type alias Model =
    { puntos : Int
    , puniciones : Int
    , ventajas : Int
    }


reset : Model
reset =
    Model 0 0 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Puntos p ->
            { model | puntos = Basics.max 0 (model.puntos + p) }

        Ventajas v ->
            { model | ventajas = Basics.max 0 (model.ventajas + v) }

        Puniciones p ->
            { model | puniciones = Basics.max 0 (model.puniciones + p) }


view : Model -> Html Msg
view model =
    div []
        [ div [ style [ ( "flex-grow", "3" ), ( "display", "flex" ) ] ]
            [ div [ flexGrow 1, onClick (Puntos -1) ] []
            , div [ class "puntos" ] [ text (toString model.puntos) ]
            , div [ flexGrow 1, onClick (Puntos 1) ] []
            ]
        , div [ style [ ( "flex-grow", "2" ), ( "display", "flex" ) ] ]
            [ div [ flexGrow 1, onClick (Ventajas -1) ] []
            , div [ class "ventajas" ] [ text (toString model.ventajas) ]
            , div [ flexGrow 1, onClick (Ventajas 1) ] []
            ]
        , div [ style [ ( "flex-grow", "1" ), ( "display", "flex" ) ] ]
            [ div [ flexGrow 1, onClick (Puniciones -1) ] []
            , div [ class "puniciones" ] [ text (toString model.puniciones) ]
            , div [ flexGrow 1, onClick (Puniciones 1) ] []
            ]
        ]


flexGrow : Int -> Attribute Msg
flexGrow w =
    style [ ( "flex-grow", toString w ) ]
