module Main exposing (Model, main)

import Accessibility.Styled exposing (Html, button, div, li, span, text, ul)
import Browser
import Css exposing (Style)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = Html.toUnstyled << view
        }



-- MODEL


type alias Model =
    { currentFloor : Int
    , selectedFloor : Maybe Int
    , estimatedTimeToArrival : Maybe Int
    , direction : Direction
    , emergenzyStop : Bool
    , timeOnThisFloor : Int
    }


type Direction
    = Up
    | Down
    | None


type Msg
    = FloorSelected Int
    | MoveLift Time.Posix
    | EmergenzyStop
    | UpdateTime Time.Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentFloor = 5
      , selectedFloor = Nothing
      , estimatedTimeToArrival = Nothing
      , direction = None
      , emergenzyStop = False
      , timeOnThisFloor = 3
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FloorSelected selectedFloor ->
            let
                direction =
                    if selectedFloor > model.currentFloor then
                        Up

                    else if selectedFloor < model.currentFloor then
                        Down

                    else
                        None
            in
            ( { model | selectedFloor = Just selectedFloor, direction = direction }
            , Cmd.none
            )

        MoveLift newTime ->
            case model.selectedFloor of
                Just selectedFloor ->
                    let
                        updateFloor =
                            case model.direction of
                                Up ->
                                    model.currentFloor + 1

                                Down ->
                                    model.currentFloor - 1

                                _ ->
                                    model.currentFloor

                        currentDirection =
                            if selectedFloor == updateFloor then
                                None

                            else
                                model.direction
                    in
                    if model.currentFloor /= selectedFloor && not model.emergenzyStop then
                        ( { model
                            | estimatedTimeToArrival = updateTime model.direction selectedFloor model.currentFloor model.timeOnThisFloor newTime
                            , currentFloor = updateFloor
                            , direction = currentDirection

                            -- Sets to 2 as one second is used to switch between functions
                            , timeOnThisFloor = 2
                          }
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        UpdateTime newTime ->
            case model.selectedFloor of
                Just selectedFloor ->
                    if model.currentFloor /= selectedFloor && not model.emergenzyStop then
                        ( { model
                            | timeOnThisFloor = model.timeOnThisFloor - 1
                            , estimatedTimeToArrival = updateTime model.direction selectedFloor model.currentFloor model.timeOnThisFloor newTime
                          }
                        , Cmd.none
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        EmergenzyStop ->
            ( { model | emergenzyStop = True }
            , Cmd.none
            )



-- HelperFunction


updateTime : Direction -> Int -> Int -> Int -> Time.Posix -> Maybe Int
updateTime direction selectedFloor currentFloor timeOnThisFloor newTime =
    case direction of
        Up ->
            let
                floors =
                    (selectedFloor - 1) - currentFloor

                totalTime =
                    (floors * 3) + timeOnThisFloor

                time =
                    totalTime
            in
            if time == 0 then
                Nothing

            else
                Just <| time

        Down ->
            let
                floors =
                    (currentFloor - 1) - selectedFloor

                totalTime =
                    (floors * 3) + timeOnThisFloor

                time =
                    totalTime
            in
            if time == 0 then
                Nothing

            else
                Just <| time

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timeOnThisFloor > 0 then
        Time.every 1000 UpdateTime

    else
        Time.every 1000 MoveLift



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| "Current floor " ++ String.fromInt model.currentFloor ]
        , div []
            [ case model.selectedFloor of
                Just newFloor ->
                    if model.currentFloor == newFloor then
                        text "Please select floor"

                    else
                        text <| "Selected floor " ++ String.fromInt newFloor

                Nothing ->
                    text "Please select floor"
            ]
        , case model.direction of
            Up ->
                div [] [ text "Lift going up" ]

            Down ->
                div []
                    [ text "Lift going down" ]

            None ->
                text ""
        , case model.estimatedTimeToArrival of
            Just time ->
                div [] [ text <| String.fromInt time ]

            Nothing ->
                text ""
        , ul
            [ Attributes.css
                [ Css.listStyle Css.none
                , Css.padding Css.zero
                ]
            ]
            [ viewListItem model 5
            , viewListItem model 4
            , viewListItem model 3
            , viewListItem model 2
            , viewListItem model 1
            ]
        , button [ Events.onClick <| EmergenzyStop ] [ text "Click to stop lift in case of emergenzy" ]
        ]


viewListItem : Model -> Int -> Html Msg
viewListItem model floor =
    let
        backgroundColor =
            if model.currentFloor == floor then
                Css.hex "FFFF00"

            else
                Css.hex "FFFFFF"
    in
    li [ Attributes.css [ Css.paddingBottom <| Css.px 12 ] ]
        [ button
            [ Events.onClick <| FloorSelected floor
            , Attributes.css
                [ case model.selectedFloor of
                    Just selectedFloor ->
                        if selectedFloor == floor then
                            Css.borderColor <| Css.hex "FFFF00"

                        else
                            Css.borderColor <| Css.hex "000000"

                    Nothing ->
                        Css.borderColor <| Css.hex "000000"
                , Css.backgroundColor backgroundColor
                , Css.borderRadius <| Css.pct 100
                , Css.width <| Css.px 50
                , Css.height <| Css.px 50
                ]
            ]
            [ text <| String.fromInt floor ]
        ]
