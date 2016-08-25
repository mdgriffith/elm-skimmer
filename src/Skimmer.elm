port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Time exposing (Time, second)


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { retrieved : String
    , packages : List Package
    }


type alias Package =
    { name : String
    , deprecated : Bool
    , summary : String
    , is_current : Bool
    , stars : Int
    , forks : Int
    , watchers : Int
    , open_issues : Int
    , has_tests : Bool
    , has_examples : Bool
    , versions : List String
    , license : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { retrieved = "Never", packages = [] }, Cmd.none )


type Msg
    = Load Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load packages ->
            let
                sortedPkgs =
                    List.reverse <|
                        List.sortWith
                            (\pkg1 pkg2 ->
                                let
                                    compareBool b1 =
                                        if b1 then
                                            1
                                        else
                                            0

                                    dep =
                                        compare
                                            (compareBool <| not pkg1.deprecated)
                                            (compareBool <| not pkg2.deprecated)

                                    current =
                                        compare
                                            (compareBool pkg1.is_current)
                                            (compareBool pkg2.is_current)

                                    stars =
                                        compare pkg1.stars pkg2.stars
                                in
                                    case dep of
                                        EQ ->
                                            case current of
                                                EQ ->
                                                    stars

                                                _ ->
                                                    current

                                        _ ->
                                            dep
                            )
                        <|
                            packages.packages
            in
                ( { packages | packages = sortedPkgs }, Cmd.none )


port packages : (Model -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    packages Load


view : Model -> Html Msg
view model =
    div []
        [ viewToolbar model.retrieved
          --, viewSidebar
        , viewPackages model.packages
        ]


viewToolbar : String -> Html Msg
viewToolbar refreshed =
    div [ class "toolbar" ]
        [ span [ class "logo" ]
            [ text "elm package"
            , br [] []
            , text "skimmer"
            ]
        , input [ class "search", placeholder "Search" ] []
        , span [ class "info" ] [ text <| "Data updated on " ++ refreshed ]
        ]


(=>) =
    (,)


viewPackages : List Package -> Html Msg
viewPackages pkgs =
    let
        viewPkg pkg =
            div
                [ class "package"
                ]
                [ h1 [ class "name" ] [ a [ href <| "http://package.elm-lang.org/packages/" ++ pkg.name ++ "/latest" ] [ text pkg.name ] ]
                , div [ class "summary" ] [ text <| pkg.summary ]
                , deprecationWarning pkg.deprecated
                , div [ class "metrics" ]
                    [ iconCount "star" "stars" pkg.stars
                    , iconCount "code-fork" "forks" pkg.forks
                    , iconCount "eye" "watchers" pkg.watchers
                    , iconCount "exclamation" "open issues" pkg.open_issues
                    , has "tests" pkg.has_tests
                    , has "examples" pkg.has_examples
                    , has "0.17 compatible" pkg.is_current
                    , case pkg.license of
                        Nothing ->
                            div [ class "metric" ]
                                [ i [ class <| "fa fa-legal" ] []
                                , text " No license"
                                ]

                        Just license ->
                            div [ class "metric" ]
                                [ i [ class <| "fa fa-legal" ] []
                                , text <| " " ++ license ++ " license"
                                ]
                    ]
                ]
    in
        div [ class "packages" ] <|
            List.map viewPkg pkgs


viewSidebar : Html Msg
viewSidebar =
    div [ class "sidebar" ]
        [ h2 [] [ text "Resources" ]
        , ul []
            [ li [] [ a [] [ text "Fancy Search" ] ]
            , li [] [ a [] [ text "Using Packages" ] ]
            , li [] [ a [] [ text "API Design Guidelines" ] ]
            , li [] [ a [] [ text "Write great docs" ] ]
            , li [] [ a [] [ text "Preview your docs" ] ]
            , li [] [ a [] [ text "Elm Website" ] ]
            ]
        , h2 [] [ text "Standard Packages" ]
        , ul []
            [ li []
                [ h2 [] [ text "General" ]
                , li [] [ a [] [ text "core" ] ]
                ]
            , li []
                [ h2 [] [ text "Rendering" ]
                , li [] [ a [] [ text "html" ] ]
                , li [] [ a [] [ text "svg" ] ]
                , li [] [ a [] [ text "markdown" ] ]
                ]
            , li []
                [ h2 [] [ text "Effects" ]
                , li [] [ a [] [ text "http" ] ]
                , li [] [ a [] [ text "geolocation" ] ]
                , li [] [ a [] [ text "navigation" ] ]
                , li [] [ a [] [ text "page-visibility" ] ]
                , li [] [ a [] [ text "websocket" ] ]
                ]
            , li []
                [ h2 [] [ text "User Input" ]
                , li [] [ a [] [ text "mouse" ] ]
                , li [] [ a [] [ text "window" ] ]
                , li [] [ a [] [ text "keyboard" ] ]
                ]
            ]
        ]


iconCount : String -> String -> Int -> Html Msg
iconCount icon label metric =
    span [ class "metric" ]
        [ i [ class <| "fa fa-" ++ icon ] []
        , text <| " " ++ toString metric ++ " " ++ label
        ]


deprecationWarning : Bool -> Html Msg
deprecationWarning deprecated =
    if deprecated then
        span [ class "deprecation-warning" ]
            [ i [ class "fa fa-exclamation" ] []
            , text " Deprecated"
            ]
    else
        span []
            []


has : String -> Bool -> Html Msg
has label metric =
    if metric then
        span [ class "metric" ]
            [ i [ class <| "fa fa-check-square-o" ] []
            , text <| " " ++ label
            ]
    else
        span [ class "metric" ]
            [ i [ class <| "fa fa-square-o" ] []
            , text <| " " ++ label
            ]
