port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import String
import Svg
import Svg.Attributes
import Color
import Json.Decode
import Result


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { retrieved : String
    , packages : List Package
    , query : Query
    , deps : List Dependency
    , depCount : List ( String, Int )
    , location : Location
    }


type alias Package =
    { name : String
    , deprecated : Bool
    , deprecation_redirect : Maybe String
    , summary : String
    , is_current : Bool
    , stars : Int
    , forks : Int
    , watchers : Int
    , open_issues : Int
    , has_tests : Bool
    , has_examples : Bool
    , versions : Maybe (List String)
    , license : Maybe String
    , is_project : Bool
    , project_type : String
    , no_data : Bool
    }


type alias Dependency =
    ( String, List Version )


type alias Version =
    ( String, List String )


type Location
    = Overview
    | PackageOverview Dependency


init : ( Model, Cmd Msg )
init =
    ( { retrieved = "never"
      , packages = []
      , query =
            { search = ""
            , projects = False
            , packages = True
            }
      , deps = []
      , location = Overview
      , depCount = []
      }
    , Cmd.none
    )


type Msg
    = Load
        { retrieved : String
        , packages : List Package
        }
    | LoadDeps Json.Decode.Value
    | Search String
    | SeePackages
    | SeeProjects
    | Goto String
    | GotoOverview


type alias Query =
    { search : String
    , projects : Bool
    , packages : Bool
    }


decodeDeps : Json.Decode.Decoder (List Dependency)
decodeDeps =
    Json.Decode.keyValuePairs decodeDep


decodeDep : Json.Decode.Decoder (List Version)
decodeDep =
    Json.Decode.keyValuePairs (Json.Decode.list Json.Decode.string)


getDepCount : Dependency -> ( String, Int )
getDepCount ( name, vers ) =
    let
        count =
            List.map (List.length << Tuple.second) vers
                |> List.sum
    in
        ( name, count )


type alias VersNum =
    ( Int, Int, Int )


toVers : List String -> VersNum
toVers versions =
    let
        asNums =
            List.filterMap (Result.toMaybe << String.toInt) versions

        first =
            Maybe.withDefault 0 <| List.head asNums

        second =
            Maybe.withDefault 0 <| List.head <| List.drop 1 asNums

        third =
            Maybe.withDefault 0 <| List.head <| List.drop 2 asNums
    in
        ( first, second, third )


decodeVersion : String -> ( VersNum, VersNum )
decodeVersion str =
    let
        lower =
            toVers <| String.split "." <| String.left 5 str

        upper =
            toVers <| String.split "." <| String.right 5 str
    in
        ( upper, lower )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto locationName ->
            case List.head <| List.filter (\dep -> Tuple.first dep == locationName) model.deps of
                Nothing ->
                    ( model, Cmd.none )

                Just pkg ->
                    { model
                        | location = PackageOverview pkg
                    }
                        ! []

        GotoOverview ->
            { model
                | location = Overview
            }
                ! []

        Search query ->
            ( { model
                | query =
                    { search = query
                    , projects = model.query.projects
                    , packages = model.query.packages
                    }
              }
            , Cmd.none
            )

        SeePackages ->
            ( { model
                | query =
                    { search = model.query.search
                    , projects = False
                    , packages = True
                    }
              }
            , Cmd.none
            )

        SeeProjects ->
            ( { model
                | query =
                    { search = model.query.search
                    , projects = True
                    , packages = False
                    }
              }
            , Cmd.none
            )

        LoadDeps json ->
            case Json.Decode.decodeValue decodeDeps json of
                Ok dependencies ->
                    let
                        sortedDeps =
                            List.map
                                (\( name, versions ) ->
                                    ( name
                                    , List.sortWith
                                        (\d1 d2 ->
                                            compare
                                                (decodeVersion <| Tuple.first d2)
                                                (decodeVersion <| Tuple.first d1)
                                        )
                                        versions
                                    )
                                )
                                dependencies
                    in
                        { model
                            | deps = sortedDeps
                            , depCount = List.map getDepCount dependencies
                        }
                            ! []

                Err err ->
                    model ! []

        Load packages ->
            let
                sortedPkgs =
                    List.sortWith
                        (\pkg1 pkg2 ->
                            let
                                boolAsInt b1 =
                                    if b1 then
                                        1
                                    else
                                        0

                                dep =
                                    compare
                                        (boolAsInt <| not pkg2.deprecated)
                                        (boolAsInt <| not pkg1.deprecated)

                                current =
                                    compare
                                        (boolAsInt
                                            (if pkg2.is_project then
                                                True
                                             else
                                                pkg2.is_current
                                            )
                                        )
                                        (boolAsInt
                                            (if pkg1.is_project then
                                                True
                                             else
                                                pkg1.is_current
                                            )
                                        )

                                stars =
                                    compare pkg2.stars pkg1.stars
                            in
                                case current of
                                    EQ ->
                                        case dep of
                                            EQ ->
                                                stars

                                            _ ->
                                                dep

                                    _ ->
                                        current
                        )
                        packages.packages
            in
                ( { model
                    | packages = sortedPkgs
                    , retrieved = packages.retrieved
                  }
                , Cmd.none
                )


port packages :
    ({ retrieved : String
     , packages : List Package
     }
     -> msg
    )
    -> Sub msg


port deps :
    (Json.Decode.Value
     -> msg
    )
    -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ packages Load
        , deps LoadDeps
        ]


searchFor : Query -> List Package -> List Package
searchFor query packages =
    let
        searched =
            if query.search == "" then
                packages
            else
                let
                    queryTerms =
                        String.words (String.toLower query.search)

                    matchesQueryTerms pkg =
                        let
                            lowerName =
                                String.toLower pkg.name

                            lowerSummary =
                                String.toLower pkg.summary

                            findTerm term =
                                String.contains term lowerName
                                    || String.contains term lowerSummary
                        in
                            List.all findTerm queryTerms
                in
                    List.filter matchesQueryTerms packages
    in
        List.filter
            (\pkg ->
                if not query.projects && pkg.is_project then
                    False
                else if not query.packages && not pkg.is_project then
                    False
                else
                    True
            )
            searched


view : Model -> Html Msg
view model =
    case model.location of
        Overview ->
            div []
                [ viewToolbar model model.retrieved model.query.search
                , viewPackages model.depCount (searchFor model.query model.packages)
                ]

        PackageOverview ( pkgName, versions ) ->
            let
                viewVersion ( name, users ) =
                    div [ class "pkg-dep-versions" ]
                        [ h1 [] [ text name ]
                        , div [ class "pkg-dep-vers-users" ]
                            (List.map (\u -> a [ href <| "http://github.com/" ++ u, style [ ( "display", "block" ) ] ] [ text u ]) users)
                        ]
            in
                div [ class "pkg-overview" ]
                    [ h2 [] [ text pkgName ]
                    , a [ onClick GotoOverview ] [ text "return to main" ]
                    , div [] (List.map viewVersion versions)
                    ]


viewToolbar : Model -> String -> String -> Html Msg
viewToolbar model refreshed query =
    div [ class "toolbar" ]
        [ div [ style [ ( "flex", "1" ) ] ] []
        , div [ class "search-container" ]
            [ span [ class "logo-text" ]
                [ span [ class "elm-name" ] [ text "elm" ]
                , span [ class "package-skimmer" ] [ text "package skimmer" ]
                ]
            , input
                [ class "search"
                , placeholder "Search"
                , value query
                , onInput Search
                , autofocus True
                ]
                []
            , fieldset []
                [ radio " packages" SeePackages model.query.packages
                , radio " projects" SeeProjects model.query.projects
                ]
            , div [ class "last-updated" ] [ text <| "updated on " ++ refreshed ]
            ]
        , div [ style [ ( "flex", "1" ) ] ] []
        ]


radio : String -> msg -> Bool -> Html msg
radio value msg isChecked =
    label
        [ style [ ( "padding", "10px" ) ]
        ]
        [ input [ type_ "radio", name "font-size", onClick msg, checked isChecked ] []
        , text value
        ]


lookupCount : List ( String, Int ) -> String -> Int
lookupCount cache name =
    Maybe.withDefault 0 <| List.head <| List.map Tuple.second <| List.filter (\x -> Tuple.first x == name) cache


viewPackages : List ( String, Int ) -> List Package -> Html Msg
viewPackages pkgCount pkgs =
    let
        viewPkg pkg =
            if pkg.is_project then
                div
                    [ class "package"
                    ]
                    [ h1 [ class "name" ] [ a [ href <| "http://github.com/" ++ pkg.name ] [ text pkg.name ] ]
                    , div [ class "summary" ] [ text <| pkg.summary ]
                    , div [ class "metrics" ]
                        [ iconCount "star gold" "stars" pkg.stars
                        , case pkg.license of
                            Nothing ->
                                div [ class "metric" ]
                                    [ i [ class <| "fa fa-legal purple" ] []
                                    , text " no license"
                                    ]

                            Just license ->
                                div [ class "metric" ]
                                    [ i [ class <| "fa fa-legal purple" ] []
                                    , text <| " " ++ license
                                    ]
                        , cornerStone "package-svg-bottom-right" AllColors
                        ]
                    , div [ class "links" ]
                        [ a [ class "pkg-link", href <| "http://github.com/" ++ pkg.name ] [ text "source" ]
                        ]
                    ]
            else
                div
                    [ class "package"
                    ]
                    [ h1 [ class "name" ] [ a [ href <| "http://package.elm-lang.org/packages/" ++ pkg.name ++ "/latest" ] [ text pkg.name ] ]
                    , div [ class "summary" ] [ text <| pkg.summary ]
                    , div [ class "metrics" ]
                        [ iconCount "star gold" "stars" pkg.stars
                        , case pkg.license of
                            Nothing ->
                                div [ class "metric" ]
                                    [ i [ class <| "fa fa-legal purple" ] []
                                    , text " No license"
                                    ]

                            Just license ->
                                div [ class "metric" ]
                                    [ i [ class <| "fa fa-legal purple" ] []
                                    , text <| " " ++ license
                                    ]
                          -- , iconCount "code-fork" "forks" pkg.forks
                          -- , iconCount "eye" "watchers" pkg.watchers
                          -- , iconCount "exclamation" "open issues" pkg.open_issues
                        , has "0.18 compatible" pkg.is_current
                          --, has "tests" pkg.has_tests
                          --, has "examples" pkg.has_examples
                        , cornerStone "package-svg-bottom-right" AllColors
                        ]
                    , div [ class "links" ]
                        [ a [ class "pkg-link", href <| "http://package.elm-lang.org/packages/" ++ pkg.name ++ "/latest" ] [ text "docs" ]
                        , a [ class "pkg-link", href <| "http://github.com/" ++ pkg.name ] [ text "source" ]
                        , let
                            count =
                                lookupCount pkgCount pkg.name
                          in
                            if count == 0 then
                                text ""
                            else
                                a [ class "pkg-link", onClick <| Goto pkg.name ] [ text <| "who uses this? ", span [ class "pkg-count" ] [ text (toString <| lookupCount pkgCount pkg.name) ] ]
                        ]
                    , flags (not pkg.is_project) pkg.deprecated
                    ]
    in
        div
            [ style
                [ ( "position", "relative" )
                , ( "margin-top", "50px" )
                , ( "z-index", "1" )
                ]
            ]
            [ span [ class "logo" ]
                [ cornerStone "logo-svg" Grey
                ]
            , div [ class "packages" ]
                (List.map viewPkg pkgs)
            ]


viewSidebar : List (Html Msg)
viewSidebar =
    [ div [ class "package standard-package-list" ]
        [ cornerStone "package-svg-bottom-right" Gold
        , cornerStone "package-svg-bottom-left" Green
        , cornerStone "package-svg-top-right" Purple
        , cornerStone "package-svg-top-left" Blue
        , div [ style [ ( "z-index", "10" ) ] ]
            [ h2 [] [ text "Resources" ]
            , ul []
                [ li [] [ a [ href "http://klaftertief.github.io/elm-search/" ] [ text "Fancy Search" ] ]
                , li [] [ a [ href "https://github.com/elm-lang/elm-package/blob/master/README.md" ] [ text "Using Packages" ] ]
                , li [] [ a [ href "http://package.elm-lang.org/help/documentation-format" ] [ text "Write great docs" ] ]
                , li [] [ a [ href "http://package.elm-lang.org/help/docs-preview" ] [ text "Preview your docs" ] ]
                , li [] [ a [ href "http://package.elm-lang.org/help/design-guidelines" ] [ text "API Design Guidelines" ] ]
                , li [] [ a [ href "http://elm-lang.org" ] [ text "Elm Website" ] ]
                ]
            ]
        , div [ style [ ( "z-index", "10" ) ] ]
            [ h2 [] [ text "Standard Packages" ]
            , ul [ class "side-package-list" ]
                [ li []
                    [ text "General"
                    , ul []
                        [ li [ class "indent" ] [ a [ href "/packages/elm-lang/core/latest" ] [ text "core" ] ]
                        ]
                    ]
                , li []
                    [ text "Effects"
                    , ul []
                        [ li [ class "indent" ] [ a [ href "/packages/evancz/elm-http/latest" ] [ text "http" ] ]
                        , li [ class "indent" ] [ a [] [ text "geolocation" ] ]
                        , li [ class "indent" ] [ a [] [ text "navigation" ] ]
                        , li [ class "indent" ] [ a [] [ text "page-visibility" ] ]
                        , li [ class "indent" ] [ a [] [ text "websocket" ] ]
                        ]
                    ]
                , li []
                    [ text "Rendering"
                    , ul []
                        [ li [ class "indent" ] [ a [] [ text "html" ] ]
                        , li [ class "indent" ] [ a [] [ text "svg" ] ]
                        , li [ class "indent" ] [ a [] [ text "markdown" ] ]
                        ]
                    ]
                , li []
                    [ text "User Input"
                    , ul []
                        [ li [ class "indent" ] [ a [] [ text "mouse" ] ]
                        , li [ class "indent" ] [ a [] [ text "window" ] ]
                        , li [ class "indent" ] [ a [] [ text "keyboard" ] ]
                        ]
                    ]
                ]
            ]
          --, img [ class "package-svg-bottom-right", src "elm_package_logo_gold.svg" ] []
          --, img [ class "package-svg-bottom-left", src "elm_package_logo_green.svg" ] []
          --, img [ class "package-svg-top-right", src "elm_package_logo_purple.svg" ] []
          --, img [ class "package-svg-top-left", src "elm_package_logo_blue.svg" ] []
        ]
    ]


iconCount : String -> String -> Int -> Html Msg
iconCount icon label metric =
    span [ class "metric" ]
        [ i [ class <| "fa fa-" ++ icon ] []
        , text <| " " ++ toString metric ++ " " ++ label
        ]


flags : Bool -> Bool -> Html Msg
flags isPackage deprecated =
    div [ class "flags" ]
        [ deprecationWarning deprecated
          --, if isPackage then
          --    span [ class "is-package" ]
          --        [ text "elm package"
          --        ]
          --  else
          --    text ""
        ]


deprecationWarning : Bool -> Html Msg
deprecationWarning deprecated =
    if deprecated then
        span [ class "deprecation-warning" ]
            [ i [ class "fa fa-exclamation" ] []
            , text " deprecated"
            ]
    else
        text ""


has : String -> Bool -> Html Msg
has label metric =
    if metric then
        span [ class "metric" ]
            [ i [ class <| "fa fa-check green" ] []
            , text <| " " ++ label
            ]
    else
        span [ class "metric" ]
            [ i [ class <| "fa fa-ban red" ] []
            , text <| " " ++ label
            ]


type CornerStoneColoring
    = AllColors
    | Gold
    | Green
    | Purple
    | Blue
    | Grey


cornerStone : String -> CornerStoneColoring -> Html Msg
cornerStone cls coloring =
    let
        blue =
            "#60B5CC"

        gold =
            "#F0AD00"

        purple =
            "#5A6378"

        green =
            "#7FD13B"

        grey =
            "#EEEEEE"

        ( color1, color2, color3, color4 ) =
            case coloring of
                AllColors ->
                    ( blue, gold, purple, green )

                Green ->
                    ( green, green, green, green )

                Gold ->
                    ( gold, gold, gold, gold )

                Purple ->
                    ( purple, purple, purple, purple )

                Blue ->
                    ( blue, blue, blue, blue )

                Grey ->
                    ( grey, grey, grey, grey )
    in
        div [ class cls, style [ ( "z-index", "0" ) ] ]
            [ Svg.svg [ Svg.Attributes.version "1.1", Svg.Attributes.x "0px", Svg.Attributes.y "0px", Svg.Attributes.viewBox "0 0 323.1 323" ]
                [ Svg.rect
                    [ Svg.Attributes.x "65.6"
                    , Svg.Attributes.y "66.5"
                    , Svg.Attributes.transform "matrix(2.256997e-03 -1 1 2.256997e-03 -1.082 158.3655)"
                    , style [ ( "fill", color1 ) ]
                    , Svg.Attributes.width "26.5"
                    , Svg.Attributes.height "26.4"
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color1 ) ]
                    , Svg.Attributes.points "62.2,62.4 0,0 0,124.5 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color2 ) ]
                    , Svg.Attributes.points "3.2,126.2 20.2,159.6 20.2,109.2 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color1 ) ]
                    , Svg.Attributes.points "161,0 131,30 299.7,0 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color3 ) ]
                    , Svg.Attributes.points "20.2,169.2 0,129.3 0,293 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color4 ) ]
                    , Svg.Attributes.points "53.3,96.4 32.9,96.4 23.6,105.8 23.6,130.7 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color4 ) ]
                    , Svg.Attributes.points "126,30 156,0 69.4,0 39.4,30 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color2 ) ]
                    , Svg.Attributes.points "34.6,30 64.6,0 4.6,0 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color3 ) ]
                    , Svg.Attributes.points "122.6,33.4 38,33.4 64.6,60 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color4 ) ]
                    , Svg.Attributes.points "92.1,63.6 92,51.5 64.8,63.6 "
                    ]
                    []
                , Svg.polygon
                    [ style [ ( "fill", color2 ) ]
                    , Svg.Attributes.points "62.2,93 62.2,67.3 36.3,93 "
                    ]
                    []
                ]
            ]
