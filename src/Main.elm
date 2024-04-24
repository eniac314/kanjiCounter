module Main exposing (..)

import Base64
import Browser
import Csv.Decode as Decode exposing (Decoder)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (..)
import File.Select as Select
import Html
import Html.Attributes as HtmlAttr
import Html.Events exposing (..)
import Json.Decode as JD
import Parser exposing (..)
import String.Extra
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { data : Dict String String }


view model =
    Element.layout [] <|
        column
            [ spacing 20
            , padding 15
            , Font.size 16
            , width fill
            ]
            [ Input.button
                [ pointer
                , Background.color (rgba255 177 177 177 0.5)
                , paddingXY 7 5
                , Border.rounded 5
                ]
                { onPress = Just ChooseFiles
                , label = text "Charger fichiers"
                }
            , column
                [ spacing 15 ]
                (Dict.map prettyView model.data
                    |> Dict.values
                )
            ]


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { data = Dict.empty }
    , Cmd.none
    )


type Msg
    = ChooseFiles
    | FileLoaded File (List File)
    | FileReadSuccess String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseFiles ->
            ( model, Select.files [ "text/plain", "text/csv" ] FileLoaded )

        FileLoaded file files ->
            ( model
            , List.map (\f -> Task.perform (FileReadSuccess (File.name f)) (File.toString f)) (file :: files)
                |> Cmd.batch
            )

        FileReadSuccess filename content ->
            ( { model | data = Dict.insert filename content model.data }
            , Cmd.none
            )



-------------------------------------------------------------------------------
-- Visualisation Html


prettyView filename rawData =
    let
        dataDict =
            buildSoundDictKanji
                (List.map toKanjiSoundPairs (decodeCsv rawData))

        data =
            dataDict
                |> Dict.toList
                |> List.map (\( m, occ ) -> { mora = m, occ = occ })

        headerStyle =
            [ paddingXY 7 10, Font.semiBold ]

        cellStyle i =
            [ paddingXY 7 10
            , if modBy 2 i == 0 then
                Background.color (rgba255 0 0 150 0.2)

              else
                noAttr
            , height fill
            ]

        noAttr =
            htmlAttribute <| HtmlAttr.class ""
    in
    column
        [ spacing 15 ]
        [ row [ width fill ]
            [ el [ Font.bold ] (text filename)
            , el
                [ alignRight
                , Font.color <| rgb255 0 0 200
                , Font.underline
                ]
                (html <|
                    Html.a
                        [ HtmlAttr.href <|
                            "data:application/octet-stream;charset=utf-16le;base64,"
                                ++ Base64.encode (toCsvString dataDict)
                        , HtmlAttr.download ("results-" ++ String.Extra.leftOfBack "." filename ++ ".csv")
                        , HtmlAttr.style "text-decoration" "inherit"
                        , HtmlAttr.style "font-family" "monospace"
                        ]
                        [ Html.text ("results-" ++ String.Extra.leftOfBack "." filename ++ ".csv") ]
                )
            ]
        , indexedTable
            [ width (px 800), centerX ]
            { data = data
            , columns =
                [ { header = el headerStyle <| Element.text "more"
                  , width = px 50
                  , view =
                        \i row ->
                            el (cellStyle i) <| Element.text row.mora
                  }
                , { header = el headerStyle <| Element.text "kanji / occurences"
                  , width = fill
                  , view =
                        \i row -> el (cellStyle i) <| occView (Dict.toList row.occ)
                  }
                ]
            }
        ]


rowView ( mora_, occ ) =
    row [ spacing 20 ]
        [ text mora_
        , occView (Dict.toList occ)
        ]


occView occ =
    List.map (\( k, n ) -> "(" ++ k ++ "," ++ String.fromInt n ++ ")") (List.sortBy Tuple.second occ |> List.reverse)
        |> String.join ", "
        |> (\t -> paragraph [] [ text t ])



-------------------------------------------------------------------------------
-- Decodeur pour données sous forme CSV
-- Dict String (Dict String Int) => dictionnaire Dict More (Dict Kanji Int)


csvDecoder : Decoder ( String, String )
csvDecoder =
    -- ;伊豆毛夜弊賀岐;idumwo yapyegaki => ("伊豆毛夜弊賀岐","idumwo yapyegaki")
    Decode.map3 (\_ k m -> ( k, m ))
        (Decode.column 0 (Decode.blank Decode.string))
        (Decode.column 1 Decode.string)
        (Decode.column 2 Decode.string)


decodeCsv raw =
    Decode.decodeCsv Decode.NoFieldNames csvDecoder (String.replace ";" "," raw)
        |> Result.withDefault []
        |> List.filter (\( k, _ ) -> k /= "JAPONAIS" && k /= "")



-------------------------------------------------------------------------------
-- Construction du dictionnaire de more / (kanji, occurences)


buildSoundDictKanji : List (List ( String, String )) -> Dict String (Dict String Int)
buildSoundDictKanji xs =
    List.foldr
        updateSoundDictKanji
        Dict.empty
        xs


updateSoundDictKanji : List ( String, String ) -> Dict String (Dict String Int) -> Dict String (Dict String Int)
updateSoundDictKanji xs sdj =
    let
        updateOccurence : Maybe Int -> Maybe Int
        updateOccurence =
            \mbOcc ->
                case mbOcc of
                    Nothing ->
                        Just 1

                    Just n ->
                        Just (n + 1)
    in
    List.foldr
        (\( k, m ) acc ->
            Dict.update m
                (\mbv ->
                    case mbv of
                        Nothing ->
                            Just <| Dict.insert k 1 Dict.empty

                        Just kanjiOccDict ->
                            Just <|
                                Dict.update k
                                    updateOccurence
                                    kanjiOccDict
                )
                acc
        )
        sdj
        xs


toKanjiSoundPairs : ( String, String ) -> List ( String, String )
toKanjiSoundPairs ( kanjiInput, romaji ) =
    List.map2 Tuple.pair
        (String.toList kanjiInput
            |> List.map String.fromChar
        )
        (toMorae romaji)



-------------------------------------------------------------------------------
-- Sauvegarde CSV


toCsvString : Dict String (Dict String Int) -> String
toCsvString data =
    let
        toCsvEntry : String -> Dict String Int -> String
        toCsvEntry mora_ occ =
            mora_
                ++ ";"
                ++ (Dict.toList occ
                        |> List.map (\( k, n ) -> "(" ++ k ++ "," ++ String.fromInt n ++ ")")
                        |> String.join ", "
                   )
    in
    String.join "\n" (Dict.map toCsvEntry data |> Dict.values)



-------------------------------------------------------------------------------
-- Parseur pour obtenir une liste de mores.
-- "idumwo yapyegaki" => ["i","du","mwo","ya","pye","ga","ki"]


isVowel c =
    List.member c [ 'a', 'i', 'u', 'e', 'o' ]


isConsonant =
    not << isVowel


mora : Parser String
mora =
    oneOf
        [ --parse les mores d'une voyelle
          Parser.succeed identity
            |= (chompIf isVowel
                    |> getChompedString
               )
            |> backtrackable

        --parse les mores de type consonne + voyelle
        , Parser.succeed (\c1 v -> String.join "" [ c1, v ])
            |= (chompIf isConsonant
                    |> getChompedString
               )
            |= (chompIf isVowel
                    |> getChompedString
               )
            |> backtrackable

        --parse les mores de type consonne + consonne +voyelle
        , Parser.succeed (\c1 c2 v -> String.join "" [ c1, c2, v ])
            |= (chompIf isConsonant
                    |> getChompedString
               )
            |= (chompIf isConsonant
                    |> getChompedString
               )
            |= (chompIf isVowel
                    |> getChompedString
               )
        ]


morae : Parser (List String)
morae =
    many mora


toMorae : String -> List String
toMorae input =
    String.replace " " "" input
        |> Parser.run morae
        |> Result.toMaybe
        |> Maybe.withDefault []



-------------------------------------------------------------------------------
-- tests


test =
    toMorae "idumwo yapyegaki"


test2 =
    toKanjiSoundPairs ( "伊弩毛夜覇餓岐", "idumwo yapyegaki" )


test3 =
    updateSoundDictKanji test2 Dict.empty


csv =
    """CHANT;JAPONAIS;TRANSCRIPTION
NSK1;夜句茂多莵;yakumo tatu
;伊弩毛夜覇餓岐;idumo yapyegaki
;莵磨語味爾;tumagome ni
;夜覇餓枳莵倶盧;yapyegaki tukuru
;贈廼夜覇餓岐廻;so no yapyegaki wo
NSK2;阿妹奈屡夜;ame naru ya
;乙登多奈婆多廼;ototanabata no
;汗奈餓勢屡;unagaseru
;多磨廼弥素磨屡廼;tama no misumaru no
;阿奈陁磨波夜;anatama pa ya
;弥多爾;mitani
;輔柁和柁邏須;putawatarasu
;阿泥素企多伽避顧祢;adisukitakapikwone"""



-------------------------------------------------------------------------------


many : Parser a -> Parser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p

        --|. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse vs))
        ]



-------------------------------------------------------------------------------
