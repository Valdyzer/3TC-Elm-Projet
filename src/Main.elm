module Main exposing (..)

import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (..)
import Random
import Random.List exposing (..)
import Json.Decode exposing (..)
import Bootstrap.Form.Checkbox as Checkbox





--- MAIN

main = Browser.element { init = init 
                       , update = update 
                       , view = view 
                       , subscriptions = subscriptions }





-- MODEL

type Model = Failure
           | Loading 
           | Success (List Intro)


type alias Intro =
    { word : String
    , meanings : List Meaning 
    }

type alias Meaning =
    { partOfSpeech : String
    , definitions : List Definition
    }

type alias Definition =
    { definition : String }

  
init : () -> ( Model, Cmd Msg )
init _ = (Loading, getWordsFromFile)





-- UPDATE

type Msg = GotText ( Result Http.Error String )       -- Si Result vaut Ok alors String sinon Http.Error
         | Word ( Maybe String , List String )        -- Maybe a = Just a | Nothing
         | GotDef ( Result Http.Error (List Intro) ) 


update : Msg -> Model -> ( Model , Cmd Msg )
update msg model = case msg of
  GotText result -> case result of 
                      Ok allWords -> ( Loading
                                     , Random.generate Word (Random.List.choose (split " " allWords)) )
                      Err _ -> ( Failure 
                               , Cmd.none )
  
  Word maybe -> case maybe of
                  ( Maybe.Just randomWord , _) -> ( Loading
                                                  , getDefFromWord randomWord )
                  ( Nothing, _ ) -> ( Failure 
                                    , Cmd.none )
  
  GotDef result -> case result of 
                      Ok listIntro -> ( Success listIntro
                                     , Cmd.none )
                      Err _ -> ( Failure 
                               , Cmd.none )
           




-- VIEW


view : Model -> Html Msg
view model = 
  case model of
    Failure -> div[] [ h1[] [ text "Error" ] ]
    Loading -> div[] [ text "...Loading..." ]
    Success listIntro -> viewSuccess listIntro


viewSuccess : List Intro -> Html Msg
viewSuccess listIntro = 
  div[]
    [ h1[] [ text "Guess it !" ]
    , ul[] (List.map viewIntro listIntro)
    , div[] [ input [] [] ] 
    , div[] [ Checkbox.checkbox [ Checkbox.id "myChk"
                                 , Checkbox.checked False
                                 {-- , Checkbox.onCheck MyCheckMsg -}   -- Si checked alors on applique la fonction MyCheckMsg 
                                 ]
                                 " show it " ] 
    ]
  
--viewIntro : Meaning -> Html msg
viewIntro listIntro =
  div[] 
    [ 
     ul[] (List.map viewMeaning listIntro.meanings)
    ]

--viewMeaning : Meaning -> Html msg
viewMeaning mean =
  li[] 
    [ text mean.partOfSpeech
    , ol[] (List.map viewDefinition mean.definitions)
    ]

--viewDefinition : Definition -> Html msg
viewDefinition def =
  li[] [ text def.definition ]






-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none





-- HTTP


getWordsFromFile : Cmd Msg
getWordsFromFile =
  Http.get { url = "../static/thousand_words_things_explainer.txt" 
           , expect = Http.expectString GotText }

getDefFromWord : String -> Cmd Msg
getDefFromWord mot =
  Http.get { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ mot
           , expect = Http.expectJson GotDef defDecoder }

defDecoder : Decoder (List Intro)
defDecoder = Json.Decode.list introDecoder

introDecoder : Decoder Intro
introDecoder =
    map2 Intro
        (field "word" string)
        (field "meanings" (Json.Decode.list meaningDecoder))
        
meaningDecoder : Decoder Meaning
meaningDecoder =
    map2 Meaning
        (field "partOfSpeech" string)
        (field "definitions" (Json.Decode.list definitionDecoder))

definitionDecoder : Decoder Definition
definitionDecoder =
    Json.Decode.map Definition
        (field "definition" string)