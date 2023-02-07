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




--- MAIN

main = Browser.element { init = init 
                       , update = update 
                       , view = view
                       , subscriptions = subscriptions }





-- MODEL

type State = Failure
           | Loading 
           | Success 

type alias Model =
  { http : State
  , jSon : State
  , listWord : String
  , word : String
  , listIntro : List Intro
  , inputContent : String
  , title : String
  }

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
init _ = ( Model Loading Loading "" "" [] "" "Guess it !" , getWordsFromFile)
-- http = Loading , jSon = Loading , listWord = "" , word = "" , listIntro = [] , inputContent = "" , title = "Guess it"




-- UPDATE

type Msg = GotText ( Result Http.Error String )
         | Word ( Maybe String , List String )
         | GotDef ( Result Http.Error (List Intro) )
         | Fill String
         | Click String


update : Msg -> Model -> ( Model , Cmd Msg )
update msg model = case msg of
  GotText result -> case result of 
                      Ok allWords -> ( {model | http = Success , listWord = allWords} , Random.generate Word (Random.List.choose (split " " allWords)) )
                      Err _ -> ( {model | http = Failure} , Cmd.none )
  
  Word maybe -> case maybe of
                  ( Maybe.Just randomWord , _) -> ( {model | word = randomWord} , getDefFromWord randomWord )
                  ( Nothing, _ ) -> ( {model | http = Failure} , Cmd.none )
  
  GotDef result -> case result of 
                      Ok data -> ( { model | jSon = Success , listIntro = data } , Cmd.none )
                      Err _ -> ( {model | jSon = Failure} , Cmd.none )

  Fill newContent -> ( {model | inputContent = newContent} , Cmd.none )        

  Click word -> if model.title == model.word then
                       ({model | title = "Guess It !"}, Cmd.none)
                     else
                       ({model | title = word}, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model = 
  case model.http of
    Failure -> div [] [ text "ERROR" ]
    Loading -> div [] [ text "...Loading..." ]
    Success -> form model ( case model.jSon of
                                     Failure -> div[] [ text "ERROR" ]
                                     Loading -> div[] [ text "...Loading..." ]
                                     Success -> ( div [style "padding-left" "300px" , style "padding-right" "300px" , style "padding-bottom" "30px" , style "text-align" "center" ,style "font-family" "avenir"] 
                                                 [ h1[] [ text (if String.toLower model.inputContent == String.toLower model.word then model.word else model.title) ]
                                                 , (ul [style "text-align" "left" , style "font-size" "15px"] (viewIntro model.listIntro)) ]) )

form : Model -> Html Msg -> Html Msg
form model stateText = 
  div [ style "text-align" "center" , style "padding-bottom" "100px" , style "font-family" "avenir" ] 
  [ stateText
  , input [style "text-align" "center" , style "margin-bottom" "10px" , style "font-size" "13px" , placeholder "Try to find me", Html.Attributes.value model.inputContent , onInput Fill] []

  , if String.toLower model.inputContent == String.toLower model.word then
        div[][ div [style "padding-top" "10px" , style "font-weight" "bold" , style "color" "green"] [text "You guessed it !"] , img [ src "http://localhost:8000/static/derek-aaron-ruell-napoleon-dynamite.gif"] [] ]
    else 
        div [] [text "" ]
  , button [ onClick (Click model.word) ] [ text "SHOW" ]
  ]

  
viewIntro : List Intro -> List (Html Msg)
viewIntro listIntro =
  case listIntro of
    [] -> []
    x :: xs -> (viewMeaning x.meanings) ++ (viewIntro xs)
     

viewMeaning : List Meaning -> List (Html Msg)
viewMeaning mean =
  case mean of
    [] -> []
    x :: xs -> [ li[] [(text x.partOfSpeech)] ] ++ [ ol[] (viewDefinition x.definitions) ] ++ (viewMeaning xs)

viewDefinition : List Definition -> List (Html Msg)
viewDefinition def =
  case def of
    [] -> []
    x :: xs -> [ li[] [(text x.definition)] ] ++ (viewDefinition xs)





-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none





-- HTTP


getWordsFromFile : Cmd Msg
getWordsFromFile =
  Http.get { url = "http://localhost:8000/static/thousand_words_things_explainer.txt" 
           , expect = Http.expectString GotText }

getDefFromWord : String -> Cmd Msg
getDefFromWord mot =
  Http.get { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ mot
           , expect = Http.expectJson GotDef defDecoder }





-- JSON


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

