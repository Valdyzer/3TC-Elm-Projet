module Main exposing (..)

import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)
import Random
import Random.List exposing (..)
import List.Extra exposing (..)


--- MAIN

main = Browser.element { init = init 
                       , update = update 
                       , view = view 
                       , subscriptions = subscriptions }



-- MODEL

type Model = Failure
           | Loading 
           | Success String
  
init : () -> ( Model, Cmd Msg )
init _ = (Loading, getWordsFromFile)




-- UPDATE

type Msg = GotText ( Result Http.Error String )       -- Si Result vaut Ok alors String sinon Http.Error
         | Word ( Maybe String , List String )        -- Maybe a = Just a | Nothing
         | GotDef( Result Http.Error String ) 
         


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
                      Ok defWord -> ( Success defWord
                                     , Cmd.none )
                      Err _ -> ( Failure 
                               , Cmd.none )
           


-- VIEW

view : Model -> Html msg
view model = 
  div []
    [ h1 [] [ text "Guess it" ] 
    , viewWord model]
  
  
viewWord model = 
  case model of
    Failure -> viewFailure
    Loading -> viewLoading
    Success a -> viewSuccess a


viewFailure : Html msg
viewFailure = text "Word not found"

viewLoading : Html msg
viewLoading = text "...Waiting..."

viewSuccess : String -> Html msg
viewSuccess a = text a




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
getDefFromWord a =
  Http.get { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ a
           , expect = Http.expectString GotDef }


