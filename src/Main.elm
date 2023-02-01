module Main exposing (..)

import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
init _ = (Loading, getWords)



-- UPDATE

type Msg = GotText (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd msg )
update msg model = case msg of
  GotText result -> case result of 
               Ok allWords -> (Success allWords, Cmd.none)
               Err _ -> (Failure, Cmd.none)
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
    Success allWords -> viewSuccess allWords


viewFailure : Html msg
viewFailure = text "Word not found"

viewLoading : Html msg
viewLoading = text "...Waiting..."

viewSuccess : String -> Html msg
viewSuccess allWords = text allWords




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- HTTP

getWords =
  Http.get { url = "../static/thousand_words_things_explainer.txt" 
           , expect = Http.expectString GotText }
  