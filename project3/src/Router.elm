module Router exposing (..)

import Browser
import Html exposing (Html, div, button, text)
import StackElm exposing (Model as StackModel, Msg as StackMsg, stackView, init as stackInit, update as stackUpdate)
import QueueElm exposing (Model as QueueModel, Msg as QueueMsg, queueView, init as queueInit, update as queueUpdate)

-- Definir el modelo común
type alias Model =
    { stackModel : StackModel
    , queueModel : QueueModel
    , currentPage : Page
    }

-- Definir las páginas posibles
type Page
    = StackPage
    | QueuePage

-- Definir el mensaje común
type Msg
    = StackMsg StackMsg
    | QueueMsg QueueMsg
    | NavigateTo Page

-- Inicializar el modelo común
init : () -> (Model, Cmd Msg)
init _ =
    let
        (initialStackModel, stackCmd) = stackInit ()
        (initialQueueModel, queueCmd) = queueInit ()
    in
    ( { stackModel = initialStackModel
      , queueModel = initialQueueModel
      , currentPage = StackPage
      }
    , Cmd.batch [ Cmd.map StackMsg stackCmd, Cmd.map QueueMsg queueCmd ]
    )

-- Actualizar el modelo común
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StackMsg stackMsg ->
            let
                (updatedStackModel, stackCmd) = stackUpdate stackMsg model.stackModel
            in
            ( { model | stackModel = updatedStackModel }
            , Cmd.map StackMsg stackCmd
            )

        QueueMsg queueMsg ->
            let
                (updatedQueueModel, queueCmd) = queueUpdate queueMsg model.queueModel
            in
            ( { model | queueModel = updatedQueueModel }
            , Cmd.map QueueMsg queueCmd
            )

        NavigateTo page ->
            ( { model | currentPage = page }, Cmd.none )

-- Vista común
view : Model -> Html Msg
view model =
    case model.currentPage of
        StackPage ->
            Html.map StackMsg (stackView model.stackModel)

        QueuePage ->
            Html.map QueueMsg (queueView model.queueModel)

