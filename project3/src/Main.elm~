module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Queue
import Stack
import Tree
import Graph_
import LinkedList

type Page
    = QueuePage
    | StackPage
    | SelectionPage 
    | TreePage
    | GraphPage
    | LinkedListPage

type alias Model =
    { currentPage : Page
    , queueModel : Queue.Model
    , stackModel : Stack.Model
    , treeModel : Tree.Model
    , graphModel : Graph_.Model
    , linkedListModel : LinkedList.Model
    }

type Msg
    = ChangePage Page
    | QueueMsg Queue.Msg
    | StackMsg Stack.Msg
    | TreeMsg Tree.Msg
    | GraphMsg Graph_.Msg
    | LinkedListMsg LinkedList.Msg
    | ShowSelectionPage  

init : () -> (Model, Cmd Msg)
init _ =
    let
        (queueModel, queueCmd) = Queue.init ()
        (stackModel, stackCmd) = Stack.init ()
        (treeModel, treeCmd) = Tree.init ()
        (graphModel, graphCmd) = Graph_.init ()
        (linkedListModel, linkedListCmd) = LinkedList.init ()
    in
    ( { currentPage = SelectionPage  -- Inicia en la página de selección
      , queueModel = queueModel
      , stackModel = stackModel
      , treeModel = treeModel
      , graphModel = graphModel
      , linkedListModel = linkedListModel
      }
    , Cmd.batch
        [ Cmd.map QueueMsg queueCmd
        , Cmd.map StackMsg stackCmd
        , Cmd.map TreeMsg treeCmd
        , Cmd.map GraphMsg graphCmd
        , Cmd.map LinkedListMsg linkedListCmd
        ]
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangePage page ->
            ({ model | currentPage = page }, Cmd.none)
        
        QueueMsg queueMsg ->
            let
                (updatedQueueModel, queueCmd) = Queue.update queueMsg model.queueModel
            in
            ({ model | queueModel = updatedQueueModel }, Cmd.map QueueMsg queueCmd)
        
        StackMsg stackMsg ->
            let
                (updatedStackModel, stackCmd) = Stack.update stackMsg model.stackModel
            in
            ({ model | stackModel = updatedStackModel }, Cmd.map StackMsg stackCmd)
            
        TreeMsg treeMsg ->
            let
                (updatedTreeModel, treeCmd) = Tree.update treeMsg model.treeModel
             in
             ({ model | treeModel = updatedTreeModel }, Cmd.map TreeMsg treeCmd)    
        GraphMsg graphMsg ->
             let
                 (updatedGraphModel, graphCmd) = Graph_.update graphMsg model.graphModel
              in
              ({ model | graphModel = updatedGraphModel }, Cmd.map GraphMsg graphCmd)
        LinkedListMsg linkedListMsg ->
             let
                 (updatedLinkedListModel, linkedListCmd) = LinkedList.update linkedListMsg model.linkedListModel
              in
              ({ model | linkedListModel = updatedLinkedListModel }, Cmd.map LinkedListMsg linkedListCmd)
        ShowSelectionPage ->
            ({ model | currentPage = SelectionPage }, Cmd.none)

viewSelectionPage : Html Msg
viewSelectionPage =
    div [ style "text-align" "center", style "padding" "20px" ]
        [ h1 [] [ text "Selecciona una Estructura de Datos" ]
        , div [ style "display" "flex", style "justify-content" "center", style "gap" "20px" ]
            [ viewStructureButton "Cola" QueuePage
            , viewStructureButton "Pila" StackPage
            , viewStructureButton "Árbol" TreePage
            , viewStructureButton "Grafo" GraphPage
            , viewStructureButton "Lista enlazada" LinkedListPage
            ]
        ]

viewStructureButton : String -> Page -> Html Msg
viewStructureButton label page =
    button 
        [ onClick (ChangePage page)
        , style "padding" "10px 20px"
        , style "font-size" "18px"
        , style "background-color" "#4CAF50"
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "5px"
        , style "cursor" "pointer"
        ]
        [ text label ]


-- VIEW
view : Model -> Html Msg
view model =
    let
        navButtonStyle =
            [ style "margin" "0 5px"
            , style "padding" "8px 12px"
            , style "background-color" "#4CAF50"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            , style "transition" "background-color 0.3s"
            ]

        activeButtonStyle =
            style "background-color" "#45a049"

        isActivePage page =
            if model.currentPage == page then
                activeButtonStyle :: navButtonStyle
            else
                navButtonStyle

        pageButton page label =
            button 
                (onClick (ChangePage page) :: isActivePage page) 
                [ text label ]
    in
    div [ style "font-family" "Arial, sans-serif" ]
        [ nav 
            [ style "padding" "15px"
            , style "background-color" "#f8f8f8"
            , style "box-shadow" "0 2px 4px rgba(0,0,0,0.1)"
            , style "margin-bottom" "20px"
            ]
            [ button 
                (onClick ShowSelectionPage :: navButtonStyle)
                [ text "Seleccionar Estructura" ]
            , pageButton QueuePage "Cola"
            , pageButton StackPage "Pila"
            , pageButton TreePage "Árbol"
            , pageButton GraphPage "Grafo"
            , pageButton LinkedListPage "Lista enlazada"
            ]
        , div 
            [ style "padding" "20px"
            , style "max-width" "800px"
            , style "margin" "0 auto"
            ]
            [ case model.currentPage of
                SelectionPage ->
                    viewSelectionPage
                QueuePage ->
                    Html.map QueueMsg (Queue.view model.queueModel)
                StackPage ->
                    Html.map StackMsg (Stack.view model.stackModel)
                TreePage ->
                    Html.map TreeMsg (Tree.view model.treeModel)
                GraphPage ->
                    Html.map GraphMsg (Graph_.view model.graphModel)
                LinkedListPage ->
                    Html.map LinkedListMsg (LinkedList.view model.linkedListModel)
            ]
        ]
        
        
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentPage of
        QueuePage ->
            Sub.map QueueMsg (Queue.subscriptions model.queueModel)
        StackPage ->
            Sub.map StackMsg (Stack.subscriptions model.stackModel)
        TreePage ->
            Sub.map TreeMsg (Tree.subscriptions model.treeModel) 
        GraphPage ->
            Sub.map GraphMsg (Graph_.subscriptions model.graphModel)
        LinkedListPage ->
            Sub.map LinkedListMsg (LinkedList.subscriptions model.linkedListModel)
        SelectionPage ->
            Sub.none  
            
-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions ---> Sub.none
        }
