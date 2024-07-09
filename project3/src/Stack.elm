module Stack exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Time

-- MODELO
type alias Model =
    { stack : List StackElement
    , inputValue : String
    , nextId : Int
    , highlightTop : Bool  -- Nuevo campo para highlight
    }
    
type alias StackElement =
    { id : Int
    , value : String
    , y : Float
    , targetY : Float
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { stack = []
      , inputValue = ""
      , nextId = 0
      , highlightTop = False  -- Inicializa el highlight
      }
    , Cmd.none
    )
    
type Msg
    = Push
    | Pop
    | UpdateInput String
    | Tick Time.Posix
    | HighlightTop 
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Push ->
            if String.isEmpty model.inputValue then
                (model, Cmd.none)
            else
                let
                    newElement =
                        { id = model.nextId
                        , value = model.inputValue
                        , y = 500  -- Empieza fuera de la pantalla
                        , targetY = getTargetY 0  -- Nuevo elemento va a la parte superior
                        }
                    -- Insertar nuevo elemento y actualizar posiciones objetivo
                    newStack =
                        List.indexedMap (\index e -> { e | targetY = getTargetY (index + 1) }) (newElement :: model.stack)
                in
                ( { model 
                  | stack = newStack
                  , inputValue = ""
                  , nextId = model.nextId + 1
                  }
                , Cmd.none
                )

        Pop ->
            case model.stack of
                [] -> 
                    (model, Cmd.none)
                _ :: rest ->
                    let
                        newStack =
                            List.indexedMap (\index e -> { e | targetY = getTargetY index }) rest
                    in
                    ( { model 
                      | stack = newStack
                      }
                    , Cmd.none
                    )

        UpdateInput newValue ->
            ( { model | inputValue = newValue }, Cmd.none )

        Tick _ ->
            ( { model | stack = List.map updateElementPosition model.stack }, Cmd.none )
        HighlightTop ->
            ( { model | highlightTop = not model.highlightTop }, Cmd.none )

-- Función para calcular la posición objetivo en el sentido inverso
getTargetY : Int -> Float
getTargetY index =
    50 + (toFloat index * 60)

updateElementPosition : StackElement -> StackElement
updateElementPosition element =
    let
        step = 5
        newY = 
            if abs (element.y - element.targetY) < step then
                element.targetY
            else if element.y < element.targetY then
                element.y + step
            else
                element.y - step
    in
    { element | y = newY }

view : Model -> Html Msg
view model =
    let
        containerStyle =
            [ Html.Attributes.style "font-family" "Arial, sans-serif"
            , Html.Attributes.style "max-width" "500px"
            , Html.Attributes.style "margin" "20px auto"
            , Html.Attributes.style "padding" "20px"
            , Html.Attributes.style "background-color" "#f9f9f9"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "box-shadow" "0 2px 4px rgba(0,0,0,0.1)"
            ]

        inputStyle = 
            [ Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "margin-right" "10px"
            , Html.Attributes.style "border" "1px solid #ccc"
            , Html.Attributes.style "border-radius" "4px"
            , Html.Attributes.style "font-size" "14px"
            , Html.Attributes.style "width" "150px"
            ]

        buttonStyle = 
            [ Html.Attributes.style "padding" "8px 12px"
            , Html.Attributes.style "margin-right" "5px"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "border-radius" "4px"
            , Html.Attributes.style "background-color" "#4CAF50"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "font-size" "14px"
            ]

        svgContainerStyle =
            [ Html.Attributes.style "border" "2px solid #ddd"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "margin-top" "20px"
            , Html.Attributes.style "background-color" "#fff"
            ]
    in
    div containerStyle
        [ h1 [ Html.Attributes.style "color" "#333", Html.Attributes.style "text-align" "center" ] [ Html.text "Visualizador de Pila" ]
        , div [ Html.Attributes.style "display" "flex", Html.Attributes.style "justify-content" "center", Html.Attributes.style "margin-bottom" "15px" ]
            [ input 
                ([ type_ "text"
                , placeholder "Valor a pushear"
                , value model.inputValue
                , onInput UpdateInput
                ] ++ inputStyle)
                []
            , button (onClick Push :: buttonStyle) [ Html.text "Push" ]
            , button (onClick Pop :: buttonStyle) [ Html.text "Pop" ]
            , button (onClick HighlightTop :: buttonStyle) [ Html.text "Highlight Top" ]
            ]
        , div svgContainerStyle
            [ svg
                [ SvgAttr.width "100%"
                , SvgAttr.height "500"
                , SvgAttr.viewBox "0 0 300 500"
                ]
                (List.indexedMap (drawStackElement model) model.stack)
            ]
        ]

drawStackElement : Model -> Int -> StackElement -> Svg Msg
drawStackElement model index element =
    let
        isTop = index == 0
        fillColor = 
            if isTop && model.highlightTop then
                "#e74c3c" -- Rojo
            else
                "#3498db" -- Azul
        --Bordes
        strokeColor =
            if isTop && model.highlightTop then
                "#c0392b" 
            else
                "#2980b9" 
    in
    g []
        [ rect
            [ SvgAttr.x "50"
            , SvgAttr.y (String.fromFloat element.y)
            , SvgAttr.width "200"
            , SvgAttr.height "50"
            , SvgAttr.fill fillColor
            , SvgAttr.rx "10"  
            , SvgAttr.ry "10"
            , SvgAttr.stroke strokeColor
            , SvgAttr.strokeWidth "2"
            ]
            []
        , text_
            [ SvgAttr.x "150"
            , SvgAttr.y (String.fromFloat (element.y + 30))
            , SvgAttr.textAnchor "middle"
            , SvgAttr.fill "white"
            , SvgAttr.fontSize "18"
            , SvgAttr.fontFamily "Arial, sans-serif"
            , SvgAttr.fontWeight "bold"
            ]
            [ Svg.text element.value ]
        ]
subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 16 Tick

