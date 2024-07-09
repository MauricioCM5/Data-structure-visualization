module Tree exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Time

-- MODELO
type alias Model =
    { tree : Tree
    , animating : Bool
    , insertValue : String
    , deleteValue : String
    , findValue : String
    , searchActive : Bool
    }

type Tree
    = Empty
    | Node NodeData

type alias NodeData =
    { value : Int
    , height : Int
    , x : Float
    , y : Float
    , targetX : Float
    , targetY : Float
    , left : Tree
    , right : Tree
    , inSearchPath : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { tree = Empty
      , animating = False
      , insertValue = ""
      , deleteValue = ""
      , findValue = ""
      , searchActive = False
      }
    , Cmd.none
    )

-- ACTUALIZACIÓN
type Msg
    = InsertNode
    | DeleteNode
    | Tick Time.Posix
    | FindNode
    | UpdateInsertInput String
    | UpdateDeleteInput String
    | UpdateFindInput String
    | ResetTree


type alias SearchResult =
    { found : Maybe Int
    , path : List Int
    }
find : Int -> Tree -> SearchResult
find value tree = 
     findHelper value tree []
     
     
findHelper : Int -> Tree -> List Int -> SearchResult
findHelper value tree path =
    case tree of
        Empty ->
            { found = Nothing, path = [] }
        Node node ->
            if value == node.value then
                { found = Just node.value, path = path }
            else if value < node.value then
                findHelper value node.left (path ++ [node.value])
            else
                findHelper value node.right (path ++ [node.value])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InsertNode ->
            case String.toInt model.insertValue of
                Just value ->
                    let
                        newTree = insert value model.tree
                        (repositionedTree, _) = repositionTree newTree 150 50 100
                    in
                    ( { model | tree = repositionedTree, animating = True, insertValue = "", searchActive = False}, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        DeleteNode ->
            case String.toInt model.deleteValue of
                Just value ->
                    let
                        newTree = delete value model.tree
                        (repositionedTree, _) = repositionTree newTree 150 50 100
                    in
                    ( { model | tree = repositionedTree, animating = True, deleteValue = "", searchActive = False }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        FindNode ->
            case String.toInt model.findValue of
                Just value ->
                    let
                        searchResult = find value model.tree
                        newTree = 
                            case searchResult.found of
                                Just _ -> markSearchPath model.tree searchResult.path (Just value)
                                Nothing -> model.tree
                    in
                    ( { model | tree = newTree, findValue = "", searchActive = True }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        ResetTree ->
            init ()

        UpdateFindInput newValue ->
            ( { model | findValue = newValue }, Cmd.none )


        Tick _ ->
            if model.animating then
                let
                    (newTree, stillAnimating) = updateTreePositions model.tree
                in
                ( { model | tree = newTree, animating = stillAnimating }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateInsertInput newValue ->
            ( { model | insertValue = newValue }, Cmd.none )

        UpdateDeleteInput newValue ->
            ( { model | deleteValue = newValue }, Cmd.none )

--FOR FINDING A NODE
markSearchPath : Tree -> List Int -> Maybe Int -> Tree
markSearchPath tree path foundValue =
    case tree of
        Empty ->
            Empty
        Node node ->
            if Just node.value == foundValue then
                Node { node | inSearchPath = True, left = markSearchPath node.left path foundValue, right = markSearchPath node.right path foundValue }
            else if List.member node.value path then
                Node { node | inSearchPath = True, left = markSearchPath node.left path foundValue, right = markSearchPath node.right path foundValue }
            else
                Node { node | inSearchPath = False, left = markSearchPath node.left path foundValue, right = markSearchPath node.right path foundValue }
        
--AVL FUNCTIONS
height : Tree -> Int
height tree =
    case tree of
        Empty -> 0
        Node node -> node.height

balanceFactor : Tree -> Int
balanceFactor tree =
    case tree of
        Empty -> 0
        Node node -> height node.left - height node.right

updateHeight : NodeData -> NodeData
updateHeight node =
    { node | height = 1 + Basics.max (height node.left) (height node.right) }

rotateRight : NodeData -> Tree
rotateRight y =
    case y.left of
        Node x ->
            Node { x | right = Node { y | left = x.right, height = 1 + Basics.max (height x.right) (height y.right) }
                 , height = 1 + Basics.max (height x.left) (height y.right)
                 , targetX = y.targetX, targetY = y.targetY
                 }
        Empty -> Node y

rotateLeft : NodeData -> Tree
rotateLeft x =
    case x.right of
        Node y ->
            Node { y | left = Node { x | right = y.left, height = 1 + Basics.max (height x.left) (height y.left) }
                 , height = 1 + Basics.max (height y.right) (height x.left)
                 , targetX = x.targetX, targetY = x.targetY
                 }
        Empty -> Node x

balance : NodeData -> Tree
balance node =
    let
        newNode = updateHeight node
    in
    if balanceFactor (Node newNode) > 1 then
        case newNode.left of
            Node left ->
                if balanceFactor newNode.left <= -1 then
                    rotateRight { newNode | left = rotateLeft left }
                else
                    rotateRight newNode
            Empty -> Node newNode
    else if balanceFactor (Node newNode) < -1 then
        case newNode.right of
            Node right ->
                if balanceFactor newNode.right >= 1 then
                    rotateLeft { newNode | right = rotateRight right }
                else
                    rotateLeft newNode
            Empty -> Node newNode
    else
        Node newNode

insert : Int -> Tree -> Tree
insert value tree =
    case tree of
        Empty ->
            Node { value = value, height = 1, x = 150, y = 50, targetX = 150, targetY = 50, left = Empty, right = Empty, inSearchPath = False}
        Node node ->
            if value < node.value then
                balance { node | left = insert value node.left }
            else if value > node.value then
                balance { node | right = insert value node.right }
            else
                Node node

delete : Int -> Tree -> Tree
delete value tree =
    case tree of
        Empty -> Empty
        Node node ->
            if value < node.value then
                balance { node | left = delete value node.left }
            else if value > node.value then
                balance { node | right = delete value node.right }
            else
                case (node.left, node.right) of
                    (Empty, Empty) -> Empty
                    (left, Empty) -> left
                    (Empty, right) -> right
                    (_, _) ->
                        let
                            (minValue, newRight) = removeMin node.right
                            newNode = { node | value = minValue, right = newRight }
                        in
                        balance newNode

removeMin : Tree -> (Int, Tree)
removeMin tree =
    case tree of
        Empty -> (0, Empty)  -- This should never happen in a valid BST
        Node node ->
            case node.left of
                Empty -> (node.value, node.right)
                _ ->
                    let
                        (minValue, newLeft) = removeMin node.left
                    in
                    (minValue, balance { node | left = newLeft })

repositionTree : Tree -> Float -> Float -> Float -> (Tree, Float)
repositionTree tree x y horizontalSpacing =
    case tree of
        Empty ->
            (Empty, x)
        Node node ->
            let
                (leftTree, leftX) = repositionTree node.left (x - horizontalSpacing) (y + 50) (horizontalSpacing / 2)
                (rightTree, rightX) = repositionTree node.right (x + horizontalSpacing) (y + 50) (horizontalSpacing / 2)
                newX = (leftX + rightX) / 2
            in
            (Node { node | targetX = newX, targetY = y, left = leftTree, right = rightTree }, newX)

updateTreePositions : Tree -> (Tree, Bool)
updateTreePositions tree =
    case tree of
        Empty ->
            (Empty, False)
        Node node ->
            let
                (newLeft, leftAnimating) = updateTreePositions node.left
                (newRight, rightAnimating) = updateTreePositions node.right
                (newX, newY, nodeAnimating) = updatePosition node.x node.y node.targetX node.targetY
                newNode = { node | x = newX, y = newY, left = newLeft, right = newRight }
            in
            (Node newNode, leftAnimating || rightAnimating || nodeAnimating)

updatePosition : Float -> Float -> Float -> Float -> (Float, Float, Bool)
updatePosition x y targetX targetY =
    let
        step = 2
        newX = moveTowards x targetX step
        newY = moveTowards y targetY step
        stillAnimating = (abs (x - targetX) > 0.1) || (abs (y - targetY) > 0.1)
    in
    (newX, newY, stillAnimating)

moveTowards : Float -> Float -> Float -> Float
moveTowards current target step =
    if abs (current - target) < step then
        target
    else if current < target then
        current + step
    else
        current - step

-- VISTA
view : Model -> Html Msg
view model =
    let
        containerStyle =
            [ Html.Attributes.style "font-family" "Arial, sans-serif"
            , Html.Attributes.style "max-width" "800px"
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
            , Html.Attributes.style "background-color" "#4CAF50"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "border-radius" "4px"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "font-size" "14px"
            , Html.Attributes.style "transition" "background-color 0.3s"
            ]
        resetButtonStyle = buttonStyle ++ [ Html.Attributes.style "background-color" "#e74c3c" ]

        inputGroupStyle =
            [ Html.Attributes.style "margin-bottom" "15px"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            ]

        svgContainerStyle =
            [ Html.Attributes.style "border" "2px solid #ddd"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "margin-top" "20px"
            , Html.Attributes.style "background-color" "#fff"
            ]
    in
    div containerStyle
        [ h1 [ Html.Attributes.style "color" "#333", Html.Attributes.style "text-align" "center" ] [ Html.text "Visualizador de AVL" ] 
        ,div inputGroupStyle
            [ input ([ type_ "text", placeholder "Valor a insertar", value model.insertValue, onInput UpdateInsertInput ] ++ inputStyle) []
            , button (onClick InsertNode :: buttonStyle) [ Html.text "Insertar nodo" ]
            ]
        , div inputGroupStyle
            [ input ([ type_ "text", placeholder "Valor a borrar", value model.deleteValue, onInput UpdateDeleteInput ] ++ inputStyle) []
            , button (onClick DeleteNode :: buttonStyle) [ Html.text "Borrar nodo" ]
            ]
         , div inputGroupStyle
            [ input ([ type_ "text", placeholder "Valor a buscar", value model.findValue, onInput UpdateFindInput ] ++ inputStyle) []
            , button (onClick FindNode :: buttonStyle) [ Html.text "Buscar nodo" ]
            ]
        , div inputGroupStyle
            [ button (onClick ResetTree :: resetButtonStyle) [ Html.text "Reiniciar árbol" ]
            ]
        , div svgContainerStyle
            [ svg
                [ SvgAttr.width "100%"
                , SvgAttr.height "500"
                , SvgAttr.viewBox "0 0 600 500"  -- Aumentado el ancho del viewBox
                ]
                [ g [ SvgAttr.transform "translate(150,0)" ] 
                    [ drawTree model.searchActive model.tree ]
                ]
            ]
        ]

drawTree : Bool -> Tree -> Svg Msg
drawTree searchActive tree =
    case tree of
        Empty -> Svg.g [] []
        Node node ->
            let
                (fillColor, strokeColor) =
                    if searchActive && node.inSearchPath then
                        if Just node.value == (find node.value tree).found then
                            ("#e74c3c", "#c0392b")  
                        else
                            ("#f39c12", "#d35400")  
                    else
                        ("#3498db", "#2980b9")
            in
            Svg.g []
                ([ drawNodeLines node
                , Svg.circle
                    [ SvgAttr.cx (String.fromFloat node.x)
                    , SvgAttr.cy (String.fromFloat node.y)
                    , SvgAttr.r "20"
                    , SvgAttr.fill fillColor
                    , SvgAttr.stroke strokeColor
                    , SvgAttr.strokeWidth "2"
                    ]
                    []
                , Svg.text_
                    [ SvgAttr.x (String.fromFloat node.x)
                    , SvgAttr.y (String.fromFloat node.y)
                    , SvgAttr.textAnchor "middle"
                    , SvgAttr.dominantBaseline "central"
                    , SvgAttr.fill "white"
                    , SvgAttr.fontSize "14"
                    , SvgAttr.fontWeight "bold"
                    ]
                    [ Svg.text (String.fromInt node.value) ]
                ] ++ List.map (drawTree searchActive) [node.left, node.right])
                
drawNodeLines : NodeData -> Svg Msg
drawNodeLines node =
    Svg.g []
        (List.filterMap identity
            [ drawLineToChild node node.left
            , drawLineToChild node node.right
            ]
        )

drawLineToChild : NodeData -> Tree -> Maybe (Svg Msg)
drawLineToChild parent child =
    case child of
        Empty ->
            Nothing
        Node childNode ->
            Just (Svg.line
                [ SvgAttr.x1 (String.fromFloat parent.x)
                , SvgAttr.y1 (String.fromFloat parent.y)
                , SvgAttr.x2 (String.fromFloat childNode.x)
                , SvgAttr.y2 (String.fromFloat childNode.y)
                , SvgAttr.stroke "#34495e"
                , SvgAttr.strokeWidth "2"
                ]
                []
            )
getNodeX : Tree -> Float
getNodeX tree =
    case tree of
        Empty -> 0
        Node node -> node.x

getNodeY : Tree -> Float
getNodeY tree =
    case tree of
        Empty -> 0
        Node node -> node.y


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 16 Tick


