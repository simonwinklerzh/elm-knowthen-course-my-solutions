module Main exposing (main)

import Debug
import Html


type alias Product =
    { name : String
    , price : Float
    , qty : Int
    , discounted : Bool
    }


type alias ProductList =
    List Product


cart : ProductList
cart =
    [ { name = "Lemon", price = 0.5, qty = 1, discounted = False }
    , { name = "Apple", price = 1.0, qty = 5, discounted = False }
    , { name = "Pear", price = 1.25, qty = 10, discounted = False }
    ]


discount : Int -> Float -> Product -> Product
discount minQty discPtc item =
    if not item.discounted && item.qty >= minQty then
        { item
            | price = item.price * (1.0 - discPtc)
            , discounted = True
        }

    else
        item


regularDiscount : Product -> Product
regularDiscount =
    discount 5 0.2


specialDiscount : Product -> Product
specialDiscount =
    discount 10 0.3


newCart : ProductList
newCart =
    List.map (specialDiscount >> regularDiscount) cart


printDiscount : ProductList -> List String
printDiscount cartList =
    List.map
        (\entry ->
            .name entry
                ++ " "
                ++ String.fromInt (.qty entry)
                ++ " "
                ++ String.fromFloat (.price entry)
        )
        cartList


main =
    let
        logCart =
            Debug.log "logCart:" cart

        logNewCart =
            Debug.log "newCart:" newCart
    in
    Html.text
        (String.join " | " (printDiscount newCart))
