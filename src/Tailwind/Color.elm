module Tailwind.Color exposing
    ( Color
    , arbitraryRgb
    , arbitraryRgba
    , Opacity(..)
    , withOpacity
    , arbitraryOpacityPct
    , propertyWithColor
    )

{-|


## Tailwind Color and Opacitites Module

This module contains some internal utilities, which elm-tailwind-modules-generated code
uses as well as some functions for generating custom colors and opacities for use with
elm-tailwind-modules-generated code.


### Colors

@docs Color
@docs arbitraryRgb
@docs arbitraryRgba


### Opacities

@docs Opacity
@docs withOpacity
@docs arbitraryOpacityPct


### Internal

These functions are only meant to be used in generated code.

@docs propertyWithColor

-}

import Css


{-| The type for tailwind colors.

You should never need to construct values of this type manually.
If you find the need to do so, use `arbitraryRgb` or similar functions instead.

Values of this type can usually be found in your elm-tailwind-modules-generated
`Theme.elm` module.

They can be used with tailwind utility functions like `bg_color`.

-}
type Color
    = Color String String String String Opacity
    | Keyword String


{-| The type for tailwind opacities.

You should never construct values of this type manually.
If you find the need to do so, use `arbitraryOpacityPct` instead.

Values of this type can usually be found in your elm-tailwind-modules-generated
`Theme.elm` module.

They can be used to modify the default opacities associated with colors
using the `withOpacity` function.

-}
type Opacity
    = Opacity String
    | ViaVariable


{-| Attach an opacity to a color.
-}
withOpacity : Opacity -> Color -> Color
withOpacity opacity color =
    case color of
        Keyword k ->
            Keyword k

        Color mode r g b _ ->
            Color mode r g b opacity


{-| Construct a Color value from red, green, and blue values (each between 0 and 255).
-}
arbitraryRgb : Int -> Int -> Int -> Color
arbitraryRgb r g b =
    Color "rgb" (String.fromInt r) (String.fromInt g) (String.fromInt b) ViaVariable


{-| Construct a Color value from red, green, and blue values (each between 0 and 255)
and an opacity value between 0 and 1.
-}
arbitraryRgba : Int -> Int -> Int -> Float -> Color
arbitraryRgba r g b alpha =
    Color "rgba" (String.fromInt r) (String.fromInt g) (String.fromInt b) (Opacity (String.fromFloat alpha))


{-| Construct an Opacity value from a given percentage (between 0 and 100),
where 0 means transparent and 100 means opaque.
-}
arbitraryOpacityPct : Int -> Opacity
arbitraryOpacityPct pct =
    Opacity (String.fromInt pct ++ "%")


{-| This is an internal function used by elm-tailwind-modules to generate the
tailwind utilities in `Utilities.elm`.

You should never need to use these yourself.

-}
propertyWithColor : String -> (String -> String) -> Maybe String -> Color -> Css.Style
propertyWithColor property embedColor opacityVarName color =
    case color of
        Color mode r g b opacity ->
            case ( opacity, opacityVarName ) of
                ( Opacity op, _ ) ->
                    Css.property property (embedColor (mode ++ "(" ++ r ++ " " ++ g ++ " " ++ b ++ " / " ++ op ++ ")"))

                ( ViaVariable, Just varName ) ->
                    Css.batch
                        [ Css.property varName "1"
                        , Css.property property (embedColor (mode ++ "(" ++ r ++ " " ++ g ++ " " ++ b ++ " / var(" ++ varName ++ "))"))
                        ]

                ( ViaVariable, Nothing ) ->
                    Css.property property (embedColor (mode ++ "(" ++ r ++ " " ++ g ++ " " ++ b ++ " / 1.0)"))

        Keyword keyword ->
            Css.property property keyword
