module Main where

import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Warp as JSaddle
import Miso
import Miso.String

type Model = Int

defaultModel :: Model
defaultModel = 0

data Event
    = NoEvent
    | Inc
    | Dec
    | SayHello
    deriving (Eq, Show)

update' :: Event -> Model -> Effect Event Model
update' event model = case event of
    NoEvent -> noEff model
    Inc     -> noEff $ succ model
    Dec     -> noEff $ pred model
    SayHello -> model <# do
        liftIO $ putStrLn "Hello!"
        pure NoEvent

view' :: Model -> View Event
view' model = div_ []
    [ text "Hello ~ Haskell GUI"
    , br_ []
    , button_ [ onClick Inc ] [ text "+" ]
    , text $ ms model
    , button_ [ onClick Dec ] [ text "-" ]
    , br_ []
    , button_ [ onClick SayHello ] [ text "Say Hello!" ]
    ]

main :: IO ()
main = do
    putStrLn "Working on http://localhost:8000"

    JSaddle.run 8000 $ startApp $ App
        { initialAction = NoEvent
        , model         = defaultModel
        , update        = update'
        , view          = view'
        , events        = defaultEvents
        , subs          = []
        , mountPoint    = Nothing
        }
