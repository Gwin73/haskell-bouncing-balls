-- From https://github.com/rahulmutt/eta-2048
module GlossYampa (playYampa) where

import Control.Monad (when)
import Data.IORef (newIORef, writeIORef, readIORef)
import Graphics.Gloss (Display, Color, Picture, blank) 
import Graphics.Gloss.Interface.IO.Game as G (Event(..), playIO) 
import FRP.Yampa as Y (Event(..), SF, reactInit, react)

-- | Play the game in a window, updating when the value of the provided 
playYampa :: Display -- ^ The display method
          -> Color   -- ^ The background color
          -> Int     -- ^ The refresh rate, in Hertz
          -> SF (Y.Event G.Event) Picture
          -> IO ()
playYampa display color frequency mainSF = do
    picRef <- newIORef blank

    handle <- reactInit
      (return NoEvent)
      (\_ updated pic -> when updated (picRef `writeIORef` pic) >> return False)
      mainSF

    playIO display
           color
           frequency
           0
           (const $ readIORef picRef) -- An action to convert the world to a picture
           (\e t -> react handle (delta, Just (Event e)) >> return (t + delta)) -- A function to handle input events
           (\d t -> let delta' = realToFrac d - t
                   in if delta' > 0
                        then react handle (delta', Just NoEvent) >> return 0.0
                        else return (-delta')) -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced
  where
    delta = 0.01 / fromIntegral frequency