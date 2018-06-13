{-# LANGUAGE Arrows #-}

module Main where

import qualified Data.List as L
import FRP.Yampa as Y
import GlossYampa
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as G
import qualified Data.Map as M

pixelsPerMeter :: Float
pixelsPerMeter = 100

main :: IO ()
main = playYampa window background refreshRate mainSF
    where
        window = InWindow "Bouncing Balls" resolution (0, 0)
        resolution = (both $ round . (pixelsPerMeter*)) simulationBounds
        background = makeColor 0.443 0.772 0.811 1
        refreshRate = 60
        mainSF = inputParser >>> logic >>> drawing

type Input = Bool

inputParser :: SF (Y.Event G.Event) Input
inputParser = arr $ isEvent . mapFilterE f
    where
        f (EventKey (MouseButton LeftButton) Down _ _) = Just ()
        f _ = Nothing

logic :: SF Input Simulation
logic = switch ((constant () >>> simulation) &&& edge) (const logic)

drawing :: SF Simulation Picture
drawing = arr drawSimulation

--------------------------------------------------------
type Vec2F = (Float, Float)

simulationBounds :: Vec2F
simulationBounds = (10, 10)

simulationCenter :: Vec2F
simulationCenter = both (/2) simulationBounds

data Simulation = Simulation
    { balls :: M.Map Int Ball }

simulation :: SF () Simulation
simulation = constant () >>> feedback balls >>^ Simulation
    where balls = M.fromList $ [0..] `zip` map ball [Ball 0 (-2, 0) (2, 0) 0.5, Ball 1 (2, 0) (-2, 0) 1, Ball 2 (0, 0) (0, 0) 0.5]

feedback :: M.Map Int (SF [Collision] Ball) -> SF () (M.Map Int Ball)
feedback ballsfs = loopPre [] $ 
    proc ((), collisions) -> do
        balls <- par routing ballsfs -< collisions 
        let collisions' = collissionsSimulation $ M.elems balls
        returnA -< (balls, collisions')
            where routing collisions = M.mapWithKey (\k sf -> (filter ((k ==) . cId) collisions, sf))

drawSimulation :: Simulation -> Picture
drawSimulation = pictures . map drawBall . M.elems . balls

collissionsSimulation :: [Ball] -> [Collision]
collissionsSimulation balls = concat $ collisionsBallsBounds ++ collisionsBallsBalls
    where 
        collisionsBallsBounds = map collisionsBallBounds balls
        collisionsBallsBalls = map (uncurry collisionsBallBall) [ (x, y) | (x:ys) <- L.tails balls, y <- ys]

--------------------------------------------------------
type BallSF = SF [Collision] Ball

data Ball = Ball
    { bId :: Int
    , position :: Vec2F
    , velocity :: Vec2F
    , radius :: Float } deriving (Show)

ball :: Ball -> BallSF
ball = bouncingBall

bouncingBall :: Ball -> BallSF
bouncingBall ball = switch sf cont
    where
        sf = proc collisions -> do
            ball <- fallingBall ball -< ()
            event <- noEvent --> bounceEvent -< (collisions, ball) -- strange
            returnA -< (ball, event)

        cont (dv, b) = bouncingBall b { velocity = (velocity b ^+^ dv) }

bounceEvent :: SF ([Collision], Ball) (Y.Event (Vec2F, Ball))
bounceEvent = proc (collisions, ball) -> do
    let event = maybeToEvent $ resultingVelocity collisions
    returnA -< event `attach` ball

fallingBall :: Ball -> SF () Ball
fallingBall ball = proc () -> do
    v <- (^+^ velocity ball) ^<< integral -< (0, g)
    p <- (^+^ position ball) ^<< integral -< v
    returnA -< ball { position = p, velocity = v }
        where g = -9.81
--
drawBall :: Ball -> Picture
drawBall ball = translate pX pY $ color c $ circleSolid r
    where 
        (pX, pY) = pixelsPerMeter *^ position ball
        c = dark red
        r = radius ball * pixelsPerMeter

collisionsBallBall :: Ball -> Ball -> [Collision]
collisionsBallBall ball1 ball2
    | (fst p2 - fst p1)^2 + (snd p2 - snd p1)^2 > (r1 + r2)^2 = []
    | (v2 ^-^ v1) `dot` (p2 ^-^ p1) > 0 = []
    | otherwise = [Collision (bId ball1) dv1, Collision (bId ball2) dv2]
    where
        dv1 = (-1) *^ (((v1 ^-^ v2) `dot` (p1 ^-^ p2)) / ((norm $ p1 ^-^ p2)^2)) *^ (p1 ^-^ p2)
        dv2 = (-1) *^ (((v2 ^-^ v1) `dot` (p2 ^-^ p1)) / ((norm $ p2 ^-^ p1)^2)) *^ (p2 ^-^ p1)
        [p1, p2] = map position [ball1, ball2]
        [v1, v2] = map velocity [ball1, ball2]
        [r1, r2] = map radius [ball1, ball2]

collisionsBallBounds :: Ball -> [Collision]
collisionsBallBounds ball = collisionsBallBoundsX ++ collisionsBallBoundsY
    where
        collisionsBallBoundsX = collisionsBallBoundsHelper fst (f *** (*0)) ball
        collisionsBallBoundsY = collisionsBallBoundsHelper snd ((*0) *** f) ball
        f x = -2*x

collisionsBallBoundsHelper f g ball
    | not $ (p <= pLow && v < 0) || (p >= pHigh && v > 0) = []
    | otherwise = [Collision (bId ball) dv]  
    where 
        (pLow, pHigh) = ((r-) *** (\x->x-r)) simulationCenter
        dv = (g $ velocity ball)
        p = f $ position ball
        v = f $ velocity ball
        r = radius ball

--------------------------------------------------------
data Collision = Collision 
    { cId :: Int
    , velocityChange :: Vec2F } deriving (Show)

resultingVelocity :: [Collision] -> Maybe Vec2F
resultingVelocity [] = Nothing
resultingVelocity collisions = Just $ L.foldl1' (^+^) $ map velocityChange collisions

--------------------------------------------------------
infixr &&^
(&&^) :: Y.Arrow a => a b c -> (b -> c') -> a b (c, c')
a &&^ f = a &&& arr f

both :: (Arrow a) => a b c -> a (b, b) (c, c)
both a = a *** a

-- Fixa glider genom marken
