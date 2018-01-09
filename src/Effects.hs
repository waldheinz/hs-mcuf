module Effects (
    Params(..),
    plasma
    ) where

data Params = Params
    { paramWidth  :: Double
    , paramHeight :: Double
    , paramMaxVal :: Double
    }

type Effect = Params -> Double -> [Double]

plasma :: Effect
plasma p t = map go [ (x / width, y / height) | y <- [0 .. height - 1], x <- [0 .. width - 1] ]
    where
        width = paramWidth p
        height = paramHeight p
        maxVal = paramMaxVal p

        go :: (Double, Double) -> Double
        go (x, y) = maxVal * sum waves * wscale
            where
                wscale = 1 / fromIntegral (length waves)
                t' = sin t
                x' = x - 0.5
                y' = y - 0.5
                p = sqrt $ x' * x' + y' * y'
                waves =
                    [ 0.5 + 0.5 * sin (p * 23 + t' * 3)
                    , 0.5 + 0.5 * cos ((t + (x * y)) * 7)
                    ]
