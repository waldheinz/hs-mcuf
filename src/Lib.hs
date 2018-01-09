module Lib
    ( someFunc
    ) where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)
import qualified Data.Binary.Put            as BIN
import qualified Data.ByteString.Lazy       as BS
import           Data.List                  (transpose)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Vector                (Vector)
import           Data.Word                  (Word16, Word8)
import qualified System.Hardware.Serialport as SERIAL
import           System.IO

data Frame = Frame
    { frameWidth  :: Word16
    , frameHeight :: Word16
    , frameMaxVal :: Word16
    , framePixels :: [[Word8]]
    }

putFrame :: Frame -> BIN.Put
putFrame f = do
    -- magick
    BIN.putWord8 0x23
    BIN.putWord8 0x54
    BIN.putWord8 0x26
    BIN.putWord8 0x66

    -- height & width
    BIN.putWord16be $ frameHeight f
    BIN.putWord16be $ frameWidth f

    -- channel count
    BIN.putWord16be . fromIntegral . length .  framePixels $ f

    BIN.putWord16be $ frameMaxVal f
    mapM_ BIN.putWord8 $ concat . transpose $ framePixels f

someFunc :: IO ()
someFunc = do
    let
        port = "/dev/ttyUSB0"
        serSettings = SERIAL.defaultSerialSettings
            { SERIAL.commSpeed = SERIAL.CS115200
            , SERIAL.bitsPerWord = 8
            , SERIAL.stopb = SERIAL.One
            , SERIAL.parity = SERIAL.NoParity
            , SERIAL.flowControl = SERIAL.NoFlowControl
            }

        maxVal :: Double
        maxVal = 7

        width, height :: Double
        width = 18
        height = 8

        mkFrame :: Double -> Frame
        mkFrame t = Frame (floor width) (floor height) (floor maxVal) [ map clamp (pixels t) ]
            where
                clamp :: Double -> Word8
                clamp v
                    | v > maxVal = floor maxVal
                    | v < 0 = 0
                    | otherwise = round v

        pixels :: Double -> [Double]
        pixels t = map go [ (x / width, y / height) | y <- [0 .. height - 1], x <- [0 .. width - 1] ]
            where
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

    tStart <- getCurrentTime

    s <- SERIAL.openSerial port serSettings
    forever $ do
        now <- getCurrentTime
        sent <- SERIAL.send s $ BS.toStrict $ BIN.runPut (putFrame $ mkFrame (realToFrac $ now `diffUTCTime` tStart))
        SERIAL.flush s
        putStrLn $ "sent " ++ show sent ++ " bytes"
        threadDelay $ 25 * 1000

    SERIAL.closeSerial s
