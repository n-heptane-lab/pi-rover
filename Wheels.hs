module Main where

import Control.Applicative ((<$>), pure)
import Control.Concurrent (threadDelay)
import Data.Bits ((.&.), complement, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import Data.Word  (Word8, Word16)
import System.RaspberryPi.GPIO (Address, withGPIO, withI2C, writeI2C, readI2C, writeReadRSI2C)

type Register = Word8
type Word12 = Word16

writeI2CReg8 :: Address -> Register -> Word8 -> IO ()
writeI2CReg8 addr reg datum =
  writeI2C addr (B.pack  [reg, datum])

readI2CReg8 :: Address -> Register -> IO Word8
readI2CReg8 addr reg =
  B.head <$> writeReadRSI2C addr (B.singleton reg) 1

data Bit
  = High
  | Low
  deriving (Eq, Ord, Read, Show)

-- 16-channel, 12-bit PWM Fm+ I2C-bus LED controller
-- https://cdn-shop.adafruit.com/datasheets/PCA9685.pdf

data Cycle
  = On
  | Off
    deriving (Eq, Ord, Read, Show)

-- Each LED has 4 control bytes associated with it
--
-- LEDn_ON_L
-- LEDn_ON_H
-- LEDn_OFF_L
-- LEDn_OFF_H
--
-- This calculates the register number for the LEDn_ON_L
ledBaseRegister :: Word8 -> Word8
ledBaseRegister led = (led * 4) + 6

-- https://www.sparkfun.com/datasheets/Robotics/TB6612FNG.pdf

data MotorMode
  = ShortBrake
  | CCW
  | CW
  | Stop
  | Standby
  deriving (Eq, Ord, Read, Show)


calcMotorMode :: Bit -- ^ IN1
              -> Bit -- ^ IN2
              -> Bit -- ^ PWM
              -> Bit -- ^ STBY
              -> MotorMode
calcMotorMode High High _    High = ShortBrake
calcMotorMode Low  High High High = CCW
calcMotorMode Low  High Low  High = ShortBrake
calcMotorMode High Low  High High = CW
calcMotorMode High Low  Low  High = ShortBrake
calcMotorMode Low  Low  High High = Stop
calcMotorMode _    _    _    Low  = Standby

modeToPins :: MotorMode -> (Bit, Bit, Bit, Bit)
modeToPins ShortBrake = (High, High, Low , High)
modeToPins CCW        = (Low , High, High, High)
modeToPins CW         = (High, Low , High, High)
modeToPins Stop       = (Low , Low , High, High)
modeToPins Standby    = (Low , Low , Low , Low )

-- each motor is wired to 3 output channels of the LED PWM chip
--
-- The in2/in1 bits are used to control motor direction. They do not
-- need PWM, so we will set there PWM cycles to either be completely
-- on or completely off.
data Motor = Motor
  { pwm :: Word8
  , in2 :: Word8
  , in1 :: Word8
  }

motor1 :: Motor
motor1 = Motor
  { pwm = 8
  , in2 = 9
  , in1 = 10
  }

motor2 :: Motor
motor2 = Motor
  { pwm = 13
  , in2 = 12
  , in1 = 11
  }

motor3 :: Motor
motor3 = Motor
  { pwm = 2
  , in2 = 3
  , in1 = 4
  }


motor4 :: Motor
motor4 = Motor
  { pwm = 7
  , in2 = 6
  , in1 = 5
  }

setMotorMode
  :: Address
  -> Motor
  -> MotorMode
  -> IO ()
setMotorMode addr motor motorMode =
  do let (in1V, in2V, _, _) = modeToPins motorMode
     setPin addr (in1 motor) in1V
     setPin addr (in2 motor) in2V

setSpeed
 :: Address
 -> Motor
 -> Word12
 -> IO ()
setSpeed addr motor speed =
  do setPWM addr (pwm motor) 0 speed
 
setPin :: Address -> Word8 -> Bit -> IO ()
setPin addr pin bit
  | pin > 15 = do putStrLn $ "setPin: pin " ++ show pin ++ " out of range. Must be 0-15."
                  pure ()
  | otherwise =
    case bit of
      Low  -> setPWM addr pin 0 4096 -- JAS: these numbers seem out of range.
      High -> setPWM addr pin 4096 0

setPWM :: Address
       -> Word8
       -> Word12
       -> Word12
       -> IO ()
setPWM addr channel on off =
  do writeI2CReg8 addr ((ledBaseRegister channel)    ) (fromIntegral (on  .&. 0xFF))
     writeI2CReg8 addr ((ledBaseRegister channel) + 1) (fromIntegral (on  `shiftR` 8))
     writeI2CReg8 addr ((ledBaseRegister channel) + 2) (fromIntegral (off .&. 0xFF))
     writeI2CReg8 addr ((ledBaseRegister channel) + 3) (fromIntegral (off `shiftR` 8))

setAllPWM
  :: Address
  -> Word8
  -> Word8
  -> IO ()
setAllPWM addr on off =
  do writeI2CReg8 addr allLedOnL  (fromIntegral (on  .&. 0xFF))
     writeI2CReg8 addr allLedOnH  (fromIntegral (on  `shiftR` 8))
     writeI2CReg8 addr allLedOffL (fromIntegral (off .&. 0xFF))
     writeI2CReg8 addr allLedOffH (fromIntegral (off `shiftR` 8))

-- registers
mode1 = 0x00
mode2 = 0x00
allLedOnL = 0xFA
allLedOnH = 0xFB
allLedOffL = 0xFC
allLedOffH = 0xFD

-- bits
allcall = 0x01
outdrv  = 0x04
sleep   = 0x10



withMotorHat addr f =
  do setAllPWM addr 0 0
     writeI2CReg8 addr mode2 outdrv  -- The 16 LEDn outputs are configured with a totem pole structure.
     writeI2CReg8 addr mode1 allcall -- PCA9685 responds to LED All Call I2C-bus address.
     threadDelay 5000                -- wait for oscillator
     m1 <- readI2CReg8 addr mode1    -- get current mode1 value
     writeI2CReg8 addr mode1 (m1 .&. (complement sleep)) -- set sleep bit to 0, aka normal mode
     threadDelay 5000                -- wait for oscillator
     f

main :: IO ()
main =
  do let addr = 0x60
--      freq = 1600
     withGPIO $ withI2C $ withMotorHat addr $ do
       threadDelay (2 * 10^6)
       setSpeed addr motor1 4095
       setSpeed addr motor2 4095
       setSpeed addr motor3 4095
       setSpeed addr motor4 4095       
       setMotorMode addr motor1 CW
       setMotorMode addr motor2 CW
       setMotorMode addr motor3 CW
       setMotorMode addr motor4 CW
       threadDelay (2 * 10^6)
       setMotorMode addr motor1 CCW
       threadDelay (2 * 10^6)
       setSpeed addr motor1 0
       setMotorMode addr motor1 Stop
       setMotorMode addr motor2 Stop
       setMotorMode addr motor3 Stop
       setMotorMode addr motor4 Stop       

