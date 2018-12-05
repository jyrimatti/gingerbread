#!/usr/bin/env runhaskell

module Main where

import System.GPIO.Linux.Sysfs 
import System.GPIO.Monad
import Control.Concurrent (threadDelay)
import Numeric.Natural
import Data.Monoid ((<>))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

r1 = Pin 2
r2 = Pin 3
r3 = Pin 4
r4 = Pin 10
r5 = Pin 9
g1 = Pin 15
g2 = Pin 8
g3 = Pin 7
g4 = Pin 1
g5 = Pin 12
b1 = Pin 11
b2 = Pin 5
b3 = Pin 6
b4 = Pin 13
b5 = Pin 14

reds   = [r1, r2, r3, r4, r5]
greens = [g1, g2, g3, g4, g5]
blues  = [b1, b2, b3, b4, b5]

treeRed   = [r2, r3]
treeGreen = [g1]
treeBlue  = [b1, b3]

eavesRed   = [r4, r5, r1]
eavesGreen = [g4, g3, g5, g2]
eavesBlue  = [b4, b5, b2]

tree = treeRed <> treeGreen <> treeBlue

eavesFront = [g4, b4, r4]
eavesSide  = [g3, b5, r5, g5, b2, r1]
eaves = eavesFront <> eavesSide

allLights = reds <> greens <> blues

type DelayMs = Natural
type Step = ([Pin],DelayMs)

steps = [(reds, 1000)
        ,(greens, 1000)
        ,(blues, 1000)
        ,(reds, 500)
        ,(greens, 500)
        ,(blues, 500)] <> blinkSlow tree <> scroll eaves <> scroll (reverse eaves) <> blinkSlow tree

blink      pins = [(pins, 250), ([], 50)]
blinkFast  pins = [(pins, 100), ([], 50)]
blinkSlow  pins = [(pins, 1000), ([], 50)]

scroll     pins = fmap (\p -> ([p],250)) pins <> [([],50)]
scrollFast pins = fmap (\p -> ([p],100)) pins <> [([],50)]
scrollSlow pins = fmap (\p -> ([p],1000)) pins <> [([],50)]

main = runSysfsGpioIO $ do
  traverse (\pin -> openPin pin >>= \h -> setPinOutputMode h OutputDefault Low) allLights
  forever $ traverse runStep steps

runStep (pins,ms) = do
    traverse unset allLights
    traverse set pins
    liftIO $ threadDelay (ms*1000)

set = drive High
unset = drive Low

drive value pin = do
  h <- openPin pin -- shamelessly get the global, already-opened, resource!
  writePin h value
