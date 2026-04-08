{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Interpreter.Type
import Interpreter.Parser
import Interpreter.Eval
import Interpreter.Dispatch

import Excel.Type
import Excel.Basics

{-

  TODO Majoris:

    - make readFrom be optional or have None option

  Minimal viable product:
    - insert value into cell
    - move between cells
    - set ref
    - apply a basic function using ref
-}


main :: IO ()
main = undefined