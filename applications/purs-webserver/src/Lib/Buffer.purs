module Lib.Buffer where

import Prelude

import Effect (Effect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))

toString :: Buffer -> Effect String
toString buffer = do
  str <- Buffer.toString UTF8 buffer
  pure str
