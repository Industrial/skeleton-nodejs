module Lib.Stream where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref as Ref
import Lib.Aff (affVersion)
import Node.Buffer (Buffer, concat)
import Node.Encoding (Encoding)
import Node.Stream (Read, Stream, Write, end, dataH, endH, writeString)

writeStringAff ∷ Stream (write ∷ Write) → Encoding → String → Aff Unit
writeStringAff stream encoding string = affVersion $ writeString stream encoding string

endAff ∷ Stream (write ∷ Write) → Aff Unit
endAff stream = affVersion $ end stream

-- Taken from HTTPurple (https://github.com/sigma-andex/purescript-httpurple/blob/623c5a4d983e60bb66ab145bd07ef3fbf11e0697/src/HTTPurple/Body.purs#L84C1-L94C1)
toBuffer :: Stream (read :: Read) -> Aff Buffer
toBuffer stream =
  makeAff \done -> do
    output <- Ref.new []
    stream # on dataH \buffer -> do
      void $ Ref.modify (_ <> [ buffer ]) output
    stream # on endH do
      body <- Ref.read output >>= concat
      done $ Right body
    pure nonCanceler
