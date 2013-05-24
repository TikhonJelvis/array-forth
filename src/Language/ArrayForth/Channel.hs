{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Defines the basic operations for reading and writing through ports.
--
-- Each core has four ports connecting it to its neighbors. The cores
-- around the edges have ports connected to IO devices. A "Channel" is
-- just a type containing the four ports that you can write to or read
-- from.
module Language.ArrayForth.Channel where

import           Control.Applicative        ((<|>))

import           Data.Bits                  (testBit)
import           Data.Monoid                (Monoid (..))

import           Language.ArrayForth.Opcode (F18Word)

-- | A channel representing the four communication directions a core
-- may use. In practice, these will either be hooked up to other cores
-- or to IO. Nothing represents no message; if there is a word,
-- execution will block.
data Channel = Channel { right, down, left, up :: Maybe F18Word } deriving (Show, Eq)

-- | The four possible port directions. 
data Port = R | D | L | U deriving (Show, Eq, Bounded, Enum)

-- The monoid instance is based around *replacement*.
instance Monoid Channel where
  mempty = emptyChannel
  c₁ `mappend` c₂ = Channel { right = right c₁ <|> right c₂
                            , down  = down c₁  <|> down c₂
                            , left  = left c₁  <|> left c₂
                            , up    = up c₁    <|> up c₂ }

-- | An empty channel has no reads or writes and doesn't block execution.
emptyChannel :: Channel
emptyChannel = Channel Nothing Nothing Nothing Nothing

-- | Write to the ports specified by the given memory address. This
-- will clear all the channels not being written to (by setting them
-- to Nothing).
--
-- The ports to use are specified by bits 5–8 of the address. These
-- bits correspond respectively to up, left, down and right. Bits 5
-- and 7 are inverted—0 turns the channel *on*.
writePort :: F18Word    -- ^ The address to write to. Only bits 5–8 are considered.
             -> F18Word -- ^ The word to write to the channel.
             -> Channel -- ^ The resulting channel, with any unused ports empty.
writePort ports word = Channel { right = [ word |     testBit ports 8 ]
                               , down  = [ word | not $ testBit ports 7 ]
                               , left  = [ word |     testBit ports 6 ]
                               , up    = [ word | not $ testBit ports 5 ] }

-- | Read the inputs from the ports specified by the given
-- address. The address is handled the same way as in
-- @'writePort'@. Returns @Nothing@ if blocked on the read.
--
-- If more than one of the read ports has data, this currently just
-- chooses the first one based on the right, down, left, up order. I
-- don't know if this is the correct behavior—perhaps I should just
-- xor them together or something?
readPort :: F18Word -> Channel -> Maybe F18Word
readPort ports Channel {..} =  [ word |     testBit ports 8, word <- right ]
                           <|> [ word | not $ testBit ports 7, word <- down  ]
                           <|> [ word |     testBit ports 6, word <- left  ]
                           <|> [ word | not $ testBit ports 5, word <- up    ]
