module Maybes where
import GHC.IO.Encoding.UTF16 (mkUTF16)

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes mx my = (+) <$> mx <*> my

m1 :: Maybe Int
m1 = Just 1

m2 :: Maybe Int
m2 = Just 2