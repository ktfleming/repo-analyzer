module Lib
    ( maybeToEither
    ) where

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left