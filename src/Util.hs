module Util (
    maybeRead
) where

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
  [] -> Nothing
  x:xs -> Just $ fst x