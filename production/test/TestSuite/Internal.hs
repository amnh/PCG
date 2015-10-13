module TestSuite.Internal where

isLeft, isRight :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
isRight = not . isLeft

leftMay  :: Either a b  -> Maybe a
leftMay (Left x) = Just x
leftMay _        = Nothing

rightMay :: Either a b  -> Maybe b
rightMay (Right x) = Just x
rightMay _         = Nothing

