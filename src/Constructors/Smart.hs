{- |
    Module used as an example on how to use smart constructors instead of placing the handle to the user
-}
module Constructors.Smart (SortedList (getSorted), makeSortedList, minimum) where
        import Data.List (sort)
        import Prelude hiding (minimum)

        newtype SortedList = SortedList {getSorted :: [Int]}

        minimum:: SortedList -> Int
        minimum (SortedList numbers) = head numbers

        {- |
            @
            '[1,2,3,5,6,7,8]'
            {
                'numbers' = [5,3,6,2,1,7,8],
            }
            @
        -}
        makeSortedList:: [Int] -> Maybe SortedList
        makeSortedList [] = Nothing
        makeSortedList numbers = Just $ SortedList (sort numbers)

