{-# LANGUAGE KindSignatures #-}
module TypeClasses.AdHocPolymorphism where
    import Data.Kind
    
    {- class Natural n where
        equal:: n -> n -> Bool
        add:: n -> n -> n
        multiply:: n -> n -> n
        additiveIdentity:: n
        multiplicativeIdentity:: n
        displayAsString:: n -> String-}

    {- instance Natural Int where
        equal = (==)
        add = (+)
        multiply = (*)
        additiveIdentity = 0
        multiplicativeIdentity = 1
        displayAsString = show-}

    {- class Eq n => NaturalC n where
        add:: n -> n -> n
        multiply :: n -> n -> n
        additiveIdentity :: n
        multiplicativeIdentity :: n
        displayAsString :: n -> String -}

    class (Show n, Eq n) => Natural n where
        add:: n -> n -> n
        multiply :: n -> n -> n
        additiveIdentity :: n
        multiplicativeIdentity :: n

    instance Natural Int where
        add = (+)
        multiply = (*)
        additiveIdentity = 0
        multiplicativeIdentity = 1

    adheresToReadShowContract:: forall a. (Read a, Show a) => a -> Bool
    adheresToReadShowContract val = 
        let a = show . read @a . show $ val
            b = show val
        in a == b

    newtype MyEither a b = MyEither (Either a b)

    newtype MyEitherC a b = MyEitherC { getEither:: Either a b}

    newtype Product = Product {getProduct:: Int}

    newtype Sum = Sum {getSum:: Int}

    instance Semigroup Product where
        (Product a) <> (Product b) = Product (a * b)

    instance Monoid Product where
        mempty = Product 1

    
    toCSV:: Show a => [a] -> String
    toCSV = 
        let
            addField :: Show a => String -> a -> String
            addField s a = s <> "," <> show a

            dropLeadingComma :: String -> String
            dropLeadingComma s = case s of
                ',':s' -> s'
                _ -> s
        in dropLeadingComma . foldl addField ""

    toCSV2 :: forall (t:: Type -> Type)(a::Type) . (Foldable t, Show a) => t a -> String
    toCSV2 =
        let addField:: Show a => String -> a -> String
            addField s a = s <> "," <> show a

            dropLeadingComma:: String -> String
            dropLeadingComma s = 
                case s of
                    ',' : s' -> s'
                    _ -> s
        in dropLeadingComma . foldl addField ""

    
    class Select (f::Type -> Type) where
        empty :: f a
        pick :: f a -> f a -> f a

    instance Select Maybe where
        empty = Nothing
        pick Nothing a = a
        pick a _ = a

    instance Select [] where
        empty = []
        pick = (<>)

    