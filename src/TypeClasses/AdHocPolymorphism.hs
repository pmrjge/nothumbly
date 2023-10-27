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

    data Customer = Customer {name:: String, mail:: String, email:: String}

    instance Eq Customer where
        (==) (Customer name mail email) (Customer name' mail' email') = name == name' && mail == mail' && email == email'

    instance Ord Customer where
        compare c0 c1 = compare c0.name c1.name <> compare c0.mail c1.mail <> compare c0.email c1.email

    instance Show Custimer where
        show (Customer name mail email) = "Customer {name = " <> show name <> ", " <> "mail=" <> show mail <> ", " <> "email=" <> show email <> "}"

    data CustomerD = CustomerD {name:: String, mail:: String, email:: String} deriving (Eq, Show, Ord)

    newtype Name = Name String deriving (Eq, Show, Ord)

    -- Deriving even more

    newtype USD = USD {getMillis :: Integer} deriving (Eq, Ord, Show)

    instance Num USD where
        (USD a) + (USD b) = USD (a+b)
        (USD a) * (USD b) = USD (a*b)
        abs (USD a) = USD (abs a)
        signum (USD a) = USD (signum a)
        fromInteger = USD
        negate (USD a) = USD (negate a)

    instance Real USD where
        toRational(USD a) = toRational a

    instance Enum USD where
        toEnum a = USD (toEnum a)
        fromEnum (USA a) = fromEnum a

    {- instance Integral USD where
        quotRem (USA a) (USD b) = 
            let (a', b') quotRem a b 
            in (USD a', USD b') 
        toInteger (USD a) = a -}
                            