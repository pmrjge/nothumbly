module TypeClasses.AdHocPolymorphism where
    
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

    
    