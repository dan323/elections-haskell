module Data.Divisor.Provided(adam,hill,dean,jefferson,saintL) where

import Data.Divisor

fromRealFunction :: RealFloat a =>(a -> a) -> Divisor a
fromRealFunction f = f. fromInteger

staticDiff :: RealFloat a => a -> Divisor a
staticDiff f = fromRealFunction (+f)

adam :: RealFloat a => Divisor a
adam = fromInteger

hill :: RealFloat a => Divisor a
hill = fromRealFunction hillF
    where hillF m = sqrt (m*(m+1))

dean :: RealFloat a => Divisor a
dean = fromRealFunction deanF
    where deanF m = m*(m+1)/(m+0.5)

jefferson :: RealFloat a => Divisor a
jefferson = staticDiff 1

saintL :: RealFloat a => Divisor a
saintL = staticDiff 0.5