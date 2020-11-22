module FixityTest where 

import Relude 

data Expr = 
    LitS String
    |
    LitN Natural
    | App Expr Expr
    deriving (Show, Eq)

tst = LitS "123" `App` LitS "234" `App` LitN 66