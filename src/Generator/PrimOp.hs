module Generator.PrimOp where

import StgSyn as Stg
import PrimOp
import Generator.Helpers
import Javascript.Language as Js

primitiveOperation :: Javascript js => Expression js -> PrimOp -> [StgArg] -> js
primitiveOperation obj CharGtOp [a, b] = jsBoolOp obj ">"  a b
primitiveOperation obj CharGeOp [a, b] = jsBoolOp obj ">=" a b
primitiveOperation obj CharEqOp [a, b] = jsBoolOp obj "==" a b
primitiveOperation obj CharNeOp [a, b] = jsBoolOp obj "!=" a b
primitiveOperation obj CharLtOp [a, b] = jsBoolOp obj "<"  a b
primitiveOperation obj CharLeOp [a, b] = jsBoolOp obj "<=" a b
primitiveOperation obj OrdOp    [a]    = Js.assign obj (Js.callMethodE (stgArgToJs a) "charCodeAt" [Js.int (0 :: Int)])
primitiveOperation obj IntAddOp [a, b] = jsOp obj "+" a b
primitiveOperation obj IntSubOp [a, b] = jsOp obj "-" a b
primitiveOperation obj IntMulOp [a, b] = jsOp obj "*" a b
primitiveOperation obj IntMulMayOfloOp [a, b] = jsOp obj "*" a b
primitiveOperation obj IntQuotOp [a, b] = Js.assign obj $ jsFloor a b
primitiveOperation obj IntRemOp [a, b] = Js.assign obj $ Js.binaryOp "-" (stgArgToJs a) (Js.binaryOp "*" (jsFloor a b) (stgArgToJs b))
primitiveOperation obj IntNegOp [a] = Js.assign obj $ Js.unaryOp "-" (stgArgToJs a)
primitiveOperation obj IntAddCOp [a, b] = jsOp obj "+" a b
primitiveOperation obj IntSubCOp [a, b] = jsOp obj "-" a b
primitiveOperation obj IntGtOp [a, b] = jsBoolOp obj ">"  a b
primitiveOperation obj IntGeOp [a, b] = jsBoolOp obj ">=" a b
primitiveOperation obj IntEqOp [a, b] = jsBoolOp obj "==" a b
primitiveOperation obj IntNeOp [a, b] = jsBoolOp obj "!=" a b
primitiveOperation obj IntLtOp [a, b] = jsBoolOp obj "<"  a b
primitiveOperation obj IntLeOp [a, b] = jsBoolOp obj "<=" a b
primitiveOperation obj ChrOp [a] = Js.assign obj $ Js.callMethodE (Js.var "String") "fromCharCode" [stgArgToJs a]
primitiveOperation obj IndexOffAddrOp_Char [a, b] = Js.assign obj $ Js.subscript (stgArgToJs a) (stgArgToJs b)
primitiveOperation obj DataToTagOp [a] = Js.assign obj $ Js.property (stgArgToJs a) "tag"
primitiveOperation _ op _ = Js.unsafeStringToP alert
  where alert = concat ["$hs.alert ('primitive operation ", show op, ". Not implemeted yet.')"]

jsFloor :: Javascript js => StgArg -> StgArg -> Expression js
jsFloor a b = Js.callMethodE (Js.var "Math") "floor" [Js.binaryOp "/" (stgArgToJs a) (stgArgToJs b)]

jsOp :: Javascript js => Expression js -> String -> StgArg -> StgArg -> js
jsOp obj op a b = Js.assign obj (Js.binaryOp op (stgArgToJs a) (stgArgToJs b))

jsBoolOp :: Javascript js => Expression js -> String -> StgArg -> StgArg -> js 
jsBoolOp obj op a b
  = ifelse (Js.binaryOp op (stgArgToJs a) (stgArgToJs b))
           (Js.assign obj (Js.new (Js.var "$hs.Data") [Js.int (2 :: Int)]))
           (Js.assign obj (Js.new (Js.var "$hs.Data") [Js.int (1 :: Int)]))


