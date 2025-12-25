module Webb.Boundary.Gen.Lang where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Webb.Boundary.Data.Param (Param)
import Webb.Boundary.Data.Param as Param


{- The typeclass representation of a _language_. And since all the generative types are related, even to the Lang itself, we define it all in this one space. -}


-- A language defines how to convert params (types) to a GParam. And it
-- defines how to write each generative structure to a string.
class Lang a where
  param :: a -> Param -> GParam
  writeParam :: a -> GParam -> String
  writeMethod :: a -> GMethod -> String
  writeInterface :: a -> GInterface -> String
  writeRecord :: a -> GRecord -> String
  writeAlias :: a -> GAlias -> String

newtype Lang_ = Lang__ (forall r. (forall z. Lang z => z -> r) -> r)

wrap :: forall z. Lang z => z -> Lang_
wrap z = Lang__ (_ $ z)

instance Lang (Lang_) where 
  param (Lang__ run) = run param
  writeParam (Lang__ run) = run writeParam
  writeMethod (Lang__ run) = run writeMethod
  writeInterface (Lang__ run) = run writeInterface
  writeRecord (Lang__ run) = run writeRecord
  writeAlias (Lang__ run) = run writeAlias

isAsync :: forall a. Lang a => a -> Param -> Boolean
isAsync _ param = Param.isAff param

newtype GParam = KP GParam_

type GParam_ = 
  { name :: String
  , args :: Array GParam
  }
  
derive instance Eq GParam
derive instance Ord GParam
derive instance Newtype GParam _
derive instance Generic GParam _
instance Show GParam where
  show (KP param) = show
    { name: param.name
    , args: show param.args
    }
    
newtype GMethod = KM 
  { name :: String
  , args :: Array MethodParam
  , return :: GParam
  , isAsync :: Boolean
  }
  
type MethodParam = String /\ GParam

derive newtype instance Eq GMethod
derive newtype instance Ord GMethod
derive newtype instance Show GMethod
derive instance Newtype GMethod _

newtype GInterface = KI
  { name :: String
  , methods :: Array GMethod
  }
  
derive newtype instance Eq GInterface
derive newtype instance Ord GInterface
derive newtype instance Show GInterface
derive instance Newtype GInterface _

newtype GAlias = KA
  { name :: String
  , param :: GParam
  }
  
derive newtype instance Eq GAlias
derive newtype instance Ord GAlias
derive newtype instance Show GAlias
derive instance Newtype GAlias _

newtype GRecord = KDC 
  { name :: String
  , params :: Array RecordPair
  }
  
type RecordPair = String /\ GParam
  
derive newtype instance Eq GRecord
derive newtype instance Ord GRecord
derive newtype instance Show GRecord
derive instance Newtype GRecord _