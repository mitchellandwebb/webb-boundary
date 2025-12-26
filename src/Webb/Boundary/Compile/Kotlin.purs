module Webb.Boundary.Compile.Kotlin where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)


newtype KotlinParams = KP KotlinParams_

type KotlinParams_ =
  { package :: String -- What is the kotlin package name?
  }
  
derive newtype instance Eq KotlinParams
derive newtype instance Ord KotlinParams
derive newtype instance Show KotlinParams
derive instance Newtype KotlinParams _
  
newKotlinParams :: KotlinParams_ -> KotlinParams
newKotlinParams = wrap

package :: KotlinParams -> String
package = unwrap >>> _.package
