module Backend.Reg.Register where

class Register a where
  formatName :: a -> String