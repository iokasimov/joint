module Control.Joint.Composition (Composition (..)) where

class Composition t where
	{-# MINIMAL unwrap #-}
	type Primary t a :: *
	unwrap :: t a -> Primary t a
