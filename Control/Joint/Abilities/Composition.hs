module Control.Joint.Abilities.Composition (Composition (..)) where

class Composition t where
	{-# MINIMAL run #-}
	type Primary t a :: *
	run :: t a -> Primary t a
