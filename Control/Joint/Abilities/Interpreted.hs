module Control.Joint.Abilities.Interpreted (Interpreted (..)) where

class Interpreted t where
	{-# MINIMAL run #-}
	type Primary t a :: *
	run :: t a -> Primary t a
