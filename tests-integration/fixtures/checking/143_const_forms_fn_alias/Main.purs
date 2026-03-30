module Main where

type Fn b a = b -> a
type ConstFn a b = a -> Fn b a

fn0 :: forall a b. ConstFn a b
fn0 = \a b -> a

fn1 :: forall a b. ConstFn a b
fn1 a b = a

fn2 :: forall a b. ConstFn a b
fn2 a = \b -> a

fn3 :: forall a b. ConstFn a b
fn3 = \a -> \b -> a

fn4 :: forall b. forall a. a -> Fn b a
fn4 a b = a

fn5 :: forall b. forall a. a -> Fn b a
fn5 a = \b -> a
