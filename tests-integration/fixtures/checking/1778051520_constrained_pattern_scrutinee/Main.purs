module Main where

data Box a = Box a

class Route route where
  method :: Box route

routeCase :: forall @route. Route route => route
routeCase = case method @route of Box value -> value

routeLet :: forall @route. Route route => route
routeLet = let Box value = method @route in value

routeGuard :: forall @route. Route route => route
routeGuard | Box value <- method @route = value
routeGuard = routeCase @route
