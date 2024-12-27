module BinderRecord where

record {} = 0
record {x} = 0
record {x, y} = 0
record {x: y, y: x} = 0
record {a, b: c, d, e: f} = 0
