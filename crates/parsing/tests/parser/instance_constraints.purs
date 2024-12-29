module InstanceConstraints where

instance Serialise a => Serialise s (Array a)
else instance (Serialise a, Serialise b) => Serialise s (Either a b)
