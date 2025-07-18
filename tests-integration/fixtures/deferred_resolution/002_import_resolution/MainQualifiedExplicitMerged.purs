module MainQualifiedExplicitMerged where

import ImplicitLib (ImplicitVoid, implicit) as L
import ExplicitLib (ExplicitVoid, explicit) as L

a = L.implicit
b = L.explicit

type A = L.ImplicitVoid
type B = L.ExplicitVoid
