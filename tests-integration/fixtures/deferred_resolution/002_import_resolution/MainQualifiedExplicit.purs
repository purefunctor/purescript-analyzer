module MainQualifiedExplicit where

import ImplicitLib (ImplicitVoid, implicit) as ImplicitLib
import ExplicitLib (ExplicitVoid, explicit) as ExplicitLib

a = ImplicitLib.implicit
b = ExplicitLib.explicit

type A = ImplicitLib.ImplicitVoid
type B = ExplicitLib.ExplicitVoid
