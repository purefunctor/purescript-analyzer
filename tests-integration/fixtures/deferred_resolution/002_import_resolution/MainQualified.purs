module MainQualified where

import ImplicitLib as ImplicitLib
import ExplicitLib as ExplicitLib

a = ImplicitLib.implicit
b = ExplicitLib.explicit

type A = ImplicitLib.ImplicitVoid
type B = ExplicitLib.ExplicitVoid
