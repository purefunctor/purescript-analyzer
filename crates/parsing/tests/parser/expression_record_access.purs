module ExpressionRecordAccess where

access = record.foo
access = record.foo.bar

keyword = record.forall
keyword = record.forall.forall

string = record."error"
string = record."error"."error"

raw_string = record."""error"""
raw_string = record."""error""".""""error""""
