# monarch-syntax
This crate defines the Concrete Syntax Tree of the language using the language-agnostic [rowan](https://crates.io/crates/rowan) crate.

## CST Grammar

The following code block describes the "grammar" for the Concrete Syntax Tree, inspired by [ungrammar](https://rust-analyzer.github.io/blog/2020/10/24/introducing-ungrammar.html). PureScript is a relatively small language; for now, we commit to handwriting the typed AST and the corresponding [AstNode](https://docs.rs/rowan/latest/rowan/ast/trait.AstNode.html) instances.

```hs
Module =
  ModuleHeader
  Declaration*

ModuleHeader =
  'module'
  ModuleName
  ExportList?
  'where'
  ImportDeclaration*

ModuleName =
  #Upper ('.' #Upper)*

QualifiedPrefix =
  ( #Upper '.' )* #Upper

QualifiedName =
  QualifiedPrefix ( #Upper | #Lower )

ExportList =
  '(' ExportItem* ')'

inline ExportItem =
  'todo'

ImportDeclaration =
  'import'
  ModuleName
  ImportList?
  ( 'as' ModuleName )?

ImportList =
  '(' ImportItem* ')'

inline ImportItem =
  'todo'

inline Expression =
  'todo'

inline Type =
  'todo'

inline Pattern =
  'todo'

inline Declaration = 
  ValueDeclaration
| AnnotationDeclaration
| DataDeclaration
| TypeDeclaration
| ClassDeclaration
| InstanceDeclaration
| ForeignDataDeclaration
| ForeignValueDeclaration
| FixityDeclaration

ValueDeclaration =
  #Lower Pattern* '=' Expression

AnnotationDeclaration =
  #Lower '::' Type
```
