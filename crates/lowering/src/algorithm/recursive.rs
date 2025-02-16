use smol_str::SmolStr;
use syntax::{cst, SyntaxToken};

use crate::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, OperatorPair, RecordItem, ResolutionDomain,
    TypeId, TypeKind, TypeRowItem, TypeVariableBinding,
};

use super::{Environment, State};

pub(super) fn lower_binder(s: &mut State, e: &Environment, cst: &cst::Binder) -> BinderId {
    let id = s.source.allocate_bd(cst);
    let kind = match cst {
        cst::Binder::BinderTyped(t) => {
            let binder = t.binder().map(|b| lower_binder(s, e, &b));
            let r#type = t.r#type().map(|t| lower_type(s, e, &t));
            BinderKind::Typed { binder, r#type }
        }
        cst::Binder::BinderOperatorChain(o) => {
            let head = o.binder().map(|b| lower_binder(s, e, &b));
            let tail = o
                .children()
                .map(|p| {
                    let qualified = p.qualified();
                    let binder = p.binder().map(|b| lower_binder(s, e, &b));
                    lower_pair(s, ResolutionDomain::Term, qualified, binder)
                })
                .collect();
            BinderKind::OperatorChain { head, tail }
        }
        cst::Binder::BinderInteger(_) => BinderKind::Integer,
        cst::Binder::BinderNumber(_) => BinderKind::Number,
        cst::Binder::BinderConstructor(c) => {
            let (qualifier, name) = c
                .name()
                .map_or((None, None), |n| lower_qualified_name(&n, cst::QualifiedName::upper));
            let resolution = s.allocate_resolution(ResolutionDomain::Term, qualifier, name);
            let arguments = c.children().map(|b| lower_binder(s, e, &b)).collect();
            BinderKind::Constructor { resolution, arguments }
        }
        cst::Binder::BinderVariable(v) => {
            let variable = v.name_token().map(|t| {
                let text = t.text();
                SmolStr::from(text)
            });
            if let Some(name) = &variable {
                s.insert_binder(name, id);
            }
            BinderKind::Variable { variable }
        }
        cst::Binder::BinderNamed(n) => {
            let named = n.name_token().map(|t| {
                let text = t.text();
                SmolStr::from(text)
            });
            if let Some(name) = &named {
                s.insert_binder(name, id);
            }
            let binder = n.binder().map(|b| lower_binder(s, e, &b));
            BinderKind::Named { named, binder }
        }
        cst::Binder::BinderWildcard(_) => BinderKind::Wildcard,
        cst::Binder::BinderString(_) => BinderKind::String,
        cst::Binder::BinderChar(_) => BinderKind::Char,
        cst::Binder::BinderTrue(_) => BinderKind::Boolean { boolean: true },
        cst::Binder::BinderFalse(_) => BinderKind::Boolean { boolean: false },
        cst::Binder::BinderArray(a) => {
            let array = a.children().map(|b| lower_binder(s, e, &b)).collect();
            BinderKind::Array { array }
        }
        cst::Binder::BinderRecord(r) => {
            let lower_item = |i| match i {
                cst::RecordItem::RecordField(f) => {
                    let name = f.name().and_then(|t| {
                        let token = t.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let value = f.binder().map(|b| lower_binder(s, e, &b));
                    RecordItem::RecordField { name, value }
                }
                cst::RecordItem::RecordPun(p) => {
                    let name = p.name().and_then(|t| {
                        let token = t.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    if let Some(name) = &name {
                        s.insert_binder(name, id);
                    }
                    RecordItem::RecordPun { name }
                }
            };
            let record = r.children().map(lower_item).collect();
            BinderKind::Record { record }
        }
        cst::Binder::BinderParenthesized(p) => {
            let parenthesized = p.binder().map(|b| lower_binder(s, e, &b));
            BinderKind::Parenthesized { parenthesized }
        }
    };
    s.associate_binder_info(id, kind);
    id
}

pub(super) fn lower_expression(
    s: &mut State,
    e: &Environment,
    cst: &cst::Expression,
) -> ExpressionId {
    let id = s.source.allocate_ex(cst);
    let kind = match cst {
        cst::Expression::ExpressionTyped(_) => todo!(),
        cst::Expression::ExpressionOperatorChain(_) => todo!(),
        cst::Expression::ExpressionInfixChain(_) => todo!(),
        cst::Expression::ExpressionNegate(_) => todo!(),
        cst::Expression::ExpressionApplicationChain(_) => todo!(),
        cst::Expression::ExpressionIfThenElse(_) => todo!(),
        cst::Expression::ExpressionLetIn(_) => todo!(),
        cst::Expression::ExpressionLambda(_) => todo!(),
        cst::Expression::ExpressionCaseOf(_) => todo!(),
        cst::Expression::ExpressionDo(_) => todo!(),
        cst::Expression::ExpressionAdo(_) => todo!(),
        cst::Expression::ExpressionConstructor(_) => todo!(),
        cst::Expression::ExpressionVariable(_) => todo!(),
        cst::Expression::ExpressionOperatorName(_) => todo!(),
        cst::Expression::ExpressionSection(_) => todo!(),
        cst::Expression::ExpressionHole(_) => todo!(),
        cst::Expression::ExpressionString(_) => todo!(),
        cst::Expression::ExpressionChar(_) => todo!(),
        cst::Expression::ExpressionTrue(_) => todo!(),
        cst::Expression::ExpressionFalse(_) => todo!(),
        cst::Expression::ExpressionInteger(_) => todo!(),
        cst::Expression::ExpressionNumber(_) => todo!(),
        cst::Expression::ExpressionArray(_) => todo!(),
        cst::Expression::ExpressionRecord(_) => todo!(),
        cst::Expression::ExpressionParenthesized(p) => {
            let parenthesized = p.expression().map(|p| lower_expression(s, e, &p));
            ExpressionKind::Parenthesized { parenthesized }
        }
        cst::Expression::ExpressionRecordAccess(_) => todo!(),
        cst::Expression::ExpressionRecordUpdate(_) => todo!(),
    };
    s.intermediate.insert_expression_kind(id, kind);
    id
}

pub(super) fn lower_type(s: &mut State, e: &Environment, cst: &cst::Type) -> TypeId {
    let id = s.source.allocate_ty(cst);
    let kind = match cst {
        cst::Type::TypeApplicationChain(a) => {
            let mut children = a.children().map(|t| lower_type(s, e, &t));
            let function = children.next();
            let arguments = children.collect();
            TypeKind::ApplicationChain { function, arguments }
        }
        cst::Type::TypeArrow(a) => {
            let mut children = a.children().map(|t| lower_type(s, e, &t));
            let argument = children.next();
            let result = children.next();
            TypeKind::Arrow { argument, result }
        }
        cst::Type::TypeConstrained(c) => {
            let mut children = c.children().map(|t| lower_type(s, e, &t));
            let constraint = children.next();
            let constrained = children.next();
            TypeKind::Constrained { constraint, constrained }
        }
        cst::Type::TypeConstructor(c) => {
            let (qualifier, name) = c
                .name()
                .map_or((None, None), |n| lower_qualified_name(&n, cst::QualifiedName::upper));
            let resolution = s.allocate_resolution(ResolutionDomain::Type, qualifier, name);
            TypeKind::Constructor { resolution }
        }
        cst::Type::TypeForall(f) => s.with_forall_scope(|s| {
            let bindings = f.children().map(|b| lower_type_variable_binding(s, e, &b)).collect();
            let r#type = f.r#type().map(|t| lower_type(s, e, &t));
            TypeKind::Forall { bindings, r#type }
        }),
        cst::Type::TypeHole(_) => TypeKind::Hole,
        cst::Type::TypeInteger(_) => TypeKind::Integer,
        cst::Type::TypeKinded(k) => {
            let mut children = k.children().map(|t| lower_type(s, e, &t));
            let r#type = children.next();
            let kind = children.next();
            TypeKind::Kinded { r#type, kind }
        }
        cst::Type::TypeOperator(o) => {
            let (qualifier, name) = o
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::operator_name))
                .unwrap_or_default();
            let resolution = s.allocate_resolution(ResolutionDomain::Type, qualifier, name);
            TypeKind::Operator { resolution }
        }
        cst::Type::TypeOperatorChain(o) => {
            let head = o.r#type().map(|t| lower_type(s, e, &t));
            let tail = o
                .children()
                .map(|p| {
                    let qualified = p.qualified();
                    let r#type = p.r#type().map(|t| lower_type(s, e, &t));
                    lower_pair(s, ResolutionDomain::Type, qualified, r#type)
                })
                .collect();
            TypeKind::OperatorChain { head, tail }
        }
        cst::Type::TypeString(_) => TypeKind::String,
        cst::Type::TypeVariable(v) => {
            let resolution = v.name_token().and_then(|t| {
                let name = t.text();
                s.resolve_type(name)
            });
            TypeKind::Variable { resolution }
        }
        cst::Type::TypeWildcard(_) => TypeKind::Wildcard,
        cst::Type::TypeRecord(r) => {
            let items = r.children().map(|i| lower_row_item(s, e, &i)).collect();
            let tail = r.tail().and_then(|t| t.r#type()).map(|t| lower_type(s, e, &t));
            TypeKind::Record { items, tail }
        }
        cst::Type::TypeRow(r) => {
            let items = r.children().map(|i| lower_row_item(s, e, &i)).collect();
            let tail = r.tail().and_then(|t| t.r#type()).map(|t| lower_type(s, e, &t));
            TypeKind::Row { items, tail }
        }
        cst::Type::TypeParenthesized(p) => {
            let parenthesized = p.r#type().map(|p| lower_type(s, e, &p));
            TypeKind::Parenthesized { parenthesized }
        }
    };
    s.associate_type_info(id, kind);
    id
}

pub(super) fn lower_forall(s: &mut State, e: &Environment, cst: &cst::Type) -> TypeId {
    if let cst::Type::TypeForall(f) = cst {
        let id = s.source.allocate_ty(cst);
        s.push_forall_scope();
        let bindings = f.children().map(|b| lower_type_variable_binding(s, e, &b)).collect();
        let r#type = f.r#type().map(|t| lower_forall(s, e, &t));
        let kind = TypeKind::Forall { bindings, r#type };
        s.associate_type_info(id, kind);
        id
    } else {
        lower_type(s, e, cst)
    }
}

fn lower_pair<T>(
    s: &mut State,
    domain: ResolutionDomain,
    qualified: Option<cst::QualifiedName>,
    element: Option<T>,
) -> OperatorPair<T> {
    let (qualifier, operator) =
        qualified.map_or((None, None), |q| lower_qualified_name(&q, cst::QualifiedName::operator));
    let resolution = s.allocate_resolution(domain, qualifier, operator);
    OperatorPair { resolution, element }
}

fn lower_qualified_name(
    cst: &cst::QualifiedName,
    token: impl Fn(&cst::QualifiedName) -> Option<SyntaxToken>,
) -> (Option<SmolStr>, Option<SmolStr>) {
    let qualifier = cst.qualifier().and_then(|q| {
        let q = q.text()?;
        let text = q.text();
        Some(SmolStr::from(text))
    });
    let name = token(cst).map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });
    (qualifier, name)
}

fn lower_type_variable_binding(
    s: &mut State,
    e: &Environment,
    cst: &cst::TypeVariableBinding,
) -> TypeVariableBinding {
    let id = s.source.allocate_tv(cst);
    let visible = cst.at().is_some();
    let name = cst.name().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });
    let kind = cst.kind().map(|k| lower_type(s, e, &k));
    if let Some(name) = &name {
        s.insert_type(name, id);
    }
    TypeVariableBinding { visible, name, kind }
}

fn lower_row_item(s: &mut State, e: &Environment, cst: &cst::TypeRowItem) -> TypeRowItem {
    let name = cst.name().and_then(|l| {
        let token = l.text()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    let r#type = cst.r#type().map(|t| lower_type(s, e, &t));
    TypeRowItem { name, r#type }
}
