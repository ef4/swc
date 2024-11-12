// This is not a public api.
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(clippy::all)]
#![allow(clippy::ptr_arg)]

#[doc(hidden)]
pub extern crate swc_ecma_ast;

use std::{borrow::Cow, fmt::Debug};

use swc_common::{pass::CompilerPass, util::take::Take, Span, DUMMY_SP};
use swc_ecma_ast::*;
use swc_visit::{Repeat, Repeated};

pub use crate::generated::*;
mod generated;

pub fn fold_pass<V>(pass: V) -> FoldPass<V>
where
    V: Fold,
{
    FoldPass { pass }
}

pub struct FoldPass<V> {
    pass: V,
}

impl<V> Pass for FoldPass<V>
where
    V: Fold,
{
    #[inline(always)]
    fn process(&mut self, program: &mut Program) {
        program.map_with_mut(|p| p.fold_with(&mut self.pass));
    }
}

impl<V> Fold for FoldPass<V>
where
    V: Fold,
{
    #[inline(always)]
    fn fold_program(&mut self, node: Program) -> Program {
        self.pass.fold_program(node)
    }

    #[inline(always)]
    fn fold_module(&mut self, node: Module) -> Module {
        self.pass.fold_module(node)
    }

    #[inline(always)]
    fn fold_script(&mut self, node: Script) -> Script {
        self.pass.fold_script(node)
    }

    #[inline(always)]
    fn fold_stmt(&mut self, node: Stmt) -> Stmt {
        self.pass.fold_stmt(node)
    }

    #[inline(always)]
    fn fold_module_item(&mut self, item: ModuleItem) -> ModuleItem {
        self.pass.fold_module_item(item)
    }

    #[inline(always)]
    fn fold_expr(&mut self, expr: Expr) -> Expr {
        self.pass.fold_expr(expr)
    }

    #[inline(always)]
    fn fold_pat(&mut self, pat: Pat) -> Pat {
        self.pass.fold_pat(pat)
    }

    #[inline(always)]
    fn fold_assign_target(&mut self, target: AssignTarget) -> AssignTarget {
        self.pass.fold_assign_target(target)
    }

    #[inline(always)]
    fn fold_ident(&mut self, ident: Ident) -> Ident {
        self.pass.fold_ident(ident)
    }
}

impl<V> Repeated for FoldPass<V>
where
    V: Fold + Repeated,
{
    fn changed(&self) -> bool {
        self.pass.changed()
    }

    fn reset(&mut self) {
        self.pass.reset();
    }
}

impl<V> CompilerPass for FoldPass<V>
where
    V: Fold + CompilerPass,
{
    fn name(&self) -> Cow<'static, str> {
        self.pass.name()
    }
}

pub fn visit_mut_pass<V>(pass: V) -> VisitMutPass<V>
where
    V: VisitMut,
{
    VisitMutPass { pass }
}

pub struct VisitMutPass<V> {
    pass: V,
}

impl<V> Pass for VisitMutPass<V>
where
    V: VisitMut,
{
    #[inline(always)]
    fn process(&mut self, program: &mut Program) {
        program.visit_mut_with(&mut self.pass);
    }
}

impl<V> VisitMut for VisitMutPass<V>
where
    V: VisitMut,
{
    #[inline(always)]
    fn visit_mut_program(&mut self, program: &mut Program) {
        self.pass.visit_mut_program(program);
    }

    #[inline(always)]
    fn visit_mut_module(&mut self, module: &mut Module) {
        self.pass.visit_mut_module(module);
    }

    #[inline(always)]
    fn visit_mut_script(&mut self, script: &mut Script) {
        self.pass.visit_mut_script(script);
    }

    #[inline(always)]
    fn visit_mut_module_item(&mut self, item: &mut ModuleItem) {
        self.pass.visit_mut_module_item(item);
    }

    #[inline(always)]
    fn visit_mut_stmt(&mut self, stmt: &mut Stmt) {
        self.pass.visit_mut_stmt(stmt);
    }

    #[inline(always)]
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        self.pass.visit_mut_expr(expr);
    }

    #[inline(always)]
    fn visit_mut_pat(&mut self, pat: &mut Pat) {
        self.pass.visit_mut_pat(pat);
    }

    #[inline(always)]
    fn visit_mut_assign_target(&mut self, target: &mut AssignTarget) {
        self.pass.visit_mut_assign_target(target);
    }

    #[inline(always)]
    fn visit_mut_ident(&mut self, ident: &mut Ident) {
        self.pass.visit_mut_ident(ident);
    }
}

impl<V> Repeated for VisitMutPass<V>
where
    V: VisitMut + Repeated,
{
    fn changed(&self) -> bool {
        self.pass.changed()
    }

    fn reset(&mut self) {
        self.pass.reset();
    }
}

impl<V> CompilerPass for VisitMutPass<V>
where
    V: VisitMut + CompilerPass,
{
    fn name(&self) -> Cow<'static, str> {
        self.pass.name()
    }
}

pub fn visit_pass<V>(pass: V) -> VisitPass<V>
where
    V: Visit,
{
    VisitPass { pass }
}

pub struct VisitPass<V> {
    pass: V,
}

impl<V> Pass for VisitPass<V>
where
    V: Visit,
{
    #[inline(always)]
    fn process(&mut self, program: &mut Program) {
        program.visit_with(&mut self.pass);
    }
}

impl<V> Repeated for VisitPass<V>
where
    V: Visit + Repeated,
{
    fn changed(&self) -> bool {
        self.pass.changed()
    }

    fn reset(&mut self) {
        self.pass.reset();
    }
}

impl<V> CompilerPass for VisitPass<V>
where
    V: Visit + CompilerPass,
{
    fn name(&self) -> Cow<'static, str> {
        self.pass.name()
    }
}

impl<V> Fold for Repeat<V>
where
    V: Fold + Repeated,
{
    fn fold_program(&mut self, mut node: Program) -> Program {
        loop {
            self.pass.reset();
            node = node.fold_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }

        node
    }

    fn fold_module(&mut self, mut node: Module) -> Module {
        loop {
            self.pass.reset();
            node = node.fold_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }

        node
    }

    fn fold_script(&mut self, mut node: Script) -> Script {
        loop {
            self.pass.reset();
            node = node.fold_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }

        node
    }
}

impl<V> VisitMut for Repeat<V>
where
    V: VisitMut + Repeated,
{
    fn visit_mut_program(&mut self, node: &mut Program) {
        loop {
            self.pass.reset();
            node.visit_mut_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }
    }

    fn visit_mut_module(&mut self, node: &mut Module) {
        loop {
            self.pass.reset();
            node.visit_mut_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }
    }

    fn visit_mut_script(&mut self, node: &mut Script) {
        loop {
            self.pass.reset();
            node.visit_mut_with(&mut self.pass);

            if !self.pass.changed() {
                break;
            }
        }
    }
}

/// Not a public api.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct SpanRemover;

/// Returns a `Fold` which changes all span into `DUMMY_SP`.
pub fn span_remover() -> impl Debug + Fold + Copy + Eq + Default + 'static {
    SpanRemover
}

impl Fold for SpanRemover {
    fn fold_span(&mut self, _: Span) -> Span {
        DUMMY_SP
    }
}

#[macro_export]
macro_rules! assert_eq_ignore_span {
    ($l:expr, $r:expr) => {{
        use $crate::FoldWith;
        let l = $l.fold_with(&mut $crate::span_remover());
        let r = $r.fold_with(&mut $crate::span_remover());

        assert_eq!(l, r);
    }};

    ($l:expr, $r:expr, $($tts:tt)*) => {{
        use $crate::FoldWith;
        let l = $l.fold_with(&mut $crate::span_remover());
        let r = $r.fold_with(&mut $crate::span_remover());

        assert_eq!(l, r, $($tts)*);
    }};
}

/// Implemented for passes which inject variables.
///
/// If a pass depends on other pass which injects variables, this trait can be
/// used to keep the variables.
pub trait InjectVars {
    fn take_vars(&mut self) -> Vec<VarDeclarator>;
}

impl<V> InjectVars for FoldPass<V>
where
    V: Fold + InjectVars,
{
    fn take_vars(&mut self) -> Vec<VarDeclarator> {
        self.pass.take_vars()
    }
}

impl<V> InjectVars for VisitMutPass<V>
where
    V: VisitMut + InjectVars,
{
    fn take_vars(&mut self) -> Vec<VarDeclarator> {
        self.pass.take_vars()
    }
}

impl<V> InjectVars for VisitPass<V>
where
    V: Visit + InjectVars,
{
    fn take_vars(&mut self) -> Vec<VarDeclarator> {
        self.pass.take_vars()
    }
}

/// Note: Ignoring more types is not considered as a breaking change.
#[macro_export]
macro_rules! noop_fold_type {
    ($name:ident, $N:tt) => {
        fn $name(&mut self, node: $crate::swc_ecma_ast::$N) -> $crate::swc_ecma_ast::$N {
            node
        }
    };
    () => {
        noop_fold_type!(fold_accessibility, Accessibility);
        noop_fold_type!(fold_true_plus_minus, TruePlusMinus);
        noop_fold_type!(fold_ts_array_type, TsArrayType);
        noop_fold_type!(fold_ts_call_signature_decl, TsCallSignatureDecl);
        noop_fold_type!(fold_ts_conditional_type, TsConditionalType);
        noop_fold_type!(fold_ts_construct_signature_decl, TsConstructSignatureDecl);
        noop_fold_type!(fold_ts_constructor_type, TsConstructorType);
        noop_fold_type!(fold_ts_entity_name, TsEntityName);
        noop_fold_type!(fold_ts_enum_decl, TsEnumDecl);
        noop_fold_type!(fold_ts_enum_member, TsEnumMember);
        noop_fold_type!(fold_ts_enum_member_id, TsEnumMemberId);
        noop_fold_type!(fold_ts_expr_with_type_args, TsExprWithTypeArgs);
        noop_fold_type!(fold_ts_fn_or_constructor_type, TsFnOrConstructorType);
        noop_fold_type!(fold_ts_fn_param, TsFnParam);
        noop_fold_type!(fold_ts_fn_type, TsFnType);
        noop_fold_type!(fold_ts_import_equals_decl, TsImportEqualsDecl);
        noop_fold_type!(fold_ts_import_type, TsImportType);
        noop_fold_type!(fold_ts_index_signature, TsIndexSignature);
        noop_fold_type!(fold_ts_indexed_access_type, TsIndexedAccessType);
        noop_fold_type!(fold_ts_infer_type, TsInferType);
        noop_fold_type!(fold_ts_interface_body, TsInterfaceBody);
        noop_fold_type!(fold_ts_interface_decl, TsInterfaceDecl);
        noop_fold_type!(fold_ts_intersection_type, TsIntersectionType);
        noop_fold_type!(fold_ts_keyword_type, TsKeywordType);
        noop_fold_type!(fold_ts_keyword_type_kind, TsKeywordTypeKind);
        noop_fold_type!(fold_ts_mapped_type, TsMappedType);
        noop_fold_type!(fold_ts_method_signature, TsMethodSignature);
        noop_fold_type!(fold_ts_module_block, TsModuleBlock);
        noop_fold_type!(fold_ts_module_decl, TsModuleDecl);
        noop_fold_type!(fold_ts_module_name, TsModuleName);
        noop_fold_type!(fold_ts_namespace_body, TsNamespaceBody);
        noop_fold_type!(fold_ts_namespace_decl, TsNamespaceDecl);
        noop_fold_type!(fold_ts_namespace_export_decl, TsNamespaceExportDecl);
        noop_fold_type!(fold_ts_optional_type, TsOptionalType);
        noop_fold_type!(fold_ts_param_prop, TsParamProp);
        noop_fold_type!(fold_ts_param_prop_param, TsParamPropParam);
        noop_fold_type!(fold_ts_parenthesized_type, TsParenthesizedType);
        noop_fold_type!(fold_ts_property_signature, TsPropertySignature);
        noop_fold_type!(fold_ts_qualified_name, TsQualifiedName);
        noop_fold_type!(fold_ts_rest_type, TsRestType);
        noop_fold_type!(fold_ts_this_type, TsThisType);
        noop_fold_type!(fold_ts_this_type_or_ident, TsThisTypeOrIdent);
        noop_fold_type!(fold_ts_tuple_type, TsTupleType);
        noop_fold_type!(fold_ts_type, TsType);
        noop_fold_type!(fold_ts_type_alias_decl, TsTypeAliasDecl);
        noop_fold_type!(fold_ts_type_ann, TsTypeAnn);
        noop_fold_type!(fold_ts_type_assertion, TsTypeAssertion);
        noop_fold_type!(fold_ts_type_element, TsTypeElement);
        noop_fold_type!(fold_ts_type_lit, TsTypeLit);
        noop_fold_type!(fold_ts_type_operator, TsTypeOperator);
        noop_fold_type!(fold_ts_type_operator_op, TsTypeOperatorOp);
        noop_fold_type!(fold_ts_type_param, TsTypeParam);
        noop_fold_type!(fold_ts_type_param_decl, TsTypeParamDecl);
        noop_fold_type!(fold_ts_type_param_instantiation, TsTypeParamInstantiation);
        noop_fold_type!(fold_ts_type_predicate, TsTypePredicate);
        noop_fold_type!(fold_ts_type_query, TsTypeQuery);
        noop_fold_type!(fold_ts_type_query_expr, TsTypeQueryExpr);
        noop_fold_type!(fold_ts_type_ref, TsTypeRef);
        noop_fold_type!(
            fold_ts_union_or_intersection_type,
            TsUnionOrIntersectionType
        );
        noop_fold_type!(fold_ts_union_type, TsUnionType);
    };
}

/// Note: Ignoring more types is not considered as a breaking change.
#[macro_export]
macro_rules! noop_visit_type {
    (fail) => {
        noop_visit_type!(visit_accessibility, Accessibility, fail);
        noop_visit_type!(visit_true_plus_minus, TruePlusMinus, fail);
        noop_visit_type!(visit_ts_array_type, TsArrayType, fail);
        noop_visit_type!(visit_ts_call_signature_decl, TsCallSignatureDecl, fail);
        noop_visit_type!(visit_ts_conditional_type, TsConditionalType, fail);
        noop_visit_type!(
            visit_ts_construct_signature_decl,
            TsConstructSignatureDecl,
            fail
        );
        noop_visit_type!(visit_ts_constructor_type, TsConstructorType, fail);
        noop_visit_type!(visit_ts_entity_name, TsEntityName, fail);
        noop_visit_type!(visit_ts_expr_with_type_args, TsExprWithTypeArgs, fail);
        noop_visit_type!(visit_ts_fn_or_constructor_type, TsFnOrConstructorType, fail);
        noop_visit_type!(visit_ts_fn_param, TsFnParam, fail);
        noop_visit_type!(visit_ts_fn_type, TsFnType, fail);
        noop_visit_type!(visit_ts_import_type, TsImportType, fail);
        noop_visit_type!(visit_ts_index_signature, TsIndexSignature, fail);
        noop_visit_type!(visit_ts_indexed_access_type, TsIndexedAccessType, fail);
        noop_visit_type!(visit_ts_infer_type, TsInferType, fail);
        noop_visit_type!(visit_ts_interface_body, TsInterfaceBody, fail);
        noop_visit_type!(visit_ts_interface_decl, TsInterfaceDecl, fail);
        noop_visit_type!(visit_ts_intersection_type, TsIntersectionType, fail);
        noop_visit_type!(visit_ts_keyword_type, TsKeywordType, fail);
        noop_visit_type!(visit_ts_keyword_type_kind, TsKeywordTypeKind, fail);
        noop_visit_type!(visit_ts_mapped_type, TsMappedType, fail);
        noop_visit_type!(visit_ts_method_signature, TsMethodSignature, fail);
        noop_visit_type!(visit_ts_optional_type, TsOptionalType, fail);
        noop_visit_type!(visit_ts_parenthesized_type, TsParenthesizedType, fail);
        noop_visit_type!(visit_ts_property_signature, TsPropertySignature, fail);
        noop_visit_type!(visit_ts_qualified_name, TsQualifiedName, fail);
        noop_visit_type!(visit_ts_rest_type, TsRestType, fail);
        noop_visit_type!(visit_ts_this_type, TsThisType, fail);
        noop_visit_type!(visit_ts_this_type_or_ident, TsThisTypeOrIdent, fail);
        noop_visit_type!(visit_ts_tuple_type, TsTupleType, fail);
        noop_visit_type!(visit_ts_type, TsType, fail);
        noop_visit_type!(visit_ts_type_alias_decl, TsTypeAliasDecl, fail);
        noop_visit_type!(visit_ts_type_ann, TsTypeAnn, fail);
        noop_visit_type!(visit_ts_type_element, TsTypeElement, fail);
        noop_visit_type!(visit_ts_type_lit, TsTypeLit, fail);
        noop_visit_type!(visit_ts_type_operator, TsTypeOperator, fail);
        noop_visit_type!(visit_ts_type_operator_op, TsTypeOperatorOp, fail);
        noop_visit_type!(visit_ts_type_param, TsTypeParam, fail);
        noop_visit_type!(visit_ts_type_param_decl, TsTypeParamDecl, fail);
        noop_visit_type!(
            visit_ts_type_param_instantiation,
            TsTypeParamInstantiation,
            fail
        );
        noop_visit_type!(visit_ts_type_predicate, TsTypePredicate, fail);
        noop_visit_type!(visit_ts_type_query, TsTypeQuery, fail);
        noop_visit_type!(visit_ts_type_query_expr, TsTypeQueryExpr, fail);
        noop_visit_type!(visit_ts_type_ref, TsTypeRef, fail);
        noop_visit_type!(
            visit_ts_union_or_intersection_type,
            TsUnionOrIntersectionType,
            fail
        );
        noop_visit_type!(visit_ts_union_type, TsUnionType, fail);
    };
    () => {
        noop_visit_type!(visit_accessibility, Accessibility);
        noop_visit_type!(visit_true_plus_minus, TruePlusMinus);
        noop_visit_type!(visit_ts_array_type, TsArrayType);
        noop_visit_type!(visit_ts_call_signature_decl, TsCallSignatureDecl);
        noop_visit_type!(visit_ts_conditional_type, TsConditionalType);
        noop_visit_type!(visit_ts_construct_signature_decl, TsConstructSignatureDecl);
        noop_visit_type!(visit_ts_constructor_type, TsConstructorType);
        noop_visit_type!(visit_ts_entity_name, TsEntityName);
        noop_visit_type!(visit_ts_expr_with_type_args, TsExprWithTypeArgs);
        noop_visit_type!(visit_ts_fn_or_constructor_type, TsFnOrConstructorType);
        noop_visit_type!(visit_ts_fn_param, TsFnParam);
        noop_visit_type!(visit_ts_fn_type, TsFnType);
        noop_visit_type!(visit_ts_import_type, TsImportType);
        noop_visit_type!(visit_ts_index_signature, TsIndexSignature);
        noop_visit_type!(visit_ts_indexed_access_type, TsIndexedAccessType);
        noop_visit_type!(visit_ts_infer_type, TsInferType);
        noop_visit_type!(visit_ts_interface_body, TsInterfaceBody);
        noop_visit_type!(visit_ts_interface_decl, TsInterfaceDecl);
        noop_visit_type!(visit_ts_intersection_type, TsIntersectionType);
        noop_visit_type!(visit_ts_keyword_type, TsKeywordType);
        noop_visit_type!(visit_ts_keyword_type_kind, TsKeywordTypeKind);
        noop_visit_type!(visit_ts_mapped_type, TsMappedType);
        noop_visit_type!(visit_ts_method_signature, TsMethodSignature);
        noop_visit_type!(visit_ts_optional_type, TsOptionalType);
        noop_visit_type!(visit_ts_parenthesized_type, TsParenthesizedType);
        noop_visit_type!(visit_ts_property_signature, TsPropertySignature);
        noop_visit_type!(visit_ts_qualified_name, TsQualifiedName);
        noop_visit_type!(visit_ts_rest_type, TsRestType);
        noop_visit_type!(visit_ts_this_type, TsThisType);
        noop_visit_type!(visit_ts_this_type_or_ident, TsThisTypeOrIdent);
        noop_visit_type!(visit_ts_tuple_type, TsTupleType);
        noop_visit_type!(visit_ts_type, TsType);
        noop_visit_type!(visit_ts_type_alias_decl, TsTypeAliasDecl);
        noop_visit_type!(visit_ts_type_ann, TsTypeAnn);
        noop_visit_type!(visit_ts_type_element, TsTypeElement);
        noop_visit_type!(visit_ts_type_lit, TsTypeLit);
        noop_visit_type!(visit_ts_type_operator, TsTypeOperator);
        noop_visit_type!(visit_ts_type_operator_op, TsTypeOperatorOp);
        noop_visit_type!(visit_ts_type_param, TsTypeParam);
        noop_visit_type!(visit_ts_type_param_decl, TsTypeParamDecl);
        noop_visit_type!(visit_ts_type_param_instantiation, TsTypeParamInstantiation);
        noop_visit_type!(visit_ts_type_predicate, TsTypePredicate);
        noop_visit_type!(visit_ts_type_query, TsTypeQuery);
        noop_visit_type!(visit_ts_type_query_expr, TsTypeQueryExpr);
        noop_visit_type!(visit_ts_type_ref, TsTypeRef);
        noop_visit_type!(
            visit_ts_union_or_intersection_type,
            TsUnionOrIntersectionType
        );
        noop_visit_type!(visit_ts_union_type, TsUnionType);
    };

    ($name:ident, $N:tt, fail) => {
        #[cfg_attr(not(debug_assertions), inline(always))]
        fn $name(&mut self, _: &$crate::swc_ecma_ast::$N) {
            $crate::fail_no_typescript(stringify!($name));
        }
    };
    ($name:ident, $N:tt) => {
        fn $name(&mut self, _: &$crate::swc_ecma_ast::$N) {}
    };
}

/// NOT A PUBLIC API
#[doc(hidden)]
#[cfg_attr(not(debug_assertions), inline(always))]
pub fn fail_not_standard() {
    unsafe {
        debug_unreachable::debug_unreachable!(
            "This visitor supports only standard ECMAScript types. This method fails for \
             optimization purpose."
        )
    }
}

/// NOT A PUBLIC API
#[doc(hidden)]
#[cfg_attr(not(debug_assertions), inline(always))]
pub fn fail_no_typescript(visitor_name: &str) {
    unsafe {
        debug_unreachable::debug_unreachable!(
            "This visitor does not support TypeScript. This method fails for optimization \
             purposes. Encountered in unreachable visitor: {visitor_name}"
        )
    }
}

/// Mark visitor as ECMAScript standard only and mark other types as
/// unreachable.
///
/// Used to reduce the binary size.
#[macro_export]
macro_rules! standard_only_fold {
    ($name:ident, $N:ident) => {
        fn $name(&mut self, n: $crate::swc_ecma_ast::$N) -> $crate::swc_ecma_ast::$N {
            $crate::fail_not_standard();
            n
        }
    };
    () => {
        standard_only_fold!(fold_accessibility, Accessibility);
        standard_only_fold!(fold_true_plus_minus, TruePlusMinus);
        standard_only_fold!(fold_ts_array_type, TsArrayType);
        standard_only_fold!(fold_ts_call_signature_decl, TsCallSignatureDecl);
        standard_only_fold!(fold_ts_conditional_type, TsConditionalType);
        standard_only_fold!(fold_ts_construct_signature_decl, TsConstructSignatureDecl);
        standard_only_fold!(fold_ts_constructor_type, TsConstructorType);
        standard_only_fold!(fold_ts_entity_name, TsEntityName);
        standard_only_fold!(fold_ts_expr_with_type_args, TsExprWithTypeArgs);
        standard_only_fold!(fold_ts_fn_or_constructor_type, TsFnOrConstructorType);
        standard_only_fold!(fold_ts_fn_param, TsFnParam);
        standard_only_fold!(fold_ts_fn_type, TsFnType);
        standard_only_fold!(fold_ts_import_type, TsImportType);
        standard_only_fold!(fold_ts_index_signature, TsIndexSignature);
        standard_only_fold!(fold_ts_indexed_access_type, TsIndexedAccessType);
        standard_only_fold!(fold_ts_infer_type, TsInferType);
        standard_only_fold!(fold_ts_interface_body, TsInterfaceBody);
        standard_only_fold!(fold_ts_interface_decl, TsInterfaceDecl);
        standard_only_fold!(fold_ts_intersection_type, TsIntersectionType);
        standard_only_fold!(fold_ts_keyword_type, TsKeywordType);
        standard_only_fold!(fold_ts_keyword_type_kind, TsKeywordTypeKind);
        standard_only_fold!(fold_ts_mapped_type, TsMappedType);
        standard_only_fold!(fold_ts_method_signature, TsMethodSignature);
        standard_only_fold!(fold_ts_optional_type, TsOptionalType);
        standard_only_fold!(fold_ts_parenthesized_type, TsParenthesizedType);
        standard_only_fold!(fold_ts_property_signature, TsPropertySignature);
        standard_only_fold!(fold_ts_qualified_name, TsQualifiedName);
        standard_only_fold!(fold_ts_rest_type, TsRestType);
        standard_only_fold!(fold_ts_this_type, TsThisType);
        standard_only_fold!(fold_ts_this_type_or_ident, TsThisTypeOrIdent);
        standard_only_fold!(fold_ts_tuple_type, TsTupleType);
        standard_only_fold!(fold_ts_type, TsType);
        standard_only_fold!(fold_ts_type_alias_decl, TsTypeAliasDecl);
        standard_only_fold!(fold_ts_type_ann, TsTypeAnn);
        standard_only_fold!(fold_ts_type_element, TsTypeElement);
        standard_only_fold!(fold_ts_type_lit, TsTypeLit);
        standard_only_fold!(fold_ts_type_operator, TsTypeOperator);
        standard_only_fold!(fold_ts_type_operator_op, TsTypeOperatorOp);
        standard_only_fold!(fold_ts_type_param, TsTypeParam);
        standard_only_fold!(fold_ts_type_param_decl, TsTypeParamDecl);
        standard_only_fold!(fold_ts_type_param_instantiation, TsTypeParamInstantiation);
        standard_only_fold!(fold_ts_type_predicate, TsTypePredicate);
        standard_only_fold!(fold_ts_type_query, TsTypeQuery);
        standard_only_fold!(fold_ts_type_query_expr, TsTypeQueryExpr);
        standard_only_fold!(fold_ts_type_ref, TsTypeRef);
        standard_only_fold!(
            fold_ts_union_or_intersection_type,
            TsUnionOrIntersectionType
        );
        standard_only_fold!(fold_ts_union_type, TsUnionType);

        standard_only_fold!(fold_jsx_element, JSXElement);
        standard_only_fold!(fold_jsx_fragment, JSXFragment);
        standard_only_fold!(fold_jsx_empty_expr, JSXEmptyExpr);
        standard_only_fold!(fold_jsx_member_expr, JSXMemberExpr);
        standard_only_fold!(fold_jsx_namespaced_name, JSXNamespacedName);
    };
}

/// Mark visitor as ECMAScript standard only and mark other types as
/// unreachable.
///
/// Used to reduce the binary size.
#[macro_export]
macro_rules! standard_only_visit {
    ($name:ident, $N:ident) => {
        fn $name(&mut self, _: &$crate::swc_ecma_ast::$N) {
            $crate::fail_not_standard()
        }
    };
    () => {
        standard_only_visit!(visit_accessibility, Accessibility);
        standard_only_visit!(visit_true_plus_minus, TruePlusMinus);
        standard_only_visit!(visit_ts_array_type, TsArrayType);
        standard_only_visit!(visit_ts_call_signature_decl, TsCallSignatureDecl);
        standard_only_visit!(visit_ts_conditional_type, TsConditionalType);
        standard_only_visit!(visit_ts_construct_signature_decl, TsConstructSignatureDecl);
        standard_only_visit!(visit_ts_constructor_type, TsConstructorType);
        standard_only_visit!(visit_ts_entity_name, TsEntityName);
        standard_only_visit!(visit_ts_expr_with_type_args, TsExprWithTypeArgs);
        standard_only_visit!(visit_ts_fn_or_constructor_type, TsFnOrConstructorType);
        standard_only_visit!(visit_ts_fn_param, TsFnParam);
        standard_only_visit!(visit_ts_fn_type, TsFnType);
        standard_only_visit!(visit_ts_import_type, TsImportType);
        standard_only_visit!(visit_ts_index_signature, TsIndexSignature);
        standard_only_visit!(visit_ts_indexed_access_type, TsIndexedAccessType);
        standard_only_visit!(visit_ts_infer_type, TsInferType);
        standard_only_visit!(visit_ts_interface_body, TsInterfaceBody);
        standard_only_visit!(visit_ts_interface_decl, TsInterfaceDecl);
        standard_only_visit!(visit_ts_intersection_type, TsIntersectionType);
        standard_only_visit!(visit_ts_keyword_type, TsKeywordType);
        standard_only_visit!(visit_ts_keyword_type_kind, TsKeywordTypeKind);
        standard_only_visit!(visit_ts_mapped_type, TsMappedType);
        standard_only_visit!(visit_ts_method_signature, TsMethodSignature);
        standard_only_visit!(visit_ts_optional_type, TsOptionalType);
        standard_only_visit!(visit_ts_parenthesized_type, TsParenthesizedType);
        standard_only_visit!(visit_ts_property_signature, TsPropertySignature);
        standard_only_visit!(visit_ts_qualified_name, TsQualifiedName);
        standard_only_visit!(visit_ts_rest_type, TsRestType);
        standard_only_visit!(visit_ts_this_type, TsThisType);
        standard_only_visit!(visit_ts_this_type_or_ident, TsThisTypeOrIdent);
        standard_only_visit!(visit_ts_tuple_type, TsTupleType);
        standard_only_visit!(visit_ts_type, TsType);
        standard_only_visit!(visit_ts_type_alias_decl, TsTypeAliasDecl);
        standard_only_visit!(visit_ts_type_ann, TsTypeAnn);
        standard_only_visit!(visit_ts_type_element, TsTypeElement);
        standard_only_visit!(visit_ts_type_lit, TsTypeLit);
        standard_only_visit!(visit_ts_type_operator, TsTypeOperator);
        standard_only_visit!(visit_ts_type_operator_op, TsTypeOperatorOp);
        standard_only_visit!(visit_ts_type_param, TsTypeParam);
        standard_only_visit!(visit_ts_type_param_decl, TsTypeParamDecl);
        standard_only_visit!(visit_ts_type_param_instantiation, TsTypeParamInstantiation);
        standard_only_visit!(visit_ts_type_predicate, TsTypePredicate);
        standard_only_visit!(visit_ts_type_query, TsTypeQuery);
        standard_only_visit!(visit_ts_type_query_expr, TsTypeQueryExpr);
        standard_only_visit!(visit_ts_type_ref, TsTypeRef);
        standard_only_visit!(
            visit_ts_union_or_intersection_type,
            TsUnionOrIntersectionType
        );
        standard_only_visit!(visit_ts_union_type, TsUnionType);

        standard_only_visit!(visit_jsx_element, JSXElement);
        standard_only_visit!(visit_jsx_fragment, JSXFragment);
        standard_only_visit!(visit_jsx_empty_expr, JSXEmptyExpr);
        standard_only_visit!(visit_jsx_member_expr, JSXMemberExpr);
        standard_only_visit!(visit_jsx_namespaced_name, JSXNamespacedName);
    };
}

/// Mark visitor as ECMAScript standard only and mark other types as
/// unreachable.
///
/// Used to reduce the binary size.
#[macro_export]
macro_rules! standard_only_visit_mut {
    ($name:ident, $N:ident) => {
        fn $name(&mut self, _: &mut $crate::swc_ecma_ast::$N) {
            $crate::fail_not_standard()
        }
    };
    () => {
        standard_only_visit_mut!(visit_mut_accessibility, Accessibility);
        standard_only_visit_mut!(visit_mut_true_plus_minus, TruePlusMinus);
        standard_only_visit_mut!(visit_mut_ts_array_type, TsArrayType);
        standard_only_visit_mut!(visit_mut_ts_call_signature_decl, TsCallSignatureDecl);
        standard_only_visit_mut!(visit_mut_ts_conditional_type, TsConditionalType);
        standard_only_visit_mut!(
            visit_mut_ts_construct_signature_decl,
            TsConstructSignatureDecl
        );
        standard_only_visit_mut!(visit_mut_ts_constructor_type, TsConstructorType);
        standard_only_visit_mut!(visit_mut_ts_entity_name, TsEntityName);
        standard_only_visit_mut!(visit_mut_ts_expr_with_type_args, TsExprWithTypeArgs);
        standard_only_visit_mut!(visit_mut_ts_fn_or_constructor_type, TsFnOrConstructorType);
        standard_only_visit_mut!(visit_mut_ts_fn_param, TsFnParam);
        standard_only_visit_mut!(visit_mut_ts_fn_type, TsFnType);
        standard_only_visit_mut!(visit_mut_ts_import_type, TsImportType);
        standard_only_visit_mut!(visit_mut_ts_index_signature, TsIndexSignature);
        standard_only_visit_mut!(visit_mut_ts_indexed_access_type, TsIndexedAccessType);
        standard_only_visit_mut!(visit_mut_ts_infer_type, TsInferType);
        standard_only_visit_mut!(visit_mut_ts_interface_body, TsInterfaceBody);
        standard_only_visit_mut!(visit_mut_ts_interface_decl, TsInterfaceDecl);
        standard_only_visit_mut!(visit_mut_ts_intersection_type, TsIntersectionType);
        standard_only_visit_mut!(visit_mut_ts_keyword_type, TsKeywordType);
        standard_only_visit_mut!(visit_mut_ts_keyword_type_kind, TsKeywordTypeKind);
        standard_only_visit_mut!(visit_mut_ts_mapped_type, TsMappedType);
        standard_only_visit_mut!(visit_mut_ts_method_signature, TsMethodSignature);
        standard_only_visit_mut!(visit_mut_ts_optional_type, TsOptionalType);
        standard_only_visit_mut!(visit_mut_ts_parenthesized_type, TsParenthesizedType);
        standard_only_visit_mut!(visit_mut_ts_property_signature, TsPropertySignature);
        standard_only_visit_mut!(visit_mut_ts_qualified_name, TsQualifiedName);
        standard_only_visit_mut!(visit_mut_ts_rest_type, TsRestType);
        standard_only_visit_mut!(visit_mut_ts_this_type, TsThisType);
        standard_only_visit_mut!(visit_mut_ts_this_type_or_ident, TsThisTypeOrIdent);
        standard_only_visit_mut!(visit_mut_ts_tuple_type, TsTupleType);
        standard_only_visit_mut!(visit_mut_ts_type, TsType);
        standard_only_visit_mut!(visit_mut_ts_type_alias_decl, TsTypeAliasDecl);
        standard_only_visit_mut!(visit_mut_ts_type_ann, TsTypeAnn);
        standard_only_visit_mut!(visit_mut_ts_type_element, TsTypeElement);
        standard_only_visit_mut!(visit_mut_ts_type_lit, TsTypeLit);
        standard_only_visit_mut!(visit_mut_ts_type_operator, TsTypeOperator);
        standard_only_visit_mut!(visit_mut_ts_type_operator_op, TsTypeOperatorOp);
        standard_only_visit_mut!(visit_mut_ts_type_param, TsTypeParam);
        standard_only_visit_mut!(visit_mut_ts_type_param_decl, TsTypeParamDecl);
        standard_only_visit_mut!(
            visit_mut_ts_type_param_instantiation,
            TsTypeParamInstantiation
        );
        standard_only_visit_mut!(visit_mut_ts_type_predicate, TsTypePredicate);
        standard_only_visit_mut!(visit_mut_ts_type_query, TsTypeQuery);
        standard_only_visit_mut!(visit_mut_ts_type_query_expr, TsTypeQueryExpr);
        standard_only_visit_mut!(visit_mut_ts_type_ref, TsTypeRef);
        standard_only_visit_mut!(
            visit_mut_ts_union_or_intersection_type,
            TsUnionOrIntersectionType
        );
        standard_only_visit_mut!(visit_mut_ts_union_type, TsUnionType);

        standard_only_visit_mut!(visit_mut_jsx_element, JSXElement);
        standard_only_visit_mut!(visit_mut_jsx_fragment, JSXFragment);
        standard_only_visit_mut!(visit_mut_jsx_empty_expr, JSXEmptyExpr);
        standard_only_visit_mut!(visit_mut_jsx_member_expr, JSXMemberExpr);
        standard_only_visit_mut!(visit_mut_jsx_namespaced_name, JSXNamespacedName);
    };
}

/// Note: Ignoring more types is not considered as a breaking change.
#[macro_export]
macro_rules! noop_visit_mut_type {
    (fail) => {
        noop_visit_mut_type!(visit_mut_accessibility, Accessibility, fail);
        noop_visit_mut_type!(visit_mut_true_plus_minus, TruePlusMinus, fail);
        noop_visit_mut_type!(visit_mut_ts_array_type, TsArrayType, fail);
        noop_visit_mut_type!(visit_mut_ts_call_signature_decl, TsCallSignatureDecl, fail);
        noop_visit_mut_type!(visit_mut_ts_conditional_type, TsConditionalType, fail);
        noop_visit_mut_type!(
            visit_mut_ts_construct_signature_decl,
            TsConstructSignatureDecl,
            fail
        );
        noop_visit_mut_type!(visit_mut_ts_constructor_type, TsConstructorType, fail);
        noop_visit_mut_type!(visit_mut_ts_entity_name, TsEntityName, fail);
        noop_visit_mut_type!(visit_mut_ts_expr_with_type_args, TsExprWithTypeArgs, fail);
        noop_visit_mut_type!(
            visit_mut_ts_fn_or_constructor_type,
            TsFnOrConstructorType,
            fail
        );
        noop_visit_mut_type!(visit_mut_ts_fn_param, TsFnParam, fail);
        noop_visit_mut_type!(visit_mut_ts_fn_type, TsFnType, fail);
        noop_visit_mut_type!(visit_mut_ts_import_type, TsImportType, fail);
        noop_visit_mut_type!(visit_mut_ts_index_signature, TsIndexSignature, fail);
        noop_visit_mut_type!(visit_mut_ts_indexed_access_type, TsIndexedAccessType, fail);
        noop_visit_mut_type!(visit_mut_ts_infer_type, TsInferType, fail);
        noop_visit_mut_type!(visit_mut_ts_interface_body, TsInterfaceBody, fail);
        noop_visit_mut_type!(visit_mut_ts_interface_decl, TsInterfaceDecl, fail);
        noop_visit_mut_type!(visit_mut_ts_intersection_type, TsIntersectionType, fail);
        noop_visit_mut_type!(visit_mut_ts_keyword_type, TsKeywordType, fail);
        noop_visit_mut_type!(visit_mut_ts_keyword_type_kind, TsKeywordTypeKind, fail);
        noop_visit_mut_type!(visit_mut_ts_mapped_type, TsMappedType, fail);
        noop_visit_mut_type!(visit_mut_ts_method_signature, TsMethodSignature, fail);
        noop_visit_mut_type!(visit_mut_ts_optional_type, TsOptionalType, fail);
        noop_visit_mut_type!(visit_mut_ts_parenthesized_type, TsParenthesizedType, fail);
        noop_visit_mut_type!(visit_mut_ts_property_signature, TsPropertySignature, fail);
        noop_visit_mut_type!(visit_mut_ts_qualified_name, TsQualifiedName, fail);
        noop_visit_mut_type!(visit_mut_ts_rest_type, TsRestType, fail);
        noop_visit_mut_type!(visit_mut_ts_this_type, TsThisType, fail);
        noop_visit_mut_type!(visit_mut_ts_this_type_or_ident, TsThisTypeOrIdent, fail);
        noop_visit_mut_type!(visit_mut_ts_tuple_type, TsTupleType, fail);
        noop_visit_mut_type!(visit_mut_ts_type, TsType, fail);
        noop_visit_mut_type!(visit_mut_ts_type_alias_decl, TsTypeAliasDecl, fail);
        noop_visit_mut_type!(visit_mut_ts_type_ann, TsTypeAnn, fail);
        noop_visit_mut_type!(visit_mut_ts_type_element, TsTypeElement, fail);
        noop_visit_mut_type!(visit_mut_ts_type_lit, TsTypeLit, fail);
        noop_visit_mut_type!(visit_mut_ts_type_operator, TsTypeOperator, fail);
        noop_visit_mut_type!(visit_mut_ts_type_operator_op, TsTypeOperatorOp, fail);
        noop_visit_mut_type!(visit_mut_ts_type_param, TsTypeParam, fail);
        noop_visit_mut_type!(visit_mut_ts_type_param_decl, TsTypeParamDecl, fail);
        noop_visit_mut_type!(
            visit_mut_ts_type_param_instantiation,
            TsTypeParamInstantiation,
            fail
        );
        noop_visit_mut_type!(visit_mut_ts_type_predicate, TsTypePredicate, fail);
        noop_visit_mut_type!(visit_mut_ts_type_query, TsTypeQuery, fail);
        noop_visit_mut_type!(visit_mut_ts_type_query_expr, TsTypeQueryExpr, fail);
        noop_visit_mut_type!(visit_mut_ts_type_ref, TsTypeRef, fail);
        noop_visit_mut_type!(
            visit_mut_ts_union_or_intersection_type,
            TsUnionOrIntersectionType,
            fail
        );
        noop_visit_mut_type!(visit_mut_ts_union_type, TsUnionType, fail);
    };
    () => {
        noop_visit_mut_type!(visit_mut_accessibility, Accessibility);
        noop_visit_mut_type!(visit_mut_true_plus_minus, TruePlusMinus);
        noop_visit_mut_type!(visit_mut_ts_array_type, TsArrayType);
        noop_visit_mut_type!(visit_mut_ts_call_signature_decl, TsCallSignatureDecl);
        noop_visit_mut_type!(visit_mut_ts_conditional_type, TsConditionalType);
        noop_visit_mut_type!(
            visit_mut_ts_construct_signature_decl,
            TsConstructSignatureDecl
        );
        noop_visit_mut_type!(visit_mut_ts_constructor_type, TsConstructorType);
        noop_visit_mut_type!(visit_mut_ts_entity_name, TsEntityName);
        noop_visit_mut_type!(visit_mut_ts_expr_with_type_args, TsExprWithTypeArgs);
        noop_visit_mut_type!(visit_mut_ts_fn_or_constructor_type, TsFnOrConstructorType);
        noop_visit_mut_type!(visit_mut_ts_fn_param, TsFnParam);
        noop_visit_mut_type!(visit_mut_ts_fn_type, TsFnType);
        noop_visit_mut_type!(visit_mut_ts_import_type, TsImportType);
        noop_visit_mut_type!(visit_mut_ts_index_signature, TsIndexSignature);
        noop_visit_mut_type!(visit_mut_ts_indexed_access_type, TsIndexedAccessType);
        noop_visit_mut_type!(visit_mut_ts_infer_type, TsInferType);
        noop_visit_mut_type!(visit_mut_ts_interface_body, TsInterfaceBody);
        noop_visit_mut_type!(visit_mut_ts_interface_decl, TsInterfaceDecl);
        noop_visit_mut_type!(visit_mut_ts_intersection_type, TsIntersectionType);
        noop_visit_mut_type!(visit_mut_ts_keyword_type, TsKeywordType);
        noop_visit_mut_type!(visit_mut_ts_keyword_type_kind, TsKeywordTypeKind);
        noop_visit_mut_type!(visit_mut_ts_mapped_type, TsMappedType);
        noop_visit_mut_type!(visit_mut_ts_method_signature, TsMethodSignature);
        noop_visit_mut_type!(visit_mut_ts_optional_type, TsOptionalType);
        noop_visit_mut_type!(visit_mut_ts_parenthesized_type, TsParenthesizedType);
        noop_visit_mut_type!(visit_mut_ts_property_signature, TsPropertySignature);
        noop_visit_mut_type!(visit_mut_ts_qualified_name, TsQualifiedName);
        noop_visit_mut_type!(visit_mut_ts_rest_type, TsRestType);
        noop_visit_mut_type!(visit_mut_ts_this_type, TsThisType);
        noop_visit_mut_type!(visit_mut_ts_this_type_or_ident, TsThisTypeOrIdent);
        noop_visit_mut_type!(visit_mut_ts_tuple_type, TsTupleType);
        noop_visit_mut_type!(visit_mut_ts_type, TsType);
        noop_visit_mut_type!(visit_mut_ts_type_alias_decl, TsTypeAliasDecl);
        noop_visit_mut_type!(visit_mut_ts_type_ann, TsTypeAnn);
        noop_visit_mut_type!(visit_mut_ts_type_element, TsTypeElement);
        noop_visit_mut_type!(visit_mut_ts_type_lit, TsTypeLit);
        noop_visit_mut_type!(visit_mut_ts_type_operator, TsTypeOperator);
        noop_visit_mut_type!(visit_mut_ts_type_operator_op, TsTypeOperatorOp);
        noop_visit_mut_type!(visit_mut_ts_type_param, TsTypeParam);
        noop_visit_mut_type!(visit_mut_ts_type_param_decl, TsTypeParamDecl);
        noop_visit_mut_type!(
            visit_mut_ts_type_param_instantiation,
            TsTypeParamInstantiation
        );
        noop_visit_mut_type!(visit_mut_ts_type_predicate, TsTypePredicate);
        noop_visit_mut_type!(visit_mut_ts_type_query, TsTypeQuery);
        noop_visit_mut_type!(visit_mut_ts_type_query_expr, TsTypeQueryExpr);
        noop_visit_mut_type!(visit_mut_ts_type_ref, TsTypeRef);
        noop_visit_mut_type!(
            visit_mut_ts_union_or_intersection_type,
            TsUnionOrIntersectionType
        );
        noop_visit_mut_type!(visit_mut_ts_union_type, TsUnionType);
    };

    ($name:ident, $N:ident, fail) => {
        #[cfg_attr(not(debug_assertions), inline(always))]
        fn $name(&mut self, _: &mut $crate::swc_ecma_ast::$N) {
            $crate::fail_no_typescript(stringify!($name));
        }
    };
    ($name:ident, $N:ident) => {
        fn $name(&mut self, _: &mut $crate::swc_ecma_ast::$N) {}
    };
}

define!({
    use BigIntValue;

    pub struct Class {
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub body: Vec<ClassMember>,
        pub super_class: Option<Box<Expr>>,
        pub is_abstract: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub super_type_params: Option<Box<TsTypeParamInstantiation>>,
        pub implements: Vec<TsExprWithTypeArgs>,
    }

    pub enum ClassMember {
        Constructor(Constructor),
        Method(ClassMethod),
        PrivateMethod(PrivateMethod),
        ClassProp(ClassProp),
        PrivateProp(PrivateProp),
        TsIndexSignature(TsIndexSignature),
        Empty(EmptyStmt),
        StaticBlock(StaticBlock),
        AutoAccessor(AutoAccessor),
        ContentTagMember(ContentTagMember),
    }

    pub struct ClassProp {
        pub span: Span,
        pub key: PropName,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
        pub readonly: bool,
        pub declare: bool,
        pub definite: bool,
    }
    pub struct PrivateProp {
        pub span: Span,
        pub key: PrivateName,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
        pub is_optional: bool,
        pub is_override: bool,
        pub readonly: bool,
        pub definite: bool,
    }
    pub struct ClassMethod {
        pub span: Span,
        pub key: PropName,
        pub function: Box<Function>,
        pub kind: MethodKind,
        pub is_static: bool,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
    }
    pub struct PrivateMethod {
        pub span: Span,
        pub key: PrivateName,
        pub function: Box<Function>,
        pub kind: MethodKind,
        pub is_static: bool,
        pub accessibility: Option<Accessibility>,
        pub is_abstract: bool,
        pub is_optional: bool,
        pub is_override: bool,
    }
    pub struct Constructor {
        pub span: Span,
        pub key: PropName,
        pub params: Vec<ParamOrTsParamProp>,
        pub body: Option<BlockStmt>,
        pub accessibility: Option<Accessibility>,
        pub is_optional: bool,
    }
    pub struct Decorator {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct StaticBlock {
        pub span: Span,
        pub body: BlockStmt,
    }
    pub enum MethodKind {
        Method,
        Getter,
        Setter,
    }
    pub enum Decl {
        Class(ClassDecl),
        Fn(FnDecl),
        Var(Box<VarDecl>),
        Using(Box<UsingDecl>),
        TsInterface(Box<TsInterfaceDecl>),
        TsTypeAlias(Box<TsTypeAliasDecl>),
        TsEnum(Box<TsEnumDecl>),
        TsModule(Box<TsModuleDecl>),
    }
    pub struct FnDecl {
        pub ident: Ident,
        pub declare: bool,
        pub function: Box<Function>,
    }
    pub struct ClassDecl {
        pub ident: Ident,
        pub declare: bool,
        pub class: Box<Class>,
    }
    pub struct VarDecl {
        pub span: Span,
        pub kind: VarDeclKind,
        pub declare: bool,
        pub decls: Vec<VarDeclarator>,
    }
    pub enum VarDeclKind {
        Var,
        Let,
        Const,
    }
    pub struct VarDeclarator {
        pub span: Span,
        pub name: Pat,
        pub init: Option<Box<Expr>>,
        pub definite: bool,
    }
    pub enum Expr {
        This(ThisExpr),
        Array(ArrayLit),
        Object(ObjectLit),
        Fn(FnExpr),
        Unary(UnaryExpr),
        Update(UpdateExpr),
        Bin(BinExpr),
        Assign(AssignExpr),
        Member(MemberExpr),
        SuperProp(SuperPropExpr),
        Cond(CondExpr),
        Call(CallExpr),
        New(NewExpr),
        Seq(SeqExpr),
        Ident(Ident),
        Lit(Lit),
        Tpl(Tpl),
        TaggedTpl(TaggedTpl),
        Arrow(ArrowExpr),
        Class(ClassExpr),
        Yield(YieldExpr),
        MetaProp(MetaPropExpr),
        Await(AwaitExpr),
        Paren(ParenExpr),
        JSXMember(JSXMemberExpr),
        JSXNamespacedName(JSXNamespacedName),
        JSXEmpty(JSXEmptyExpr),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
        TsTypeAssertion(TsTypeAssertion),
        TsConstAssertion(TsConstAssertion),
        TsNonNull(TsNonNullExpr),
        TsAs(TsAsExpr),
        TsSatisfies(TsSatisfiesExpr),
        TsInstantiation(TsInstantiation),
        PrivateName(PrivateName),
        OptChain(OptChainExpr),
        Invalid(Invalid),
        ContentTagExpression(ContentTagExpression),
    }
    pub struct ThisExpr {
        pub span: Span,
    }
    pub struct ArrayLit {
        pub span: Span,
        pub elems: Vec<Option<ExprOrSpread>>,
    }
    pub struct ObjectLit {
        pub span: Span,
        pub props: Vec<PropOrSpread>,
    }
    pub enum PropOrSpread {
        Spread(SpreadElement),
        Prop(Box<Prop>),
    }
    pub struct SpreadElement {
        pub dot3_token: Span,
        pub expr: Box<Expr>,
    }
    pub struct UnaryExpr {
        pub span: Span,
        pub op: UnaryOp,
        pub arg: Box<Expr>,
    }
    pub struct UpdateExpr {
        pub span: Span,
        pub op: UpdateOp,
        pub prefix: bool,
        pub arg: Box<Expr>,
    }
    pub struct BinExpr {
        pub span: Span,
        pub op: BinaryOp,
        pub left: Box<Expr>,
        pub right: Box<Expr>,
    }
    pub struct FnExpr {
        pub ident: Option<Ident>,
        pub function: Box<Function>,
    }
    pub struct ClassExpr {
        pub ident: Option<Ident>,
        pub class: Box<Class>,
    }
    pub struct AssignExpr {
        pub span: Span,
        pub op: AssignOp,
        pub left: AssignTarget,
        pub right: Box<Expr>,
    }
    pub struct MemberExpr {
        pub span: Span,
        pub obj: Box<Expr>,
        pub prop: MemberProp,
    }
    pub enum MemberProp {
        Ident(Ident),
        PrivateName(PrivateName),
        Computed(ComputedPropName),
    }
    pub struct SuperPropExpr {
        pub span: Span,
        pub obj: Super,
        pub prop: SuperProp,
    }
    pub enum SuperProp {
        Ident(Ident),
        Computed(ComputedPropName),
    }
    pub struct CondExpr {
        pub span: Span,
        pub test: Box<Expr>,
        pub cons: Box<Expr>,
        pub alt: Box<Expr>,
    }
    pub struct CallExpr {
        pub span: Span,
        pub callee: Callee,
        pub args: Vec<ExprOrSpread>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct NewExpr {
        pub span: Span,
        pub callee: Box<Expr>,
        pub args: Option<Vec<ExprOrSpread>>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct SeqExpr {
        pub span: Span,
        pub exprs: Vec<Box<Expr>>,
    }
    pub struct ArrowExpr {
        pub span: Span,
        pub params: Vec<Pat>,
        pub body: Box<BlockStmtOrExpr>,
        pub is_async: bool,
        pub is_generator: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub return_type: Option<Box<TsTypeAnn>>,
    }
    pub struct YieldExpr {
        pub span: Span,
        pub arg: Option<Box<Expr>>,
        pub delegate: bool,
    }
    pub struct MetaPropExpr {
        pub span: Span,
        pub kind: MetaPropKind,
    }
    pub enum MetaPropKind {
        NewTarget,
        ImportMeta,
    }
    pub struct AwaitExpr {
        pub span: Span,
        pub arg: Box<Expr>,
    }
    pub struct Tpl {
        pub span: Span,
        pub exprs: Vec<Box<Expr>>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TaggedTpl {
        pub span: Span,
        pub tag: Box<Expr>,
        pub type_params: Option<Box<TsTypeParamInstantiation>>,
        pub tpl: Box<Tpl>,
    }
    pub struct TplElement {
        pub span: Span,
        pub tail: bool,
        pub cooked: Option<Atom>,
        pub raw: Atom,
    }
    pub struct ParenExpr {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum Callee {
        Super(Super),
        Import(Import),
        Expr(Box<Expr>),
    }
    pub struct Super {
        pub span: Span,
    }
    pub struct Import {
        pub span: Span,
        pub phase: ImportPhase,
    }
    pub struct ExprOrSpread {
        pub spread: Option<Span>,
        pub expr: Box<Expr>,
    }
    pub enum BlockStmtOrExpr {
        BlockStmt(BlockStmt),
        Expr(Box<Expr>),
    }

    pub enum AssignTarget {
        Simple(SimpleAssignTarget),
        Pat(AssignTargetPat),
    }

    pub enum AssignTargetPat {
        Array(ArrayPat),
        Object(ObjectPat),
        Invalid(Invalid),
    }

    pub enum SimpleAssignTarget {
        Ident(BindingIdent),
        Member(MemberExpr),
        SuperProp(SuperPropExpr),
        OptChain(OptChainExpr),
        Paren(ParenExpr),
        TsAs(TsAsExpr),
        TsSatisfies(TsSatisfiesExpr),
        TsNonNull(TsNonNullExpr),
        TsTypeAssertion(TsTypeAssertion),
        TsInstantiation(TsInstantiation),
        Invalid(Invalid),
    }

    pub struct OptChainExpr {
        pub span: Span,
        pub optional: bool,
        pub base: Box<OptChainBase>,
    }
    pub enum OptChainBase {
        Member(MemberExpr),
        Call(OptCall),
    }
    pub struct OptCall {
        pub span: Span,
        pub callee: Box<Expr>,
        pub args: Vec<ExprOrSpread>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct Function {
        pub params: Vec<Param>,
        pub decorators: Vec<Decorator>,
        pub span: Span,
        pub body: Option<BlockStmt>,
        pub is_generator: bool,
        pub is_async: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub return_type: Option<Box<TsTypeAnn>>,
    }
    pub struct Param {
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub pat: Pat,
    }
    pub enum ParamOrTsParamProp {
        TsParamProp(TsParamProp),
        Param(Param),
    }

    pub struct BindingIdent {
        pub id: Ident,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }

    pub struct Ident {
        pub span: Span,
        pub sym: Atom,
        pub optional: bool,
    }

    pub struct PrivateName {
        pub span: Span,
        pub id: Ident,
    }

    pub enum JSXObject {
        JSXMemberExpr(Box<JSXMemberExpr>),
        Ident(Ident),
    }
    pub struct JSXMemberExpr {
        pub obj: JSXObject,
        pub prop: Ident,
    }
    pub struct JSXNamespacedName {
        pub ns: Ident,
        pub name: Ident,
    }
    pub struct JSXEmptyExpr {
        pub span: Span,
    }
    pub struct JSXExprContainer {
        pub span: Span,
        pub expr: JSXExpr,
    }
    pub enum JSXExpr {
        JSXEmptyExpr(JSXEmptyExpr),
        Expr(Box<Expr>),
    }
    pub struct JSXSpreadChild {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum JSXElementName {
        Ident(Ident),
        JSXMemberExpr(JSXMemberExpr),
        JSXNamespacedName(JSXNamespacedName),
    }
    pub struct JSXOpeningElement {
        pub name: JSXElementName,
        pub span: Span,
        pub attrs: Vec<JSXAttrOrSpread>,
        pub self_closing: bool,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub enum JSXAttrOrSpread {
        JSXAttr(JSXAttr),
        SpreadElement(SpreadElement),
    }
    pub struct JSXClosingElement {
        pub span: Span,
        pub name: JSXElementName,
    }
    pub struct JSXAttr {
        pub span: Span,
        pub name: JSXAttrName,
        pub value: Option<JSXAttrValue>,
    }
    pub enum JSXAttrName {
        Ident(Ident),
        JSXNamespacedName(JSXNamespacedName),
    }
    pub enum JSXAttrValue {
        Lit(Lit),
        JSXExprContainer(JSXExprContainer),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
    }
    pub struct JSXText {
        pub span: Span,
        pub value: Atom,
        pub raw: Atom,
    }
    pub struct JSXElement {
        pub span: Span,
        pub opening: JSXOpeningElement,
        pub children: Vec<JSXElementChild>,
        pub closing: Option<JSXClosingElement>,
    }
    pub enum JSXElementChild {
        JSXText(JSXText),
        JSXExprContainer(JSXExprContainer),
        JSXSpreadChild(JSXSpreadChild),
        JSXElement(Box<JSXElement>),
        JSXFragment(JSXFragment),
    }
    pub struct JSXFragment {
        pub span: Span,
        pub opening: JSXOpeningFragment,
        pub children: Vec<JSXElementChild>,
        pub closing: JSXClosingFragment,
    }
    pub struct JSXOpeningFragment {
        pub span: Span,
    }
    pub struct JSXClosingFragment {
        pub span: Span,
    }
    pub struct Invalid {
        pub span: Span,
    }
    pub enum Lit {
        Str(Str),
        Bool(Bool),
        Null(Null),
        Num(Number),
        BigInt(BigInt),
        Regex(Regex),
        JSXText(JSXText),
    }
    pub struct BigInt {
        pub span: Span,
        pub value: Box<BigIntValue>,
        pub raw: Option<Atom>,
    }
    pub struct Str {
        pub span: Span,
        pub value: Atom,
        pub raw: Option<Atom>,
    }
    pub struct Bool {
        pub span: Span,
        pub value: bool,
    }
    pub struct Null {
        pub span: Span,
    }
    pub struct Regex {
        pub span: Span,
        pub exp: Atom,
        pub flags: Atom,
    }
    pub struct Number {
        pub span: Span,
        pub value: f64,
        pub raw: Option<Atom>,
    }
    pub enum Program {
        Module(Module),
        Script(Script),
        // TODO: reenable once experimental_metadata breaking change is merged
        // ReservedUnused(ReservedUnused),
    }
    pub struct Module {
        pub span: Span,
        pub body: Vec<ModuleItem>,
        pub shebang: Option<Atom>,
    }
    pub struct Script {
        pub span: Span,
        pub body: Vec<Stmt>,
        pub shebang: Option<Atom>,
    }
    pub enum ModuleItem {
        ModuleDecl(ModuleDecl),
        Stmt(Stmt),
    }
    pub enum ModuleDecl {
        Import(ImportDecl),
        ExportDecl(ExportDecl),
        ExportNamed(NamedExport),
        ExportDefaultDecl(ExportDefaultDecl),
        ExportDefaultExpr(ExportDefaultExpr),
        ExportAll(ExportAll),
        TsImportEquals(Box<TsImportEqualsDecl>),
        TsExportAssignment(TsExportAssignment),
        TsNamespaceExport(TsNamespaceExportDecl),
    }
    pub struct ExportDefaultExpr {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct ExportDecl {
        pub span: Span,
        pub decl: Decl,
    }
    pub struct ImportDecl {
        pub span: Span,
        pub specifiers: Vec<ImportSpecifier>,
        pub src: Box<Str>,
        pub type_only: bool,
        pub with: Option<Box<ObjectLit>>,
        pub phase: ImportPhase,
    }
    pub struct ExportAll {
        pub span: Span,
        pub src: Box<Str>,
        pub type_only: bool,
        pub with: Option<Box<ObjectLit>>,
    }
    pub struct NamedExport {
        pub span: Span,
        pub specifiers: Vec<ExportSpecifier>,
        pub src: Option<Box<Str>>,
        pub type_only: bool,
        pub with: Option<Box<ObjectLit>>,
    }
    pub struct ExportDefaultDecl {
        pub span: Span,
        pub decl: DefaultDecl,
    }
    pub enum DefaultDecl {
        Class(ClassExpr),
        Fn(FnExpr),
        TsInterfaceDecl(Box<TsInterfaceDecl>),
    }
    pub enum ImportSpecifier {
        Named(ImportNamedSpecifier),
        Default(ImportDefaultSpecifier),
        Namespace(ImportStarAsSpecifier),
    }
    pub struct ImportDefaultSpecifier {
        pub span: Span,
        pub local: Ident,
    }
    pub struct ImportStarAsSpecifier {
        pub span: Span,
        pub local: Ident,
    }
    pub struct ImportNamedSpecifier {
        pub span: Span,
        pub local: Ident,
        pub imported: Option<ModuleExportName>,
        pub is_type_only: bool,
    }
    pub enum ExportSpecifier {
        Namespace(ExportNamespaceSpecifier),
        Default(ExportDefaultSpecifier),
        Named(ExportNamedSpecifier),
    }
    pub struct ExportNamespaceSpecifier {
        pub span: Span,
        pub name: ModuleExportName,
    }
    pub struct ExportDefaultSpecifier {
        pub exported: Ident,
    }
    pub enum ModuleExportName {
        Ident(Ident),
        Str(Str),
    }
    pub struct ExportNamedSpecifier {
        pub span: Span,
        pub orig: ModuleExportName,
        pub exported: Option<ModuleExportName>,
        pub is_type_only: bool,
    }
    pub enum BinaryOp {
        EqEq,
        NotEq,
        EqEqEq,
        NotEqEq,
        Lt,
        LtEq,
        Gt,
        GtEq,
        LShift,
        RShift,
        ZeroFillRShift,
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        BitOr,
        BitXor,
        BitAnd,
        LogicalOr,
        LogicalAnd,
        In,
        InstanceOf,
        Exp,
        NullishCoalescing,
    }
    pub enum AssignOp {
        Assign,
        AddAssign,
        SubAssign,
        MulAssign,
        DivAssign,
        ModAssign,
        LShiftAssign,
        RShiftAssign,
        ZeroFillRShiftAssign,
        BitOrAssign,
        BitXorAssign,
        BitAndAssign,
        ExpAssign,
        AndAssign,
        OrAssign,
        NullishAssign,
    }

    pub enum UpdateOp {
        PlusPlus,
        MinusMinus,
    }
    pub enum UnaryOp {
        Minus,
        Plus,
        Bang,
        Tilde,
        TypeOf,
        Void,
        Delete,
    }
    pub enum Pat {
        Ident(BindingIdent),
        Array(ArrayPat),
        Rest(RestPat),
        Object(ObjectPat),
        Assign(AssignPat),
        Invalid(Invalid),
        Expr(Box<Expr>),
    }
    pub struct ArrayPat {
        pub span: Span,
        pub elems: Vec<Option<Pat>>,
        pub optional: bool,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub struct ObjectPat {
        pub span: Span,
        pub props: Vec<ObjectPatProp>,
        pub optional: bool,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub struct AssignPat {
        pub span: Span,
        pub left: Box<Pat>,
        pub right: Box<Expr>,
    }
    pub struct RestPat {
        pub span: Span,
        pub dot3_token: Span,
        pub arg: Box<Pat>,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub enum ObjectPatProp {
        KeyValue(KeyValuePatProp),
        Assign(AssignPatProp),
        Rest(RestPat),
    }
    pub struct KeyValuePatProp {
        pub key: PropName,
        pub value: Box<Pat>,
    }
    pub struct AssignPatProp {
        pub span: Span,
        pub key: BindingIdent,
        pub value: Option<Box<Expr>>,
    }
    pub enum Prop {
        Shorthand(Ident),
        KeyValue(KeyValueProp),
        Assign(AssignProp),
        Getter(GetterProp),
        Setter(SetterProp),
        Method(MethodProp),
    }
    pub struct KeyValueProp {
        pub key: PropName,
        pub value: Box<Expr>,
    }
    pub struct AssignProp {
        pub key: Ident,
        pub value: Box<Expr>,
    }
    pub struct GetterProp {
        pub span: Span,
        pub key: PropName,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub body: Option<BlockStmt>,
    }
    pub struct SetterProp {
        pub span: Span,
        pub key: PropName,
        pub this_param: Option<Pat>,
        pub param: Box<Pat>,
        pub body: Option<BlockStmt>,
    }
    pub struct MethodProp {
        pub key: PropName,
        pub function: Box<Function>,
    }
    pub enum PropName {
        Ident(Ident),
        Str(Str),
        Num(Number),
        BigInt(BigInt),
        Computed(ComputedPropName),
    }
    pub struct ComputedPropName {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct BlockStmt {
        pub span: Span,
        pub stmts: Vec<Stmt>,
    }
    pub enum Stmt {
        Block(BlockStmt),
        Empty(EmptyStmt),
        Debugger(DebuggerStmt),
        With(WithStmt),
        Return(ReturnStmt),
        Labeled(LabeledStmt),
        Break(BreakStmt),
        Continue(ContinueStmt),
        If(IfStmt),
        Switch(SwitchStmt),
        Throw(ThrowStmt),
        Try(Box<TryStmt>),
        While(WhileStmt),
        DoWhile(DoWhileStmt),
        For(ForStmt),
        ForIn(ForInStmt),
        ForOf(ForOfStmt),
        Decl(Decl),
        Expr(ExprStmt),
    }
    pub struct ExprStmt {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct EmptyStmt {
        pub span: Span,
    }
    pub struct DebuggerStmt {
        pub span: Span,
    }
    pub struct WithStmt {
        pub span: Span,
        pub obj: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ReturnStmt {
        pub span: Span,
        pub arg: Option<Box<Expr>>,
    }
    pub struct LabeledStmt {
        pub span: Span,
        pub label: Ident,
        pub body: Box<Stmt>,
    }
    pub struct BreakStmt {
        pub span: Span,
        pub label: Option<Ident>,
    }
    pub struct ContinueStmt {
        pub span: Span,
        pub label: Option<Ident>,
    }
    pub struct IfStmt {
        pub span: Span,
        pub test: Box<Expr>,
        pub cons: Box<Stmt>,
        pub alt: Option<Box<Stmt>>,
    }
    pub struct SwitchStmt {
        pub span: Span,
        pub discriminant: Box<Expr>,
        pub cases: Vec<SwitchCase>,
    }
    pub struct ThrowStmt {
        pub span: Span,
        pub arg: Box<Expr>,
    }
    pub struct TryStmt {
        pub span: Span,
        pub block: BlockStmt,
        pub handler: Option<CatchClause>,
        pub finalizer: Option<BlockStmt>,
    }
    pub struct WhileStmt {
        pub span: Span,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct DoWhileStmt {
        pub span: Span,
        pub test: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForStmt {
        pub span: Span,
        pub init: Option<VarDeclOrExpr>,
        pub test: Option<Box<Expr>>,
        pub update: Option<Box<Expr>>,
        pub body: Box<Stmt>,
    }
    pub struct ForInStmt {
        pub span: Span,
        pub left: ForHead,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct ForOfStmt {
        pub span: Span,
        pub is_await: bool,
        pub left: ForHead,
        pub right: Box<Expr>,
        pub body: Box<Stmt>,
    }
    pub struct SwitchCase {
        pub span: Span,
        pub test: Option<Box<Expr>>,
        pub cons: Vec<Stmt>,
    }
    pub struct CatchClause {
        pub span: Span,
        pub param: Option<Pat>,
        pub body: BlockStmt,
    }
    pub enum ForHead {
        VarDecl(Box<VarDecl>),
        UsingDecl(Box<UsingDecl>),
        Pat(Box<Pat>),
    }
    pub enum VarDeclOrExpr {
        VarDecl(Box<VarDecl>),
        Expr(Box<Expr>),
    }
    pub struct TsTypeAnn {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeParamDecl {
        pub span: Span,
        pub params: Vec<TsTypeParam>,
    }
    pub struct TsTypeParam {
        pub span: Span,
        pub name: Ident,
        pub is_in: bool,
        pub is_out: bool,
        pub is_const: bool,
        pub constraint: Option<Box<TsType>>,
        pub default: Option<Box<TsType>>,
    }
    pub struct TsTypeParamInstantiation {
        pub span: Span,
        pub params: Vec<Box<TsType>>,
    }
    pub struct TsParamProp {
        pub span: Span,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
        pub is_override: bool,
        pub readonly: bool,
        pub param: TsParamPropParam,
    }
    pub enum TsParamPropParam {
        Ident(BindingIdent),
        Assign(AssignPat),
    }
    pub struct TsQualifiedName {
        pub left: TsEntityName,
        pub right: Ident,
    }
    pub enum TsEntityName {
        TsQualifiedName(Box<TsQualifiedName>),
        Ident(Ident),
    }
    pub enum TsTypeElement {
        TsCallSignatureDecl(TsCallSignatureDecl),
        TsConstructSignatureDecl(TsConstructSignatureDecl),
        TsPropertySignature(TsPropertySignature),
        TsGetterSignature(TsGetterSignature),
        TsSetterSignature(TsSetterSignature),
        TsMethodSignature(TsMethodSignature),
        TsIndexSignature(TsIndexSignature),
    }
    pub struct TsCallSignatureDecl {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }
    pub struct TsConstructSignatureDecl {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }
    pub struct TsPropertySignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub init: Option<Box<Expr>>,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }

    pub struct TsGetterSignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }

    pub struct TsSetterSignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub param: TsFnParam,
    }
    pub struct TsMethodSignature {
        pub span: Span,
        pub readonly: bool,
        pub key: Box<Expr>,
        pub computed: bool,
        pub optional: bool,
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
    }
    pub struct TsIndexSignature {
        pub params: Vec<TsFnParam>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub readonly: bool,
        pub is_static: bool,
        pub span: Span,
    }
    pub enum TsType {
        TsKeywordType(TsKeywordType),
        TsThisType(TsThisType),
        TsFnOrConstructorType(TsFnOrConstructorType),
        TsTypeRef(TsTypeRef),
        TsTypeQuery(TsTypeQuery),
        TsTypeLit(TsTypeLit),
        TsArrayType(TsArrayType),
        TsTupleType(TsTupleType),
        TsOptionalType(TsOptionalType),
        TsRestType(TsRestType),
        TsUnionOrIntersectionType(TsUnionOrIntersectionType),
        TsConditionalType(TsConditionalType),
        TsInferType(TsInferType),
        TsParenthesizedType(TsParenthesizedType),
        TsTypeOperator(TsTypeOperator),
        TsIndexedAccessType(TsIndexedAccessType),
        TsMappedType(TsMappedType),
        TsLitType(TsLitType),
        TsTypePredicate(TsTypePredicate),
        TsImportType(TsImportType),
    }
    pub enum TsFnOrConstructorType {
        TsFnType(TsFnType),
        TsConstructorType(TsConstructorType),
    }
    pub struct TsKeywordType {
        pub span: Span,
        pub kind: TsKeywordTypeKind,
    }
    pub enum TsKeywordTypeKind {
        TsAnyKeyword,
        TsUnknownKeyword,
        TsNumberKeyword,
        TsObjectKeyword,
        TsBooleanKeyword,
        TsBigIntKeyword,
        TsStringKeyword,
        TsSymbolKeyword,
        TsVoidKeyword,
        TsUndefinedKeyword,
        TsNullKeyword,
        TsNeverKeyword,
        TsIntrinsicKeyword,
    }
    pub struct TsThisType {
        pub span: Span,
    }
    pub enum TsFnParam {
        Ident(BindingIdent),
        Array(ArrayPat),
        Rest(RestPat),
        Object(ObjectPat),
    }
    pub struct TsFnType {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub type_ann: Box<TsTypeAnn>,
    }
    pub struct TsConstructorType {
        pub span: Span,
        pub params: Vec<TsFnParam>,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub type_ann: Box<TsTypeAnn>,
        pub is_abstract: bool,
    }
    pub struct TsTypeRef {
        pub span: Span,
        pub type_name: TsEntityName,
        pub type_params: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct TsTypePredicate {
        pub span: Span,
        pub asserts: bool,
        pub param_name: TsThisTypeOrIdent,
        pub type_ann: Option<Box<TsTypeAnn>>,
    }
    pub enum TsThisTypeOrIdent {
        TsThisType(TsThisType),
        Ident(Ident),
    }
    pub struct TsTypeQuery {
        pub span: Span,
        pub expr_name: TsTypeQueryExpr,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub enum TsTypeQueryExpr {
        TsEntityName(TsEntityName),
        Import(TsImportType),
    }
    pub struct TsImportType {
        pub span: Span,
        pub arg: Str,
        pub qualifier: Option<TsEntityName>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct TsTypeLit {
        pub span: Span,
        pub members: Vec<TsTypeElement>,
    }
    pub struct TsArrayType {
        pub span: Span,
        pub elem_type: Box<TsType>,
    }

    pub struct TsTupleType {
        pub span: Span,
        pub elem_types: Vec<TsTupleElement>,
    }

    pub struct TsTupleElement {
        pub span: Span,
        pub label: Option<Pat>,
        pub ty: Box<TsType>,
    }

    pub struct TsOptionalType {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsRestType {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub enum TsUnionOrIntersectionType {
        TsUnionType(TsUnionType),
        TsIntersectionType(TsIntersectionType),
    }
    pub struct TsUnionType {
        pub span: Span,
        pub types: Vec<Box<TsType>>,
    }
    pub struct TsIntersectionType {
        pub span: Span,
        pub types: Vec<Box<TsType>>,
    }
    pub struct TsConditionalType {
        pub span: Span,
        pub check_type: Box<TsType>,
        pub extends_type: Box<TsType>,
        pub true_type: Box<TsType>,
        pub false_type: Box<TsType>,
    }
    pub struct TsInferType {
        pub span: Span,
        pub type_param: TsTypeParam,
    }
    pub struct TsParenthesizedType {
        pub span: Span,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeOperator {
        pub span: Span,
        pub op: TsTypeOperatorOp,
        pub type_ann: Box<TsType>,
    }
    pub enum TsTypeOperatorOp {
        KeyOf,
        Unique,
        ReadOnly,
    }
    pub struct TsIndexedAccessType {
        pub span: Span,
        pub readonly: bool,
        pub obj_type: Box<TsType>,
        pub index_type: Box<TsType>,
    }
    pub enum TruePlusMinus {
        True,
        Plus,
        Minus,
    }
    pub struct TsMappedType {
        pub span: Span,
        pub readonly: Option<TruePlusMinus>,
        pub type_param: TsTypeParam,
        pub name_type: Option<Box<TsType>>,
        pub optional: Option<TruePlusMinus>,
        pub type_ann: Option<Box<TsType>>,
    }
    pub struct TsLitType {
        pub span: Span,
        pub lit: TsLit,
    }
    pub enum TsLit {
        BigInt(BigInt),
        Number(Number),
        Str(Str),
        Bool(Bool),
        Tpl(TsTplLitType),
    }
    pub struct TsTplLitType {
        pub span: Span,
        pub types: Vec<Box<TsType>>,
        pub quasis: Vec<TplElement>,
    }
    pub struct TsInterfaceDecl {
        pub span: Span,
        pub id: Ident,
        pub declare: bool,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub extends: Vec<TsExprWithTypeArgs>,
        pub body: TsInterfaceBody,
    }
    pub struct TsInterfaceBody {
        pub span: Span,
        pub body: Vec<TsTypeElement>,
    }
    pub struct TsExprWithTypeArgs {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_args: Option<Box<TsTypeParamInstantiation>>,
    }
    pub struct TsTypeAliasDecl {
        pub span: Span,
        pub declare: bool,
        pub id: Ident,
        pub type_params: Option<Box<TsTypeParamDecl>>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsEnumDecl {
        pub span: Span,
        pub declare: bool,
        pub is_const: bool,
        pub id: Ident,
        pub members: Vec<TsEnumMember>,
    }
    pub struct TsEnumMember {
        pub span: Span,
        pub id: TsEnumMemberId,
        pub init: Option<Box<Expr>>,
    }
    pub enum TsEnumMemberId {
        Ident(Ident),
        Str(Str),
    }
    pub struct TsModuleDecl {
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: TsModuleName,
        pub body: Option<TsNamespaceBody>,
    }
    pub enum TsNamespaceBody {
        TsModuleBlock(TsModuleBlock),
        TsNamespaceDecl(TsNamespaceDecl),
    }
    pub struct TsModuleBlock {
        pub span: Span,
        pub body: Vec<ModuleItem>,
    }
    pub struct TsNamespaceDecl {
        pub span: Span,
        pub declare: bool,
        pub global: bool,
        pub id: Ident,
        pub body: Box<TsNamespaceBody>,
    }
    pub enum TsModuleName {
        Ident(Ident),
        Str(Str),
    }
    pub struct TsImportEqualsDecl {
        pub span: Span,
        pub is_export: bool,
        pub is_type_only: bool,
        pub id: Ident,
        pub module_ref: TsModuleRef,
    }
    pub enum TsModuleRef {
        TsEntityName(TsEntityName),
        TsExternalModuleRef(TsExternalModuleRef),
    }
    pub struct TsExternalModuleRef {
        pub span: Span,
        pub expr: Str,
    }
    pub struct TsExportAssignment {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub struct TsNamespaceExportDecl {
        pub span: Span,
        pub id: Ident,
    }
    pub struct TsAsExpr {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsTypeAssertion {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }
    pub struct TsNonNullExpr {
        pub span: Span,
        pub expr: Box<Expr>,
    }
    pub enum Accessibility {
        Public,
        Protected,
        Private,
    }
    pub struct TsConstAssertion {
        pub span: Span,
        pub expr: Box<Expr>,
    }

    pub struct TsInstantiation {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_args: Box<TsTypeParamInstantiation>,
    }

    pub struct TsSatisfiesExpr {
        pub span: Span,
        pub expr: Box<Expr>,
        pub type_ann: Box<TsType>,
    }

    pub struct ReservedUnused {
        pub span: Span,
        pub body: Option<Vec<ModuleItem>>,
    }

    pub struct AutoAccessor {
        pub span: Span,
        pub key: Key,
        pub value: Option<Box<Expr>>,
        pub type_ann: Option<Box<TsTypeAnn>>,
        pub is_static: bool,
        pub decorators: Vec<Decorator>,
        pub accessibility: Option<Accessibility>,
        pub is_override: bool,
        pub definite: bool,
    }

    pub enum Key {
        Private(PrivateName),
        Public(PropName),
    }

    pub struct UsingDecl {
        pub span: Span,

        pub is_await: bool,

        pub decls: Vec<VarDeclarator>,
    }
});

#[macro_export]
macro_rules! visit_obj_and_computed {
    () => {
        fn visit_member_prop(&mut self, n: &$crate::swc_ecma_ast::MemberProp) {
            if let $crate::swc_ecma_ast::MemberProp::Computed(c) = n {
                c.visit_with(self);
            }
        }

        fn visit_jsx_member_expr(&mut self, n: &$crate::swc_ecma_ast::JSXMemberExpr) {
            n.obj.visit_with(self);
        }

        fn visit_super_prop(&mut self, n: &$crate::swc_ecma_ast::SuperProp) {
            if let $crate::swc_ecma_ast::SuperProp::Computed(c) = n {
                c.visit_with(self);
            }
        }
    };
}

#[macro_export]
macro_rules! visit_mut_obj_and_computed {
    () => {
        fn visit_mut_member_prop(&mut self, n: &mut $crate::swc_ecma_ast::MemberProp) {
            if let $crate::swc_ecma_ast::MemberProp::Computed(c) = n {
                c.visit_mut_with(self);
            }
        }

        fn visit_mut_jsx_member_expr(&mut self, n: &mut $crate::swc_ecma_ast::JSXMemberExpr) {
            n.obj.visit_mut_with(self);
        }

        fn visit_mut_super_prop(&mut self, n: &mut $crate::swc_ecma_ast::SuperProp) {
            if let $crate::swc_ecma_ast::SuperProp::Computed(c) = n {
                c.visit_mut_with(self);
            }
        }
    };
}

macro_rules! impl_traits_for_tuple {
    (
        [$idx:tt, $name:ident], $([$idx_rest:tt, $name_rest:ident]),*
    ) => {
        impl<$name, $($name_rest),*> VisitMut for ($name, $($name_rest),*)
        where
            $name: VisitMut,
            $($name_rest: VisitMut),*
        {

            fn visit_mut_program(&mut self, program: &mut Program) {
                self.$idx.visit_mut_program(program);

                $(
                    self.$idx_rest.visit_mut_program(program);
                )*
            }

            fn visit_mut_module(&mut self, module: &mut Module) {
                self.$idx.visit_mut_module(module);

                $(
                    self.$idx_rest.visit_mut_module(module);
                )*
            }

            fn visit_mut_script(&mut self, script: &mut Script) {
                self.$idx.visit_mut_script(script);

                $(
                    self.$idx_rest.visit_mut_script(script);
                )*
            }

            fn visit_mut_stmt(&mut self, stmt: &mut Stmt) {
                self.$idx.visit_mut_stmt(stmt);

                $(
                    self.$idx_rest.visit_mut_stmt(stmt);
                )*
            }

            fn visit_mut_expr(&mut self, expr: &mut Expr) {
                self.$idx.visit_mut_expr(expr);

                $(
                    self.$idx_rest.visit_mut_expr(expr);
                )*
            }

            fn visit_mut_pat(&mut self, pat: &mut Pat) {
                self.$idx.visit_mut_pat(pat);

                $(
                    self.$idx_rest.visit_mut_pat(pat);
                )*
            }

            fn visit_mut_assign_target(&mut self, target: &mut AssignTarget) {
                self.$idx.visit_mut_assign_target(target);

                $(
                    self.$idx_rest.visit_mut_assign_target(target);
                )*
            }

            fn visit_mut_ident(&mut self, ident: &mut Ident) {
                self.$idx.visit_mut_ident(ident);

                $(
                    self.$idx_rest.visit_mut_ident(ident);
                )*
            }
        }
    };
}

impl_traits_for_tuple!([0, A], [1, B]);
impl_traits_for_tuple!([0, A], [1, B], [2, C]);
impl_traits_for_tuple!([0, A], [1, B], [2, C], [3, D]);
impl_traits_for_tuple!([0, A], [1, B], [2, C], [3, D], [4, E]);
impl_traits_for_tuple!([0, A], [1, B], [2, C], [3, D], [4, E], [5, F]);
impl_traits_for_tuple!([0, A], [1, B], [2, C], [3, D], [4, E], [5, F], [6, G]);
impl_traits_for_tuple!(
    [0, A],
    [1, B],
    [2, C],
    [3, D],
    [4, E],
    [5, F],
    [6, G],
    [7, H]
);
impl_traits_for_tuple!(
    [0, A],
    [1, B],
    [2, C],
    [3, D],
    [4, E],
    [5, F],
    [6, G],
    [7, H],
    [8, I]
);
impl_traits_for_tuple!(
    [0, A],
    [1, B],
    [2, C],
    [3, D],
    [4, E],
    [5, F],
    [6, G],
    [7, H],
    [8, I],
    [9, J]
);
impl_traits_for_tuple!(
    [0, A],
    [1, B],
    [2, C],
    [3, D],
    [4, E],
    [5, F],
    [6, G],
    [7, H],
    [8, I],
    [9, J],
    [10, K]
);
impl_traits_for_tuple!(
    [0, A],
    [1, B],
    [2, C],
    [3, D],
    [4, E],
    [5, F],
    [6, G],
    [7, H],
    [8, I],
    [9, J],
    [10, K],
    [11, L]
);
impl_traits_for_tuple!(
    [0, A],
    [1, B],
    [2, C],
    [3, D],
    [4, E],
    [5, F],
    [6, G],
    [7, H],
    [8, I],
    [9, J],
    [10, K],
    [11, L],
    [12, M]
);
