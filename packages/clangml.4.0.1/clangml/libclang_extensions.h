#include <stdbool.h>
#include <stdint.h>

#ifdef LLVM_VERSION_3_4_2
#define LLVM_VERSION_BEFORE_3_5_0
#define LLVM_VERSION_BEFORE_3_6_0
#endif
#ifdef LLVM_VERSION_3_5_2
#define LLVM_VERSION_BEFORE_3_6_0
#endif
#ifdef LLVM_VERSION_BEFORE_3_6_0
#define LLVM_VERSION_BEFORE_3_8_0
#endif
#ifdef LLVM_VERSION_3_6_2
#define LLVM_VERSION_BEFORE_3_8_0
#endif
#ifdef LLVM_VERSION_3_7_1
#define LLVM_VERSION_BEFORE_3_8_0
#endif
#ifdef LLVM_VERSION_BEFORE_3_8_0
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_8_1
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_BEFORE_3_9_0
#define LLVM_VERSION_BEFORE_4_0_0
#endif
#ifdef LLVM_VERSION_3_9_1
#define LLVM_VERSION_BEFORE_4_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_4_0_0
#define LLVM_VERSION_BEFORE_5_0_0
#endif
#ifdef LLVM_VERSION_4_0_1
#define LLVM_VERSION_BEFORE_5_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_5_0_0
#define LLVM_VERSION_BEFORE_7_0_0
#endif
#ifdef LLVM_VERSION_5_0_2
#define LLVM_VERSION_BEFORE_7_0_0
#endif
#ifdef LLVM_VERSION_6_0_1
#define LLVM_VERSION_BEFORE_7_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_7_0_0
#define LLVM_VERSION_BEFORE_8_0_0
#endif
#ifdef LLVM_VERSION_7_1_0
#define LLVM_VERSION_BEFORE_8_0_0
#endif
#ifdef LLVM_VERSION_BEFORE_8_0_0
#define LLVM_VERSION_BEFORE_9_0_0
#endif
#ifdef LLVM_VERSION_8_0_1
#define LLVM_VERSION_BEFORE_9_0_0
#endif

CXVersion
clang_ext_getVersion();

typedef struct {
  void *data;
} CXInt;

bool
clang_equal_cxint(CXInt a, CXInt b);

int
clang_compare_cxint(CXInt a, CXInt b);

CXInt
clang_ext_IntegerLiteral_getValue(CXCursor c);

void
clang_ext_Int_dispose(CXInt c);

bool
clang_ext_Int_isValid(CXInt c);

CXString
clang_ext_Int_toString(CXInt c, unsigned Radix, bool isSigned);

double
clang_ext_Int_roundToDouble(CXInt c, bool isSigned);

float
clang_ext_Int_bitsToFloat(CXInt c);

unsigned
clang_ext_Int_getBitWidth(CXInt c);

unsigned
clang_ext_Int_getActiveBits(CXInt c);

unsigned
clang_ext_Int_getMinSignedBits(CXInt c);

bool
clang_ext_Int_getBoolValue(CXInt c);

int
clang_ext_Int_getSExtValue(CXInt c);

int64_t
clang_ext_Int_getSExtValue64(CXInt c);

typedef struct {
  void *data;
} CXFloat;

bool
clang_equal_cxfloat(CXFloat a, CXFloat b);

int
clang_compare_cxfloat(CXFloat a, CXFloat b);

CXFloat
clang_ext_FloatingLiteral_getValue(CXCursor c);

void
clang_ext_Float_dispose(CXFloat c);

bool
clang_ext_Float_isValid(CXFloat f);

CXString
clang_ext_Float_toString(CXFloat f);

enum clang_ext_fltSemantics {
  CLANG_EXT_fltSemantics_IEEEhalf,
  CLANG_EXT_fltSemantics_IEEEsingle,
  CLANG_EXT_fltSemantics_IEEEdouble,
  CLANG_EXT_fltSemantics_IEEEquad,
  CLANG_EXT_fltSemantics_PPCDoubleDouble,
  CLANG_EXT_fltSemantics_x87DoubleExtended,
  CLANG_EXT_fltSemantics_Bogus,
  CLANG_EXT_fltSemantics_Invalid
};

enum clang_ext_fltSemantics
clang_ext_Float_getSemantics(CXFloat f);

float
clang_ext_Float_convertToFloat(CXFloat f);

double
clang_ext_Float_convertToDouble(CXFloat f);

CXString
clang_ext_StringLiteral_GetString(CXCursor c);

CXString
clang_ext_StringLiteral_getBytes(CXCursor c);

unsigned int
clang_ext_StringLiteral_getByteLength(CXCursor c);

unsigned int
clang_ext_StringLiteral_getCharByteWidth(CXCursor c);

/* Copied from Expr.h:StringLiteral:StringKind */
enum clang_ext_StringKind {
  clang_ext_StringKind_Ascii,
  clang_ext_StringKind_Wide,
  clang_ext_StringKind_UTF8,
  clang_ext_StringKind_UTF16,
  clang_ext_StringKind_UTF32,
  clang_ext_StringKind_InvalidStringKind
};

enum clang_ext_StringKind
clang_ext_StringLiteral_getKind(CXCursor c);

#ifdef LLVM_VERSION_BEFORE_3_9_0
#define CLANG_EXT_UNARY_OPERATOR_InvalidUnaryOperator \
  CLANG_EXT_UNARY_OPERATOR_UO_Invalid
#endif

enum clang_ext_UnaryOperatorKind {
  #define UNARY_OPERATION(Name, Spelling) CLANG_EXT_UNARY_OPERATOR_##Name,
  #include "clangml_OperationKinds.def"

  CLANG_EXT_UNARY_OPERATOR_InvalidUnaryOperator
};

enum clang_ext_UnaryOperatorKind
clang_ext_UnaryOperator_getOpcode(CXCursor c);

CXString
clang_ext_UnaryOperator_getOpcodeSpelling(
  enum clang_ext_UnaryOperatorKind Kind);

#ifdef LLVM_VERSION_BEFORE_3_9_0
#define CLANG_EXT_BINARY_OPERATOR_InvalidBinaryOperator \
  CLANG_EXT_BINARY_OPERATOR_BO_Invalid
#endif

enum clang_ext_BinaryOperatorKind {
  #define BINARY_OPERATION(Name, Spelling) CLANG_EXT_BINARY_OPERATOR_##Name,
  #include "clangml_OperationKinds.def"
  CLANG_EXT_BINARY_OPERATOR_InvalidBinaryOperator
};

enum clang_ext_BinaryOperatorKind
clang_ext_BinaryOperator_getOpcode(
  CXCursor c);

CXString
clang_ext_BinaryOperator_getOpcodeSpelling(
  enum clang_ext_BinaryOperatorKind Kind);

enum clang_ext_ForStmtChild {
  CLANG_EXT_FOR_STMT_INIT = 1,
  CLANG_EXT_FOR_STMT_CONDITION_VARIABLE = 2,
  CLANG_EXT_FOR_STMT_COND = 4,
  CLANG_EXT_FOR_STMT_INC = 8
};

unsigned
clang_ext_ForStmt_getChildrenSet(CXCursor c);

enum clang_ext_IfStmtChild {
  CLANG_EXT_IF_STMT_INIT = 1,
  CLANG_EXT_IF_STMT_CONDITION_VARIABLE = 2,
  CLANG_EXT_IF_STMT_ELSE = 4
};

unsigned
clang_ext_IfStmt_getChildrenSet(CXCursor c);

CXCursor
clang_ext_IfStmt_getInit(CXCursor c);

enum clang_ext_SwitchStmtChild {
  CLANG_EXT_SWITCH_STMT_INIT = 1,
  CLANG_EXT_SWITCH_STMT_CONDITION_VARIABLE = 2
};

unsigned
clang_ext_SwitchStmt_getChildrenSet(CXCursor c);

CXCursor
clang_ext_SwitchStmt_getInit(CXCursor c);

enum clang_ext_WhileStmtChild {
  CLANG_EXT_WHILE_STMT_CONDITION_VARIABLE = 1
};

unsigned
clang_ext_WhileStmt_getChildrenSet(CXCursor c);

/* From clang/AST/Type.h */
enum clang_ext_ElaboratedTypeKeyword {
  ETK_Struct,
  ETK_Interface,
  ETK_Union,
  ETK_Class,
  ETK_Enum,
  ETK_Typename,
  ETK_NoKeyword
};

enum clang_ext_ElaboratedTypeKeyword
clang_ext_ElaboratedType_getKeyword(CXType c);

CXString
clang_ext_ElaboratedType_getKeywordSpelling(
  enum clang_ext_ElaboratedTypeKeyword keyword);

bool
clang_ext_VarDecl_hasInit(CXCursor c);

bool
clang_ext_VarDecl_isConstexpr(CXCursor c);

bool
clang_ext_MemberRefExpr_isArrow(CXCursor c);

CXString
clang_ext_Stmt_GetClassName(CXCursor c);

int
clang_ext_Stmt_GetClassKind(CXCursor c);

enum clang_ext_CursorKind {
  ECK_ImplicitCastExpr,
  ECK_BinaryConditionalOperator,
  ECK_UnaryExprOrTypeTraitExpr, /* for Clang <3.9.0 */
  ECK_EmptyDecl,
  ECK_LinkageSpecDecl,
  ECK_Unknown
};

enum clang_ext_CursorKind
clang_ext_GetCursorKind(CXCursor c);

enum clang_ext_DeclKind {
  CLANG_EXT_DECL_InvalidDecl,
  #define DECL(Class, _Base) CLANG_EXT_DECL_##Class,
  #define ABSTRACT_DECL(_Decl)
  #include <clang/AST/DeclNodes.inc>
  CLANG_EXT_DECL_UnknownDecl
};

enum clang_ext_DeclKind
clang_ext_Decl_GetKind(CXCursor);

enum clang_ext_StmtKind {
  CLANG_EXT_STMT_InvalidStmt,
  #define STMT(Class, _Base) CLANG_EXT_STMT_##Class,
  #define ABSTRACT_STMT(_Stmt)
  #include <clang/AST/StmtNodes.inc>
  CLANG_EXT_STMT_UnknownStmt
};

enum clang_ext_StmtKind
clang_ext_Stmt_GetKind(CXCursor);

enum clang_ext_TypeKind {
  CLANG_EXT_TYPE_InvalidType,
  #define TYPE(Class, _Base) CLANG_EXT_TYPE_##Class,
  #define ABSTRACT_TYPE(_Class, _Base)
  #include <clang/AST/TypeNodes.def>
  CLANG_EXT_TYPE_UnknownType
};

enum clang_ext_TypeKind
clang_ext_Type_GetKind(CXType c);

enum clang_ext_TypeKind
clang_ext_GetTypeKind(CXType c);

CXType
clang_ext_GetInnerType(CXType c);

CXCursor
clang_ext_VariableArrayType_GetSizeExpr(CXType c);

enum clang_ext_StringKind
clang_ext_CharacterLiteral_GetCharacterKind(CXCursor c);

unsigned
clang_ext_CharacterLiteral_GetValue(CXCursor c);

/* From clang/Basic/TypeTraits.h */
enum clang_ext_UnaryExpr {
  UETT_SizeOf,
  UETT_AlignOf,
  UETT_VecStep,
  UETT_OpenMPRequiredSimdAlign,
  UETT_PreferredAlignOf,
};

enum clang_ext_UnaryExpr
clang_ext_UnaryExpr_GetKind(CXCursor);

bool
clang_ext_UnaryExpr_isArgumentType(CXCursor);

CXType
clang_ext_UnaryExpr_GetArgumentType(CXCursor);

CXType
clang_ext_Type_getNamedType(CXType CT);

enum clang_ext_AttrKind {
  CLANG_EXT_ATTR_NoAttr = -1,
  #define ATTR(Name) CLANG_EXT_ATTR_##Name,
  #include <clang/Basic/AttrList.inc>
};

enum clang_ext_AttrKind
clang_ext_Type_GetAttributeKind(CXType CT);

CXString
clang_ext_AttrKind_GetSpelling(enum clang_ext_AttrKind AttrKind);

unsigned
clang_ext_CXXMethod_isDefaulted(CXCursor C);

unsigned
clang_ext_CXXMethod_isConst(CXCursor C);

unsigned
clang_ext_CXXConstructor_isExplicit(CXCursor C);

unsigned
clang_ext_FunctionDecl_isDeleted(CXCursor C);

unsigned
clang_ext_FunctionDecl_getNumParams(CXCursor C);

CXCursor
clang_ext_FunctionDecl_getParamDecl(CXCursor C, unsigned i);

bool
clang_ext_FunctionDecl_isConstexpr(CXCursor c);

/* Adapted from DeclCXX.h:LinkageSpecDecl:LanguageIDs */
enum clang_ext_LanguageIDs {
  CLANG_EXT_LANG_C = 0x0002,
  CLANG_EXT_LANG_CXX = 0x0004
};

unsigned
clang_ext_LinkageSpecDecl_getLanguageIDs(CXCursor C);

CXType
clang_ext_TemplateTypeParmDecl_getDefaultArgument(CXCursor C);

struct clang_ext_TemplateName {
  const void *data;
  CXTranslationUnit TU;
};

void
clang_ext_TemplateName_dispose(struct clang_ext_TemplateName);

enum clang_ext_TemplateName_NameKind {
  CLANG_EXT_Template,
  CLANG_EXT_OverloadedTemplate,
  CLANG_EXT_QualifiedTemplate,
  CLANG_EXT_DependentTemplate,
  CLANG_EXT_SubstTemplateTemplateParm,
  CLANG_EXT_SubstTemplateTemplateParmPack,
  CLANG_EXT_InvalidNameKind
};

enum clang_ext_TemplateName_NameKind
clang_ext_TemplateName_getKind(struct clang_ext_TemplateName);

CXCursor
clang_ext_TemplateName_getAsTemplateDecl(struct clang_ext_TemplateName);

struct clang_ext_TemplateArgument {
  const void *data;
  CXTranslationUnit TU;
};

void
clang_ext_TemplateArgument_dispose(struct clang_ext_TemplateArgument);

#ifdef LLVM_VERSION_BEFORE_3_6_0
enum CXTemplateArgumentKind {
  CXTemplateArgumentKind_Null,
  CXTemplateArgumentKind_Type,
  CXTemplateArgumentKind_Declaration,
  CXTemplateArgumentKind_NullPtr,
  CXTemplateArgumentKind_Integral,
  CXTemplateArgumentKind_Template,
  CXTemplateArgumentKind_TemplateExpansion,
  CXTemplateArgumentKind_Expression,
  CXTemplateArgumentKind_Pack,
  CXTemplateArgumentKind_Invalid
};
#endif

enum CXTemplateArgumentKind
clang_ext_TemplateArgument_getKind(struct clang_ext_TemplateArgument);

CXType
clang_ext_TemplateArgument_getAsType(struct clang_ext_TemplateArgument);

CXCursor
clang_ext_TemplateArgument_getAsDecl(struct clang_ext_TemplateArgument);

CXType
clang_ext_TemplateArgument_getNullPtrType(struct clang_ext_TemplateArgument);

struct clang_ext_TemplateName
clang_ext_TemplateArgument_getAsTemplateOrTemplatePattern(
  struct clang_ext_TemplateArgument);

CXInt
clang_ext_TemplateArgument_getAsIntegral(struct clang_ext_TemplateArgument);

CXType
clang_ext_TemplateArgument_getIntegralType(struct clang_ext_TemplateArgument);

CXCursor
clang_ext_TemplateArgument_getAsExpr(struct clang_ext_TemplateArgument);

unsigned int
clang_ext_TemplateArgument_getPackSize(struct clang_ext_TemplateArgument);

struct clang_ext_TemplateArgument
clang_ext_TemplateArgument_getPackArgument(
  struct clang_ext_TemplateArgument, unsigned int);

struct clang_ext_TemplateArgument
clang_ext_TemplateArgument_getPackExpansionPattern(
  struct clang_ext_TemplateArgument);

struct clang_ext_TemplateName
clang_ext_TemplateSpecializationType_getTemplateName(CXType);

unsigned
clang_ext_TemplateSpecializationType_getNumArgs(CXType);

struct clang_ext_TemplateArgument
clang_ext_TemplateSpecializationType_getArgument(CXType, unsigned);

CXCursor
clang_ext_FriendDecl_getFriendDecl(CXCursor c);

CXType
clang_ext_FriendDecl_getFriendType(CXCursor c);

CXCursor
clang_ext_FieldDecl_getInClassInitializer(CXCursor c);

CXType
clang_ext_GenericSelectionExpr_getAssocType(CXCursor c, unsigned i);

bool
clang_ext_TemplateParm_isParameterPack(CXCursor c);

CXCursor
clang_ext_ClassTemplateDecl_getTemplatedDecl(CXCursor c);

/* Copied from AST/Expr.h */
enum clang_ext_PredefinedExpr_IdentKind {
  clang_ext_PredefinedExpr_Func,
  clang_ext_PredefinedExpr_Function,
  clang_ext_PredefinedExpr_LFunction,
  clang_ext_PredefinedExpr_FuncDName,
  clang_ext_PredefinedExpr_FuncSig,
  clang_ext_PredefinedExpr_LFuncSig,
  clang_ext_PredefinedExpr_PrettyFunction,
  clang_ext_PredefinedExpr_PrettyFunctionNoVirtual,
  clang_ext_PredefinedExpr_InvalidPredefinedExpr
};

enum clang_ext_PredefinedExpr_IdentKind
clang_ext_PredefinedExpr_getIdentKind(CXCursor c);

#ifndef LLVM_VERSION_BEFORE_3_6_0
CXString
clang_ext_PredefinedExpr_getFunctionName(CXCursor c);
#endif

CXString
clang_ext_PredefinedExpr_ComputeName(
  enum clang_ext_PredefinedExpr_IdentKind kind,
  CXCursor decl);

bool
clang_ext_LambdaExpr_isMutable(CXCursor c);

bool
clang_ext_LambdaExpr_hasExplicitParameters(CXCursor c);

bool
clang_ext_LambdaExpr_hasExplicitResultType(CXCursor c);

/* Copied from Basic/Lambda.h.
   clang_ext_LCD_None is renamed clang_ext_LCD_CaptureNone to avoid name clash */
enum clang_ext_LambdaCaptureDefault {
  clang_ext_LCD_CaptureNone,
  clang_ext_LCD_ByCopy,
  clang_ext_LCD_ByRef
};

enum clang_ext_LambdaCaptureDefault
clang_ext_LambdaExpr_getCaptureDefault(CXCursor c);

unsigned
clang_ext_LambdaExpr_getCaptureCount(CXCursor c);

struct clang_ext_LambdaCapture {
  const void *data;
  CXTranslationUnit TU;
};

struct clang_ext_LambdaCapture
clang_ext_LambdaExpr_getCapture(CXCursor c, unsigned index);

CXCursor
clang_ext_LambdaExpr_getCallOperator(CXCursor c);

/* Copied from Basic/Lambda.h */
enum clang_ext_LambdaCaptureKind {
  clang_ext_LCK_This,
  clang_ext_LCK_StarThis,
  clang_ext_LCK_ByCopy,
  clang_ext_LCK_ByRef,
  clang_ext_LCK_VLAType
};

enum clang_ext_LambdaCaptureKind
clang_ext_LambdaCapture_getKind(struct clang_ext_LambdaCapture capture);

CXCursor
clang_ext_LambdaCapture_getCapturedVar(struct clang_ext_LambdaCapture capture);

bool
clang_ext_LambdaCapture_isImplicit(struct clang_ext_LambdaCapture capture);

bool
clang_ext_LambdaCapture_isPackExpansion(struct clang_ext_LambdaCapture capture);

void
clang_ext_LambdaCapture_dispose(struct clang_ext_LambdaCapture capture);

CXType
clang_ext_CXXNewExpr_getAllocatedType(CXCursor c);

CXCursor
clang_ext_CXXNewExpr_getArraySize(CXCursor c);

unsigned int
clang_ext_CXXNewExpr_getNumPlacementArgs(CXCursor c);

CXCursor
clang_ext_CXXNewExpr_getPlacementArg(CXCursor c, unsigned int i);

CXCursor
clang_ext_CXXNewExpr_getInitializer(CXCursor c);

bool
clang_ext_CXXDeleteExpr_isGlobalDelete(CXCursor c);

bool
clang_ext_CXXDeleteExpr_isArrayForm(CXCursor c);

bool
clang_ext_CXXTypeidExpr_isTypeOperand(CXCursor c);

CXType
clang_ext_CXXTypeidExpr_getTypeOperand(CXCursor c);

CXCursor
clang_ext_CXXTypeidExpr_getExprOperand(CXCursor c);

enum clang_ext_langstandards {
  #define FOREACH_STANDARD(Ident, Name) \
    CLANG_EXT_LANGSTANDARDS_##Ident,
  #ifdef LLVM_VERSION_BEFORE_5_0_0
  #define LANGSTANDARD(Ident, Name, _Desc, _Features) \
    FOREACH_STANDARD(Ident, Name)
  #else
  #define LANGSTANDARD(Ident, Name, _Lang, _Desc, _Features) \
    FOREACH_STANDARD(Ident, Name)
  #endif
  #include <clang/Frontend/LangStandards.def>
  #undef FOREACH_STANDARD
  CLANG_EXT_LANGSTANDARDS_InvalidLang
};

const char *
clang_ext_LangStandard_getName(enum clang_ext_langstandards s);

enum clang_ext_langstandards
clang_ext_LangStandard_ofName(const char *s);

CXType
clang_ext_PackExpansion_getPattern(CXType c);

bool
clang_ext_CXXFoldExpr_isRightFold(CXCursor);

enum clang_ext_BinaryOperatorKind
clang_ext_CXXFoldExpr_getOperator(CXCursor);

bool
clang_ext_CXXBoolLiteralExpr_getValue(CXCursor c);

CXCursor
clang_ext_CallExpr_getCallee(CXCursor c);

unsigned int
clang_ext_CallExpr_getNumArgs(CXCursor c);

CXCursor
clang_ext_CallExpr_getArg(CXCursor c, unsigned int i);

CXCursor
clang_ext_SizeOfPackExpr_getPack(CXCursor c);

CXCursor
clang_ext_DecltypeType_getUnderlyingExpr(CXType t);

bool
clang_ext_NamespaceDecl_isInline(CXCursor c);

typedef enum CXVisitorResult (*CXDeclContextVisitor)(
  CXCursor , CXClientData client_data);

enum clang_ext_OverloadedOperatorKind {
  CLANG_EXT_OVERLOADED_OPERATOR_InvalidOverloadedOperator,
  #define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
    CLANG_EXT_OVERLOADED_OPERATOR_##Name,
  #include <clang/Basic/OperatorKinds.def>
};

const char *
clang_ext_OverloadedOperator_getSpelling(
  enum clang_ext_OverloadedOperatorKind kind);

struct clang_ext_DeclarationName {
  const void *data;
  CXTranslationUnit tu;
};

void
clang_ext_DeclarationName_dispose(struct clang_ext_DeclarationName name);

/* Copied from clang/AST/DeclarationName.h */
enum clang_ext_DeclarationNameKind {
  CLANG_EXT_DECLARATION_NAME_Identifier,
  CLANG_EXT_DECLARATION_NAME_ObjCZeroArgSelector,
  CLANG_EXT_DECLARATION_NAME_ObjCOneArgSelector,
  CLANG_EXT_DECLARATION_NAME_ObjCMultiArgSelector,
  CLANG_EXT_DECLARATION_NAME_CXXConstructorName,
  CLANG_EXT_DECLARATION_NAME_CXXDestructorName,
  CLANG_EXT_DECLARATION_NAME_CXXConversionFunctionName,
  CLANG_EXT_DECLARATION_NAME_CXXDeductionGuideName,
  CLANG_EXT_DECLARATION_NAME_CXXOperatorName,
  CLANG_EXT_DECLARATION_NAME_CXXLiteralOperatorName,
  CLANG_EXT_DECLARATION_NAME_CXXUsingDirective,
  CLANG_EXT_DECLARATION_NAME_InvalidDeclarationName
};

enum clang_ext_DeclarationNameKind
clang_ext_DeclarationName_getKind(
  struct clang_ext_DeclarationName);

enum clang_ext_OverloadedOperatorKind
clang_ext_DeclarationName_getCXXOverloadedOperator(
  struct clang_ext_DeclarationName);

CXType
clang_ext_DeclarationName_getCXXNameType(
  struct clang_ext_DeclarationName);

CXString
clang_ext_DeclarationName_getAsIdentifier(
  struct clang_ext_DeclarationName name);

CXCursor
clang_ext_DeclarationName_getCXXDeductionGuideTemplate(
  struct clang_ext_DeclarationName name);

CXString
clang_ext_DeclarationName_getCXXLiteralIdentifier(
  struct clang_ext_DeclarationName name);

struct clang_ext_DeclarationName
clang_ext_Decl_getName(CXCursor);

CXCursor
clang_ext_UsingDirectiveDecl_getNominatedNamespace(CXCursor c);

struct clang_ext_NestedNameSpecifier {
  const void *data;
  CXTranslationUnit tu;
};

/* Copied from clang/AST/NestedNameSpecifier.h */
enum clang_ext_NestedNameSpecifierKind {
  CLANG_EXT_NESTED_NAME_SPECIFIER_InvalidNestedNameSpecifier,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Identifier,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Namespace,
  CLANG_EXT_NESTED_NAME_SPECIFIER_NamespaceAlias,
  CLANG_EXT_NESTED_NAME_SPECIFIER_TypeSpec,
  CLANG_EXT_NESTED_NAME_SPECIFIER_TypeSpecWithTemplate,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Global,
  CLANG_EXT_NESTED_NAME_SPECIFIER_Super
};

enum clang_ext_NestedNameSpecifierKind
clang_ext_NestedNameSpecifier_getKind(
  struct clang_ext_NestedNameSpecifier);

struct clang_ext_NestedNameSpecifier
clang_ext_NestedNameSpecifier_getPrefix(
  struct clang_ext_NestedNameSpecifier);

CXString
clang_ext_NestedNameSpecifier_getAsIdentifier(
  struct clang_ext_NestedNameSpecifier);

CXCursor
clang_ext_NestedNameSpecifier_getAsNamespace(
  struct clang_ext_NestedNameSpecifier);

CXType
clang_ext_NestedNameSpecifier_getAsType(
  struct clang_ext_NestedNameSpecifier);

struct clang_ext_NestedNameSpecifier
clang_ext_Decl_getNestedNameSpecifier(CXCursor);

struct clang_ext_NestedNameSpecifier
clang_ext_Type_getQualifier(CXType);

bool
clang_ext_TagDecl_isCompleteDefinition(CXCursor);

CXType
clang_ext_CXXPseudoDestructorExpr_getDestroyedType(CXCursor);

unsigned int
clang_ext_Cursor_getNumTemplateArgs(CXCursor);

struct clang_ext_TemplateArgument
clang_ext_Cursor_getTemplateArg(CXCursor, unsigned int);

CXCursor
clang_ext_TypeAliasTemplateDecl_getTemplatedDecl(CXCursor);

unsigned int
clang_ext_TemplateDecl_getParameterCount(CXCursor);

CXCursor
clang_ext_TemplateDecl_getParameter(CXCursor, unsigned int);

CXCursor
clang_ext_SubstNonTypeTemplateParmExpr_getReplacement(CXCursor);

unsigned int
clang_ext_AttributedStmt_GetAttributeCount(CXCursor);

enum clang_ext_AttrKind
clang_ext_AttributedStmt_GetAttributeKind(CXCursor, unsigned int);

unsigned int
clang_ext_DecompositionDecl_GetBindingsCount(CXCursor);

CXCursor
clang_ext_DecompositionDecl_GetBindings(CXCursor, unsigned int);

enum clang_ext_AttrKind
clang_ext_Attr_GetKind(CXCursor);

CXCursor
clang_ext_VarTemplateDecl_getTemplatedDecl(CXCursor);

/* From clang/Basic/ExceptionSpecificationType.h */
enum clang_ext_ExceptionSpecificationType {
  CLANG_EXT_EST_NoExceptionSpecification,
  CLANG_EXT_EST_DynamicNone,
  CLANG_EXT_EST_Dynamic,
  CLANG_EXT_EST_MSAny,
  CLANG_EXT_EST_NoThrow, /* Clang >=9.0.0 */
  CLANG_EXT_EST_BasicNoexcept,
  CLANG_EXT_EST_DependentNoexcept,
  /* EST_ComputedNoexcept (Clang <7.0.0) is mapped to
     CLANG_EXT_EST_DependentNoexcept */
  CLANG_EXT_EST_NoexceptFalse,
  CLANG_EXT_EST_NoexceptTrue,
  CLANG_EXT_EST_Unevaluated,
  CLANG_EXT_EST_Uninstantiated,
  CLANG_EXT_EST_Unparsed /* Clang >=3.6.0 */
};

enum clang_ext_ExceptionSpecificationType
clang_ext_FunctionProtoType_getExceptionSpecType(CXType);

unsigned int
clang_ext_FunctionProtoType_getNumExceptions(CXType);

CXType
clang_ext_FunctionProtoType_getExceptionType(CXType, unsigned int);

CXCursor
clang_ext_FunctionProtoType_getNoexceptExpr(CXType);

CXString
clang_ext_AsmStmt_GetAsmString(CXCursor c);

unsigned int
clang_ext_AsmStmt_getNumOutputs(CXCursor);

CXString
clang_ext_AsmStmt_getOutputConstraint(CXCursor, unsigned);

CXCursor
clang_ext_AsmStmt_getOutputExpr(CXCursor, unsigned);

unsigned int
clang_ext_AsmStmt_getNumInputs(CXCursor);

CXString
clang_ext_AsmStmt_getInputConstraint(CXCursor, unsigned);

CXCursor
clang_ext_AsmStmt_getInputExpr(CXCursor, unsigned);
