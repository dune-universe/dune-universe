#include <stdbool.h>
#include <stdint.h>

#ifdef LLVM_VERSION_3_4_2
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_5_2
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_6_2
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_7_1
#define LLVM_VERSION_BEFORE_3_9_0
#endif
#ifdef LLVM_VERSION_3_8_1
#define LLVM_VERSION_BEFORE_3_9_0
#endif

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

double
clang_ext_Float_convertToDouble(CXFloat f);

CXString
clang_ext_StringLiteral_GetString(CXCursor c);

enum clang_ext_UnaryOperatorKind {
  #define UNARY_OPERATION(Name, Spelling) CLANG_EXT_UNARY_OPERATOR_##Name,
  #include "clangml_OperationKinds.def"
};

enum clang_ext_UnaryOperatorKind
clang_ext_UnaryOperator_getOpcode(CXCursor c);

CXString
clang_ext_UnaryOperator_getOpcodeSpelling(
  enum clang_ext_UnaryOperatorKind Kind);

enum clang_ext_BinaryOperatorKind {
  #define BINARY_OPERATION(Name, Spelling) CLANG_EXT_BINARY_OPERATOR_##Name,
  #include "clangml_OperationKinds.def"
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
  ETK_None
};

enum clang_ext_ElaboratedTypeKeyword
clang_ext_ElaboratedType_getKeyword(CXType c);

CXString
clang_ext_ElaboratedType_getKeywordSpelling(
  enum clang_ext_ElaboratedTypeKeyword keyword);

bool
clang_ext_VarDecl_hasInit(CXCursor c);

bool
clang_ext_MemberRefExpr_isArrow(CXCursor c);

CXString
clang_ext_Stmt_GetClassName(CXCursor c);

int
clang_ext_Stmt_GetClassKind(CXCursor c);

enum clang_ext_CursorKind {
  ECK_ImplicitCastExpr,
  ECK_BinaryConditionalOperator,
  ECK_UnaryExprOrTypeTraitExpr, /* for Clang 3.8.1 */
  ECK_Unknown
};

enum clang_ext_CursorKind
clang_ext_GetCursorKind(CXCursor c);

enum clang_ext_TypeKind {
  ETK_Invalid,
  ETK_Paren,
  ETK_Elaborated, /* for Clang 3.8.1 */
  ETK_Unknown
};

enum clang_ext_TypeKind
clang_ext_GetTypeKind(CXType c);

CXType
clang_ext_GetInnerType(CXType c);

CXCursor
clang_ext_VariableArrayType_GetSizeExpr(CXType c);

CXString
clang_ext_AsmStmt_GetAsmString(CXCursor c);

enum clang_ext_CharacterKind {
  ECK_Ascii,
  ECK_Wide,
  ECK_UTF8,
  ECK_UTF16,
  ECK_UTF32
};

enum clang_ext_CharacterKind
clang_ext_CharacterLiteral_GetCharacterKind(CXCursor c);

unsigned
clang_ext_CharacterLiteral_GetValue(CXCursor c);

/* From clang/Basic/TypeTraits.h */
enum clang_ext_UnaryExpr {
  UETT_SizeOf,
  UETT_AlignOf,
  UETT_VecStep,
  UETT_OpenMPRequiredSimdAlign,
};

enum clang_ext_UnaryExpr
clang_ext_UnaryExpr_GetKind(CXCursor c);

CXType
clang_ext_UnaryExpr_GetArgumentType(CXCursor c);

CXType
clang_ext_Type_getNamedType(CXType CT);
