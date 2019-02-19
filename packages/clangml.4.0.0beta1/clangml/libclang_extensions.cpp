#include <clang-c/Index.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Type.h>
#include "clang/Frontend/ASTUnit.h"
#include "clang/Basic/SourceLocation.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>

extern "C" {
  #include "libclang_extensions.h"
}

// Copied from clang source tree: tools/libclang/CXString.cpp
enum CXStringFlag {
  /// CXString contains a 'const char *' that it doesn't own.
  CXS_Unmanaged,

  /// CXString contains a 'const char *' that it allocated with malloc().
  CXS_Malloc,

  /// CXString contains a CXStringBuf that needs to be returned to the
  /// CXStringPool.
  CXS_StringBuf
};

// Copied from clang source tree: tools/libclang/CXString.cpp
static CXString
cxstring_createRef(const char *String)
{
  CXString Str;
  Str.data = String;
  Str.private_flags = CXS_Unmanaged;
  return Str;
}

// Copied from clang source tree: tools/libclang/CXString.cpp
CXString
cxstring_createDup(llvm::StringRef String)
{
  CXString Result;
  char *Spelling = static_cast<char *>(malloc(String.size() + 1));
  if (Spelling == NULL) {
    return cxstring_createRef("");
  }
  memmove(Spelling, String.data(), String.size());
  Spelling[String.size()] = 0;
  Result.data = Spelling;
  Result.private_flags = (unsigned) CXS_Malloc;
  return Result;
}

/*
static CXString cxstring_createDupFromString(std::string &s) {
  CXString Result;
  char *Spelling = static_cast<char *>(llvm::safe_malloc(s.size() + 1));
  memmove(Spelling, s.data(), s.size());
  Spelling[s.size()] = 0;
  Result.data = Spelling;
  Result.private_flags = CXS_Malloc;
  return Result;
}
*/

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Decl *
getCursorDecl(CXCursor Cursor)
{
  return static_cast<const clang::Decl *>(Cursor.data[0]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Stmt *
getCursorStmt(CXCursor Cursor)
{
  if (Cursor.kind == CXCursor_ObjCSuperClassRef ||
      Cursor.kind == CXCursor_ObjCProtocolRef ||
      Cursor.kind == CXCursor_ObjCClassRef)
    return nullptr;

  return static_cast<const clang::Stmt *>(Cursor.data[1]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static const clang::Expr *
getCursorExpr(CXCursor Cursor)
{
  return llvm::dyn_cast_or_null<clang::Expr>(getCursorStmt(Cursor));
}

// From clang source tree: tools/libclang/CXType.cpp
static inline clang::QualType
GetQualType(CXType CT)
{
  return clang::QualType::getFromOpaquePtr(CT.data[0]);
}

// Copied from clang source tree: tools/libclang/CXType.cpp
static inline CXTranslationUnit
GetTU(CXType CT)
{
  return static_cast<CXTranslationUnit>(CT.data[1]);
}

// Copied from clang source tree: tools/libclang/CXCursor.cpp
static CXTranslationUnit
getCursorTU(CXCursor Cursor)
{
  return static_cast<CXTranslationUnit>(const_cast<void*>(Cursor.data[2]));
}

// From clang source tree: tools/libclang/CXTranslationUnit.h
struct CXTranslationUnitImpl {
  void *CIdx;
  clang::ASTUnit *TheASTUnit;
  void *StringPool;
  void *Diagnostics;
  void *OverridenCursorsPool;
  void *CommentToXML;
  unsigned ParsingOptions;
  std::vector<std::string> Arguments;
};

CXInt Invalid_CXInt = { NULL };

static inline CXInt
MakeCXInt(const llvm::APInt &value)
{
  CXInt result;
  result.data = new llvm::APInt(value);
  return result;
}

CXFloat Invalid_CXFloat = { NULL };

static inline CXFloat
MakeCXFloat(const llvm::APFloat &value)
{
  CXFloat result;
  result.data = new llvm::APFloat(value);
  return result;
}

/* Copied from clang source tree: tools/libclang/CXCursor.cpp */
static CXCursor
MakeCXCursorInvalid(CXCursorKind K, CXTranslationUnit TU)
{
  assert(K >= CXCursor_FirstInvalid && K <= CXCursor_LastInvalid);
  CXCursor C = { K, 0, { nullptr, nullptr, TU } };
  return C;
}

static CXType
MakeCXTypeInvalid(CXTranslationUnit TU)
{
  CXType CT = { CXType_Invalid, { nullptr, TU }};
  return CT;
}

/* MakeCXCursor is not exported in libclang.
   The following implementation makes a (not well-formed) cursor on an
   OpaqueValueExpr with E as source expression. Visiting the (single) child of
   this cursor calls libclang's MakeCXCursor on E.
*/
enum CXChildVisitResult
MakeCXCursor_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
  *((CXCursor *) client_data) = cursor;
  return CXChildVisit_Break;
}

static CXCursor
MakeCXCursor(clang::Expr *E, CXTranslationUnit TU)
{
  clang::OpaqueValueExpr OV(
    clang::SourceLocation::getFromRawEncoding(0), E->getType(),
    clang::VK_RValue, clang::OK_Ordinary, E);
  CXCursor C = { CXCursor_FirstExpr, 0, { NULL, &OV, TU }};
  CXCursor Result;
  clang_visitChildren(C, MakeCXCursor_visitor, &Result);
  return Result;
}

/* The following implementation makes a (not well-formed) cursor on a
   default statement with S as substatement. Visiting the (single) child of
   this cursor calls libclang's MakeCXCursor on S.
*/
static CXCursor __attribute__((unused))
MakeCXCursor(const clang::Stmt *S, CXTranslationUnit TU)
{
  clang::DefaultStmt CS(
    clang::SourceLocation::getFromRawEncoding(0),
    clang::SourceLocation::getFromRawEncoding(0), (clang::Stmt *) S);
  CXCursor C = { CXCursor_CompoundStmt, 0, { NULL, &CS, TU }};
  CXCursor Result;
  clang_visitChildren(C, MakeCXCursor_visitor, &Result);
  return Result;
}

/* MakeCXType is not exported in libclang.
   The following implementation makes a (not well-formed) cursor on an
   OpaqueValueExpr of type T. Querying the type of this cursor calls
   libclang's MakeCXType on T.
*/
static CXType
MakeCXType(clang::QualType T, CXTranslationUnit TU)
{
  clang::OpaqueValueExpr OV(
    clang::SourceLocation::getFromRawEncoding(0), T, clang::VK_RValue);
  CXCursor C = { CXCursor_FirstExpr, 0, { NULL, &OV, TU }};
  return clang_getCursorType(C);
}

extern "C" {
  bool
  clang_equal_cxint(CXInt a, CXInt b)
  {
    if (auto i = static_cast<llvm::APInt *>(a.data)) {
      if (auto j = static_cast<llvm::APInt *>(b.data)) {
        return *i == *j;
      }
    }
    return false;
  }

  int
  clang_compare_cxint(CXInt a, CXInt b)
  {
    if (auto i = static_cast<llvm::APInt *>(a.data)) {
      if (auto j = static_cast<llvm::APInt *>(b.data)) {
        if (i->ult(*j)) { // sadly, APInt::compare is private
          return -1;
        }
        else if (j->ult(*i)) {
          return 1;
        }
        else {
          return 0;
        }
      }
    }
    return -1;
  }

  CXInt
  clang_ext_IntegerLiteral_getValue(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::IntegerLiteral>(getCursorStmt(c))) {
      return MakeCXInt(e->getValue());
    }
    return Invalid_CXInt;
  }

  void
  clang_ext_Int_dispose(CXInt c)
  {
    delete(static_cast<llvm::APInt *>(c.data));
    c.data = NULL;
  }

  bool
  clang_ext_Int_isValid(CXInt c)
  {
    return c.data != NULL;
  }

  CXString
  clang_ext_Int_toString(CXInt c, unsigned Radix, bool isSigned)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      std::string s = i->toString(Radix, isSigned);
      return cxstring_createDup(s);
    }
    return cxstring_createRef("");
  }

  double
  clang_ext_Int_roundToDouble(CXInt c, bool isSigned)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->roundToDouble(isSigned);
    }
    return 0.;
  }

  float
  clang_ext_Int_bitsToFloat(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->bitsToFloat();
    }
    return 0.f;
  }

  unsigned
  clang_ext_Int_getBitWidth(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getBitWidth();
    }
    return 0;
  }

  unsigned
  clang_ext_Int_getActiveBits(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getActiveBits();
    }
    return 0;
  }

  unsigned
  clang_ext_Int_getMinSignedBits(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getMinSignedBits();
    }
    return 0;
  }

  bool
  clang_ext_Int_getBoolValue(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getBoolValue();
    }
    return false;
  }

  int
  clang_ext_Int_getSExtValue(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getSExtValue();
    }
    return 0;
  }

  int64_t
  clang_ext_Int_getSExtValue64(CXInt c)
  {
    if (auto i = static_cast<llvm::APInt *>(c.data)) {
      return i->getSExtValue();
    }
    return 0;
  }

  CXFloat
  clang_ext_FloatingLiteral_getValue(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::FloatingLiteral>(getCursorStmt(c))) {
      return MakeCXFloat(e->getValue());
    }
    return Invalid_CXFloat;
  }

  bool
  clang_equal_cxfloat(CXFloat a, CXFloat b)
  {
    if (auto x = static_cast<llvm::APFloat *>(a.data)) {
      if (auto y = static_cast<llvm::APFloat *>(b.data)) {
        return x->compare(*y) == llvm::APFloat::cmpEqual;
      }
    }
    return false;
  }

  int
  clang_compare_cxfloat(CXFloat a, CXFloat b)
  {
    if (auto x = static_cast<llvm::APFloat *>(a.data)) {
      if (auto y = static_cast<llvm::APFloat *>(b.data)) {
        switch (x->compare(*y)) {
        case llvm::APFloat::cmpLessThan : return -1;
        case llvm::APFloat::cmpEqual : return 0;
        case llvm::APFloat::cmpGreaterThan : return 1;
        case llvm::APFloat::cmpUnordered : ;
        }
      }
    }
    return -1;
  }

  void
  clang_ext_Float_dispose(CXFloat c)
  {
    delete(static_cast<llvm::APFloat *>(c.data));
    c.data = NULL;
  }

  bool
  clang_ext_Float_isValid(CXFloat c)
  {
    return c.data != NULL;
  }

  CXString
  clang_ext_Float_toString(CXFloat c)
  {
    if (auto f = static_cast<llvm::APFloat *>(c.data)) {
      llvm::SmallString<40> S;
      f->toString(S);
      return cxstring_createDup(S.str());
    }
    return cxstring_createRef("");
  }

  double
  clang_ext_Float_convertToDouble(CXFloat c)
  {
    if (auto f = static_cast<llvm::APFloat *>(c.data)) {
      return f->convertToDouble();
    }
    return 0.;
  }

  CXString
  clang_ext_StringLiteral_GetString(CXCursor c)
  {
    const clang::Expr *e = getCursorExpr(c);
    if (auto m = llvm::dyn_cast_or_null<clang::StringLiteral>(e)) {
      return cxstring_createDup(m->getString());
    }
    return cxstring_createRef("");
  }

  enum clang_ext_UnaryOperatorKind
  clang_ext_UnaryOperator_getOpcode(CXCursor c)
  {
    if (auto Op =
        llvm::dyn_cast_or_null<clang::UnaryOperator>(getCursorStmt(c))) {
      return static_cast<clang_ext_UnaryOperatorKind>(Op->getOpcode());
    }
    return static_cast<clang_ext_UnaryOperatorKind>(0);
  }

  CXString
  clang_ext_UnaryOperator_getOpcodeSpelling(
      enum clang_ext_UnaryOperatorKind Kind)
  {
    switch (Kind) {
#define UNARY_OPERATION(Name, Spelling)        \
      case CLANG_EXT_UNARY_OPERATOR_##Name:     \
        return cxstring_createRef(Spelling);
#include "clangml_OperationKinds.def"
    }
    //llvm_unreachable("Unsupported BinaryOperatorKind");
    return cxstring_createRef("");
  }

  enum clang_ext_BinaryOperatorKind
  clang_ext_BinaryOperator_getOpcode(CXCursor c)
  {
    if (auto Op =
        llvm::dyn_cast_or_null<clang::BinaryOperator>(getCursorStmt(c))) {
      return static_cast<clang_ext_BinaryOperatorKind>(Op->getOpcode());
    }
    return static_cast<clang_ext_BinaryOperatorKind>(0);
  }

  CXString
  clang_ext_BinaryOperator_getOpcodeSpelling(
      enum clang_ext_BinaryOperatorKind Kind)
  {
    switch (Kind) {
#define BINARY_OPERATION(Name, Spelling)        \
      case CLANG_EXT_BINARY_OPERATOR_##Name:     \
        return cxstring_createRef(Spelling);
#include "clangml_OperationKinds.def"
    }
    //llvm_unreachable("Unsupported BinaryOperatorKind");
    return cxstring_createRef("");
  }

  unsigned
  clang_ext_ForStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::ForStmt>(getCursorStmt(c))) {
      unsigned Result = 0;
      if (S->getInit()) {
        Result |= CLANG_EXT_FOR_STMT_INIT;
      }
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_FOR_STMT_CONDITION_VARIABLE;
      }
      if (S->getCond()) {
        Result |= CLANG_EXT_FOR_STMT_COND;
      }
      if (S->getInc()) {
        Result |= CLANG_EXT_FOR_STMT_INC;
      }
      return Result;
    }
    return 0;
  }

  unsigned
  clang_ext_IfStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::IfStmt>(getCursorStmt(c))) {
      unsigned Result = 0;
      #ifndef LLVM_VERSION_BEFORE_3_9_0
      if (S->getInit()) {
        Result |= CLANG_EXT_IF_STMT_INIT;
      }
      #endif
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_IF_STMT_CONDITION_VARIABLE;
      }
      if (S->getElse()) {
        Result |= CLANG_EXT_IF_STMT_ELSE;
      }
      return Result;
    }
    return 0;
  }

  CXCursor
  clang_ext_IfStmt_getInit(CXCursor c)
  {
    #ifndef LLVM_VERSION_BEFORE_3_9_0
    if (auto S = llvm::dyn_cast_or_null<clang::IfStmt>(getCursorStmt(c))) {
      return MakeCXCursor(S->getInit(), getCursorTU(c));
    }
    #endif
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  unsigned
  clang_ext_SwitchStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::SwitchStmt>(getCursorStmt(c))) {
      unsigned Result = 0;
      #ifndef LLVM_VERSION_BEFORE_3_9_0
      if (S->getInit()) {
        Result |= CLANG_EXT_SWITCH_STMT_INIT;
      }
      #endif
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_SWITCH_STMT_CONDITION_VARIABLE;
      }
      return Result;
    }
    return 0;
  }

  CXCursor
  clang_ext_SwitchStmt_getInit(CXCursor c)
  {
    #ifndef LLVM_VERSION_BEFORE_3_9_0
    if (auto S = llvm::dyn_cast_or_null<clang::SwitchStmt>(getCursorStmt(c))) {
      return MakeCXCursor(S->getInit(), getCursorTU(c));
    }
    #endif
    return MakeCXCursorInvalid(CXCursor_InvalidCode, getCursorTU(c));
  }

  unsigned
  clang_ext_WhileStmt_getChildrenSet(CXCursor c)
  {
    if (auto S = llvm::dyn_cast_or_null<clang::WhileStmt>(getCursorStmt(c))) {
      unsigned Result = 0;
      if (S->getConditionVariable()) {
        Result |= CLANG_EXT_WHILE_STMT_CONDITION_VARIABLE;
      }
      return Result;
    }
    return 0;
  }

  enum clang_ext_ElaboratedTypeKeyword
  clang_ext_ElaboratedType_getKeyword(
      CXType CT)
  {
    clang::QualType T = GetQualType(CT);
    const clang::Type *TP = T.getTypePtrOrNull();
    if (TP && TP->getTypeClass() == clang::Type::Elaborated) {
      return static_cast<enum clang_ext_ElaboratedTypeKeyword>(
        clang::cast<clang::ElaboratedType>(TP)->getKeyword());
    }
    return static_cast<enum clang_ext_ElaboratedTypeKeyword>(0);
  }

  CXString
  clang_ext_ElaboratedType_getKeywordSpelling(
      enum clang_ext_ElaboratedTypeKeyword keyword)
  {
    return cxstring_createDup(clang::TypeWithKeyword::getKeywordName(
      static_cast<clang::ElaboratedTypeKeyword>(keyword)));
  }

  bool
  clang_ext_VarDecl_hasInit(CXCursor c)
  {
    if (auto D = llvm::dyn_cast_or_null<clang::VarDecl>(getCursorDecl(c))) {
      return D->getInit() != NULL;
    }
    return false;
  }

  bool
  clang_ext_MemberRefExpr_isArrow(CXCursor c)
  {
    const clang::Expr *e = getCursorExpr(c);
    if (auto m = llvm::dyn_cast_or_null<clang::MemberExpr>(e)) {
      return m->isArrow();
    }
    return false;
  }

  CXString
  clang_ext_Stmt_GetClassName(CXCursor c)
  {
    const clang::Stmt *s = getCursorStmt(c);
    return cxstring_createRef(s->getStmtClassName());
  }

  int
  clang_ext_Stmt_GetClassKind(CXCursor c)
  {
    const clang::Stmt *s = getCursorStmt(c);
    return s->getStmtClass();
  }

  enum clang_ext_CursorKind
  clang_ext_GetCursorKind(CXCursor c)
  {
    const clang::Stmt *s = getCursorStmt(c);
    #define CASE(X) case clang::Stmt::X##Class: return ECK_##X
    switch (s->getStmtClass()) {
    CASE(ImplicitCastExpr);
    CASE(BinaryConditionalOperator);
    CASE(UnaryExprOrTypeTraitExpr);
    default:
      return ECK_Unknown;
    }
    #undef CASE
  }

  enum clang_ext_TypeKind
  clang_ext_GetTypeKind(CXType c)
  {
    clang::QualType T = GetQualType(c);
    if (auto TP = T.getTypePtrOrNull()) {
      #define CASE(X) case clang::Type::X: return ETK_##X
      switch (TP->getTypeClass()) {
      CASE(Paren);
      CASE(Elaborated); /* For Clang 3.8.1 */
      default:
        return ETK_Unknown;
      }
      #undef CASE
    }
    return ETK_Invalid;
  }

  CXType
  clang_ext_GetInnerType(CXType c)
  {
    clang::QualType T = GetQualType(c);
    if (auto PTT = T->getAs<clang::ParenType>()) {
      return MakeCXType(PTT->getInnerType(), GetTU(c));
    }
    return c;
  }

  CXCursor
  clang_ext_VariableArrayType_GetSizeExpr(CXType c)
  {
    clang::QualType T = GetQualType(c);
    if (auto TP = T.getTypePtrOrNull()) {
      switch (TP->getTypeClass()) {
      case clang::Type::VariableArray:
        return MakeCXCursor(
          clang::cast<clang::VariableArrayType>(TP)->getSizeExpr(), GetTU(c));
      default:
        break;
      }
    }
    return MakeCXCursorInvalid(CXCursor_InvalidCode, GetTU(c));
  }

  CXString
  clang_ext_AsmStmt_GetAsmString(CXCursor c)
  {
    const clang::Stmt *s = getCursorStmt(c);
    switch (s->getStmtClass()) {
    case clang::Stmt::GCCAsmStmtClass:;
      return cxstring_createDup(
        clang::cast<clang::GCCAsmStmt>(s)->getAsmString()->getString());
    case clang::Stmt::MSAsmStmtClass:
      return cxstring_createDup(
        clang::cast<clang::MSAsmStmt>(s)->getAsmString());
    default:
      return cxstring_createRef("");
    }
  }

  enum clang_ext_CharacterKind
  clang_ext_CharacterLiteral_GetCharacterKind(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CharacterLiteral>(getCursorStmt(c))) {
      return static_cast<enum clang_ext_CharacterKind>(e->getKind());
    }
    return ECK_Ascii;
  }

  unsigned
  clang_ext_CharacterLiteral_GetValue(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::CharacterLiteral>(getCursorStmt(c))) {
      return e->getValue();
    }
    return 0;
  }

  enum clang_ext_UnaryExpr
  clang_ext_UnaryExpr_GetKind(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(getCursorStmt(c))) {
      return static_cast<enum clang_ext_UnaryExpr>(e->getKind());
    }
    return UETT_SizeOf;
  }

  CXType
  clang_ext_UnaryExpr_GetArgumentType(CXCursor c)
  {
    if (auto e =
      llvm::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(getCursorStmt(c))) {
      return MakeCXType(e->getArgumentType(), getCursorTU(c));
    }
    return MakeCXTypeInvalid(getCursorTU(c));
  }

  CXType
  clang_ext_Type_getNamedType(CXType CT)
  {
    #ifdef LLVM_VERSION_BEFORE_3_9_0
    clang::QualType T = GetQualType(CT);
    const clang::Type *TP = T.getTypePtrOrNull();

    if (TP && TP->getTypeClass() == clang::Type::Elaborated)
      return MakeCXType(llvm::cast<clang::ElaboratedType>(TP)->getNamedType(), GetTU(CT));

    return MakeCXTypeInvalid(GetTU(CT));
    #else
    return clang_Type_getNamedType(CT);
    #endif
  }

}
