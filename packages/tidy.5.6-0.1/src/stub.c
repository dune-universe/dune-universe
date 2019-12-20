#include <tidy.h>
#include <tidybuffio.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/threads.h>

#define p_val(v) (*(void* *)Data_custom_val(v))

#define doc_val(v) (*(TidyDoc*)Data_custom_val(v))
#define node_val(v) (*(TidyNode*)Data_custom_val(v))
#define attr_val(v) (*(TidyAttr*)Data_custom_val(v))
#define opt_val(v) (*(TidyOption*)Data_custom_val(v))

static intnat p_hash_op(value v) {
    return (intnat)p_val(v);
}

static int p_comp_op(value v1, value v2) {
    return p_val(v1) - p_val(v2);
}

// doc {
    void doc_free_op(value doc) {
        tidyRelease(doc_val(doc));
        return;
    }

    struct custom_operations doc_ops= {
        "org.machinelife.ocaml.tidy.doc_ops",
        doc_free_op,
        p_comp_op,
        p_hash_op,
        NULL,
        NULL,
        NULL,
    };
// }

// opaque pointer: node / attr / option {
    struct custom_operations p_ops= {
        "org.machinelife.ocaml.tidy.p_ops",
        NULL,
        p_comp_op,
        p_hash_op,
        NULL,
        NULL,
        NULL,
    };
// }

CAMLprim value tidyCreate_stub(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);
    result= caml_alloc_custom(&doc_ops, sizeof(TidyDoc), 0,0);
    TidyDoc doc= tidyCreate();
    doc_val(result)= doc;
    CAMLreturn(result);
}

CAMLprim value tidyAccessWarningCount_stub(value doc) {
    return Val_int(tidyAccessWarningCount(doc_val(doc)));
}

CAMLprim value tidyConfigErrorCount_stub(value doc) {
    return Val_int(tidyConfigErrorCount(doc_val(doc)));
}

CAMLprim value tidyDetectedGenericXml_stub(value doc) {
    return Val_bool(tidyDetectedGenericXml(doc_val(doc)));
}

CAMLprim value tidyDetectedHtmlVersion_stub(value doc) {
    return Val_int(tidyDetectedHtmlVersion(doc_val(doc)));
}

CAMLprim value tidyDetectedXhtml_stub(value doc) {
    return Val_bool(tidyDetectedXhtml(doc_val(doc)));
}

CAMLprim value tidyErrorCount_stub(value doc) {
    return Val_int(tidyErrorCount(doc_val(doc)));
}

CAMLprim value tidyErrorSummary_stub(value doc) {
    tidyErrorSummary(doc_val(doc));
    return Val_unit;
}

CAMLprim value tidyFileExists_stub(value doc, value file) {
    return Val_bool(tidyFileExists(doc_val(doc), String_val(file)));
}

CAMLprim value tidyGeneralInfo_stub(value doc) {
    tidyGeneralInfo(doc_val(doc));
    return Val_unit;
}

CAMLprim value tidyLibraryVersion_stub(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);
    result= caml_copy_string(tidyLibraryVersion());
    CAMLreturn(result);
}

CAMLprim value tidyLoadConfig_stub(value doc, value path) {
    tidyLoadConfig(doc_val(doc), String_val(path));
    return Val_unit;
}

CAMLprim value tidyWarningCount_stub(value doc) {
    return Val_int(tidyWarningCount(doc_val(doc)));
}

CAMLprim value tidyGetOption_stub(value doc, value optId) {
    CAMLparam2(doc, optId);
    CAMLlocal1(result);
    result= caml_alloc_custom(&p_ops, sizeof(TidyOption), 0,0);
    opt_val(result)= tidyGetOption(doc_val(doc), Int_val(optId));
    CAMLreturn(result);
}

CAMLprim value tidyOptGetName_stub(value opt) {
    CAMLparam1(opt);
    CAMLlocal1(result);
    result= caml_copy_string(tidyOptGetName(opt_val(opt)));
    CAMLreturn(result);
}

CAMLprim value tidyOptGetType_stub(value opt) {
    CAMLparam1(opt);
    CAMLlocal1(result);
    switch (tidyOptGetType(opt_val(opt))) {
        case TidyString:
            result= Val_int(0);
            break;
        case TidyInteger:
            result= Val_int(1);
            break;
        case TidyBoolean:
            result= Val_int(2);
            break;
    }
    CAMLreturn(result);
}

CAMLprim value tidyOptGetId_stub(value opt) {
    CAMLparam1(opt);
    CAMLlocal1(result);
    result= Val_int(tidyOptGetId(opt_val(opt)));
    CAMLreturn(result);
}

CAMLprim value tidyOptGetValue_stub(value doc, value optId) {
    CAMLparam2(doc, optId);
    CAMLlocal1(result);
    char const * v= tidyOptGetValue(doc_val(doc), Int_val(optId));
    if (v != NULL) {
        result= caml_copy_string(v);
    } else {
        result= caml_copy_string("");
    }
    CAMLreturn(result);
}

CAMLprim value tidyOptGetInt_stub(value doc, value optId) {
    CAMLparam2(doc, optId);
    CAMLlocal1(result);
    result= Val_int(tidyOptGetInt(doc_val(doc), Int_val(optId)));
    CAMLreturn(result);
}

CAMLprim value tidyOptGetBool_stub(value doc, value optId) {
    CAMLparam2(doc, optId);
    CAMLreturn(Val_bool(tidyOptGetBool(doc_val(doc), Int_val(optId))));
}

CAMLprim value tidyOptSetValue_stub(value doc, value optId, value val) {
    CAMLparam3(doc, optId, val);
    tidyOptSetValue(doc_val(doc), Int_val(optId), String_val(val));
    CAMLreturn(Val_unit);
}

CAMLprim value tidyOptSetInt_stub(value doc, value optId, value val) {
    CAMLparam3(doc, optId, val);
    tidyOptSetInt(doc_val(doc), Int_val(optId), Int_val(val));
    CAMLreturn(Val_unit);
}

CAMLprim value tidyOptSetBool_stub(value doc, value optId, value val) {
    CAMLparam3(doc, optId, val);
    tidyOptSetBool(doc_val(doc), Int_val(optId), Bool_val(val));
    CAMLreturn(Val_unit);
}

CAMLprim value tidyParseFile_stub(value doc, value file) {
    CAMLparam2(doc, file);
    CAMLlocal1(result);
    result= Val_int(tidyParseFile(doc_val(doc), String_val(file)));
    if (Int_val(result) == -1) {
        result= Val_int(3);
    }
    CAMLreturn(result);
}

CAMLprim value tidyParseString_stub(value doc, value file) {
    CAMLparam2(doc, file);
    CAMLlocal1(result);
    result= Val_int(tidyParseString(doc_val(doc), String_val(file)));
    if (Int_val(result) == -1) {
        result= Val_int(3);
    }
    CAMLreturn(result);
}

CAMLprim value tidyCleanAndRepair_stub(value doc) {
    tidyCleanAndRepair(doc_val(doc));
    return Val_unit;
}

CAMLprim value tidyReportDoctype_stub(value doc) {
    tidyReportDoctype(doc_val(doc));
    return Val_unit;
}

CAMLprim value tidyRunDiagnostics_stub(value doc) {
    tidyRunDiagnostics(doc_val(doc));
    return Val_unit;
}

CAMLprim value tidySaveFile_stub(value doc, value file) {
    CAMLparam2(doc, file);
    tidySaveFile(doc_val(doc), String_val(file));
    CAMLreturn(Val_unit);
}

CAMLprim value tidySaveString_stub(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(result);
    TidyBuffer buffer;
    tidyBufInit(&buffer);
    if (tidySaveBuffer(doc_val(doc), &buffer)) {
        result= caml_alloc(1, 0);
        Store_field(result, 0, caml_copy_string((char*)buffer.bp));
    } else {
        result= Val_int(0); // None
    }
    tidyBufFree(&buffer);
    CAMLreturn(result);
}

CAMLprim value tidySetCharEncoding_stub(value doc, value enc) {
    CAMLparam2(doc, enc);
    tidySetCharEncoding(doc_val(doc), String_val(enc));
    CAMLreturn(Val_unit);
}

CAMLprim value tidySetInCharEncoding_stub(value doc, value enc) {
    CAMLparam2(doc, enc);
    tidySetInCharEncoding(doc_val(doc), String_val(enc));
    CAMLreturn(Val_unit);
}

CAMLprim value tidySetOutCharEncoding_stub(value doc, value enc) {
    CAMLparam2(doc, enc);
    tidySetOutCharEncoding(doc_val(doc), String_val(enc));
    CAMLreturn(Val_unit);
}

CAMLprim value tidyDeclareBlockTags_stub(value doc, value tags) {
    CAMLparam2(doc, tags);
    tidyOptSetValue(doc_val(doc), TidyBlockTags, String_val(tags));
    CAMLreturn(Val_unit);
}

CAMLprim value tidySetDropEmptyParas_stub(value doc, value ifDrop) {
    tidyOptSetBool(doc_val(doc), TidyDropEmptyParas, Int_val(ifDrop));
    return Val_unit;
}

CAMLprim value tidySetDropEmptyElems_stub(value doc, value ifDrop) {
    tidyOptSetBool(doc_val(doc), TidyDropEmptyElems, Int_val(ifDrop));
    return Val_unit;
}

CAMLprim value tidyGetRoot_stub(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(result);
    result= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
    node_val(result)= tidyGetRoot(doc_val(doc));
    CAMLreturn(result);
}

CAMLprim value tidyGetHtml_stub(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(result);
    result= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
    node_val(result)= tidyGetHtml(doc_val(doc));
    CAMLreturn(result);
}

CAMLprim value tidyGetHead_stub(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(result);
    result= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
    node_val(result)= tidyGetHead(doc_val(doc));
    CAMLreturn(result);
}

CAMLprim value tidyGetBody_stub(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(result);
    result= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
    node_val(result)= tidyGetBody(doc_val(doc));
    CAMLreturn(result);
}

CAMLprim value tidyAttrGetId_stub(value attr) {
    CAMLparam1(attr);
    CAMLlocal1(result);
    result= Val_int(tidyAttrGetId(attr_val(attr)));
    CAMLreturn(result);
}

CAMLprim value tidyAttrIsEvent_stub(value attr) {
    CAMLparam1(attr);
    CAMLlocal1(result);
    result= Val_bool(tidyAttrIsEvent(attr_val(attr)));
    CAMLreturn(result);
}

CAMLprim value tidyAttrGetById_stub(value node, value attrId) {
    CAMLparam2(node, attrId);
    CAMLlocal2(result, data);
    TidyAttr attr= tidyAttrGetById(node_val(node), Int_val(attrId));
    if (attr == NULL) {
        result= Val_int(0); // None
    } else {
        result= caml_alloc(1, 0);
        data= caml_alloc_custom(&p_ops, sizeof(TidyAttr), 0,0);
        attr_val(data)= attr;
        Store_field(result, 0, data);
    }
    CAMLreturn(result);
}

CAMLprim value tidyGetParent_stub(value node) {
    CAMLparam1(node);
    CAMLlocal2(result, data);
    TidyNode parent= tidyGetParent(node_val(node));
    if (parent == NULL) {
        result= Val_int(0); // None
    } else {
        result= caml_alloc(1, 0);
        data= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
        node_val(data)= parent;
        Store_field(result, 0, data);
    }
    CAMLreturn(result);
}

CAMLprim value tidyGetChild_stub(value node) {
    CAMLparam1(node);
    CAMLlocal2(result, data);
    TidyNode child= tidyGetChild(node_val(node));
    if (child == NULL) {
        result= Val_int(0); // None
    } else {
        result= caml_alloc(1, 0);
        data= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
        node_val(data)= child;
        Store_field(result, 0, data);
    }
    CAMLreturn(result);
}

CAMLprim value tidyGetNext_stub(value node) {
    CAMLparam1(node);
    CAMLlocal2(result, data);
    TidyNode next= tidyGetNext(node_val(node));
    if (next == NULL) {
        result= Val_int(0); // None
    } else {
        result= caml_alloc(1, 0);
        data= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
        node_val(data)= next;
        Store_field(result, 0, data);
    }
    CAMLreturn(result);
}

CAMLprim value tidyGetPrev_stub(value node) {
    CAMLparam1(node);
    CAMLlocal2(result, data);
    TidyNode prev= tidyGetPrev(node_val(node));
    if (prev == NULL) {
        result= Val_int(0); // None
    } else {
        result= caml_alloc(1, 0);
        data= caml_alloc_custom(&p_ops, sizeof(TidyNode), 0,0);
        node_val(data)= prev;
        Store_field(result, 0, data);
    }
    CAMLreturn(result);
}

CAMLprim value tidyAttrFirst_stub(value node) {
    CAMLparam1(node);
    CAMLlocal2(result, data);
    TidyAttr attr= tidyAttrFirst(node_val(node));
    if (attr == NULL) {
        result= Val_int(0); // None
    } else {
        result= caml_alloc(1, 0);
        data= caml_alloc_custom(&p_ops, sizeof(TidyAttr), 0,0);
        attr_val(data)= attr;
        Store_field(result, 0, data);
    }
    CAMLreturn(result);
}

CAMLprim value tidyAttrNext_stub(value attr) {
    CAMLparam1(attr);
    CAMLlocal2(result, data);
    TidyAttr nextAttr= tidyAttrNext(attr_val(attr));
    if (nextAttr == NULL) {
        result= Val_int(0); // None
    } else {
        result= caml_alloc(1, 0);
        data= caml_alloc_custom(&p_ops, sizeof(TidyAttr), 0,0);
        attr_val(data)= nextAttr;
        Store_field(result, 0, data);
    }
    CAMLreturn(result);
}

CAMLprim value tidyAttrName_stub(value attr) {
    CAMLparam1(attr);
    CAMLlocal1(result);
    result= caml_copy_string(tidyAttrName(attr_val(attr)));
    CAMLreturn(result);
}

CAMLprim value tidyAttrValue_stub(value attr) {
    CAMLparam1(attr);
    CAMLlocal1(result);
    ctmbstr attrValue= tidyAttrValue(attr_val(attr));
    if (attrValue == NULL) {
        attrValue= "";
    }
    result= caml_copy_string(attrValue);
    CAMLreturn(result);
}

CAMLprim value tidyNodeGetId_stub(value node) {
    CAMLparam1(node);
    CAMLlocal1(result);
    result= Val_int(tidyNodeGetId(node_val(node)));
    CAMLreturn(result);
}

CAMLprim value tidyNodeGetType_stub(value node) {
    CAMLparam1(node);
    CAMLlocal1(result);
    result= Val_int(tidyNodeGetType(node_val(node)));
    CAMLreturn(result);
}

CAMLprim value tidyNodeGetValue_stub(value doc, value node) {
    CAMLparam2(doc, node);
    CAMLlocal1(result);
    TidyBuffer buffer;
    tidyBufInit(&buffer);
    if (tidyNodeGetValue(doc_val(doc), node_val(node), &buffer)) {
        result= caml_alloc(1, 0);
        Store_field(result, 0, caml_copy_string((char*)buffer.bp));
    } else {
        result= Val_int(0); // None
    }
    tidyBufFree(&buffer);
    CAMLreturn(result);
}

CAMLprim value tidyNodeGetName_stub(value node) {
    CAMLparam1(node);
    CAMLlocal1(result);
    result= caml_copy_string(tidyNodeGetName(node_val(node)));
    CAMLreturn(result);
}

CAMLprim value tidyNodeIsText_stub(value node) {
    CAMLparam1(node);
    CAMLlocal1(result);
    result= Val_int((tidyNodeIsText(node_val(node)))?1:0);
    CAMLreturn(result);
}

CAMLprim value tidyNodeIsProp_stub(value doc, value node) {
    CAMLparam2(doc, node);
    CAMLlocal1(result);
    result= Val_int((tidyNodeIsProp(doc_val(doc), node_val(node)))?1:0);
    CAMLreturn(result);
}

CAMLprim value tidyNodeIsHeader_stub(value node) {
    CAMLparam1(node);
    CAMLlocal1(result);
    result= Val_int((tidyNodeIsHeader(node_val(node)))?1:0);
    CAMLreturn(result);
}

CAMLprim value tidyNodeHasText_stub(value doc, value node) {
    CAMLparam2(doc, node);
    CAMLlocal1(result);
    result= Val_int((tidyNodeHasText(doc_val(doc), node_val(node)))?1:0);
    CAMLreturn(result);
}

CAMLprim value tidyNodeGetText_stub(value doc, value node) {
    CAMLparam2(doc, node);
    CAMLlocal1(result);
    TidyBuffer buffer;
    tidyBufInit(&buffer);
    if (tidyNodeGetText(doc_val(doc), node_val(node), &buffer)) {
        result= caml_alloc(1, 0);
        Store_field(result, 0, caml_copy_string((char*)buffer.bp));
    } else {
        result= Val_int(0); // None
    }
    tidyBufFree(&buffer);
    CAMLreturn(result);
}

CAMLprim value tidyNodeLine_stub(value node) {
    CAMLparam1(node);
    CAMLlocal1(result);
    result= Val_int(tidyNodeLine(node_val(node)));
    CAMLreturn(result);
}

CAMLprim value tidyNodeColumn_stub(value node) {
    CAMLparam1(node);
    CAMLlocal1(result);
    result= Val_int(tidyNodeColumn(node_val(node)));
    CAMLreturn(result);
}

