open Core_kernel

type nodeType=
  | Node_Root      (* Root *)
  | Node_DocType   (* DOCTYPE *)
  | Node_Comment   (* Comment *)
  | Node_ProcIns   (* Processing Instruction *)
  | Node_Text      (* Text *)
  | Node_Start     (* Start Tag *)
  | Node_End       (* End Tag *)
  | Node_StartEnd  (* Start/End (empty) Tag *)
  | Node_CDATA     (* Unparsed Text *)
  | Node_Section   (* XML Section *)
  | Node_Asp       (* ASP Source *)
  | Node_Jste      (* JSTE Source *)
  | Node_Php       (* PHP Source *)
  | Node_XmlDecl   (* XML Declaration *)

type tagId=
  | Tag_UNKNOWN (* Unknown tag! *)
  | Tag_A (* A. *)
  | Tag_ABBR (* ABBR. *)
  | Tag_ACRONYM (* ACRONYM. *)
  | Tag_ADDRESS (* ADDRESS. *)
  | Tag_ALIGN (* ALIGN. *)
  | Tag_APPLET (* APPLET. *)
  | Tag_AREA (* AREA. *)
  | Tag_B (* B. *)
  | Tag_BASE (* BASE. *)
  | Tag_BASEFONT (* BASEFONT. *)
  | Tag_BDO (* BDO. *)
  | Tag_BGSOUND (* BGSOUND. *)
  | Tag_BIG (* BIG. *)
  | Tag_BLINK (* BLINK. *)
  | Tag_BLOCKQUOTE (* BLOCKQUOTE. *)
  | Tag_BODY (* BODY. *)
  | Tag_BR (* BR. *)
  | Tag_BUTTON (* BUTTON. *)
  | Tag_CAPTION (* CAPTION. *)
  | Tag_CENTER (* CENTER. *)
  | Tag_CITE (* CITE. *)
  | Tag_CODE (* CODE. *)
  | Tag_COL (* COL. *)
  | Tag_COLGROUP (* COLGROUP. *)
  | Tag_COMMENT (* COMMENT. *)
  | Tag_DD (* DD. *)
  | Tag_DEL (* DEL. *)
  | Tag_DFN (* DFN. *)
  | Tag_DIR (* DIR. *)
  | Tag_DIV (* DIF. *)
  | Tag_DL (* DL. *)
  | Tag_DT (* DT. *)
  | Tag_EM (* EM. *)
  | Tag_EMBED (* EMBED. *)
  | Tag_FIELDSET (* FIELDSET. *)
  | Tag_FONT (* FONT. *)
  | Tag_FORM (* FORM. *)
  | Tag_FRAME (* FRAME. *)
  | Tag_FRAMESET (* FRAMESET. *)
  | Tag_H1 (* H1. *)
  | Tag_H2 (* H2. *)
  | Tag_H3 (* H3. *)
  | Tag_H4 (* H4. *)
  | Tag_H5 (* H5. *)
  | Tag_H6 (* H6. *)
  | Tag_HEAD (* HEAD. *)
  | Tag_HR (* HR. *)
  | Tag_HTML (* HTML. *)
  | Tag_I (* I. *)
  | Tag_IFRAME (* IFRAME. *)
  | Tag_ILAYER (* ILAYER. *)
  | Tag_IMG (* IMG. *)
  | Tag_INPUT (* INPUT. *)
  | Tag_INS (* INS. *)
  | Tag_ISINDEX (* ISINDEX. *)
  | Tag_KBD (* KBD. *)
  | Tag_KEYGEN (* KEYGEN. *)
  | Tag_LABEL (* LABEL. *)
  | Tag_LAYER (* LAYER. *)
  | Tag_LEGEND (* LEGEND. *)
  | Tag_LI (* LI. *)
  | Tag_LINK (* LINK. *)
  | Tag_LISTING (* LISTING. *)
  | Tag_MAP (* MAP. *)
  | Tag_MATHML (* MATH (HTML5) [i_a]2 MathML embedded in [X]HTML. *)
  | Tag_MARQUEE (* MARQUEE. *)
  | Tag_MENU (* MENU. *)
  | Tag_META (* META. *)
  | Tag_MULTICOL (* MULTICOL. *)
  | Tag_NOBR (* NOBR. *)
  | Tag_NOEMBED (* NOEMBED. *)
  | Tag_NOFRAMES (* NOFRAMES. *)
  | Tag_NOLAYER (* NOLAYER. *)
  | Tag_NOSAVE (* NOSAVE. *)
  | Tag_NOSCRIPT (* NOSCRIPT. *)
  | Tag_OBJECT (* OBJECT. *)
  | Tag_OL (* OL. *)
  | Tag_OPTGROUP (* OPTGROUP. *)
  | Tag_OPTION (* OPTION. *)
  | Tag_P (* P. *)
  | Tag_PARAM (* PARAM. *)
  | Tag_PICTURE (* PICTURE (HTML5) *)
  | Tag_PLAINTEXT (* PLAINTEXT. *)
  | Tag_PRE (* PRE. *)
  | Tag_Q (* Q. *)
  | Tag_RB (* RB. *)
  | Tag_RBC (* RBC. *)
  | Tag_RP (* RP. *)
  | Tag_RT (* RT. *)
  | Tag_RTC (* RTC. *)
  | Tag_RUBY (* RUBY. *)
  | Tag_S (* S. *)
  | Tag_SAMP (* SAMP. *)
  | Tag_SCRIPT (* SCRIPT. *)
  | Tag_SELECT (* SELECT. *)
  | Tag_SERVER (* SERVER. *)
  | Tag_SERVLET (* SERVLET. *)
  | Tag_SMALL (* SMALL. *)
  | Tag_SPACER (* SPACER. *)
  | Tag_SPAN (* SPAN. *)
  | Tag_STRIKE (* STRIKE. *)
  | Tag_STRONG (* STRONG. *)
  | Tag_STYLE (* STYLE. *)
  | Tag_SUB (* SUB. *)
  | Tag_SUP (* SUP. *)
  | Tag_SVG (* SVG (HTML5) *)
  | Tag_TABLE (* TABLE. *)
  | Tag_TBODY (* TBODY. *)
  | Tag_TD (* TD. *)
  | Tag_TEXTAREA (* TEXTAREA. *)
  | Tag_TFOOT (* TFOOT. *)
  | Tag_TH (* TH. *)
  | Tag_THEAD (* THEAD. *)
  | Tag_TITLE (* TITLE. *)
  | Tag_TR (* TR. *)
  | Tag_TT (* TT. *)
  | Tag_U (* U. *)
  | Tag_UL (* UL. *)
  | Tag_VAR (* VAR. *)
  | Tag_WBR (* WBR. *)
  | Tag_XMP (* XMP. *)
  | Tag_NEXTID (* NEXTID. *)
  | Tag_ARTICLE
  | Tag_ASIDE
  | Tag_AUDIO
  | Tag_BDI
  | Tag_CANVAS
  | Tag_COMMAND
  | Tag_DATALIST
  | Tag_DETAILS
  | Tag_DIALOG
  | Tag_FIGCAPTION
  | Tag_FIGURE
  | Tag_FOOTER
  | Tag_HEADER
  | Tag_HGROUP
  | Tag_MAIN
  | Tag_MARK
  | Tag_MENUITEM
  | Tag_METER
  | Tag_NAV
  | Tag_OUTPUT
  | Tag_PROGRESS
  | Tag_SECTION
  | Tag_SOURCE
  | Tag_SUMMARY
  | Tag_TEMPLATE
  | Tag_TIME
  | Tag_TRACK
  | Tag_VIDEO
  | N_TIDY_TAGS

type attrId=
  | Attr_UNKNOWN (* UNKNOWN=. *)
  | Attr_ABBR (* ABBR=. *)
  | Attr_ACCEPT (* ACCEPT=. *)
  | Attr_ACCEPT_CHARSET (* ACCEPT_CHARSET=. *)
  | Attr_ACCESSKEY (* ACCESSKEY=. *)
  | Attr_ACTION (* ACTION=. *)
  | Attr_ADD_DATE (* ADD_DATE=. *)
  | Attr_ALIGN (* ALIGN=. *)
  | Attr_ALINK (* ALINK=. *)
  | Attr_ALT (* ALT=. *)
  | Attr_ARCHIVE (* ARCHIVE=. *)
  | Attr_AXIS (* AXIS=. *)
  | Attr_BACKGROUND (* BACKGROUND=. *)
  | Attr_BGCOLOR (* BGCOLOR=. *)
  | Attr_BGPROPERTIES (* BGPROPERTIES=. *)
  | Attr_BORDER (* BORDER=. *)
  | Attr_BORDERCOLOR (* BORDERCOLOR=. *)
  | Attr_BOTTOMMARGIN (* BOTTOMMARGIN=. *)
  | Attr_CELLPADDING (* CELLPADDING=. *)
  | Attr_CELLSPACING (* CELLSPACING=. *)
  | Attr_CHAR (* CHAR=. *)
  | Attr_CHAROFF (* CHAROFF=. *)
  | Attr_CHARSET (* CHARSET=. *)
  | Attr_CHECKED (* CHECKED=. *)
  | Attr_CITE (* CITE=. *)
  | Attr_CLASS (* CLASS=. *)
  | Attr_CLASSID (* CLASSID=. *)
  | Attr_CLEAR (* CLEAR=. *)
  | Attr_CODE (* CODE=. *)
  | Attr_CODEBASE (* CODEBASE=. *)
  | Attr_CODETYPE (* CODETYPE=. *)
  | Attr_COLOR (* COLOR=. *)
  | Attr_COLS (* COLS=. *)
  | Attr_COLSPAN (* COLSPAN=. *)
  | Attr_COMPACT (* COMPACT=. *)
  | Attr_CONTENT (* CONTENT=. *)
  | Attr_COORDS (* COORDS=. *)
  | Attr_DATA (* DATA=. *)
  | Attr_DATAFLD (* DATAFLD=. *)
  | Attr_DATAFORMATAS (* DATAFORMATAS=. *)
  | Attr_DATAPAGESIZE (* DATAPAGESIZE=. *)
  | Attr_DATASRC (* DATASRC=. *)
  | Attr_DATETIME (* DATETIME=. *)
  | Attr_DECLARE (* DECLARE=. *)
  | Attr_DEFER (* DEFER=. *)
  | Attr_DIR (* DIR=. *)
  | Attr_DISABLED (* DISABLED=. *)
  | Attr_ENCODING (* ENCODING=. *)
  | Attr_ENCTYPE (* ENCTYPE=. *)
  | Attr_FACE (* FACE=. *)
  | Attr_FOR (* FOR=. *)
  | Attr_FRAME (* FRAME=. *)
  | Attr_FRAMEBORDER (* FRAMEBORDER=. *)
  | Attr_FRAMESPACING (* FRAMESPACING=. *)
  | Attr_GRIDX (* GRIDX=. *)
  | Attr_GRIDY (* GRIDY=. *)
  | Attr_HEADERS (* HEADERS=. *)
  | Attr_HEIGHT (* HEIGHT=. *)
  | Attr_HREF (* HREF=. *)
  | Attr_HREFLANG (* HREFLANG=. *)
  | Attr_HSPACE (* HSPACE=. *)
  | Attr_HTTP_EQUIV (* HTTP_EQUIV=. *)
  | Attr_ID (* ID=. *)
  | Attr_ISMAP (* ISMAP=. *)
  | Attr_ITEMID (* ITEMID=. *)
  | Attr_ITEMPROP (* ITEMPROP=. *)
  | Attr_ITEMREF (* ITEMREF=. *)
  | Attr_ITEMSCOPE (* ITEMSCOPE=. *)
  | Attr_ITEMTYPE (* ITEMTYPE=. *)
  | Attr_LABEL (* LABEL=. *)
  | Attr_LANG (* LANG=. *)
  | Attr_LANGUAGE (* LANGUAGE=. *)
  | Attr_LAST_MODIFIED (* LAST_MODIFIED=. *)
  | Attr_LAST_VISIT (* LAST_VISIT=. *)
  | Attr_LEFTMARGIN (* LEFTMARGIN=. *)
  | Attr_LINK (* LINK=. *)
  | Attr_LONGDESC (* LONGDESC=. *)
  | Attr_LOWSRC (* LOWSRC=. *)
  | Attr_MARGINHEIGHT (* MARGINHEIGHT=. *)
  | Attr_MARGINWIDTH (* MARGINWIDTH=. *)
  | Attr_MAXLENGTH (* MAXLENGTH=. *)
  | Attr_MEDIA (* MEDIA=. *)
  | Attr_METHOD (* METHOD=. *)
  | Attr_MULTIPLE (* MULTIPLE=. *)
  | Attr_NAME (* NAME=. *)
  | Attr_NOHREF (* NOHREF=. *)
  | Attr_NORESIZE (* NORESIZE=. *)
  | Attr_NOSHADE (* NOSHADE=. *)
  | Attr_NOWRAP (* NOWRAP=. *)
  | Attr_OBJECT (* OBJECT=. *)
  | Attr_OnAFTERUPDATE (* OnAFTERUPDATE=. *)
  | Attr_OnBEFOREUNLOAD (* OnBEFOREUNLOAD=. *)
  | Attr_OnBEFOREUPDATE (* OnBEFOREUPDATE=. *)
  | Attr_OnBLUR (* OnBLUR=. *)
  | Attr_OnCHANGE (* OnCHANGE=. *)
  | Attr_OnCLICK (* OnCLICK=. *)
  | Attr_OnDATAAVAILABLE (* OnDATAAVAILABLE=. *)
  | Attr_OnDATASETCHANGED (* OnDATASETCHANGED=. *)
  | Attr_OnDATASETCOMPLETE (* OnDATASETCOMPLETE=. *)
  | Attr_OnDBLCLICK (* OnDBLCLICK=. *)
  | Attr_OnERRORUPDATE (* OnERRORUPDATE=. *)
  | Attr_OnFOCUS (* OnFOCUS=. *)
  | Attr_OnKEYDOWN (* OnKEYDOWN=. *)
  | Attr_OnKEYPRESS (* OnKEYPRESS=. *)
  | Attr_OnKEYUP (* OnKEYUP=. *)
  | Attr_OnLOAD (* OnLOAD=. *)
  | Attr_OnMOUSEDOWN (* OnMOUSEDOWN=. *)
  | Attr_OnMOUSEMOVE (* OnMOUSEMOVE=. *)
  | Attr_OnMOUSEOUT (* OnMOUSEOUT=. *)
  | Attr_OnMOUSEOVER (* OnMOUSEOVER=. *)
  | Attr_OnMOUSEUP (* OnMOUSEUP=. *)
  | Attr_OnRESET (* OnRESET=. *)
  | Attr_OnROWENTER (* OnROWENTER=. *)
  | Attr_OnROWEXIT (* OnROWEXIT=. *)
  | Attr_OnSELECT (* OnSELECT=. *)
  | Attr_OnSUBMIT (* OnSUBMIT=. *)
  | Attr_OnUNLOAD (* OnUNLOAD=. *)
  | Attr_PROFILE (* PROFILE=. *)
  | Attr_PROMPT (* PROMPT=. *)
  | Attr_RBSPAN (* RBSPAN=. *)
  | Attr_READONLY (* READONLY=. *)
  | Attr_REL (* REL=. *)
  | Attr_REV (* REV=. *)
  | Attr_RIGHTMARGIN (* RIGHTMARGIN=. *)
  | Attr_ROLE (* ROLE=. *)
  | Attr_ROWS (* ROWS=. *)
  | Attr_ROWSPAN (* ROWSPAN=. *)
  | Attr_RULES (* RULES=. *)
  | Attr_SCHEME (* SCHEME=. *)
  | Attr_SCOPE (* SCOPE=. *)
  | Attr_SCROLLING (* SCROLLING=. *)
  | Attr_SELECTED (* SELECTED=. *)
  | Attr_SHAPE (* SHAPE=. *)
  | Attr_SHOWGRID (* SHOWGRID=. *)
  | Attr_SHOWGRIDX (* SHOWGRIDX=. *)
  | Attr_SHOWGRIDY (* SHOWGRIDY=. *)
  | Attr_SIZE (* SIZE=. *)
  | Attr_SPAN (* SPAN=. *)
  | Attr_SRC (* SRC=. *)
  | Attr_SRCSET (* SRCSET= (HTML5) *)
  | Attr_STANDBY (* STANDBY=. *)
  | Attr_START (* START=. *)
  | Attr_STYLE (* STYLE=. *)
  | Attr_SUMMARY (* SUMMARY=. *)
  | Attr_TABINDEX (* TABINDEX=. *)
  | Attr_TARGET (* TARGET=. *)
  | Attr_TEXT (* TEXT=. *)
  | Attr_TITLE (* TITLE=. *)
  | Attr_TOPMARGIN (* TOPMARGIN=. *)
  | Attr_TYPE (* TYPE=. *)
  | Attr_USEMAP (* USEMAP=. *)
  | Attr_VALIGN (* VALIGN=. *)
  | Attr_VALUE (* VALUE=. *)
  | Attr_VALUETYPE (* VALUETYPE=. *)
  | Attr_VERSION (* VERSION=. *)
  | Attr_VLINK (* VLINK=. *)
  | Attr_VSPACE (* VSPACE=. *)
  | Attr_WIDTH (* WIDTH=. *)
  | Attr_WRAP (* WRAP=. *)
  | Attr_XML_LANG (* XML_LANG=. *)
  | Attr_XML_SPACE (* XML_SPACE=. *)
  | Attr_XMLNS (* XMLNS=. *)
  | Attr_EVENT (* EVENT=. *)
  | Attr_METHODS (* METHODS=. *)
  | Attr_N (* N=. *)
  | Attr_SDAFORM (* SDAFORM=. *)
  | Attr_SDAPREF (* SDAPREF=. *)
  | Attr_SDASUFF (* SDASUFF=. *)
  | Attr_URN (* URN=. *)
  | Attr_ASYNC
  | Attr_AUTOCOMPLETE
  | Attr_AUTOFOCUS
  | Attr_AUTOPLAY
  | Attr_CHALLENGE
  | Attr_CONTENTEDITABLE
  | Attr_CONTEXTMENU
  | Attr_CONTROLS
  | Attr_DEFAULT
  | Attr_DIRNAME
  | Attr_DRAGGABLE
  | Attr_DROPZONE
  | Attr_FORM
  | Attr_FORMACTION
  | Attr_FORMENCTYPE
  | Attr_FORMMETHOD
  | Attr_FORMNOVALIDATE
  | Attr_FORMTARGET
  | Attr_HIDDEN
  | Attr_HIGH
  | Attr_ICON
  | Attr_KEYTYPE
  | Attr_KIND
  | Attr_LIST
  | Attr_LOOP
  | Attr_LOW
  | Attr_MANIFEST
  | Attr_MAX
  | Attr_MEDIAGROUP
  | Attr_MIN
  | Attr_NOVALIDATE
  | Attr_OPEN
  | Attr_OPTIMUM
  | Attr_OnABORT
  | Attr_OnAFTERPRINT
  | Attr_OnBEFOREPRINT
  | Attr_OnCANPLAY
  | Attr_OnCANPLAYTHROUGH
  | Attr_OnCONTEXTMENU
  | Attr_OnCUECHANGE
  | Attr_OnDRAG
  | Attr_OnDRAGEND
  | Attr_OnDRAGENTER
  | Attr_OnDRAGLEAVE
  | Attr_OnDRAGOVER
  | Attr_OnDRAGSTART
  | Attr_OnDROP
  | Attr_OnDURATIONCHANGE
  | Attr_OnEMPTIED
  | Attr_OnENDED
  | Attr_OnERROR
  | Attr_OnHASHCHANGE
  | Attr_OnINPUT
  | Attr_OnINVALID
  | Attr_OnLOADEDDATA
  | Attr_OnLOADEDMETADATA
  | Attr_OnLOADSTART
  | Attr_OnMESSAGE
  | Attr_OnMOUSEWHEEL
  | Attr_OnOFFLINE
  | Attr_OnONLINE
  | Attr_OnPAGEHIDE
  | Attr_OnPAGESHOW
  | Attr_OnPAUSE
  | Attr_OnPLAY
  | Attr_OnPLAYING
  | Attr_OnPOPSTATE
  | Attr_OnPROGRESS
  | Attr_OnRATECHANGE
  | Attr_OnREADYSTATECHANGE
  | Attr_OnREDO
  | Attr_OnRESIZE
  | Attr_OnSCROLL
  | Attr_OnSEEKED
  | Attr_OnSEEKING
  | Attr_OnSHOW
  | Attr_OnSTALLED
  | Attr_OnSTORAGE
  | Attr_OnSUSPEND
  | Attr_OnTIMEUPDATE
  | Attr_OnUNDO
  | Attr_OnVOLUMECHANGE
  | Attr_OnWAITING
  | Attr_PATTERN
  | Attr_PLACEHOLDER
  | Attr_POSTER
  | Attr_PRELOAD
  | Attr_PUBDATE
  | Attr_RADIOGROUP
  | Attr_REQUIRED
  | Attr_REVERSED
  | Attr_SANDBOX
  | Attr_SCOPED
  | Attr_SEAMLESS
  | Attr_SIZES
  | Attr_SPELLCHECK
  | Attr_SRCDOC
  | Attr_SRCLANG
  | Attr_STEP
  | Attr_ARIA_ACTIVEDESCENDANT
  | Attr_ARIA_ATOMIC
  | Attr_ARIA_AUTOCOMPLETE
  | Attr_ARIA_BUSY
  | Attr_ARIA_CHECKED
  | Attr_ARIA_CONTROLS
  | Attr_ARIA_DESCRIBEDBY
  | Attr_ARIA_DISABLED
  | Attr_ARIA_DROPEFFECT
  | Attr_ARIA_EXPANDED
  | Attr_ARIA_FLOWTO
  | Attr_ARIA_GRABBED
  | Attr_ARIA_HASPOPUP
  | Attr_ARIA_HIDDEN
  | Attr_ARIA_INVALID
  | Attr_ARIA_LABEL
  | Attr_ARIA_LABELLEDBY
  | Attr_ARIA_LEVEL
  | Attr_ARIA_LIVE
  | Attr_ARIA_MULTILINE
  | Attr_ARIA_MULTISELECTABLE
  | Attr_ARIA_ORIENTATION
  | Attr_ARIA_OWNS
  | Attr_ARIA_POSINSET
  | Attr_ARIA_PRESSED
  | Attr_ARIA_READONLY
  | Attr_ARIA_RELEVANT
  | Attr_ARIA_REQUIRED
  | Attr_ARIA_SELECTED
  | Attr_ARIA_SETSIZE
  | Attr_ARIA_SORT
  | Attr_ARIA_VALUEMAX
  | Attr_ARIA_VALUEMIN
  | Attr_ARIA_VALUENOW
  | Attr_ARIA_VALUETEXT
  | Attr_X (* X=. *)
  | Attr_Y (* Y=. *)
  | Attr_VIEWBOX (* VIEWBOX=. *)
  | Attr_PRESERVEASPECTRATIO (* PRESERVEASPECTRATIO=. *)
  | Attr_ZOOMANDPAN (* ZOOMANDPAN=. *)
  | Attr_BASEPROFILE (* BASEPROFILE=. *)
  | Attr_CONTENTSCRIPTTYPE (* CONTENTSCRIPTTYPE=. *)
  | Attr_CONTENTSTYLETYPE (* CONTENTSTYLETYPE=. *)
  | Attr_DISPLAY (* DISPLAY= (html5) *)
  | N_TIDY_ATTRIBS

type optionId=
  | TidyUnknownOption  (* Unknown option! *)
  | TidyIndentSpaces  (* Indentation n spaces/tabs. *)
  | TidyWrapLen  (* Wrap margin. *)
  | TidyTabSize  (* Expand tabs to n spaces. *)
  | TidyCharEncoding  (* In/out character encoding. *)
  | TidyInCharEncoding  (* Input character encoding (if different) *)
  | TidyOutCharEncoding  (* Output character encoding (if different) *)
  | TidyNewline  (* Output line ending (default to platform) *)
  | TidyDoctypeMode  (* See doctype property. *)
  | TidyDoctype  (* User specified doctype. *)
  | TidyDuplicateAttrs  (* Keep first or last duplicate attribute. *)
  | TidyAltText  (* Default text for alt attribute. *)
  | TidySlideStyle  (* Style sheet for slides: not used for anything yet. *)
  | TidyErrFile  (* File name to write errors to. *)
  | TidyOutFile  (* File name to write markup to. *)
  | TidyWriteBack  (* If true then output tidied markup. *)
  | TidyShowMarkup  (* If false, normal output is suppressed. *)
  | TidyShowInfo  (* If true, info-level messages are shown. *)
  | TidyShowWarnings  (* However errors are always shown. *)
  | TidyQuiet  (* No 'Parsing X', guessed DTD or summary. *)
  | TidyIndentContent  (* Indent content of appropriate tags. "auto" does text/block level content indentation *)
  | TidyCoerceEndTags  (* Coerce end tags from start tags where probably intended. *)
  | TidyOmitOptionalTags  (* Suppress optional start tags and end tags. *)
  | TidyHideEndTags  (* Legacy name for TidyOmitOptionalTags. *)
  | TidyXmlTags  (* Treat input as XML. *)
  | TidyXmlOut  (* Create output as XML. *)
  | TidyXhtmlOut  (* Output extensible HTML. *)
  | TidyHtmlOut  (* Output plain HTML, even for XHTML input. Yes means set explicitly. *)
  | TidyXmlDecl  (* Add <?xml?> for XML docs. *)
  | TidyUpperCaseTags  (* Output tags in upper not lower case. *)
  | TidyUpperCaseAttrs  (* Output attributes in upper not lower case. *)
  | TidyMakeBare  (* Make bare HTML: remove Microsoft cruft. *)
  | TidyMakeClean  (* Replace presentational clutter by style rules. *)
  | TidyGDocClean  (* Clean up HTML exported from Google Docs. *)
  | TidyLogicalEmphasis  (* Replace i by em and b by strong. *)
  | TidyDropPropAttrs  (* Discard proprietary attributes. *)
  | TidyDropFontTags  (* Discard presentation tags. *)
  | TidyDropEmptyElems  (* Discard empty elements. *)
  | TidyDropEmptyParas  (* Discard empty p elements. *)
  | TidyFixComments  (* Fix comments with adjacent hyphens. *)
  | TidyBreakBeforeBR  (* Output newline before or not? *)
  | TidyBurstSlides  (* Create slides on each h2 element. *)
  | TidyNumEntities  (* Use numeric entities. *)
  | TidyQuoteMarks  (* Output " marks as ". *)
  | TidyQuoteNbsp  (* Output non-breaking space as entity. *)
  | TidyQuoteAmpersand  (* Output naked ampersand as &. *)
  | TidyWrapAttVals  (* Wrap within attribute values. *)
  | TidyWrapScriptlets  (* Wrap within JavaScript string literals. *)
  | TidyWrapSection  (* Wrap within <![ ... ]> section tags *)
  | TidyWrapAsp  (* Wrap within ASP pseudo elements. *)
  | TidyWrapJste  (* Wrap within JSTE pseudo elements. *)
  | TidyWrapPhp  (* Wrap within PHP pseudo elements. *)
  | TidyFixBackslash  (* Fix URLs by replacing \ with /. *)
  | TidyIndentAttributes  (* Newline+indent before each attribute. *)
  | TidyXmlPIs  (* If set to yes PIs must end with ?> *)
  | TidyXmlSpace  (* If set to yes adds xml:space attr as needed. *)
  | TidyEncloseBodyText  (* If yes text at body is wrapped in P's. *)
  | TidyEncloseBlockText  (* If yes text in blocks is wrapped in P's. *)
  | TidyKeepFileTimes  (* If yes last modied time is preserved. *)
  | TidyWord2000  (* Draconian cleaning for Word2000. *)
  | TidyMark  (* Add meta element indicating tidied doc. *)
  | TidyEmacs  (* If true format error output for GNU Emacs. *)
  | TidyEmacsFile  (* Name of current Emacs file. *)
  | TidyLiteralAttribs  (* If true attributes may use newlines. *)
  | TidyBodyOnly  (* Output BODY content only. *)
  | TidyFixUri  (* Applies URI encoding if necessary. *)
  | TidyLowerLiterals  (* Folds known attribute values to lower case. *)
  | TidyHideComments  (* Hides all (real) comments in output. *)
  | TidyIndentCdata  (* Indent <!CDATA[ ... ]]> section *)
  | TidyForceOutput  (* Output document even if errors were found. *)
  | TidyShowErrors  (* Number of errors to put out. *)
  | TidyAsciiChars  (* Convert quotes and dashes to nearest ASCII char. *)
  | TidyJoinClasses  (* Join multiple class attributes. *)
  | TidyJoinStyles  (* Join multiple style attributes. *)
  | TidyEscapeCdata  (* Replace <![CDATA[]]> sections with escaped text. *)
  | TidyLanguage
  | TidyNCR
  | TidyOutputBOM
  | TidyReplaceColor  (* Replace hex color attribute values with names. *)
  | TidyCSSPrefix  (* CSS class naming for -clean option. *)
  | TidyInlineTags  (* Declared inline tags. *)
  | TidyBlockTags  (* Declared block tags. *)
  | TidyEmptyTags  (* Declared empty tags. *)
  | TidyPreTags  (* Declared pre tags. *)
  | TidyAccessibilityCheckLevel  (* Accessibility check level 0 (old style), or 1, 2, 3. *)
  | TidyVertSpace  (* degree to which markup is spread out vertically *)
  | TidyPunctWrap
  | TidyMergeEmphasis  (* Merge nested B and I elements. *)
  | TidyMergeDivs  (* Merge multiple DIVs. *)
  | TidyDecorateInferredUL  (* Mark inferred UL elements with no indent CSS. *)
  | TidyPreserveEntities  (* Preserve entities. *)
  | TidySortAttributes  (* Sort attributes. *)
  | TidyMergeSpans  (* Merge multiple SPANs. *)
  | TidyAnchorAsName  (* Define anchors as name attributes. *)
  | TidyPPrintTabs  (* Indent using tabs istead of spaces. *)
  | N_TIDY_OPTIONS  (* Must be last.  *)

type opt=
  | Str of string
  | Int of int
  | Bool of bool

module Stub = struct
  type doc
  type node
  type attr
  type opt

  (* uninitialized char encoding *)
  let ce_ascii= ref 0
  let ce_latin1= ref 0
  let ce_raw= ref 0
  let ce_utf8= ref 0
  let ce_iso2022= ref 0
  let ce_mac= ref 0
  let ce_win1252= ref 0
  let ce_utf16le= ref 0
  let ce_utf16be= ref 0
  let ce_utf16= ref 0
  let ce_big5= ref 0
  let ce_shiftjis= ref 0

  external create: unit -> doc= "tidyCreate_stub"

  type result=
    | Success     (* 0 *)
    | Td_warning  (* 1 *)
    | Td_error    (* 2 *)
    | Sv_error    (* -1 *)

  type optionType=
    | String
    | Integer
    | Boolean

  let string_of_optionType= function
    | String-> "string"
    | Integer-> "int"
    | Boolean-> "bool"

  let optionType_of_string str=
    match String.lowercase str with
    | "string"-> String
    | "int"-> Integer
    | "bool"-> Boolean
    | _-> failwith "unknown type"

  external accessWarningCount: doc -> int = "tidyAccessWarningCount_stub"
  external configErrorCount: doc -> int = "tidyConfigErrorCount_stub"
  external detectedGenericXml: doc -> bool = "tidyDetectedGenericXml_stub"
  external detectedHtmlVersion: doc -> int = "tidyDetectedHtmlVersion_stub"
  external detectedXhtml: doc -> bool = "tidyDetectedXhtml_stub"
  external errorCount: doc -> int = "tidyErrorCount_stub"
  external errorSummary: doc -> unit = "tidyErrorSummary_stub"
  external fileExists: doc -> string -> unit = "tidyFileExists_stub"
  external generalInfo: doc -> unit = "tidyGeneralInfo_stub"
  external libraryVersion: unit -> string = "tidyLibraryVersion_stub"
  external loadConfig: doc -> string -> unit = "tidyLoadConfig_stub"
  external warningCount: doc -> int = "tidyWarningCount_stub"

  external getOption: doc -> optionId -> opt = "tidyGetOption_stub"
  external optGetName: opt -> string = "tidyOptGetName_stub"
  external optGetType: opt -> optionType = "tidyOptGetType_stub"
  external optGetId: opt -> optionId = "tidyOptGetId_stub"

  external optGetValue: doc -> optionId -> string = "tidyOptGetValue_stub"
  external optGetInt: doc -> optionId -> int = "tidyOptGetInt_stub"
  external optGetBool: doc -> optionId -> bool = "tidyOptGetBool_stub"

  external optSetValue: doc -> optionId -> string -> unit= "tidyOptSetValue_stub"
  external optSetInt: doc -> optionId -> int -> unit= "tidyOptSetInt_stub"
  external optSetBool: doc -> optionId -> bool -> unit= "tidyOptSetBool_stub"

  external setCharEncoding: doc -> string -> unit = "tidySetCharEncoding_stub"
  external setInCharEncoding: doc -> string -> unit = "tidySetInCharEncoding_stub"
  external setOutCharEncoding: doc -> string -> unit = "tidySetOutCharEncoding_stub"
  external declareBlockTags: doc -> string -> unit = "tidyDeclareBlockTags_stub"
  external setDropEmptyParas: doc -> bool -> unit = "tidySetDropEmptyParas_stub"
  external setDropEmptyElems: doc -> bool -> unit = "tidySetDropEmptyElems_stub"

  external parseFile: doc -> string -> result= "tidyParseFile_stub"
  external parseString: doc -> string -> result= "tidyParseString_stub"

  external cleanAndRepair: doc -> unit= "tidyCleanAndRepair_stub" [@@noalloc]
  external reportDoctype: doc -> unit= "tidyReportDoctype_stub" [@@noalloc]
  external runDiagnostics: doc -> unit= "tidyRunDiagnostics_stub" [@@noalloc]

  external saveFile: doc -> string -> unit= "tidySaveFile_stub"
  external saveString: doc -> string option= "tidySaveString_stub"

  external getRoot: doc -> node= "tidyGetRoot_stub"
  external getHtml: doc -> node= "tidyGetHtml_stub"
  external getHead: doc -> node= "tidyGetHead_stub"
  external getBody: doc -> node= "tidyGetBody_stub"

  external attrGetId: attr -> attrId = "tidyAttrGetId_stub"
  external attrIsEvent: attr -> bool = "tidyAttrIsEvent_stub"

  external attrGetById: node -> attrId -> attr option= "tidyAttrGetById_stub"

  external getParent: node -> node option= "tidyGetParent_stub"
  external getChild: node -> node option= "tidyGetChild_stub"
  external getNext: node -> node option= "tidyGetNext_stub"
  external getPrev: node -> node option= "tidyGetPrev_stub"
  external attrFirst: node -> attr option= "tidyAttrFirst_stub"
  external attrNext: attr -> attr option= "tidyAttrNext_stub"
  external attrName: attr -> string= "tidyAttrName_stub"
  external attrValue: attr -> string= "tidyAttrValue_stub"

  external nodeGetId: node -> tagId= "tidyNodeGetId_stub"
  external nodeGetType: node -> nodeType= "tidyNodeGetType_stub"
  external nodeGetValue: doc -> node -> string option= "tidyNodeGetValue_stub"
  external nodeGetName: node -> string= "tidyNodeGetName_stub"
  external nodeIsText: node -> bool= "tidyNodeIsText_stub"
  external nodeIsProp: doc -> node -> bool= "tidyNodeIsProp_stub"
  external nodeIsHeader: node -> bool= "tidyNodeIsHeader_stub"
  external nodeHasText: doc -> node -> bool= "tidyNodeHasText_stub"
  external nodeGetText: doc -> node -> string option= "tidyNodeGetText_stub"
  external nodeLine: node -> int= "tidyNodeLine_stub"
  external nodeColumn: node -> int= "tidyNodeColumn_stub"
end

type doc= Stub.doc

type node= {
  doc: Stub.doc;
  node: Stub.node;
}

type attr= {
  doc: Stub.doc;
  attr: Stub.attr;
}

let string_of_opt opt=
  match opt with
  | Str v-> sprintf "%s:string" v
  | Int v-> sprintf "%d:int" v
  | Bool v-> sprintf "%b:bool" v

let opt_str= function
  | Str s-> s
  | _-> failwith "string option expected"

let opt_int= function
  | Int s-> s
  | _-> failwith "integer option expected"

let opt_bool= function
  | Bool s-> s
  | _-> failwith "boolean option expected"

module Config = struct
  let blockTags doc tags=
    Stub.declareBlockTags doc (String.concat ~sep:" " tags)

  let getOption doc optId=
    match Stub.getOption doc optId |> Stub.optGetType with
    | Stub.String-> Str (Stub.optGetValue doc optId)
    | Stub.Integer-> Int (Stub.optGetInt doc optId)
    | Stub.Boolean-> Bool (Stub.optGetBool doc optId)

  let setOption doc optId value=
    let opt= Stub.getOption doc optId in
    let name= Stub.optGetName opt
    and optType= Stub.optGetType opt in
    match optType with
    | Stub.String->
      (match value with
      | Str v-> Stub.optSetValue doc optId v
      | _-> failwith (sprintf
        "%s requires a string parameter, but a %s is encounted"
        name
        (Stub.string_of_optionType optType)))
    | Stub.Integer->
      (match value with
      | Int v-> Stub.optSetInt doc optId v
      | _-> failwith (sprintf
        "%s requires a int parameter, but a %s is encounted"
        name
        (Stub.string_of_optionType optType)))
    | Stub.Boolean->
      (match value with
      | Bool v-> Stub.optSetBool doc optId v
      | _-> failwith (sprintf
        "%s requires a bool parameter, but a %s is encounted"
        name
        (Stub.string_of_optionType optType)))

  type charEncoding=
    | Ascii
    | Latin1
    | Raw
    | Utf8
    | Iso2022
    | Mac
    | Win1252
    | Utf16le
    | Utf16be
    | Utf16
    | Big5
    | Shiftjis

  let charEncoding_to_string= function
    | Ascii-> "Ascii"
    | Latin1-> "Latin1"
    | Raw-> "Raw"
    | Utf8-> "Utf8"
    | Iso2022-> "Iso2022"
    | Mac-> "Mac"
    | Win1252-> "Win1252"
    | Utf16le-> "Utf16le"
    | Utf16be-> "Utf16be"
    | Utf16-> "Utf16"
    | Big5-> "Big5"
    | Shiftjis-> "Shiftjis"

  let charEncoding_of_string s=
    let low= String.lowercase s in
    match low with
    | "ascii"-> Ascii
    | "latin1"-> Latin1
    | "raw"-> Raw
    | "utf8"-> Utf8
    | "iso2022"-> Iso2022
    | "mac"-> Mac
    | "win1252"-> Win1252
    | "utf16le"-> Utf16le
    | "utf16be"-> Utf16be
    | "utf16"-> Utf16
    | "big5"-> Big5
    | "shiftjis"-> Shiftjis
    | _-> failwith "charEncoding_of_string"


  module Raw = struct

    let getIndentSpaces doc= getOption doc TidyIndentSpaces |> opt_int
    let setIndentSpaces doc opt= setOption doc TidyIndentSpaces (Int opt)

    let getWrap doc= getOption doc TidyWrapLen |> opt_int
    let setWrap doc opt= setOption doc TidyWrapLen (Int opt)

    let getTabSize doc= getOption doc TidyTabSize |> opt_int
    let setTabSize doc opt= setOption doc TidyTabSize (Int opt)

    let getCharEncoding doc= getOption doc TidyCharEncoding |> opt_str |> charEncoding_of_string
    let setCharEncoding doc opt= setOption doc TidyCharEncoding (Str (opt |> charEncoding_to_string))

    let getInputEncoding doc= getOption doc TidyInCharEncoding |> opt_str |> charEncoding_of_string
    let setInputEncoding doc opt= setOption doc TidyInCharEncoding (Str (opt |> charEncoding_to_string))

    let getOutputEncoding doc= getOption doc TidyOutCharEncoding |> opt_str |> charEncoding_of_string
    let setOutputEncoding doc opt= setOption doc TidyOutCharEncoding (Str (opt |> charEncoding_to_string))

    let getNewline doc= getOption doc TidyNewline |> opt_int
    let setNewline doc opt= setOption doc TidyNewline (Int opt)

    let getDoctypeMode doc= getOption doc TidyDoctypeMode |> opt_int
    let setDoctypeMode doc opt= setOption doc TidyDoctypeMode (Int opt)

    let getDoctype doc= getOption doc TidyDoctype |> opt_str
    let setDoctype doc opt= setOption doc TidyDoctype (Str opt)

    let getRepeatedAttributes doc= getOption doc TidyDuplicateAttrs |> opt_int
    let setRepeatedAttributes doc opt= setOption doc TidyDuplicateAttrs (Int opt)

    let getAltText doc= getOption doc TidyAltText |> opt_str
    let setAltText doc opt= setOption doc TidyAltText (Str opt)

    let getSlideStyle doc= getOption doc TidySlideStyle |> opt_str
    let setSlideStyle doc opt= setOption doc TidySlideStyle (Str opt)

    let getErrorFile doc= getOption doc TidyErrFile |> opt_str
    let setErrorFile doc opt= setOption doc TidyErrFile (Str opt)

    let getOutputFile doc= getOption doc TidyOutFile |> opt_str
    let setOutputFile doc opt= setOption doc TidyOutFile (Str opt)

    let getWriteBack doc= getOption doc TidyWriteBack |> opt_bool
    let setWriteBack doc opt= setOption doc TidyWriteBack (Bool opt)

    let getMarkup doc= getOption doc TidyShowMarkup |> opt_bool
    let setMarkup doc opt= setOption doc TidyShowMarkup (Bool opt)

    let getShowInfo doc= getOption doc TidyShowInfo |> opt_bool
    let setShowInfo doc opt= setOption doc TidyShowInfo (Bool opt)

    let getShowWarnings doc= getOption doc TidyShowWarnings |> opt_bool
    let setShowWarnings doc opt= setOption doc TidyShowWarnings (Bool opt)

    let getQuiet doc= getOption doc TidyQuiet |> opt_bool
    let setQuiet doc opt= setOption doc TidyQuiet (Bool opt)

    let getIndent doc= getOption doc TidyIndentContent |> opt_int
    let setIndent doc opt= setOption doc TidyIndentContent (Int opt)

    let getCoerceEndtags doc= getOption doc TidyCoerceEndTags |> opt_bool
    let setCoerceEndtags doc opt= setOption doc TidyCoerceEndTags (Bool opt)

    let getOmitOptionalTags doc= getOption doc TidyOmitOptionalTags |> opt_bool
    let setOmitOptionalTags doc opt= setOption doc TidyOmitOptionalTags (Bool opt)

    let getHideEndtags doc= getOption doc TidyHideEndTags |> opt_bool
    let setHideEndtags doc opt= setOption doc TidyHideEndTags (Bool opt)

    let getInputXml doc= getOption doc TidyXmlTags |> opt_bool
    let setInputXml doc opt= setOption doc TidyXmlTags (Bool opt)

    let getOutputXml doc= getOption doc TidyXmlOut |> opt_bool
    let setOutputXml doc opt= setOption doc TidyXmlOut (Bool opt)

    let getOutputXhtml doc= getOption doc TidyXhtmlOut |> opt_bool
    let setOutputXhtml doc opt= setOption doc TidyXhtmlOut (Bool opt)

    let getOutputHtml doc= getOption doc TidyHtmlOut |> opt_bool
    let setOutputHtml doc opt= setOption doc TidyHtmlOut (Bool opt)

    let getAddXmlDecl doc= getOption doc TidyXmlDecl |> opt_bool
    let setAddXmlDecl doc opt= setOption doc TidyXmlDecl (Bool opt)

    let getUppercaseTags doc= getOption doc TidyUpperCaseTags |> opt_bool
    let setUppercaseTags doc opt= setOption doc TidyUpperCaseTags (Bool opt)

    let getUppercaseAttributes doc= getOption doc TidyUpperCaseAttrs |> opt_bool
    let setUppercaseAttributes doc opt= setOption doc TidyUpperCaseAttrs (Bool opt)

    let getBare doc= getOption doc TidyMakeBare |> opt_bool
    let setBare doc opt= setOption doc TidyMakeBare (Bool opt)

    let getClean doc= getOption doc TidyMakeClean |> opt_bool
    let setClean doc opt= setOption doc TidyMakeClean (Bool opt)

    let getGdoc doc= getOption doc TidyGDocClean |> opt_bool
    let setGdoc doc opt= setOption doc TidyGDocClean (Bool opt)

    let getLogicalEmphasis doc= getOption doc TidyLogicalEmphasis |> opt_bool
    let setLogicalEmphasis doc opt= setOption doc TidyLogicalEmphasis (Bool opt)

    let getDropProprietaryAttributes doc= getOption doc TidyDropPropAttrs |> opt_bool
    let setDropProprietaryAttributes doc opt= setOption doc TidyDropPropAttrs (Bool opt)

    let getDropFontTags doc= getOption doc TidyDropFontTags |> opt_bool
    let setDropFontTags doc opt= setOption doc TidyDropFontTags (Bool opt)

    let getDropEmptyElements doc= getOption doc TidyDropEmptyElems |> opt_bool
    let setDropEmptyElements doc opt= setOption doc TidyDropEmptyElems (Bool opt)

    let getDropEmptyParas doc= getOption doc TidyDropEmptyParas |> opt_bool
    let setDropEmptyParas doc opt= setOption doc TidyDropEmptyParas (Bool opt)

    let getFixBadComments doc= getOption doc TidyFixComments |> opt_bool
    let setFixBadComments doc opt= setOption doc TidyFixComments (Bool opt)

    let getBreakBeforeBr doc= getOption doc TidyBreakBeforeBR |> opt_bool
    let setBreakBeforeBr doc opt= setOption doc TidyBreakBeforeBR (Bool opt)

    let getSplit doc= getOption doc TidyBurstSlides |> opt_bool
    let setSplit doc opt= setOption doc TidyBurstSlides (Bool opt)

    let getNumericEntities doc= getOption doc TidyNumEntities |> opt_bool
    let setNumericEntities doc opt= setOption doc TidyNumEntities (Bool opt)

    let getQuoteMarks doc= getOption doc TidyQuoteMarks |> opt_bool
    let setQuoteMarks doc opt= setOption doc TidyQuoteMarks (Bool opt)

    let getQuoteNbsp doc= getOption doc TidyQuoteNbsp |> opt_bool
    let setQuoteNbsp doc opt= setOption doc TidyQuoteNbsp (Bool opt)

    let getQuoteAmpersand doc= getOption doc TidyQuoteAmpersand |> opt_bool
    let setQuoteAmpersand doc opt= setOption doc TidyQuoteAmpersand (Bool opt)

    let getWrapAttributes doc= getOption doc TidyWrapAttVals |> opt_bool
    let setWrapAttributes doc opt= setOption doc TidyWrapAttVals (Bool opt)

    let getWrapScriptLiterals doc= getOption doc TidyWrapScriptlets |> opt_bool
    let setWrapScriptLiterals doc opt= setOption doc TidyWrapScriptlets (Bool opt)

    let getWrapSections doc= getOption doc TidyWrapSection |> opt_bool
    let setWrapSections doc opt= setOption doc TidyWrapSection (Bool opt)

    let getWrapAsp doc= getOption doc TidyWrapAsp |> opt_bool
    let setWrapAsp doc opt= setOption doc TidyWrapAsp (Bool opt)

    let getWrapJste doc= getOption doc TidyWrapJste |> opt_bool
    let setWrapJste doc opt= setOption doc TidyWrapJste (Bool opt)

    let getWrapPhp doc= getOption doc TidyWrapPhp |> opt_bool
    let setWrapPhp doc opt= setOption doc TidyWrapPhp (Bool opt)

    let getFixBackslash doc= getOption doc TidyFixBackslash |> opt_bool
    let setFixBackslash doc opt= setOption doc TidyFixBackslash (Bool opt)

    let getIndentAttributes doc= getOption doc TidyIndentAttributes |> opt_bool
    let setIndentAttributes doc opt= setOption doc TidyIndentAttributes (Bool opt)

    let getAssumeXmlProcins doc= getOption doc TidyXmlPIs |> opt_bool
    let setAssumeXmlProcins doc opt= setOption doc TidyXmlPIs (Bool opt)

    let getAddXmlSpace doc= getOption doc TidyXmlSpace |> opt_bool
    let setAddXmlSpace doc opt= setOption doc TidyXmlSpace (Bool opt)

    let getEncloseText doc= getOption doc TidyEncloseBodyText |> opt_bool
    let setEncloseText doc opt= setOption doc TidyEncloseBodyText (Bool opt)

    let getEncloseBlockText doc= getOption doc TidyEncloseBlockText |> opt_bool
    let setEncloseBlockText doc opt= setOption doc TidyEncloseBlockText (Bool opt)

    let getKeepTime doc= getOption doc TidyKeepFileTimes |> opt_bool
    let setKeepTime doc opt= setOption doc TidyKeepFileTimes (Bool opt)

    let getWord2000 doc= getOption doc TidyWord2000 |> opt_bool
    let setWord2000 doc opt= setOption doc TidyWord2000 (Bool opt)

    let getTidyMark doc= getOption doc TidyMark |> opt_bool
    let setTidyMark doc opt= setOption doc TidyMark (Bool opt)

    let getGnuEmacs doc= getOption doc TidyEmacs |> opt_bool
    let setGnuEmacs doc opt= setOption doc TidyEmacs (Bool opt)

    let getGnuEmacsFile doc= getOption doc TidyEmacsFile |> opt_str
    let setGnuEmacsFile doc opt= setOption doc TidyEmacsFile (Str opt)

    let getLiteralAttributes doc= getOption doc TidyLiteralAttribs |> opt_bool
    let setLiteralAttributes doc opt= setOption doc TidyLiteralAttribs (Bool opt)

    let getShowBodyOnly doc= getOption doc TidyBodyOnly |> opt_int
    let setShowBodyOnly doc opt= setOption doc TidyBodyOnly (Int opt)

    let getFixUri doc= getOption doc TidyFixUri |> opt_bool
    let setFixUri doc opt= setOption doc TidyFixUri (Bool opt)

    let getLowerLiterals doc= getOption doc TidyLowerLiterals |> opt_bool
    let setLowerLiterals doc opt= setOption doc TidyLowerLiterals (Bool opt)

    let getHideComments doc= getOption doc TidyBodyOnly |> opt_bool
    let setHideComments doc opt= setOption doc TidyBodyOnly (Bool opt)

    let getIndentCdata doc= getOption doc TidyIndentCdata |> opt_bool
    let setIndentCdata doc opt= setOption doc TidyIndentCdata (Bool opt)

    let getForceOutput doc= getOption doc TidyForceOutput |> opt_bool
    let setForceOutput doc opt= setOption doc TidyForceOutput (Bool opt)

    let getShowErrors doc= getOption doc TidyShowErrors |> opt_int
    let setShowErrors doc opt= setOption doc TidyShowErrors (Int opt)

    let getAsciiChars doc= getOption doc TidyAsciiChars |> opt_bool
    let setAsciiChars doc opt= setOption doc TidyAsciiChars (Bool opt)

    let getJoinClasses doc= getOption doc TidyJoinClasses |> opt_bool
    let setJoinClasses doc opt= setOption doc TidyJoinClasses (Bool opt)

    let getJoinStyles doc= getOption doc TidyJoinStyles |> opt_bool
    let setJoinStyles doc opt= setOption doc TidyJoinStyles (Bool opt)

    let getEscapeCdata doc= getOption doc TidyEscapeCdata |> opt_bool
    let setEscapeCdata doc opt= setOption doc TidyEscapeCdata (Bool opt)

    let getLanguage doc= getOption doc TidyLanguage |> opt_str
    let setLanguage doc opt= setOption doc TidyLanguage (Str opt)

    let getNcr doc= getOption doc TidyNCR |> opt_bool
    let setNcr doc opt= setOption doc TidyNCR (Bool opt)

    let getOutputBom doc= getOption doc TidyOutputBOM |> opt_int
    let setOutputBom doc opt= setOption doc TidyOutputBOM (Int opt)

    let getReplaceColor doc= getOption doc TidyReplaceColor |> opt_bool
    let setReplaceColor doc opt= setOption doc TidyReplaceColor (Bool opt)

    let getCssPrefix doc= getOption doc TidyCSSPrefix |> opt_str
    let setCssPrefix doc opt= setOption doc TidyCSSPrefix (Str opt)

    let getNewInlineTags doc= getOption doc TidyInlineTags |> opt_str
    let setNewInlineTags doc opt= setOption doc TidyInlineTags (Str opt)

    let getNewBlocklevelTags doc= getOption doc TidyBlockTags |> opt_str
    let setNewBlocklevelTags doc opt= setOption doc TidyBlockTags (Str opt)

    let getNewEmptyTags doc= getOption doc TidyEmptyTags |> opt_str
    let setNewEmptyTags doc opt= setOption doc TidyEmptyTags (Str opt)

    let getNewPreTags doc= getOption doc TidyPreTags |> opt_str
    let setNewPreTags doc opt= setOption doc TidyPreTags (Str opt)

    let getAccessibilityCheck doc= getOption doc TidyAccessibilityCheckLevel |> opt_int
    let setAccessibilityCheck doc opt= setOption doc TidyAccessibilityCheckLevel (Int opt)

    let getVerticalSpace doc= getOption doc TidyVertSpace |> opt_bool
    let setVerticalSpace doc opt= setOption doc TidyVertSpace (Bool opt)

    let getPunctuationWrap doc= getOption doc TidyPunctWrap |> opt_bool
    let setPunctuationWrap doc opt= setOption doc TidyPunctWrap (Bool opt)

    let getMergeEmphasis doc= getOption doc TidyMergeEmphasis |> opt_bool
    let setMergeEmphasis doc opt= setOption doc TidyMergeEmphasis (Bool opt)

    let getMergeDivs doc= getOption doc TidyMergeDivs |> opt_int
    let setMergeDivs doc opt= setOption doc TidyMergeDivs (Int opt)

    let getDecorateInferredUl doc= getOption doc TidyDecorateInferredUL |> opt_bool
    let setDecorateInferredUl doc opt= setOption doc TidyDecorateInferredUL (Bool opt)

    let getPreserveEntities doc= getOption doc TidyPreserveEntities |> opt_bool
    let setPreserveEntities doc opt= setOption doc TidyPreserveEntities (Bool opt)

    let getSortAttributes doc= getOption doc TidySortAttributes |> opt_int
    let setSortAttributes doc opt= setOption doc TidySortAttributes (Int opt)

    let getMergeSpans doc= getOption doc TidyMergeSpans |> opt_int
    let setMergeSpans doc opt= setOption doc TidyMergeSpans (Int opt)

    let getAnchorAsName doc= getOption doc TidyAnchorAsName |> opt_bool
    let setAnchorAsName doc opt= setOption doc TidyAnchorAsName (Bool opt)
  end

  include Raw

  let getNewInlineTags doc=
    Raw.getNewInlineTags doc |> String.split ~on:' '
  let setNewInlineTags doc opt=
    String.concat ~sep:" " opt |> Raw.setNewInlineTags doc

  let getNewBlocklevelTags doc=
    Raw.getNewBlocklevelTags doc |> String.split ~on:' '
  let setNewBlocklevelTags doc opt=
    String.concat ~sep:" " opt |> Raw.setNewBlocklevelTags doc

  let getNewEmptyTags doc=
    Raw.getNewEmptyTags doc |> String.split ~on:' '
  let setNewEmptyTags doc opt=
    String.concat ~sep:" " opt |> Raw.setNewEmptyTags doc

  let getNewPreTags doc=
    Raw.getNewPreTags doc |> String.split ~on:' '
  let setNewPreTags doc opt=
    String.concat ~sep:" " opt |> Raw.setNewPreTags doc

  let getCharEncoding= Raw.getCharEncoding
  let setCharEncoding= Raw.setCharEncoding

  let getInputEncoding= Raw.getInputEncoding
  let setInputEncoding= Raw.setInputEncoding

  let getOutputEncoding= Raw.getOutputEncoding
  let setOutputEncoding= Raw.setOutputEncoding
end

module Attr = struct
  let attrGetId {doc; attr}= Stub.attrGetId attr
  let attrIsEvent {doc; attr}= Stub.attrIsEvent attr

  let attrGetById {doc; node} attrId=
    match Stub.attrGetById node attrId with
    | Some attr-> Some { doc; attr }
    | None-> None
end

module DocTree = struct
  let getRoot doc= {doc; node= Stub.getRoot doc}
  let getHtml doc= {doc; node= Stub.getHtml doc}
  let getHead doc= {doc; node= Stub.getHead doc}
  let getBody doc= {doc; node= Stub.getBody doc}

  let getParent {doc; node}=
    match Stub.getParent node with
    | Some node -> Some {doc;node}
    | None-> None

  let getChildren {doc; node}=
    let rec get_children node=
      match Stub.getNext node with
      | Some node-> {doc; node} :: get_children node
      | None-> []
    in
    match Stub.getChild node with
    | Some node -> {doc; node} :: get_children node
    | None-> []

  let getAttrs {doc; node}=
    let rec get_attrs attr map=
      match Stub.attrNext attr with
      | Some attr-> get_attrs attr
        (String.Map.set map
          ~key:(Stub.attrName attr)
          ~data:(Stub.attrValue attr))
      | None-> map
    in
    match Stub.attrFirst node with
    | Some attr-> get_attrs attr
        (String.Map.singleton (Stub.attrName attr) (Stub.attrValue attr))
    | None-> String.Map.empty
end


module Node = struct
  let getType {doc; node}= Stub.nodeGetType node
  let getName {doc; node}= let open Stub in
    match nodeGetType node with
    | Node_Root-> "root"
    | Node_DocType-> "doctype"
    | Node_Comment-> "comment"
    | Node_ProcIns-> "processing instruction"
    | Node_Text-> "text"
    | Node_Start | Node_End | Node_StartEnd-> nodeGetName node
    | Node_CDATA-> "cdata"
    | Node_Section-> "xml section"
    | Node_Asp-> "asp"
    | Node_Jste-> "jste"
    | Node_Php-> "php"
    | Node_XmlDecl-> "xml declaration"

  let isText {doc; node}= Stub.nodeIsText node
  let isProp {doc; node}= Stub.nodeIsProp doc node
  let isHeader {doc; node}= Stub.nodeIsHeader node
  let hasText {doc; node}= Stub.nodeHasText doc node
  let getValue {doc; node}= Stub.nodeGetValue doc node
  let getValue_exn node=
    match getValue node with
    | Some value-> value
    | None-> raise Caml.Not_found
  let getId {doc; node}= Stub.nodeGetId node
  let getText {doc; node}= Stub.nodeGetText doc node
  let getText_exn node=
    match getText node with
    | Some text-> text
    | None-> raise Caml.Not_found

  let line {doc; node}= Stub.nodeLine node
  let column {doc; node}= Stub.nodeColumn node

  let rec extractText node=
    match getType node with
    | Node_Text-> [Option.value (getText node) ~default:""]
    | Node_Start | Node_End | Node_StartEnd->
      DocTree.getChildren node
        |> List.map ~f:extractText
        |> List.concat
    | _-> []
end

module Tree = struct
  type index= {
    byType: node list String.Map.t;
    byAttr: node list String.Map.t;
  }

  let rec generateIndex node=
    if Node.isText node then
      { byType= String.Map.singleton "text" [node];
        byAttr= String.Map.empty}
    else
      let name= Node.getName node
      and attrs= DocTree.getAttrs node
      and childrenIndex= List.map (DocTree.getChildren node) ~f:generateIndex in
      let index= List.fold childrenIndex
        ~init:{byType= String.Map.empty; byAttr= String.Map.empty}
        ~f:(fun acc index->
          { byType= String.Map.merge acc.byType index.byType
              ~f:(fun ~key value->
                match value with
                | `Both (a, b)-> Some (List.merge a b ~compare:Poly.compare)
                | `Left v-> Some v
                | `Right v-> Some v);
            byAttr= String.Map.merge acc.byAttr index.byAttr
              ~f:(fun ~key value->
                match value with
                | `Both (a, b)-> Some (List.merge a b ~compare:Poly.compare)
                | `Left v-> Some v
                | `Right v-> Some v)})
      in
      { byType= String.Map.add_multi index.byType ~key:name ~data:node;
        byAttr= List.fold (String.Map.keys attrs)
          ~init:index.byAttr
          ~f:(fun acc attr-> String.Map.add_multi acc ~key:attr ~data:node)
      }

  let find index attr value=
    let _value= value in
    let open Option in
    String.Map.find index.byAttr attr
      >>| (List.filter
        ~f:(fun node->
          Option.value
            (let attrs= DocTree.getAttrs node in
            String.Map.find attrs attr >>| (Poly.equal _value))
            ~default:false))
      |> value ~default:[]
end


let create= Stub.create

let parseFile doc filePath=
  match Stub.parseFile doc filePath with
  | Stub.Success | Stub.Td_warning | Stub.Td_error -> ()
  | Stub.Sv_error-> failwith (sprintf "can't parse file %s" filePath)

let parseString doc str=
  match Stub.parseString doc str with
  | Stub.Success | Stub.Td_warning | Stub.Td_error -> ()
  | Stub.Sv_error-> failwith "sv_error"

let cleanAndRepair= Stub.cleanAndRepair
let reportDoctype= Stub.reportDoctype
let runDiagnostics doc=
  cleanAndRepair doc;
  Stub.cleanAndRepair doc

let saveFile= Stub.saveFile
let saveString= Stub.saveString

let accessWarningCount= Stub.accessWarningCount
let configErrorCount= Stub.configErrorCount
let detectedGenericXml= Stub.detectedGenericXml
let detectedHtmlVersion= Stub.detectedHtmlVersion
let detectedXhtml= Stub.detectedXhtml
let errorCount= Stub.errorCount
let errorSummary= Stub.errorSummary
let fileExists= Stub.fileExists
let generalInfo= Stub.generalInfo
let libraryVersion= Stub.libraryVersion
let loadConfig= Stub.loadConfig
let warningCount= Stub.warningCount

