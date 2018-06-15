SeverityNames=(Success=0x0:STATUS_SEVERITY_SUCCESS
               Informational=0x1:STATUS_SEVERITY_INFORMATIONAL
               Warning=0x2:STATUS_SEVERITY_WARNING
               Error=0x3:STATUS_SEVERITY_ERROR
              )
FacilityNames=(System=0x0:FACILITY_SYSTEM
              )
LanguageNames=(English=0x409:MSG00409)

;

MessageIdTypedef=WORD
MessageId=0x1
SymbolicName=DEFAULT_CATEGORY
Language=English
Default Category
.

;

MessageIdTypedef=DWORD

MessageId=0x1
Severity=Error
Facility=System
SymbolicName=MSG_ERROR
Language=English
%1
.

MessageId=0x2
Severity=Warning
Facility=System
SymbolicName=MSG_WARNING
Language=English
%1
.

MessageId=0x3
Severity=Informational
Facility=System
SymbolicName=MSG_INFORMATIONAL
Language=English
%1
.

MessageId=0x4
Severity=Success
Facility=System
SymbolicName=MSG_SUCCESS
Language=English
%1
.

