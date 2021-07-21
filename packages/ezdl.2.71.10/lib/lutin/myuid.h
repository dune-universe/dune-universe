/* Time-stamp: <modified the 13/12/2017 (at 17:48) by Erwan Jahier> */

// dllcamlidl.so requires that those are defined
// From what I understand, it's only used on windows (finger crossed)
typedef struct { unsigned char data[16]; } IID;

IID IID_IUnknown, IID_IX, IID_IY;
