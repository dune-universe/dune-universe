/*-------------------------------------------------------------

Une collection de macros qui permet de faire du link
dynamique indifféremment sous unix ou win32

Les macros fournies sont des versions "simplifiées"
de dlopen et dlsym (posix) :

void* ezdlopen(char* n)
   - retourne le handler (NULL si erreur)

void* ezdlsym(void* h, char* n)
   - retourne la fonction n dans h (NULL si erreur)

char* ezdlerror()
   - une explication si et quand il vient juste
   d'y avoir une erreur (indéfini sinon)

Pour utiliser il faut juste inclure : 

#include "ezdl.h"

et, sous win32, compiler avec l'option -DWIN32
(par défaut c'est du posix)

--------------------------------------------------------------*/

#include <stdio.h>

#ifdef WIN32
#include <windows.h>
#define HANDLE HANDLE
#ifndef EZDL_ERROR_BUFF_ALOC
#define EZDL_ERROR_BUFF_ALOC
char EZDL_ERROR_BUFF[1024];
#endif
#define RTLD_LAZY 0
#define ezdlopen(n) ((void*)LoadLibrary(n))
#define ezdlsym(l,n) ((void*)GetProcAddress(l,n))
#define ezdlerror() (sprintf(EZDL_ERROR_BUFF,"win32 dll error %d", GetLastError()),&EZDL_ERROR_BUFF[0])
#define ezdlclose(l) (FreeLibrary(l))
#else
#include <dlfcn.h>
#define ezdlopen(n) (dlopen(n,RTLD_LAZY))
#define ezdlsym(l,n) (dlsym(l,n))
#define ezdlerror() (dlerror())
#define ezdlclose(l) (dlclose(l))
#endif

