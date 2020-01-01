#include <stdio.h>
#include <girepository.h>

/**
 * Compile with
 * gcc $(pkg-config --libs --cflags gobject-introspection-1.0) -o gi_function_flags gi_function_flags.c
 */

static void
print_function_flags (void)
{
    printf("IS_METHOD %d\n", GI_FUNCTION_IS_METHOD);
    printf("IS_CONSTRUCTOR %d\n", GI_FUNCTION_IS_CONSTRUCTOR);
    printf("IS_GETTER %d\n", GI_FUNCTION_IS_GETTER);
    printf("IS_SETTER %d\n", GI_FUNCTION_IS_SETTER);
    printf("WRAPS_VFUNC %d\n", GI_FUNCTION_WRAPS_VFUNC);
    printf("THROWS %d\n", GI_FUNCTION_THROWS);
}

int main (int argc, char **argv)
{
    const char *namespace = "GObject";
    const char *name = "Value";
    GIBaseInfo *info;
    GIFunctionInfoFlags c_flags;

    GIRepository *repo = g_irepository_get_default ();
    g_irepository_require (repo,
                           namespace,
                           NULL,
                           0,
                           NULL);
    printf("Get flags for all function infos\n");
    int n_infos = g_irepository_get_n_infos(repo, namespace);
    int j = 0;
    for(j = 0; j < n_infos; ++j) {
        info = g_irepository_get_info (repo, namespace, j);
        GIInfoType t = g_base_info_get_type (info);
        if(t == GI_INFO_TYPE_FUNCTION ) {
             c_flags = g_function_info_get_flags ((GIFunctionInfo *)info);
              printf("flags = %d\n", c_flags);
             if ((c_flags & GI_FUNCTION_IS_METHOD) != 0 )
                 printf("********************method\n");
             if ((c_flags & GI_FUNCTION_IS_CONSTRUCTOR) != 0 )
                 printf("********************constructor\n");
             if ((c_flags & GI_FUNCTION_IS_GETTER) != 0 )
                 printf("********************getter\n");
             if ((c_flags & GI_FUNCTION_IS_SETTER) != 0 )
                 printf("********************setter\n");
             if ((c_flags & GI_FUNCTION_WRAPS_VFUNC) != 0 )
                 printf("********************wraps vfunc\n");
             if ((c_flags & GI_FUNCTION_THROWS) != 0 )
                 printf("********************throws\n");

        }
    }

    printf("Get flags for all methods of GObject::Value\n");
    info = g_irepository_find_by_name (repo,
                                       namespace,
                                       name);
    int n_methods = 0;
    n_methods =g_struct_info_get_n_methods ((GIStructInfo *)info);

    int i = 0;
    for(i = 0; i < n_methods; ++i) {
        GIFunctionInfo *method;
        method = g_struct_info_get_method ((GIStructInfo *)info, i);
        c_flags = g_function_info_get_flags (method);
         printf("flags = %d\n", c_flags);
        if ((c_flags & GI_FUNCTION_IS_METHOD) != 0 )
            printf("********************method\n");
        if ((c_flags & GI_FUNCTION_IS_CONSTRUCTOR)  != 0)
            printf("********************constructor\n");
        if ((c_flags & GI_FUNCTION_IS_GETTER) != 0 )
            printf("********************getter\n");
        if ((c_flags & GI_FUNCTION_IS_SETTER) != 0 )
            printf("********************setter\n");
        if ((c_flags & GI_FUNCTION_WRAPS_VFUNC) != 0 )
            printf("********************wraps vfunc\n");
        if ((c_flags & GI_FUNCTION_THROWS) != 0 )
            printf("********************throws\n");
    }

    return 0;
}
