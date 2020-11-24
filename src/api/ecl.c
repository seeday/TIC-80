#include "api.h"
#include "core/core.h"

#if defined(TIC_BUILD_WITH_ECL)

#include <ecl/ecl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void init_eclapi(cl_object);

static const char *const EclKeywords[] = {
  "defun",  "defvar", "defconstant", "setq", "setf", "defmacro", "if",   "let",  "not",
    "lambda", "flet", "car",  "cdr", "cadr", "cons", "nth-value"};
static const char *const FakeArgv[] = {"tic80"};

static bool initEcl(tic_mem *tic, const char *code) {
  tic_core *core = (tic_core *)tic;

  if (ecl_get_option(ECL_OPT_BOOTED)) {
    cl_shutdown();
  }

  printf("INITTING ECL\n");
  ECL_WITH_LISP_FPE_BEGIN {
    cl_boot(1, FakeArgv); // can't pass NULL here
    ecl_init_module(NULL, init_eclapi);
    cl_eval(c_string_to_object("(in-package :cl-user)"));
    cl_eval(c_string_to_object("(shadowing-import 'tic80:print)"));
    cl_eval(c_string_to_object("(use-package :tic80)"));
    cl_eval(c_string_to_object("(setf *debugger-hook* nil)"));
  }
  ECL_WITH_LISP_FPE_END;
  core->ecl = ecl_process_env();
  ecl_defvar(ecl_make_symbol("*TICCORE*", "TIC80"),
             ecl_make_uint64_t((uint64_t)core));

  ECL_CATCH_ALL_BEGIN(core->ecl) {
    /*
     * Code that is protected. Uncaught lisp conditions, THROW,
     * signals such as SIGSEGV and SIGBUS may cause jump to
     * this region.
     */
    char *wrapped = calloc(strlen(code) + 8 + 2, sizeof(char));
    strncat(wrapped, "(progn\n ", 8);
    strcat(wrapped, code);
    strncat(wrapped, "\n)", 2);

    ECL_WITH_LISP_FPE_BEGIN { cl_eval(c_string_to_object(wrapped)); }
    ECL_WITH_LISP_FPE_END;
    return true;
  }
  ECL_CATCH_ALL_IF_CAUGHT {
    /*
     * If the exception, lisp condition or other control transfer
     * is caught, this code is executed.
     */
    core->data->error(core->data->data, "ERROR");
    return false;
  }
  ECL_CATCH_ALL_END;
}

static void closeEcl(tic_mem *tic) {
  printf("Destroying ecl context\n");
  cl_shutdown();
  ((tic_core *)tic)->ecl = NULL;
}

static void callEclTick(tic_mem *tic) {
  tic_core *core = (tic_core *)tic;
  cl_object ticfun = ecl_make_symbol(TIC_FN, "CL-USER");
  ECL_CATCH_ALL_BEGIN(ecl_process_env()) {
    ECL_WITH_LISP_FPE_BEGIN { cl_funcall(1, ticfun); }
    ECL_WITH_LISP_FPE_END;
  }
  ECL_CATCH_ALL_IF_CAUGHT { core->data->error(core->data->data, "ERROR"); }
  ECL_CATCH_ALL_END;
}

static void callEclScanline(tic_mem *tic, int row, void *data) {
  cl_object ticfun = ecl_make_symbol(SCN_FN, "COMMON-LISP-USER");
  ECL_WITH_LISP_FPE_BEGIN { cl_funcall(1, ticfun); }
  ECL_WITH_LISP_FPE_END;
}

static void callEclOverline(tic_mem *tic, void *data) {
  cl_object ticfun = ecl_make_symbol(OVR_FN, "COMMON-LISP-USER");
  ECL_WITH_LISP_FPE_BEGIN { cl_funcall(1, ticfun); }
  ECL_WITH_LISP_FPE_END;
}

static const tic_outline_item *getEclOutline(const char *code, s32 *size) {
  *size = 0;
  return NULL;
}

//#define ECL_FN_DEF(NAME, argc, ret, ticp, args...) { \
//  ret ecl_api_ ## NAME ## (args) { \
//    tic_mem *tic = (tic_mem *)ecl_to_uint64_t(ecl_symbol_value(ecl_make_symbol("*TICCORE*", "CL-USER"))); \
//    tic_api_#name#(tic, args); \
//  } \
//  }
//
////TIC_API_LIST(ECL_FN_DEF)
// ECL_FN_DEF(print, 7, s32, tic_mem*, const char* text)
//#undef ECL_FN_DEF

static void evalEcl(tic_mem *tic, const char *code) {
  printf("Attempting to load code \n%s\n", code);
}

static const tic_script_config EclSyntaxConfig = {
    .init = initEcl,
    .close = closeEcl,
    .tick = callEclTick,
    .scanline = callEclScanline,
    .overline = callEclOverline,

    .getOutline = getEclOutline,
    .eval = evalEcl,

    .singleComment = ";",
    .blockCommentStart = "#|",
    .blockCommentEnd = "|#",

    .keywords = EclKeywords,
    .keywordsCount = COUNT_OF(EclKeywords),
};

const tic_script_config *getEclScriptConfig() { return &EclSyntaxConfig; }
#endif
