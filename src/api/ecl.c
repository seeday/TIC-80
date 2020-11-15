#include "api.h"
#include "core/core.h"

#if defined(TIC_BUILD_WITH_ECL)

#include <ecl/ecl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void init_eclapi(cl_object);

static const char *const EclKeywords[] = {
    "defun", "setq", "setf", "defmacro", "if", "let", "not", "lambda", "flet",
};

static const char *const FakeArgv[] = {"tic80"};

static bool initEcl(tic_mem *tic, const char *code) {
  tic_core *core = (tic_core *)tic;

  printf("INITTING ECL\n");
  cl_boot(1, FakeArgv); // can't pass NULL here
  ecl_init_module(NULL, init_eclapi);
  core->ecl = ecl_process_env();
  ecl_defvar(ecl_make_symbol("*TICCORE*", "CL-USER"),
             ecl_make_uint64_t((uint64_t)core->ecl));
  printf("ECL INIT PASSED, TRYING EVAL\n");

  ECL_CATCH_ALL_BEGIN(core->ecl) {
    /*
     * Code that is protected. Uncaught lisp conditions, THROW,
     * signals such as SIGSEGV and SIGBUS may cause jump to
     * this region.
     */
    // exit(-1);
  }
  ECL_CATCH_ALL_IF_CAUGHT {
    /*
     * If the exception, lisp condition or other control transfer
     * is caught, this code is executed.
     */
  }
  ECL_CATCH_ALL_END;

  char *wrapped = calloc(strlen(code) + 8 + 2, sizeof(char));
  strncat(wrapped, "(progn\n ", 8);
  strcat(wrapped, code);
  strncat(wrapped, "\n)", 2);

  printf("Loading code...\n%s\n", wrapped);
  cl_eval(c_string_to_object(wrapped));
  printf("Code loaded\n");
  free(wrapped);
  return true;
}

static void closeEcl(tic_mem *tic) {
  printf("Destroying ecl context\n");
  cl_shutdown();
  ((tic_core *)tic)->ecl = NULL;
}

static void callEclTick(tic_mem *tic) {
  cl_object ticfun = ecl_make_symbol(TIC_FN, "COMMON-LISP-USER");
  cl_funcall(1, ticfun);
}

static void callEclScanline(tic_mem *tic, int row, void *data) {
  cl_object ticfun = ecl_make_symbol(SCN_FN, "COMMON-LISP-USER");
  cl_funcall(1, ticfun);
}

static void callEclOverline(tic_mem *tic, void *data) {
  cl_object ticfun = ecl_make_symbol(OVR_FN, "COMMON-LISP-USER");
  cl_funcall(1, ticfun);
}

static const tic_outline_item *getEclOutline(const char *code, s32 *size) {
  *size = 0;
  return NULL;
}

void eclApiCls(int color) {
  tic_mem *tic = (tic_mem *)ecl_to_uint64_t(
      ecl_symbol_value(ecl_make_symbol("*TICCORE*", "CL-USER")));
  printf("Lisp clsing with color %d\n", color);
  tic_api_cls(tic, color);
}

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
