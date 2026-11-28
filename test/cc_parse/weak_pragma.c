#pragma weak function_alias = function_target
extern int function_alias(void);
static int function_target(void) { return 7; }
#pragma weak function_alias = function_target

int object_target = 42;
_Pragma("weak object_alias = object_target")
extern int object_alias;
